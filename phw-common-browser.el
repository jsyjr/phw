;;; phw-common-browser.el --- common browsing stuff for  Emacs

;; Copyright (C) 2000 - 2015 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Ryan Ware,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;;         Ryan Ware <ryan.r.ware@intel.com>
;; Maintainer: Ryan Ware <ryan.r.ware@intel.com>
;; Keywords: browser, code, programming, tools
;; Created: 2004

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PATICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id$


;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.

;;; Code:

(eval-when-compile
  (require 'silentcomp))


(require 'phw-util)

(require 'tree-buffer)
(require 'phw-mode-line)
(require 'phw-navigate)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(silentcomp-defvar modeline-map)

(defgroup phw-tree-buffer nil
  "General settings related to the tree-buffers of PHW."
  :group 'phw
  :prefix "phw-")

(defcustom phw-bucket-node-display '("" "" phw-bucket-node-face)
  "*How PHW displays bucket-nodes in a PHW tree-buffer.
Bucket-nodes have only one job: Nodes with similar properties will be dropped
into one bucket for such a common property and all these nodes will be added
as children to the bucket-node. Besides being expandable and collapsable a
bucket-node has no senseful action assigned. Examples for bucket-nodes are
\"[+] Variables\", \"[+] Dependencies\" etc. in the Methods-buffer or buckets
which combine filenames with same extension under a bucket-node with name this
extension.

This option defines how bucket-node should be displayed. The name of the
bucket-node is computed by PHW but you can define a prefix, a suffix and a
special face for the bucket-node

The default are empty prefix/suffix-strings and 'phw-bucket-node-face'. But
an alternative can be for example '\(\"[\" \"]\" nil) which means no special
face and a display like \"[+] [<bucket-name>]\"."
  :group 'phw-tree-buffer
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (phw-clear-tag-tree-cache)))
  :type '(list (string :tag "Bucket-prefix" :value "[")
               (string :tag "Bucket-suffix" :value "]")
               (choice :tag "Bucket-face" :menu-tag "Bucket-face"
                       (const :tag "No special face" :value nil)
                       (face :tag "Face" :value phw-bucket-node-face)))
  :initialize 'custom-initialize-default)

(defcustom phw-use-speedbar-instead-native-tree-buffer nil
  "*If true then uses speedbar for directories, sources or methods.
This means that speedbar is integrated in the PHW-frame and is displayed in
that window normally displaying the standard PHW-directories-buffer,
PHW-sources-buffer or PHW-methods-buffer.

This option takes effect in all layouts which contain either a directory
window, a sources window or a method window.

This option can have four valid values:
- nil: Do not use speedbar \(default)
- dir: Use speedbar instead of the standard directories-buffer
- source: Use speedbar instead of the standard sources-buffer
- method: Use speedbar instead of the standard methods-buffer

Note: For directories and sources a similar effect and usability is available
by setting this option to nil \(or 'method) and setting
`phw-show-sources-in-directories-buffer' to not nil, because this combination
displays also directories and sources in one window.

`phw-use-speedbar-instead-native-tree-buffer' is for people who like the
speedbar way handling directories and source-files or methods and want it in
conjunction with PHW."
  :group 'phw-tree-buffer
  :group 'phw-directories
  :group 'phw-sources
  :group 'phw-methods
  :type '(radio (const :tag "Do not use speedbar" :value nil)
                (const :tag "For directories" :value dir)
                (const :tag "For sources" :value source)
                (const :tag "For methods" :value method))
  :initialize 'custom-initialize-default
  :set (function (lambda (sym val)
                   (set sym val)
                   (if (and (boundp 'phw-minor-mode) phw-minor-mode)
                       (phw-redraw-layout-full)))))

(defvar phw-tree-do-not-leave-window-after-select--internal nil
  "Only set by customizing `phw-tree-do-not-leave-window-after-select' or
calling `phw-toggle-do-not-leave-window-after-select'! Do not set this
variable directly, it is only for internal uses!")

(defcustom phw-tree-do-not-leave-window-after-select nil
  "*Tree-buffers which stay selected after a key- or mouse-selection.
If a buffer \(either its name or the variable-symbol which holds the name) is
contained in this list then selecting a tree-node either by RET or by a
mouse-click doesn't leave that tree-buffer after the node-selection but
performes only the appropriate action \(opening a new source, selecting a
method etc.) but point stays in the tree-buffer. In tree-buffers not contained
in this option normaly a node-selection selects as \"last\" action the right
edit-window or maximizes the next senseful tree-buffer in case of a currently
maximized tree-buffer \(see `phw-maximize-next-after-maximized-select').

The buffer-name can either be defined as plain string or with a symbol which
contains the buffer-name as value. The latter one is recommended for the
builtin PHW-tree-buffers because then simply the related option-symbol can be
used.

A special remark for the `phw-directories-buffer-name': Of course here the
value of this option is only relevant if the name of the current layout is
contained in `phw-show-sources-in-directories-buffer' or if the value of
`phw-show-sources-in-directories-buffer' is 'always and the clicked ot hitted
node represents a sourcefile \(otherwise this would not make any sense)!

The setting in this option is only the default for each tree-buffer. With the
command `phw-toggle-do-not-leave-window-after-select' the behavior of a
node-selection can be changed fast and easy in a tree-buffer without
customizing this option, but of course not for future Emacs sessions!"
  :group 'phw-tree-buffer
  :set (function (lambda (sym val)
                   (set sym val)
                   (setq phw-tree-do-not-leave-window-after-select--internal
                         (phw-copy-list val))))
  :type '(repeat (choice :menu-tag "Buffer-name"
                        (string :tag "Buffer-name as string")
                        (symbol :tag "Symbol holding buffer-name"))))

(defcustom phw-tree-indent 4
  "*Indent size for tree buffer.
If you change this during PHW is activated you must deactivate and activate
PHW again to take effect."
  :group 'phw-tree-buffer
  :group 'phw-most-important
  :type 'integer)

(defcustom phw-tree-expand-symbol-before t
  "*Show the expand symbol before the items in a tree.
When the expand-symbol is located before the items then the tree looks like:

\[-] PHW
    \[+] code-save
    \[-] phw-images
        \[-] directories

When located after then the tree looks like:

PHW \[-]
  code-save \[+]
  phw-images \[-]
    directories \[-]

The after-example above use a value of 2 for `phw-tree-indent' whereas the
before-example uses a value of 4.

It is recommended to display the expand-symbol before because otherwise it
could be that with a deep nested item-structure with and/or with long
item-names \(e.g. a deep directory-structure with some long
subdirectory-names) the expand-symbol is not visible in the tree-buffer and
the tree-buffer has to be horizontal scrolled to expand an item."
  :group 'phw-tree-buffer
  :group 'phw-most-important
  :type 'boolean)

;; we can savely set here 'image as default because the tree-buffer-lib checks
;; itslef if image-support is possible and switches back to 'ascii-style if not.
(defcustom phw-tree-buffer-style 'image
  "*The style of the tree-buffers.
There are three different styles available:

Image-style \(value 'image):
Very nice and modern - just try it. For this style the options
`phw-tree-indent' and `phw-tree-expand-symbol-before' have no effect!
The value 'image means use image-style if images can be displayed with current
Emacs-setup \(otherwise auto. 'ascii-style is used).
Note: GNU Emacs <= 21.3.X for Windows does not support image-display so PHW
uses always 'ascii-guides even when here 'image is set!

Ascii-style with guide-lines \(value 'ascii-guides):
\[-] PHW
 |  \[+] code-save
 `- \[-] phw-images
     |  \[-] directories
     |   |  \[-] height-15
     |   |   |  * close.xpm
     |   |   |  * empty.xpm
     |   |   |  * leaf.xpm
     |   |   `- * open.xpm
     |   |  \[+] height-17
     |   |  \[+] height-19
     |   `- \[+] height-21
     |  \[x] history
     |  \[x] methods
     `- \[x] sources

Ascii-style without guide-lines \(value 'ascii-no-guides) - this is the style
used by PHW <= 1.96:
\[-] PHW
    \[+] code-save
    \[-] phw-images
        \[-] directories
            \[-] height-15
                * close.xpm
                * empty.xpm
                * leaf.xpm
                * open.xpm
            \[+] height-17
            \[+] height-19
            \[+] height-21
        \[x] history
        \[x] methods
        \[x] sources

With both ascii-styles the tree-layout can be affected with the options
`phw-tree-indent' and `phw-tree-expand-symbol-before'."
  :group 'phw-tree-buffer
  :group 'phw-most-important
  :type '(radio (const :tag "Images-style" :value image)
                (const :tag "Ascii-style with guide-lines" :value ascii-guides)
                (const :tag "Ascii-style w/o guide-lines" :value ascii-no-guides)))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: add here the analyse buffer if
;; additonal images are necessary - but currently i don't think we need
;; special images for this analyse-stuff.
(defcustom phw-tree-image-icons-directories
  (let ((base (concat (if phw-regular-xemacs-package-p
                          (format "%s" (locate-data-directory "phw"))
                        phw-phw-dir)
                      "phw-images/")))
    (cons (concat base "default/height-17")
          (mapcar (function (lambda (i)
                              (cons (car i) (concat base (cdr i)))))
                  '((phw-directories-buffer-name . "directories/height-17")
                    (phw-sources-buffer-name . "sources/height-14_to_21")
                    (phw-methods-buffer-name . "methods/height-14_to_21")))))
  "*Directories where the images for the tree-buffer can be found.
This is a cons cell where:

car: Default directory where the default images for the tree-buffer can be
found. It should contain an image for every name of
`tree-buffer-tree-image-names'. The name of an image-file must be:
\"phw-<NAME of TREE-BUFFER-TREE-IMAGE-NAMES>.<ALLOWED EXTENSIONS>\".

cdr: This is a list where each element is a cons again with: car is the buffer
name of the tree-buffer for which a special image-path should be used. The
buffer-name can either be defined as plain string or with a symbol which
contains the buffer-name as value. The latter one is recommended for the
builtin PHW-tree-buffers because then simply the related option-symbol can be
used \(e.g. the symbol `phw-directories-buffer-name'). The cdr is the the
full-path of an additional image-directorie which is searched first for images
needed for the related tree-buffer. If the image can not be found in this
directory then the default-directory \(see above) is searched. If the
image can't even be found there the related ascii-symbol is used - which is
defined in `tree-buffer-tree-image-names'. If a tree-buffer is not contained
in this list then there is no additional special image-directory for it.

PHW comes with predefined images in several different heights - so for the
most senseful font-heights of a tree-buffer a fitting image-size should be
available. The images reside either in the subdirectory \"phw-images\" of the
PHW-installation or - if PHW is installed as regular XEmacs-package - in the
PHW-etc data-directory \(the directory returned by \(locate-data-directory
\"phw\")."
  :group 'phw-tree-buffer
  :type '(cons (directory :tag "Full default image-path")
               (repeat (cons (choice :menu-tag "Buffer-name"
                                     (string :tag "Buffer-name as string")
                                     (symbol :tag "Symbol holding
                                     buffer-name"))                             
                             (directory :tag "Full image-path for this tree-buffer")))))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: rename this to phw-truncate-lines.
(defcustom phw-tree-truncate-lines '(phw-directories-buffer-name
                                     phw-sources-buffer-name
                                     phw-methods-buffer-name
                                     phw-history-buffer-name
                                     phw-analyse-buffer-name)
  "*Truncate lines in PHW buffers.
If a buffer \(either its name or the variable-symbol which holds the name) is
contained in this list then line-truncation is switched on for this buffer
otherwise it is off.

The buffer-name can either be defined as plain string or with a symbol which
contains the buffer-name as value. The latter one is recommended to switch on
line-truncation for one of the builtin PHW-tree-buffers because then simply
the related option-symbol can be used. To truncate lines in the builtin
directories tree-buffer just add the symbol `phw-directories-buffer-name' to
this option.

If you change this during PHW is activated you must deactivate and activate
PHW again to take effect."
  :group 'phw-tree-buffer
  :group 'phw-most-important
  :type '(repeat (choice :menu-tag "Buffer-name"
                         (string :tag "Buffer-name as string")
                         (symbol :tag "Symbol holding buffer-name"))))

(defcustom phw-tree-easy-hor-scroll 5
  "*Scroll step for easy hor. scrolling via mouse-click in tree-buffers.
XEmacs has horizontal scroll-bars so invisible parts beyond the right
window-border of a tree-buffer can always made visible very easy.

GNU Emacs does not have hor. scroll-bars so especially with the mouse it is
quite impossible to scroll smoothly right and left. The functions
`scroll-left' and `scroll-right' can be annoying and are also not bound to
mouse-buttons.

If this option is a positive integer S then in all PHW-tree-buffers the keys
\[M-mouse-1] and \[M-mouse-3] are bound to scrolling left rsp. right with
scroll-step S - clicking with mouse-1 or mouse-2 onto the edge of the modeline
has the same effect, i.e. if you click with mouse-1 onto the left \(rsp.
right) edge of the modeline you will scroll left \(rsp. right). Additionally
\[C-M-mouse-1] and \[C-M-mouse-3] are bound to scrolling left rsp. right with
scroll-step `window-width' - 2. Default is a scroll-step of 5. If the value is
nil then no keys for horizontal scrolling are bound."
  :group 'phw-tree-buffer
  :type '(radio :value 5
                (const :tag "No hor. mouse scrolling" :value nil)
                (integer :tag "Scroll step")))


(defcustom phw-tree-make-parent-node-sticky t
  "*Make the parent-node sticky in the headerline of the tree-buffer.

If not nil then the first line of the tree-buffer is used as header-line which
is used to display the next unvisible parent of the first visible node as
sticky, so always the parent of a node is visible and clickable. If a node has
no parent then just the next node above is displayed in the header-line. The
node displayed in the header-line is exactly in the same manner clickable as
all other nodes.

See also `phw-tree-stickynode-indent-string'.

This feature is only available with Gnu Emacs, not with XEmacs."
  :group 'phw-tree-buffer
  :group 'phw-most-important
  :type 'boolean)

(defcustom phw-tree-stickynode-indent-string (tree-buffer-sticky-default-indent-string)
  "*String used to indent the stickynode.
This string is used to match the space used by scrollbars and
fringe so it does not appear that the node-name is moving left/right
when it lands in the sticky line.

Normally the needed value is computed automatically by PHW. But if the result
is not matching this option allows to customize the indent-string.
The default value is computed by the function
`tree-buffer-sticky-default-indent-string', so you can change the needed value
with that starting-point.

Changing this option takes only effect after restarting Emacs!"
  :group 'phw-tree-buffer
  :type 'string)
  

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe we should change this to a
;; type analogous to phw-tree-truncate-lines
(defcustom phw-truncate-long-names t
  "*Truncate long names that don't fit in the width of the PHW windows.
If you change this during PHW is activated you must deactivate and activate
PHW again to take effect."
  :group 'phw-tree-buffer
  :type 'boolean)

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe we should change this to a
;; type analogous to phw-tree-truncate-lines
(defcustom phw-tree-incremental-search 'prefix
  "*Enable incremental search in the PHW-tree-buffers.
For a detailed explanation see the online help section \"Working with the
keyboard in the PHW buffers\". If you change this during PHW is activated you
must deactivate and activate PHW again to take effect."
  :group 'phw-tree-buffer
  :type '(radio (const :tag "Match only prefix"
                       :value prefix)
                (const :tag "Match every substring"
                       :value substring)
                (const :tag "No incremental search"
                       :value nil)))

(defcustom phw-tree-navigation-by-arrow t
  "*Enable smart navigation in the tree-windows by horizontal arrow-keys.
If not nil then the left- and right-arrow keys work in the PHW tree-window in
the following smart way if onto an expandable node:
+ Left-arrow: If node is expanded then it will be collapsed otherwise point
  jumps to the next \"higher\" node in the hierarchical tree \(higher means
  the next higher tree-level or - if no higher level available - the next
  higher node on the same level).
+ Right-arrow: If node is not expanded then it will be expanded.
Onto a not expandable node the horizontal arrow-keys go one character in the
senseful correct direction.

If this option is changed the new value takes first effect after deactivating
PHW and then activating it again!"
  :group 'phw-tree-buffer
  :type 'boolean)

(defun phw-show-any-node-info-by-mouse-moving-p ()
  "Return not nil if for at least one tree-buffer showing node info only by
moving the mouse over a node is activated. See
`phw-directories-show-node-info' etc...."
  (let ((when-list (mapcar (function (lambda (elem)
                                       (car (symbol-value elem))))
                           '(phw-directories-show-node-info
                             phw-sources-show-node-info
                             phw-methods-show-node-info
                             phw-history-show-node-info
                             phw-analyse-show-node-info
                             ))))
    (or (member 'if-too-long when-list)
        (member 'always when-list))))

(defcustom phw-primary-secondary-mouse-buttons 'mouse-2--C-mouse-2
  "*Primary- and secondary mouse button for using the PHW-buffers.
A click with the primary button causes the main effect in each PHW-buffer:
- PHW Directories: Expanding/collapsing nodes and displaying files in the PHW
  Sources buffer.
- PHW sources/history: Opening the file in that edit-window specified by the
  option `phw-mouse-click-destination'.
- PHW Methods: Jumping to the method in that edit-window specified by the
  option `phw-mouse-click-destination'.

A click with the primary mouse-button while the SHIFT-key is pressed called
the POWER-click and does the following \(depending on the PHW-buffer where the
POWER-click occurs):
+ Directory-buffer: Refreshing the directory-contents-cache \(see
  `phw-cache-directory-contents').
+ Sources- and History-buffer: Only displaying the source-contents in the
  method-buffer but not displaying the source-file in the edit-window.
+ Methods-buffer: Narrowing to the clicked method/variable/ect... \(see
  `phw-tag-visit-post-actions'). This works only for sources supported by
  semantic!

In addition always the whole node-name is displayed in the minibuffer after a
POWER-click \(for this see `phw-directories-show-node-info' etc...).

The secondary mouse-button is for opening \(jumping to) the file in another
edit-window \(see the documentation `phw-mouse-click-destination').

The following combinations are possible:
- primary: mouse-2, secondary: C-mouse-2 \(means mouse-2 while CTRL-key is
  pressed). This is the default setting.
- primary: mouse-1, secondary: C-mouse-1
- primary: mouse-1, secondary: mouse-2

Note: If the tree-buffers are used with the keyboard instead with the mouse
then [RET] is interpreted as primary mouse-button and [C-RET] as secondary
mouse-button!

If you change this during PHW is activated you must deactivate and activate
PHW again to take effect!"
  :group 'phw-tree-buffer
  :group 'phw-most-important
  :type '(radio (const :tag "Primary: mouse-2, secondary: Ctrl-mouse-2"
                       :value mouse-2--C-mouse-2)
                (const :tag "Primary: mouse-1, secondary: Ctrl-mouse-1"
                       :value mouse-1--C-mouse-1)
                (const :tag "Primary: mouse-1, secondary: mouse-2"
                       :value mouse-1--mouse-2)))

(defcustom phw-tree-mouse-action-trigger 'button-release
  "*When the tree-buffer mouse-action should be triggered.
This option determines the moment a mouse-action in a tree-buffer is
triggered. This can be either direct after pressing a mouse-button \(value
'button-press) or not until releasing the mouse-button \(value:
'button-release).

If you change this during PHW is activated you must deactivate and activate
PHW again to take effect!"
  :group 'phw-tree-buffer
  :type '(radio (const :tag "After button release" :value button-release)
                (const :tag "After button press" :value button-press)))

(defcustom phw-mouse-click-destination 'last-point
  "*Destination of a mouse-button click.
Defines in which edit-window \(if splitted) PHW does the \"right\" action
\(opening a source, jumping to a method/variable etc.) after clicking with a
mouse-button \(see `phw-primary-secondary-mouse-buttons') onto a node. There
are two possible choices:
- left-top: Does the \"right\" action always in the left/topmost edit-window.
- last-point: Does the \"right\" action always in that edit-window which had
  the point before.
This is if the user has clicked either with the primary mouse-button or
has activated a popup-menu in the tree-buffer.

A click with the secondary mouse-button \(see again
`phw-primary-secondary-mouse-buttons') does the \"right\" action always in
another edit-window related to the setting in this option: If there are two
edit-windows then the \"other\" edit-window is used and for more than 2
edit-windows the \"next\" edit-window is used \(whereas the next edit-window
of the last edit-window is the first edit-window).

If the edit-window is not splitted this setting has no effect.

Note: If the tree-buffers are used with the keyboard instead with the mouse
then this option takes effect too because [RET] is interpreted as primary
mouse-button and [C-RET] as secondary mouse-button!"
  :group 'phw-tree-buffer
  :group 'phw-most-important
  :type '(radio (const :tag "Left/topmost edit-window"
                       :value left-top)
                (const :tag "Last edit-window with point"
                       :value last-point)))


(defcustom phw-common-tree-buffer-after-create-hook nil
  "*Local hook running at the end of each tree-buffer creation.
Every function of this hook is called once without arguments direct after
creating a tree-buffer of PHW and it's local key-map. So for example a function
could be added which performs calls of `local-set-key' to define new
key-bindings for EVERY tree-buffer.

The following keys must not be rebind in all tree-buffers:
- <RET> and all combinations with <Shift> and <Ctrl>
- <TAB>
- `C-t'"
  :group 'phw-tree-buffer
  :type 'hook)

(defcustom phw-basic-buffer-sync '(Info-mode dired-mode)
  "*Synchronize the basic PHW-buffers automatically with current edit buffer.

The basic PHW-buffers are the buffers for directories, sources, methods and
history.

If 'always then the synchronization takes place always a buffer changes in the
edit window, if nil then never. If a list of major-modes then only if the
`major-mode' of the new buffer belongs NOT to this list.

But in every case the synchronization takes only place if the current-buffer
in the edit-window has a relation to files or directories. Examples for the
former one are all programming-language-modes, `Info-mode' too, an example for
the latter one is `dired-mode'. For all major-modes related to
non-file/directory-buffers like `help-mode', `customize-mode' and others never
an autom. synchronization will be done!

It's recommended to exclude at least `Info-mode' because it makes no sense to
synchronize the PHW-windows after calling the Info help. Per default also
`dired-mode' is excluded but it can also making sense to synchronize the
PHW-directories/sources windows with the current directory in the
dired-buffer.

IMPORTANT NOTE: Every time the synchronization is done the hook
`phw-basic-buffer-sync-hook' is evaluated."
  :group 'phw-tree-buffer
  :type '(radio :tag "Synchronize basic PHW buffers"
                (const :tag "Always" :value always)
                (const :tag "Never" nil)
                (repeat :tag "Not with these modes"
                        (symbol :tag "mode"))))

(defcustom phw-basic-buffer-sync-delay 0.25
  "*Time Emacs must be idle before the special PHW-buffers are synchronized.
Synchronizing is done with the current source displayed in the edit window. If
nil then there is no delay, means synchronization takes place immediately. A
small value of about 0.25 seconds saves CPU resources and you get even though
almost the same effect as if you set no delay."
  :group 'phw-tree-buffer
  :type '(radio (const :tag "No synchronizing delay"
                       :value nil)
                (number :tag "Idle time before synchronizing"
                        :value 0.25))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if phw-minor-mode
                       (phw-activate-phw-autocontrol-function
                        value 'phw-basic-buffer-sync))))
  :initialize 'custom-initialize-default)

(defcustom phw-basic-buffer-sync-hook nil
  "*Hook run at the end of `phw-basic-buffer-sync'.
See documentation of `phw-basic-buffer-sync' for conditions when
synchronization takes place and so in turn these hooks are evaluated.

Precondition for such a hook:
Current buffer is the buffer of the currently selected edit-window.

Postcondition for such a hook:
Point must stay in the same edit-window as before evaluating the hook.

Important note: If the option `phw-basic-buffer-sync' is not nil
the function `phw-basic-buffer-sync' is running either every time
Emacs is idle or even after every command \(see
`phw-basic-buffer-sync-delay'). So these hooks can be really
called very often! Therefore each function of this hook
should/must check in an efficient way at beginning if its task
have to be really performed and then do them only if really
necessary! Otherwise performance of Emacs could slow down
dramatically!

It is strongly recommended that each function added to this hook uses the
macro `phw-do-if-buffer-visible-in-phw-frame' at beginning! See
`phw-speedbar-buffer-sync' and `phw-eshell-buffer-sync' for
examples how to use this macro!"
  :group 'phw-tree-buffer
  :type 'hook)

;;====================================================
;; Internals
;;====================================================

;;; ----- advice stuff -------------------------------------

(defvar phw-adviced-function-sets nil
  "A list of adviced-function sets defined with `defphw-advice-set'.
Each element is a cons-cell where car is the advice-set-var and cdr is an
indicator if the caller of `phw-with-original-adviced-function-set' is the
outmost caller.

DO NOT CHANGE THIS!")

(defvar phw-adviced-permanent-function-sets nil
  "A list of symbols, each of them an advice-set which should be permanent.
Permanent means this advice set will not be disabled during deactivation of
PHW. This variable is only set by `defphw-advice-set'.

DO NOT CHANGE THIS!")

(defvar phw-adviced-functions nil
  "A list of all advices defined with `defphw-advice'.
This list is the set union of the values of all function-sets of
`phw-adviced-function-sets'.

DO NOT CHANGE THIS!")

(defvar phw-advices-debug-error nil
  "It not nil then each advice of PHW reports when it's en/disabled or called.")

(defun phw-advices-debug-error (advice class action &rest args)
  "Run ARGS through `format' and write it to the *Messages*-buffer.
ADVICE is the adviced-function-symbol, CLASS is the advice-class \(after,
around or before) and ACTION is one of the symbols 'calling, 'enabling,
'disabling or 'reporting.

This will build up a message string like:
PHW <version>: debug <ACTION> of '<CLASS>' advice ADVICE: ARGS.
If ARGS is nil then only the message above is reported."
  (when phw-advices-debug-error
    (message (concat (format "PHW %s: debug %s of '%s' advice %s "
                             phw-version
                             action
                             class
                             advice)
                     (if args
                         (apply 'format args))))))

(defmacro defphw-advice-set (advice-set docstring &optional permanent)
  "Defines an advice-set for PHW.
This defines a variable which will contain adviced functions defined by
`defphw-advice-set'. This is a set of advices which can be enabled or disabled
\"en block\" which must be done either by `phw-enable-advices',
`phw-disable-advices' or `phw-with-original-adviced-function-set'.

Before defining a new advice-set it's recommended to take a look at the value
of `phw-adviced-function-sets' if there is already a suitable advice-set.

IMPORTANT: Each advice in PHW must be defined by `defphw-advice' and must
belong to an advice-set previously defined by `defphw-advice-set'!

All advice-sets of PHW will be automatically\(!) disabled at load-time of the
phw-library and at deactivation-time of PHW. But: Enabling of a certain
advice-set must be done appropriately.

If optional argument PERMANENT is t then this advice-set will NOT be disabled
at deactivation-time of PHW! Calling `phw-disable-advices' for an advice set
defined with permanent is t will take no effect unless the optional argument
FORCE-PERMANENT of this function is set to not nil.
PERMANENT can also be a function which will be called by `phw-disable-advices'
for this advice set \(the function gets one argument: the symbol of the
advice-set) and have to return not nil if the advice-set should not be disable
by `phw-disable-advices' unless the FORCE-PERMANENT of this function is set to
not nil. 

Example:

\(defphw-advice-set phw-always-disabled-advices
  \"These advices are always disabled.\")"
  `(eval-and-compile
     (add-to-list 'phw-adviced-function-sets (cons (quote ,advice-set), nil))
     ,(if permanent
          `(add-to-list 'phw-adviced-permanent-function-sets
                        (cons (quote ,advice-set) ,permanent)))
     (defvar ,advice-set nil ,docstring)))

(put 'defphw-advice-set 'lisp-indent-function 1)

(defmacro defphw-advice (adviced-function advice-class advice-set advice-docstring &rest body)
  "Defines an advice for ADVICED-FUNCTION with ADVICE-CLASS for ADVICE-SET.
ADVICED-FUNCTION must be an advicable object \(e.g. a function, a subr
etc...). ADVICE-CLASS must be one of around, after or before. ADVICE-SET must
ba an advice-set previously defined by `defphw-advice-set'. ADVICE-DOCSTRING
ist the docstring for the advice. BODY is the program-code for the advice as
it would be written with `defadvice'.

Do not quote ADVICED-FUNCTION, ADVICE-CLASS and ADVICE-SET.

Example:

\(defphw-advice delete-frame around phw-layout-basic-adviced-functions
  \"If FRAME is equal to the PHW frame then...\"
  \(let \(\(frame \(or \(ad-get-arg 0) \(selected-frame))))
    \(if \(and phw-minor-mode
             \(equal frame phw-frame))
        \(when \(phw-confirm \"Attempt to delete the PHW-frame....Proceed? \")
	  \(phw-deactivate-internal) 
	  ad-do-it)
      ad-do-it)))"
  `(progn
     (if (assoc (quote ,advice-set) phw-adviced-function-sets)
         (add-to-list (quote ,advice-set)
                      (cons (quote ,adviced-function) (quote ,advice-class)))
       (error "The advice-set %s does not exist!"
              (symbol-name (quote ,advice-set))))
     (if (not (member (quote ,advice-class)
                      '(around after before)))
         (error "The advice-class %s is not allowed - only around, after and before!"
                (symbol-name (quote ,advice-class))))
     (add-to-list 'phw-adviced-functions (cons (quote ,adviced-function) (quote ,advice-class)))
     (eval-and-compile
       (defadvice ,adviced-function (,advice-class phw)
         ,advice-docstring
         (phw-advices-debug-error (quote ,adviced-function)
                                  (quote ,advice-class)
                                  'calling)
         ,@body))))

(put 'defphw-advice 'lisp-indent-function 3)

(defun phw-enable-phw-advice (function-symbol advice-class arg)
  "If ARG is greater or equal zero then enable the adviced version of
FUNCTION-SYMBOL. Otherwise disable the adviced version. The advice must be
defined with class ADVICE-CLASS by `defphw-advice'.

IMPORTANT: Do not use the function directly. Always use `phw-enable-advices',
`phw-disable-advices' or `phw-with-original-adviced-function-set'!."
  (if (< arg 0)
      (progn
        (ad-disable-advice function-symbol advice-class 'phw)
        (ad-activate function-symbol)
        (phw-advices-debug-error function-symbol advice-class 'disabling))
    (ad-enable-advice function-symbol advice-class 'phw)
    (ad-activate function-symbol)
    (phw-advices-debug-error function-symbol advice-class 'enabling)))
    

(defun phw-enable-advices (adviced-function-set-var)
  "Enable all advices of ADVICED-FUNCTION-SET-VAR, which must be defined by
`defphw-advice-set'."
  (if phw-advices-debug-error
      (message "PHW %s: debug enabling the advice-set: %s"
               phw-version adviced-function-set-var))
  (if (eq adviced-function-set-var 'phw-always-disabled-advices)
      (error "The advice-set phw-always-disabled-advices must not be enabled!"))
  (if (not (assq adviced-function-set-var phw-adviced-function-sets))
      (error "The adviced function set %s is not defined by defphw-advice-set!"
             (symbol-name adviced-function-set-var)))
  (dolist (elem (symbol-value adviced-function-set-var))
    (phw-enable-phw-advice (car elem) (cdr elem) 1)))
  
(defun phw-disable-advices (adviced-function-set-var &optional force-permanent)
  "Disable all advices of ADVICED-FUNCTION-SET-VAR, which must be defined by
`defphw-advice-set'

This function tests if ADVICED-FUNCTION-SET-VAR has been defined as permanent
by `defphw-advice-set'.

Calling `phw-disable-advices' for an advice set defined with
permanent t will take no effect unless the optional argument
FORCE-PERMANENT is set to not nil. If the advice set is defined as permanent
with a permanent-disable-function then this function is called with
ADVICED-FUNCTION-SET-VAR as argument; if this function returns not nil then
the adviced will be treated as permanent and will not being disabled.

If optional FORCE-PERMANENT is not nil then ADVICED-FUNCTION-SET-VAR will
be disabled regardless if permanent or not."
  (if phw-advices-debug-error
      (message "PHW %s: debug disabling the advice-set: %s"
               phw-version adviced-function-set-var))
  (if (not (assq adviced-function-set-var phw-adviced-function-sets))
      (error "The adviced function set %s is not defined by defphw-advice-set!"
             (symbol-name adviced-function-set-var)))
  (let ((permanent (if force-permanent
                       nil
                     (cdr (assq adviced-function-set-var
                                phw-adviced-permanent-function-sets)))))
    (unless (or (eq permanent t)
                (and (functionp permanent)
                     (funcall permanent adviced-function-set-var)))
      (dolist (elem (symbol-value adviced-function-set-var))
        (phw-enable-phw-advice (car elem) (cdr elem) -1)))))

;; for the outmost-caller-stuff see phw-with-original-adviced-function-set
(defmacro phw-with-phw-advice (function-symbol advice-class &rest body)
  "Evaluates BODY with the adviced version of FUNCTION-SYMBOL. The advice must
be defined by `defphw-advice' with class ADVICE-CLASS for the advice-set
`phw-always-disabled-advices'. Otherwise an error occurs. The advice is only
active during BODY.

BODY is protected by `unwind-protect' so in each case the advice
will be disabled after finishing this macro unless it is nested
within a call to this macro for the *same* FUNCTION-SYMBOL and
ADVICE-CLASS-combination! This means that the usage of this macro
is save for arbitrary nested calls, so full BODY is guaranted
being evaluated with enabled ADVICE-CLASS advice for
FUNCTION-SYMBOL.

Returns the value of BODY.

Example where this macro is used for `walk-windows' within another advice:

\(phw-with-phw-advice 'walk-windows 'around
   ad-do-it)"
  (let ((outmost-caller-p (make-symbol "outmost-caller-p")))
    ;; we have to check if we are the outmost-caller of this macro for this
    ;; adviced function AND the advice-class! different advice-classes for the
    ;; same function have to be treated differently!!
    `(let ((,outmost-caller-p (unless (member ,advice-class (get ,function-symbol 'phw-with-phw-advice))
                                (put ,function-symbol 'phw-with-phw-advice
                                     (append (list ,advice-class) (get ,function-symbol 'phw-with-phw-advice)))
                                ,advice-class)))
       (if (not (member (cons ,function-symbol ,advice-class)
                      phw-always-disabled-advices))
         (error "Advice for %s with class %s not registered in phw-always-disabled-advices!"
                (symbol-name ,function-symbol)
                (symbol-name ,advice-class)))
       (if phw-advices-debug-error
           (message "PHW %s: debug with always disabled phw-advice: %s %s - ENTRY"
                    phw-version ,advice-class ,function-symbol))
       (unwind-protect
         (progn
           (when ,outmost-caller-p
             (phw-enable-phw-advice ,function-symbol ,advice-class 1))
           ,@body)
         (when ,outmost-caller-p
           ;; Only if we are the outmost caller we are allowed to disable the
           ;; enabled advice
           (put ,function-symbol 'phw-with-phw-advice
                (delete ,advice-class (get ,function-symbol 'phw-with-phw-advice)))
           (phw-enable-phw-advice ,function-symbol ,advice-class -1))
         (if phw-advices-debug-error
             (message "PHW %s: debug with always disabled phw-advice: %s %s - EXIT"
                      phw-version ,advice-class ,function-symbol))))))
         
(put 'phw-with-phw-advice 'lisp-indent-function 2)

;; (insert (pp (macroexpand '(phw-with-phw-advice 'one-window-p 'around
;;                             (message "")))))

(defmacro phw-with-original-adviced-function-set (adviced-function-set-var &rest body)
  "Evaluates BODY with all adviced functions of ADVICED-FUNCTION-SET-VAR
being disabled \(means with their original definition). Restores always \(even
if an error occurs during evaluating BODY) the previous state of the adviced
functions, means it depends if the call to this macro is the outermost call:
Only if it is the outermost-call the advices of the used advice-set will be
disabled after finishing. So full BODY is guaranted being evaluated with
disabled advices of ADVICED-FUNCTION-SET-VAR.

ADVICED-FUNCTION-SET-VAR must be defined by `defphw-advice-set' and all
advices of this set must be defined by `defphw-advice'. Otherwise an error
occurs.

Example:

\(phw-with-original-adviced-function-set 'phw-layout-basic-adviced-functions
   \(do-something..))"
  (let ((outmost-caller-p (make-symbol "outmost-caller-p")))
    `(let ((,outmost-caller-p 
            (unless (equal (cdr (assq ,adviced-function-set-var phw-adviced-function-sets))
                           'outmost-caller)
              ;; if we are the outmost caller of this macro we store this
              ;; for
              ;; a) following callers
              ;; b) ourself, so we can later reset is
              (setcdr (assq ,adviced-function-set-var phw-adviced-function-sets) 'outmost-caller))
            ))
       (if phw-advices-debug-error
           (message "PHW %s: debug with original advice-set: %s - ENTRY"
                    phw-version ,adviced-function-set-var))
       (unwind-protect
           (progn
             (when ,outmost-caller-p
               ;; we must force disabling permanent advice-sets too
               (phw-disable-advices ,adviced-function-set-var t))
             ,@body)
         (when ,outmost-caller-p
           ;; Only if we are the outmost caller we are allowed to re-enable the
           ;; disabled advice-set
           (setcdr (assq ,adviced-function-set-var phw-adviced-function-sets) nil)
           (phw-enable-advices ,adviced-function-set-var))
         (if phw-advices-debug-error
             (message "PHW %s: debug with original advice-set: %s - EXIT"
                      phw-version ,adviced-function-set-var))))))


(put 'phw-with-original-adviced-function-set 'lisp-indent-function 1)


;; !!!!!!!!!!!!!!!! Caution !!!!!!!!!!!!!!!!!!!!!!!!!!
;; when editing that file which defines such an advice and then saving and
;; byte-compiling this file then this reactivates this advice - just a hint -
;; should be not a problem for PHW because the users should not edit the
;; phw-code ;-) But we should have this in mind!!!!!!!!!!!!!!!!!!!!!!!
(defphw-advice-set phw-always-disabled-advices
  "These advices are always disabled.
This advice-set can not be enabled by `phw-enable-advices' but such an
advice has to be activated 'on demand' by the caller. Such an advice must be
used with the macro `phw-with-phw-advice'.")


(defun phw-source-make (filename &optional buffer)
  "Build a source-object from FILENAME and BUFFER.
If optional arg BUFFER is nil then the just FILENAME is returned.
If BUFFER is not nil then it can be either a buffer-object or a buffer-name.
A cons is returned where car is FILENAME and cdr is the buffername of BUFFER."
  (let ((buffername (when buffer
                      (if (bufferp buffer)
                          (buffer-name buffer)
                        buffer))))
    (if buffername
        (cons filename buffername)
      filename)))

(defun phw-source-get-filename (source)
  "SOURCE is either a string, then it is a filename or a cons, then the car is
the filename and the cdr is the buffer-name, whereas the latter one can be the
name of an indirect-buffer."
  (if (consp source)
      (car source)
    source))

(defun phw-source-get-buffername (source)
  "SOURCE is either a string, then it is a filename or a cons, then the car is
the filename and the cdr is the buffer-name, whereas the latter one can be the
name of an indirect-buffer."
  (if (consp source)
      (cdr source)))

(defun phw-source-get-buffer (source)
  "Return a living buffer-object for SOURCE.
SOURCE is either a string, then it is a filename or a cons, then the car is
the filename and the cdr is the buffer-name, whereas the latter one can be the
name of an indirect-buffer.

If SOURCE contains a living buffer then this buffer is returned. Otherwise a
buffer for the filename-part of SOURCE is created and returned. For an
existing ans readable file this means the file is loaded into a buffer.

Note: The buffer is just returned but not displayed."
  (let* ((my-source (if (consp source) source (cons source nil)))
         (filename (car my-source))
         (buffer (phw-buffer-obj (cdr my-source))))
    (or buffer
        (find-file-noselect filename))))

(defvar phw-path-selected-directory nil
  "Path to currently selected directory.")

(defvar phw-path-selected-source nil
  "Path to currently selected source.

It is a cons where the cdr is a buffer-object of the current selected source
The name of this file is the car of the cons:
\(<filename> . <indirect-buffer-object>).

This variable is only set by `phw-path-selected-source-set' and evaluated by
the function `phw-path-selected-source'.

Do not use it directly! Use always one of the mentioned functions!")

(defun phw-path-selected-source-set (filename buffer)
  "Set `phw-path-selected-source' to FILENAME and BUFFER.
Returns in the new value. FILENAME and BUFFER must not be nil.
For a description of FILENAME and BUFFER see `phw-source-make'."
  (unless (and filename buffer)
    (error "PHW %s: Invalid setting of `phw-path-selected-source with file %s, buffer %s"
           phw-version filename buffer))
  (setq phw-path-selected-source (phw-source-make filename buffer)))
  
(defun phw-path-selected-source (&optional type)
  "Get the value of the internal variable `phw-path-selected-source'.
If optional arg TYPE is the symbol 'file then the filename-part
is returned as string, if it is the symbol 'buffername then the
stored buffername is returned if there is any and and if it is the
symbol 'buffer then the buffer-object of the stored buffername is
returned if there is any or nil.

In all other cases of TYPE always that value is returned
`phw-path-selected-source' has been set by most recent
`phw-path-selected-source-set'."
  (case type
    (file (phw-source-get-filename phw-path-selected-source))
    (buffername (phw-source-get-buffername phw-path-selected-source))
    (buffer (phw-source-get-buffer phw-path-selected-source))
    (otherwise phw-path-selected-source)))


;; all defined tree-buffer creators

(defvar phw-tree-buffer-creators nil
  "The tree-buffer creators of PHW.
An alist where each element is a cons where the car is a symbol which contains
the name of a tree-buffer \(e.g. `phw-sources-buffer-name') and the cdr is the
associated function-symbol which creates the tree-buffer with that name.")

(defun phw-tree-buffer-creators-init ()
  "Initialize `phw-tree-buffer-creators'.
Removes all creators and set it to nil."
  (setq phw-tree-buffer-creators nil))

(defun phw-tree-buffer-creators-register (name-symbol fn)
  "Register the creator-function FN for the tree-buffer NAME-SYMBOL."
  (add-to-list 'phw-tree-buffer-creators (cons name-symbol fn)))

(defun phw-tree-buffer-creators-run ()
  "Run all currently registered creator-functions."
  (dolist (creator-elem phw-tree-buffer-creators)
    ;; create all the tree-buffers if they don't already exist
    (funcall (cdr creator-elem))))
  

(defmacro defphw-tree-buffer-creator (creator
                                      tree-buffer-name-symbol
                                      docstring &rest body)
  "Define a creator-function CREATOR for a tree-buffer which name is hold in
the symbol TREE-BUFFER-NAME-SYMBOL. Do not quote CREATOR and
TREE-BUFFER-NAME-SYMBOL. DOCSTRING is the docstring for CREATOR. BODY is all
the program-code of CREATOR \(must contain a call to `tree-buffer-create'). It
makes sense that BODY returns the created tree-buffer.

When creating a tree-buffer with this macro then this tree-buffer will be
automatically created \(i.e. its creator-function defined with this macro will
be called) when activating PHW and the tree-buffer will automatically
registered at PHW. This means that some features of PHW will work
automatically out of the box with this tree-buffer.

When creating a tree-buffer for PHW then it MUST be created with this macro
and not with `tree-buffer-create'!"
  `(eval-and-compile
     (phw-tree-buffer-creators-register (quote ,tree-buffer-name-symbol)
                                        (quote ,creator))
     (defun ,creator ()
       ,docstring
       (unless (phw-phw-buffer-registry-get-symbol ,tree-buffer-name-symbol)
         (phw-phw-buffer-registry-add ,tree-buffer-name-symbol
                                      (quote ,tree-buffer-name-symbol)
                                      t)
         ,@body))))

(put 'defphw-tree-buffer-creator 'lisp-indent-function 2)

;; all created phw-buffers 

;; KB: `defphw-tree-buffer-creator' calls phw-phw-buffer-registry-add. Besides
;; `defphw-window-dedicator-to-phw-buffer' this is the only modifier of that
;; variable!
(defvar phw-phw-buffer-registry nil
  "The phw-buffers registry.

Each special phw-buffer must be registered at PHW in this registry.

Do not change this variable! This registration is done completely
by the macro `defphw-window-dedicator-to-phw-buffer' and the
results are stored in this registry. Each item is a 4-element
list with the following contents:
1. elem: The buffer-name of the registered phw-buffer
2. elem: The symbol which holds this buffer-name
3. elem: An indicator if the registered phw-buffer is of type tree-buffer
   \(i.e. this tree-buffer is created with `defphw-tree-buffer-creator'). Not
   nil in case of a tree-buffer otherwise nil
4. elem: A function which displays that buffer in current window
   when called and makes this window dedicated to this buffer. This is the
   DEDICATOR-argument of `defphw-window-dedicator-to-phw-buffer'.

The dedicator-function must do:
1. switch to that buffer in current window
2. all things necessary for this buffer - e.g. making it read-only

The setting function must ensure that the current window is still
current at the end and that the related phw-buffer is displayed
in this window at the end. One examples of such a setting
function is `phw-set-history-buffer' for the buffer with name
`phw-history-buffer-name'.

See `defphw-window-dedicator-to-phw-buffer' for more details and an example.")

(defun phw-phw-buffer-registry-init ()
  (setq phw-phw-buffer-registry nil))

(defun phw-phw-buffer-registry-add (name name-symbol tree-buffer-p &optional set-fcn-symbol)
  (if (assoc name phw-phw-buffer-registry)
      (phw-set-elt (assoc name phw-phw-buffer-registry) 3 set-fcn-symbol)
    (setq phw-phw-buffer-registry
          (cons (list name name-symbol tree-buffer-p set-fcn-symbol)
                phw-phw-buffer-registry))))

(defsubst phw-phw-buffer-registry-name-list (&optional only-tree-buffers)
  (delq nil (mapcar (function (lambda (e)
                                (and (or (not only-tree-buffers)
                                         (nth 2 e))
                                     (nth 0 e))))
                    phw-phw-buffer-registry)))

(defsubst phw-phw-buffer-registry-symbol-list (&optional only-tree-buffers)
  (delq nil (mapcar (function (lambda (e)
                                (and (or (not only-tree-buffers)
                                         (nth 2 e))
                                     (nth 1 e))))
                    phw-phw-buffer-registry)))

(defsubst phw-phw-buffer-registry-buffer-list (&optional only-tree-buffers)
  (delq nil (mapcar (function (lambda (e)
                                (and (or (not only-tree-buffers)
                                         (nth 2 e))
                                     (phw-buffer-obj (nth 0 e)))))
                    phw-phw-buffer-registry)))
  
(defsubst phw-phw-buffer-registry-get-symbol (name)
  (nth 1 (assoc name phw-phw-buffer-registry)))

(defsubst phw-phw-buffer-registry-get-set-fcn (name)
  (nth 3 (assoc name phw-phw-buffer-registry)))


(defvar phw-tree-buffer-callbacks '((expand . nil) (select . nil))
  "All callback-functions for the tree-buffers of PHW.
This list contains two items of the form:
\(<callback-type> .\(<buffer-callback-alist>))
where <callback-type> is 'select and 'expand and
<buffer-callback-alist> is an alist where each item is a cons
like \(<buffer-name-symbol> . <callback-symbol>)."
  )


(defun phw-tree-buffer-callbacks-add (type buffer-name-symbol callback)
  (unless (member type '(select expand))
    (error "PHW %s tries to add tree-buffer-callback of unknown type %s"
           phw-version type))
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: maybe we should add a check if
  ;; the buffer-name-symbol is already registered with defphw-tree-buffer-creator
  (let ((type-elem (phw-find-assoc type phw-tree-buffer-callbacks)))
    (unless (phw-find-assoc buffer-name-symbol type-elem)
      (setcdr type-elem (cons (cons buffer-name-symbol callback)
                              (cdr type-elem))))))


(defun phw-tree-buffer-callbacks-alist-of-type (type)
  (unless (member type '(select expand))
    (error "PHW %s tries to get tree-buffer-callback of unknown type %s"
           phw-version type))
  (cdr (assoc type phw-tree-buffer-callbacks)))


(defmacro defphw-tree-buffer-callback (callback
                                       tree-buffer-name-symbol
                                       callback-type
                                       optional-arg-list
                                       docstring &rest body)
  "Define a callback-function CALLBACK for a tree-buffer which name is hold in
the symbol TREE-BUFFER-NAME-SYMBOL. Do not quote CALLBACK and
TREE-BUFFER-NAME-SYMBOL and CALLBACK-TYPE. DOCSTRING is the
docstring for CALLBACK. BODY is all the program-code of CALLBACK.

CALLBACK-TYPE must be either 'expand or 'select, whereas the
former one defines a callback for handling expanding a node and
the latter one for clicking onto a node.

CALLBACK is defined as a function with at least five arguments:
NODE, PHW-BUTTON, EDIT-WINDOW-NR, SHIFT-MODE and META-MODE.
CALLBACK must handle clicking onto NODE in the tree-buffer for
which the callback is defined. PHW-BUTTON can be 1, 2 or 3 \(=
mouse-buttons). If 3 then EDIT-WINDOW-NR contains the number of
the edit-window the NODE should be displayed or whatever should
be done with NODE. For 1 and 2 the value of EDIT-WINDOW-NR is
ignored. SHIFT-MODE and META-MODE are not nil if the user has
pressed the shift- rsp. the meta-button during his click. Note:
using the keyboard in the tree-buffer instead the mouse is
already handled by the caller of CALLBACK, so CALLBACK has no
need to bother with keyboard or mouse specific stuff!

If OPTIONAL-ARG-LIST is not nil then it must be a list with all
optional or rest arguments. You have to include the keywords
&optional or/and &rest! The first item of this list must be
either the keyword &optional or &rest! The defined CALLBACK gets
exactly these additional arguments after the reqired 5 arguments
described above. Do not quote OPTIONAL-ARG-LIST!

The defined CALLBACK automatically hides the phw-windows after
selecting a node in case META-MODE is not nil and if the CALLBACK
is of type 'select; this is a must for every tree-buffer. Do not
do this within BODY! But: During the evaluation of BODY the local
variable no-meta-hiding is bound and set to nil per default. If
BODY sets it to not nil then the hiding of the phw-windows is
prevented even if META-MODE is not nil.

The value of the last expression of BODY is returned.

This macro automatically adds the appropriate description of the
5 arguments of the defined CALLBACK to DOCSTRING. So just
describe what the CALLBACK does!

It is strongly recommended defining a callback-function for a
tree-buffer of PHW with this macro and not with plain `defun',
because then a lot of stuff needed to be done by every
tree-buffer is automatically performed."
  `(eval-and-compile
     (phw-tree-buffer-callbacks-add (quote ,callback-type)
                                    (quote ,tree-buffer-name-symbol)
                                    (quote ,callback))
     (defun ,callback ,(append '(node phw-button edit-window-nr shift-mode meta-mode)
                               optional-arg-list)
       ,(concat (if (equal callback-type 'select)
                    (concat
                     "Handle clicking onto NODE in the current tree-buffer.\n"
                     "PHW-BUTTON can be 1, 2 or 3. If 3 then EDIT-WINDOW-NR contains the number\n"
                     "of the edit-window the NODE should be displayed or whatever should be done\n"
                     "with NODE. For 1 and 2 the value of EDIT-WINDOW-NR is ignored.\n"
                     "SHIFT-MODE and META-MODE are self-explanatory.")
                  (concat
                   "Handle expanding NODE in the current tree-buffer.\n"
                   "PHW-BUTTON can be 1, 2 or 3. If 3 then EDIT-WINDOW-NR contains the number\n"
                   "of the edit-window the NODE should be displayed or whatever should be done\n"
                   "with NODE. For 1 and 2 the value of EDIT-WINDOW-NR is ignored.\n"
                   "SHIFT-MODE and META-MODE are self-explanatory."))
                "\n\n"
                docstring)
       (let ((no-meta-hiding nil))
         (prog1
             (progn
               ,@body)
           ,(if (equal callback-type 'select)
                `(when (and (not no-meta-hiding) meta-mode)
                   (phw-run-with-idle-timer 0.001 nil 'phw-hide-phw-windows))))))))

(put 'defphw-tree-buffer-callback 'lisp-indent-function 4)

;; the filename/path cache

(defphw-multicache phw-filename-cache 500 nil '(FILES-AND-SUBDIRS
                                                EMPTY-DIR-P
                                                SOURCES
                                                VC
                                                FIXED-FILENAMES
                                                REMOTE-PATH
                                                HOST-ACCESSIBLE)
  "Cache used for the filebrowser to cache all necessary informations
associated to file- or directory-names.

Currently there are the following subcaches managed within this cache:

  FILES-AND-SUBDIRS:
  
  Cache for every directory all subdirs and files. This is a cache with
     key:   <directory>
     value: \(<file-list> . <subdirs-list>)
  
  EMPTY-DIR-P:
  
  Cache for every directory if it is empty or not. This is a cache with
     key:   <directory>
     value: \(\[nil|t] . <checked-with-show-sources>)
  
  SOURCES:
  
  Cache for the contents of the buffer `phw-sources-buffer-name'. This is a
  cache with
     key:   <directory>
     value: \(<full-content> . <filtered-content>)
  whereas <full-content> is a 3-elem list \(tree-buffer-root <copy of
  tree-buffer-displayed-nodes> buffer-string) for a full \(i.e. all files)
  cache and <filtered-content> is a 4-elem list \(tree-buffer-root <copy of
  tree-buffer-displayed-nodes> sources-buffer-string <filter>) for a filtered
  cache where <filter> is a cons-cell \(<filter-regexp> . <filter-display>).

  VC:

  Cache necessary informations for the version-control-support. This is a
  cache for filenames and directories. In case of a file with
     key: <filename> of a sourcefile
     value: \(<state> <check-timestamp> <checked-buffers>)
  whereas <state> is the that VC-state the file had at time <check-timestamp>.
  <checked-buffers> is a list of tree-buffer-names for which <state> was
  checked.
  In case of a directory with
     key: <dirname> of a directory
     value: <vc-state-fcn> or 'NO-VC
  <vc-state-fcn> is the function used to get the VC-state if <check-timestamp>
  is older than the most recent modification-timestamp of <filename>.

  FIXED-FILENAMES:

  Cache for fixed filenames which can speedup handling-remote-paths \(like
  tramp-paths)
     key: The concatenation of the args PATH and FILENAME of `phw-fix-filename'.
     value: The result of `phw-fix-filename' for these args.

  REMOTE-PATH:

  Cache if a path is a remote path and store its components if yes.
     key: a path
     value: 'NOT-REMOTE if not a remote path otherwise the result of
     `phw-remote-path'.

  HOST-ACCESSIBLE:

  Cache if a host is accessible or not.
     key: a host \(e.g. phw.sourceforge.net)
     value: \(<timestamp> . <value>) whereas <timestamp> is the cache time of
     <value> and <value> is either 'NOT-ACCESSIBLE if host is not accessible
     or t if accessible.
")

(defun phw-filename-cache-init ()
  "Initialize the whole cache for file- and directory-names"
  (if (phw-multicache-p 'phw-filename-cache)
      (phw-multicache-clear 'phw-filename-cache)))

;; directory separator

(defconst phw-directory-sep-char
  (if phw-running-xemacs
      ;; to avoid compiler complainings
      (symbol-value 'directory-sep-char)
    ?/))

(defsubst phw-directory-sep-char (&optional refdir)
  (if (or (null refdir)
          (not (phw-remote-path refdir)))
      phw-directory-sep-char
    ?/))

(defsubst phw-directory-sep-string (&optional refdir)
  (char-to-string (phw-directory-sep-char refdir)))   

;; autocontrol/sync-stuff --------------------------------------------

(defvar phw-autotrace-autocontrol/sync-functions nil
  "Allow autotracing the internal autocontrol/synchronisations of PHW.
All functions defined with `defphw-autocontrol/sync-function' can
be autotraced. The value of this variable is either nil \(then no
autotracing will be performed) or t \(then all functions defined with
`defphw-autocontrol/sync-function' will be traced) or a list of
function-symbols \(then exactly these functions will be traced).

Auto-tracing means that each\(!) time the function runs \(either
by idle-timer or within pre- or post-command-hook) a trace-message on
function-entry and a trace-message on function exit is writen.")

(defvar phw-bodytrace-autocontrol/sync-functions nil
  "Allows a body tracing of the internal autocontrol/synchronisations of PHW.
For allowed values for this variable see `phw-autotrace-autocontrol/sync-functions'

Body-tracing means calls to `phw-bodytrace-autocontrol/sync-fcn-error'
somewhere within the BODY of functions defined by
`defphw-autocontrol/sync-function'. So by setting this variable tracing of the
BODY of autocontrol/sync-function can be switched on or off.")

(defun phw-autotrace-autocontrol/sync-fcn-error (autocontrol-fcn &rest args)
  "Run ARGS through `format' and write it to the *Messages*-buffer.
Do not use this function for own traces, but use the function
`phw-bodytrace-autocontrol/sync-fcn-error'!
Entry and exit-traces are already buildin in functions defined with
`defphw-autocontrol/sync-function' and can be switched on/off by
`phw-autotrace-autocontrol/sync-functions'."
  (when (or (eq phw-autotrace-autocontrol/sync-functions t)
            (member autocontrol-fcn phw-autotrace-autocontrol/sync-functions))
    (message (concat (format "PHW %s autocontrol-fcn %s autotrace [%s] "
                             phw-version autocontrol-fcn
                             (format-time-string "%H:%M:%S"))
                     (apply 'format args)))))

(defun phw-bodytrace-autocontrol/sync-fcn-error (autocontrol-fcn &rest args)
  "Run ARGS through `format' and write it to the *Messages*-buffer.

Use this function only for traces used within the BODY of a function defined
with `defphw-autocontrol/sync-function' and not for entry and exit-traces
because these ones are already buildin in functions defined with
`defphw-autocontrol/sync-function' and can be switched on/off by
`phw-autotrace-autocontrol/sync-functions'.

Body-tracing can be switched on/off with
`phw-bodytrace-autocontrol/sync-function'."
  (when (or (eq phw-bodytrace-autocontrol/sync-functions t)
            (member autocontrol-fcn phw-bodytrace-autocontrol/sync-functions))
    (message (concat (format "PHW %s autocontrol-fcn %s bodytrace [%s] "
                             phw-version autocontrol-fcn
                             (format-time-string "%H:%M:%S"))
                     (apply 'format args)))))

(defvar phw-autocontrol/sync-fcn-register nil
  "List of autocontrol/sync-functions defined by
`defphw-autocontrol/sync-function'. Each element is a cons where car is the
function symbol and cdr is either the symbol of a variable containing a
buffer-name of a special PHW-buffer or nil.")
(defvar phw-idle-timer-alist nil
  "List of active idle-timers of PHW")
(defvar phw-post-command-hooks nil
  "List of functions which have to be part of the value of `post-command-hook'")
(defvar phw-pre-command-hooks nil
  "List of functions which have to be part of the value of `pre-command-hook'")

(defun phw-register-autocontrol/sync-function (fcn-symbol buffer-name-symbol)
  (add-to-list 'phw-autocontrol/sync-fcn-register (cons fcn-symbol buffer-name-symbol)))

(defun phw-stop-all-autocontrol/sync-functions ()
  (dolist (fcn-elem phw-autocontrol/sync-fcn-register)
    (phw-stop-autocontrol/sync-function (car fcn-elem))))

(defun phw-stop-autocontrol/sync-function (fcn-symbol)
  (let* ((timer-elem (assoc fcn-symbol phw-idle-timer-alist))
         (timer (cdr timer-elem)))
    (when timer-elem
      (phw-cancel-timer timer)
      (setq phw-idle-timer-alist (delq timer-elem phw-idle-timer-alist)))
    (remove-hook 'post-command-hook fcn-symbol)
    (remove-hook 'pre-command-hook fcn-symbol)
    (setq phw-post-command-hooks (delq fcn-symbol phw-post-command-hooks))
    (setq phw-pre-command-hooks (delq fcn-symbol phw-pre-command-hooks))))
  

(defun phw-activate-phw-autocontrol-function (value func)
  "Adds function FUNC to `phw-idle-timer-alist' and activates an idle-timer
with idle-time VALUE if VALUE is a number. If nil or 'post the
FUNC is added to `post-command-hook' and `phw-post-command-hooks'
and removed from the idle-list \(if it has been contained). If
'pre the FUNC is added to `pre-command-hook' and
`phw-pre-command-hooks' and removed from the idle-list \(if it
has been contained)."
  (unless (assoc func phw-autocontrol/sync-fcn-register)
    (error "PHW %s: Try to activate unregistered %s as autocontrol/sync-function"
           phw-version func))
  ;; `phw-basic-buffer-sync-delay' can never have the value 'basic so this
  ;; recursion is save
  (if (equal 'basic value)
      (phw-activate-phw-autocontrol-function phw-basic-buffer-sync-delay func)
    (phw-stop-autocontrol/sync-function func)
    (case value
      ((nil post)
       (add-hook 'post-command-hook func)
       (add-to-list 'phw-post-command-hooks func))
      (pre
       (add-hook 'pre-command-hook func)
       (add-to-list 'phw-pre-command-hooks func))
      (otherwise
       (add-to-list 'phw-idle-timer-alist
                    (cons func
                          (phw-run-with-idle-timer value t func)))))))

(defmacro defphw-autocontrol/sync-function (fcn buffer-name-symbol
                                                buffer-sync-option-symbol
                                                phw-interactive-p docstring
                                                &rest body)
  "Define a function run either by idle-timer or before or after each command.
Such a function is used either for automatic self-controlling certain aspects
of PHW or for synchronizing a special window/buffer of PHW with contents of
the active buffer in the edit-area.

FCN is the name of the defined function and BUFFER-NAME-SYMBOL is
either nil or a variable-symbol containing the buffer-name
of a special PHW-window/buffer for which the defined function is
used for synchronizing it with the edit-area. In the latter case
BODY is encapsulated with the macros a)
`phw-when-point-in-edit-window-phw-windows-visible' and b)
`phw-do-if-buffer-visible-in-phw-frame' so BODY runs only if a)
point stays in an edit-window \(ie. the currently selected window
is an edit-window) and the phw-windows of current layout are
not hidden and b) the buffer of BUFFER-NAME-SYMBOL is displayed in a
window of the PHW-frame \(for details see the documentation of
this macro).

Please note: If BUFFER-NAME-SYMBOL is nil then BODY is not
encapsulated with these two macros mentioned above!

The defined function has an optional argument FORCE which can be used within
BODY.

BUFFER-SYNC-OPTION-SYMBOL is either nil or the name of an option
which defines if and under which circumstances the
synchronization should take place. Such an option must be of the
same type and must offer exactly the same values as `phw-analyse-buffer-sync'.
If BUFFER-SYNC-OPTION-SYMBOL is not nil and a valid symbol then the generated
function encapsulates BODY in a when-statement whereas the condition-clause is
exactly true when either:
- force is not nil or
- The value of BUFFER-SYNC-OPTION-SYMBOL is t or 'always or
- The major-mode of current buffer is not contained in the list-value of
  BUFFER-SYNC-OPTION-SYMBOL or
- The value of BUFFER-SYNC-OPTION-SYMBOL is 'basic and the conditions above
  are true for `phw-basic-buffer-sync'.

If PHW-INTERACTIVE-P is not nil then FCN will be defined as an interactice
command, i.e. it will containe the clause \(interactive \"P\").

The defined function is automatically prepared for tracing its calls when
`phw-autotrace-autocontrol/sync-functions' is either set to t or the symbol of FCN
is contained in the list of this variable. In this case a trace-statemant with
time-details is reported to the message-buffer directly before and after BODY.

Do not quote FCN, BUFFER-NAME-SYMBOL and BUFFER-SYNC-OPTION-SYMBOL!

Example:

\(defphw-autocontrol/sync-function phw-sync-a-buffer phw-a-special-buffer-name
   phw-a-special-buffer-sync nil
  \"Synchronize the buffer of PHW-A-SPECIAL-BUFFER-NAME with...\"
  \(let \(\(x nil))
    \(if force
        ;; do something
      ;; do something else
      )
    ))

This defines a non interactive function `phw-sync-a-buffer' which
should be used for synchronizing the special buffer the name is
hold in the variable `phw-a-special-buffer-name'.
"
  `(eval-and-compile
     (phw-register-autocontrol/sync-function (quote ,fcn) (quote ,buffer-name-symbol))

     (defun ,fcn (&optional force)
       ,docstring
       ,(if phw-interactive-p
            '(interactive "P"))
       (phw-autotrace-autocontrol/sync-fcn-error (quote ,fcn)
                                                 "Begin: Cur-buf: %s" (current-buffer))
       (let (,(if (and buffer-sync-option-symbol (symbolp buffer-sync-option-symbol))
                  `(,buffer-sync-option-symbol (if (equal
                                                    ,buffer-sync-option-symbol
                                                    'basic)
                                                   phw-basic-buffer-sync
                                                 ,buffer-sync-option-symbol))
                ;; we need anything to bound in the else-fork, so we just bind
                ;; major-mode to major-mode - we could use any variable, takes
                ;; no effect
                `(major-mode major-mode))
;;                 `(,(make-symbol "abc123xyz456efg789") nil))
             )
         (when ,(if (and buffer-sync-option-symbol (symbolp buffer-sync-option-symbol))
                    `(or force
                         (equal 'always ,buffer-sync-option-symbol)
                         (equal t ,buffer-sync-option-symbol)
                         (and ,buffer-sync-option-symbol
                              (listp ,buffer-sync-option-symbol)
                              (not (member major-mode ,buffer-sync-option-symbol))))
                  t)
           ,(if (and buffer-name-symbol (symbolp buffer-name-symbol))
                `(phw-when-point-in-edit-window-phw-windows-visible
                  (phw-do-if-buffer-visible-in-phw-frame (quote ,buffer-name-symbol)
                    (phw-bodytrace-autocontrol/sync-fcn-error (quote ,fcn)
                                                              "After conditions: Cur-buf: %s" (current-buffer))
                    ,@body
                    (phw-autotrace-autocontrol/sync-fcn-error (quote ,fcn)
                                                              "End:   Cur-buf: %s" (current-buffer))
                    nil ;; we always return nil
                    ))
              `(progn
                 (phw-bodytrace-autocontrol/sync-fcn-error (quote ,fcn)
                                                           "After conditions: Cur-buf: %s" (current-buffer))
                 ,@body
                 (phw-autotrace-autocontrol/sync-fcn-error (quote ,fcn)
                                                           "End:   Cur-buf: %s"
                                                           (current-buffer))
                 nil ;; we always return nil
                 )
              ))))))
(put 'defphw-autocontrol/sync-function 'lisp-indent-function 4)

(defphw-autocontrol/sync-function phw-monitor-autocontrol-functions nil nil nil
  "Checks if all necessary PHW-hooks are contained in `post-command-hook' rsp.
`pre-command-hook'. If one of them has been removed by Emacs \(Emacs resets
these hooks to nil if any of the contained functions fails!) then this
function reads them to these hooks."
  ;; post-command-hook
  (dolist (hook phw-post-command-hooks)
    (when (not (member hook post-command-hook))
      (add-hook 'post-command-hook hook)))
  ;; pre-command-hook
  (dolist (hook phw-pre-command-hooks)
    (when (not (member hook pre-command-hook))
      (add-hook 'pre-command-hook hook))))

;; -- end of sync stuff



;;; ----- Wrappers for file- and directory-operations ------

(dolist (f '(file-name-nondirectory
             file-exists-p
             file-name-directory
             file-readable-p
             file-attributes
             file-name-extension
             file-directory-p
             file-accessible-directory-p
             file-name-sans-extension
             file-writable-p
             file-name-as-directory))
  (fset (intern (format "phw-%s" f))
        `(lambda (file-or-dir-name &rest args)
           ,(format "Delegate all args to `%s' but call first `phw-fix-path' for FILE-OR-DIR-NAME." f)
           (apply (quote ,f) (phw-fix-path file-or-dir-name) args))))

(defun phw-directory-files (dir &rest args)
  "Wrapper for directory-files that fixes the file name & catch file errors"
  (condition-case nil
      (apply 'directory-files (phw-fix-path dir) args)
    (error nil)))

(defun phw-expand-file-name (name &optional default-dir)
  "Delegate all args to `expand-file-name' but call first `phw-fix-path'
for both args."
  (expand-file-name (phw-fix-path name) (phw-fix-path default-dir)))

;;; ----- Canonical filenames ------------------------------

(defun phw-fix-path (path)
  "Fixes an annoying behavior of the native windows-version of XEmacs:
When PATH contains only a drive-letter and a : then `expand-file-name' does
not interpret this PATH as root of that drive. So we add a trailing
`directory-sep-char' and return this new path because then `expand-file-name'
treats this as root-dir of that drive. For all \(X)Emacs-version besides the
native-windows-XEmacs PATH is returned."
  (if (and phw-running-xemacs
           (equal system-type 'windows-nt))
      (if (and (= (length path) 2)
               (equal (aref path 1) ?:))
          (concat path (phw-directory-sep-string))
        path)
    path))

;; accessors for the FIXED-FILENAMES-cache

(defsubst phw-fixed-filename-cache-put (path filename fixed-filename)
  "Add FIXED-FILENAME for PATH and FILENAME to the FIXED-FILENAMES-cache
of `phw-filename-cache'."
  (phw-multicache-put-value 'phw-filename-cache
                            (concat path filename)
                            'FIXED-FILENAMES
                            fixed-filename))

(defsubst phw-fixed-filename-cache-get (path filename)
  "Get the cached value for PATH and FILENAME from the FIXED-FILENAMES-cache
in `phw-filename-cache'. If no vaue is cached for PATH and FILENAME then nil
is returned."
  (phw-multicache-get-value 'phw-filename-cache
                            (concat path filename)
                            'FIXED-FILENAMES))

(defun phw-fixed-filename-cache-dump (&optional no-nil-value)
  "Dump the whole FIXED-FILENAMES-cache. If NO-NIL-VALUE is not nil then these
cache-entries are not dumped. This command is not intended for end-users of PHW."
  (interactive "P")
  (phw-multicache-print-subcache 'phw-filename-cache
                                 'FIXED-FILENAMES
                                 no-nil-value))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: What about the new cygwin-version
;; of GNU Emacs 21? We have to test if this function and all locations where
;; `phw-fix-path' is used work correctly with the cygwin-port of GNU Emacs.
(silentcomp-defun mswindows-cygwin-to-win32-path)
(defun phw-fix-filename (path &optional filename substitute-env-vars)
  "Normalizes path- and filenames for PHW. If FILENAME is not nil its pure
filename \(i.e. without directory part) will be concatenated to PATH. The
result will never end with the directory-separator! If SUBSTITUTE-ENV-VARS is
not nil then in both PATH and FILENAME env-var substitution is done. If the
`system-type' is 'cygwin32 then the path is converted to win32-path-style!"
  (when (stringp path)
    (or (phw-fixed-filename-cache-get path filename)
        (let ((remote-path (phw-remote-path path))
              (norm-path nil)
              (result nil))
          (if (or (not remote-path)
                  (phw-host-accessible-p (nth 1 remote-path)))
              (progn
                (setq norm-path (if phw-running-xemacs
                                    (case system-type
                                      (cygwin32
                                       (mswindows-cygwin-to-win32-path
                                        (expand-file-name path)))
                                      (windows-nt
                                       (expand-file-name (phw-fix-path path)))
                                      (otherwise
                                       (expand-file-name path)))
                                  (expand-file-name path)))
                ;; substitute environment-variables
                (setq norm-path (expand-file-name (if substitute-env-vars
                                                      (substitute-in-file-name norm-path)
                                                    norm-path))))
            (setq norm-path path))
          ;; For windows systems we normalize drive-letters to downcase
          (setq norm-path (if (and (member system-type '(windows-nt cygwin32))
                                   (> (length norm-path) 1)
                                   (equal (aref norm-path 1) ?:))
                              (concat (downcase (substring norm-path 0 2))
                                      (substring norm-path 2))
                            norm-path))
          ;; delete a trailing directory-separator if there is any
          (setq norm-path (if (and (> (length norm-path) 1)
                                   (= (aref norm-path (1- (length norm-path)))
                                      (phw-directory-sep-char path)))
                              (substring norm-path 0 (1- (length norm-path)))
                            norm-path))
          (setq result
                (concat norm-path
                        (if (stringp filename)
                            (concat (when (> (length norm-path) 1)
                                      ;; currently all protocols like tramp,
                                      ;; ange-ftp or efs support only not
                                      ;; windows-remote-hosts ==> we must not
                                      ;; add a backslash here (would be done
                                      ;; in case of a native Windows-XEmacs)
                                      (phw-directory-sep-string path))
                                    (file-name-nondirectory (if substitute-env-vars
                                                                (substitute-in-file-name filename)
                                                              filename))))))
          (phw-fixed-filename-cache-put path filename result)
          result))))

;; -- end of canonical filenames


(defun phw-format-bucket-name (name &optional ignore-prefix-suffix ignore-bucket-face)
  "Format NAME as a bucket-name according to `phw-bucket-node-display'.
If optional arg IGNORE-PREFIX-SUFFIX is not nil then
these settings of `phw-bucket-node-display' are ignored. If IGNORE-BUCKET-FACE
it t then the face of `phw-bucket-node-display' is completely ignored, if it
is 'only-name then the face of `phw-bucket-node-display' is only ignored for
NAME but not for a prefix or suffix of `phw-bucket-node-display' \(if any)."
  (let ((formated-prefix (unless ignore-prefix-suffix
                           (if (eq ignore-bucket-face t)
                               (nth 0 phw-bucket-node-display)
                             (phw-merge-face-into-text
                              (nth 0 phw-bucket-node-display)
                              (nth 2 phw-bucket-node-display)))))
        (formated-suffix (unless ignore-prefix-suffix
                           (if (eq ignore-bucket-face t)
                               (nth 1 phw-bucket-node-display)
                             (phw-merge-face-into-text
                              (nth 1 phw-bucket-node-display)
                              (nth 2 phw-bucket-node-display)))))
        (formatted-name (if ignore-bucket-face
                            name
                          (phw-merge-face-into-text name (nth 2 phw-bucket-node-display)))))
    (concat formated-prefix formatted-name formated-suffix)))

(defun phw-toggle-do-not-leave-window-after-select ()
  "Toggles if a node-selection in a tree-buffer leaves the tree-window.
See also the option `phw-tree-do-not-leave-window-after-select'."
  (interactive)
  (let ((tree-buffer (phw-point-in-phw-tree-buffer)))
    (if tree-buffer
        (let ((tree-buf-name (buffer-name tree-buffer)))
          (if (phw-member-of-symbol/value-list
               tree-buf-name
               phw-tree-do-not-leave-window-after-select--internal)
              (progn
                (setq phw-tree-do-not-leave-window-after-select--internal
                      ;; we must try both - the symbol of the tree-buffer-name
                      ;; and the tree-buffer-name because we do not know what
                      ;; the user has specified in
                      ;; `phw-tree-do-not-leave-window-after-select'!
                      (delete (phw-phw-buffer-registry-get-symbol tree-buf-name)
                              (delete tree-buf-name
                                      phw-tree-do-not-leave-window-after-select--internal)))
                (message "Selections leave the tree-window of %s" tree-buf-name))
            (setq phw-tree-do-not-leave-window-after-select--internal
                  (append phw-tree-do-not-leave-window-after-select--internal
                          (list (phw-phw-buffer-registry-get-symbol tree-buf-name))))
            (message "Selections don't leave the tree-window of %s." tree-buf-name)))
      (message "Point must stay in an PHW tree-buffer!"))))

(defun phw-common-tree-buffer-modeline-menu-creator (buf-name)
  "Return a menu for the modeline of all PHW-tree-buffers."
  '((delete-other-windows "Maximize Window Above")
    (phw-redraw-layout-preserving-compwin-state "Display All PHW-windows")))

(defun phw-common-after-tree-buffer-create-actions ()
  "Things which should be performed after creating a tree-buffer.
The tree-buffer is the current buffer."
  (local-set-key (kbd "C-t") 'phw-toggle-do-not-leave-window-after-select)
  (if phw-running-xemacs
      ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Is it necessary to make
      ;; modeline-map buffer-local for current buffer first?!
      (define-key modeline-map
        '(button2up)
        'phw-toggle-maximize-phw-window-with-mouse)
    (local-set-key [mode-line mouse-2]
                   'phw-toggle-maximize-phw-window-with-mouse)))
  
 


;;====================================================
;; Mouse callbacks
;;====================================================

(defun phw-tree-buffer-node-select-callback (node
					     mouse-button
					     shift-pressed
					     control-pressed
                                             meta-pressed
					     tree-buffer-name)
  "This is the callback-function phw.el gives to every tree-buffer to call
when a node has been selected. This function does nothing if the click
combination is invalid \(see `phw-interpret-mouse-click'."
  (let* ((phw-button-list (phw-interpret-mouse-click mouse-button
						     shift-pressed
						     control-pressed
                                                     meta-pressed
						     tree-buffer-name))
	 (phw-button (nth 0 phw-button-list))
	 (shift-mode (nth 1 phw-button-list))
         (meta-mode (nth 2 phw-button-list))
         (keyboard-p (equal (nth 3 phw-button-list) 'keyboard))
         (maximized-p (phw-buffer-is-maximized-p tree-buffer-name))
         (select-callbacks (phw-tree-buffer-callbacks-alist-of-type 'select))
         (callback-fcn nil))
    ;; we need maybe later that something has clicked in a tree-buffer, e.g.
    ;; in `phw-handle-major-mode-visibilty'.
    (setq phw-item-in-tree-buffer-selected t)
    (if (not keyboard-p)
        (setq phw-layout-prevent-handle-phw-window-selection t))
    
    ;; first we dispatch to the right action
    (when phw-button-list
      (setq callback-fcn
            (phw-member-of-symbol/value-list tree-buffer-name
                                             select-callbacks
                                             'car
                                             'cdr))
      (when (functionp callback-fcn)
        (funcall callback-fcn node phw-button nil shift-mode meta-mode)))

    ;; now we go back to the tree-buffer but only if all of the following
    ;; conditions are true:
    ;; 1. The phw-windows are now not hidden
    ;; 2. The tree-buffer-name is contained in
    ;;    phw-tree-do-not-leave-window-after-select--internal
    ;; 3. Either it is not the phw-directories-buffer-name or
    ;;    at least `phw-show-sources-in-directories-buffer-p' is true and the
    ;;    hitted node is a sourcefile
    (when (and (not (phw-windows-all-hidden))
               (phw-member-of-symbol/value-list
                tree-buffer-name
                phw-tree-do-not-leave-window-after-select--internal)
               (or (not (phw-string= tree-buffer-name phw-directories-buffer-name))
                   (and (phw-show-sources-in-directories-buffer-p)
                        (= phw-directories-nodetype-sourcefile
                           (tree-node->type node)))))
      ;; If there is currently no maximized window then we can savely call
      ;; `phw-goto-phw-window'. If we have now a maximized window then there
      ;; are two possibilities:
      ;; - if it is not equal to the maximzed tree-buffer before the selection
      ;;   then we must maximi
      (if (and maximized-p
               (not (phw-buffer-is-maximized-p tree-buffer-name)))
          (phw-maximize-phw-buffer tree-buffer-name t)
        (phw-goto-phw-window tree-buffer-name))
      (tree-buffer-remove-highlight))))
 
(defun phw-tree-buffer-node-collapsed-callback (node
                                                mouse-button
                                                shift-pressed
                                                control-pressed
                                                meta-pressed
                                                tree-buffer-name)
  "This is the callback-function phw.el gives to every tree-buffer to call
when a node has been collapsed."
  (let* ((phw-button-list (phw-interpret-mouse-click mouse-button
						     shift-pressed
						     control-pressed
                                                     meta-pressed
						     tree-buffer-name))
         (keyboard-p (equal (nth 3 phw-button-list) 'keyboard)))
    (if (not keyboard-p)
        (setq phw-layout-prevent-handle-phw-window-selection t))))

(defun phw-tree-buffer-node-expand-callback (node
					     mouse-button
					     shift-pressed
					     control-pressed
                                             meta-pressed
					     tree-buffer-name)
  "This is the callback-function phw.el gives to every tree-buffer to call
when a node should be expanded. This function does nothing if the click
combination is invalid \(see `phw-interpret-mouse-click')."
  (let* ((phw-button-list (phw-interpret-mouse-click mouse-button
						     shift-pressed
						     control-pressed
                                                     meta-pressed
						     tree-buffer-name))
	 (phw-button (nth 0 phw-button-list))
	 (shift-mode (nth 1 phw-button-list))
         (meta-mode (nth 2 phw-button-list))
         (keyboard-p (equal (nth 3 phw-button-list) 'keyboard))
         (expand-callbacks (phw-tree-buffer-callbacks-alist-of-type 'expand))
         (callback-fcn nil))
    (if (not keyboard-p)
        (setq phw-layout-prevent-handle-phw-window-selection t))
    ;; we just dispatch to the right action
    (when phw-button-list
      (setq callback-fcn
            (phw-member-of-symbol/value-list tree-buffer-name
                                             expand-callbacks
                                             'car
                                             'cdr))
      (when (functionp callback-fcn)
        (funcall callback-fcn node phw-button nil shift-mode meta-mode)))))

(defun phw-interpret-mouse-click (mouse-button
                                  shift-pressed
                                  control-pressed
                                  meta-pressed
                                  tree-buffer-name)
  "Converts the physically pressed MOUSE-BUTTON \(1 = mouse-1, 2 = mouse-2, 0 =
no mouse-button but the keys RET or TAB) to PHW-mouse-buttons: either primary
or secondary mouse-button depending on the value of CONTROL-PRESSED and the
setting in `phw-primary-secondary-mouse-buttons'. Returns a list
'\(<PHW-button> <shift-mode> <meta-mode> <device>) where <PHW-button> is
either 1 \(= primary) or 2 \(= secondary) and <shift-mode> and <meta-mode> are
non nil if SHIFT-PRESSED rsp. META-PRESSED is non nil. <device> is either
'mouse or 'keyboard dependent if the uses has used the mouse rsp. the keyboard
in the tree-buffer. For an invalid and not accepted click combination nil is
returned.

Note: If MOUSE-BUTTON is 0 \(means no mouse-button but a key like RET or TAB
was hitted) then CONTROL-PRESSED is interpreted as PHW-button 2.

Currently the fourth argument TREE-BUFFER-NAME is not used here."
  (if (eq mouse-button 0)
      (list (if control-pressed 2 1) shift-pressed meta-pressed 'keyboard)
    (if (and (not (eq mouse-button 1)) (not (eq mouse-button 2)))
	nil
      (case phw-primary-secondary-mouse-buttons
        (mouse-1--mouse-2
         (if control-pressed
             nil
           (list mouse-button shift-pressed meta-pressed 'mouse)))
        (mouse-1--C-mouse-1
         (if (not (eq mouse-button 1))
             nil
           (list (if control-pressed 2 1) shift-pressed meta-pressed 'mouse)))
        (mouse-2--C-mouse-2
         (if (not (eq mouse-button 2))
             nil
           (list (if control-pressed 2 1) shift-pressed meta-pressed 'mouse)))
        (otherwise nil)))))

(defun phw-show-minibuffer-info (node window when-spec)
  "Checks if any info about the current node in the PHW-window WINDOW should
be displayed. WHEN-SPEC must have the same format as the car of
`phw-directories-show-node-info'."
  (or (eq when-spec 'always)
      (and (eq when-spec 'if-too-long)
           window
           (>= (tree-node-linelength node)
               (window-width window)))))


(tree-buffer-defpopup-command phw-maximize-phw-window-menu-wrapper
  "Expand the current PHW-window from popup-menu."
  (phw-maximize-phw-buffer (buffer-name (current-buffer)) t))

;; stealthy mechanism

(defvar phw-stealthy-function-list nil
  "List of functions which PHW runs stealthy. Do not modify this variable!
This variable is autom. set by the macro `defphw-stealthy'!")

(defvar phw-stealthy-function-state-alist nil
  "Alist which stores the state of each function of
`phw-stealthy-function-list'. Do not add new items to this variable because
this is autom. done by the macro `defphw-stealthy'!")

(defun phw-stealthy-function-list-add (fcn)
  (add-to-list 'phw-stealthy-function-list fcn))

(defun phw-stealthy-function-state-alist-add (fcn)
  (add-to-list 'phw-stealthy-function-state-alist
               (cons fcn 'done)))

(defun phw-stealthy-function-state-get (fcn)
  "Getter for `phw-stealthy-function-state-alist'. Return state for the
stealthy function FCN."
  (cdr (assoc fcn phw-stealthy-function-state-alist)))

(defun phw-stealthy-function-state-set (fcn state)
  "Setter for `phw-stealthy-function-state-alist'. Set STATE for the
stealthy function FCN. Return STATE."
  (setcdr (assoc fcn phw-stealthy-function-state-alist) state))

(defun phw-stealthy-function-p (fcn)
  "Return not nil if FCN is a stealthy function defined with
`defphw-stealthy'."
  (member fcn phw-stealthy-function-list))

(defun phw-stealthy-function-state-init (&optional fcn state)
  "Reset the state of stealthy functions. If first optional arg FCN is a
stealthy function then only the state of this function is reset - otherwise
all stealthy functions of `phw-stealthy-function-list' are reset. If second
optional arg STATE is nil then the state will be reset to the special state
'restart - otherwise to the value STATE."
  (if (and fcn (phw-stealthy-function-p fcn))
      (phw-stealthy-function-state-set fcn (or state 'restart))
    (dolist (f phw-stealthy-function-list)
      (phw-stealthy-function-state-set f (or state 'restart)))))

(defmacro defphw-stealthy (name docstring &rest body)
  "Define a so called stealthy function with NAME. This function will be
registered by this macro in `phw-stealthy-function-list' and
`phw-stealthy-function-state-alist'. During the evaluation of BODY the
variable `state' will be bound and initialized with the stealthy state. BODY
can use and modify `state'. After evaluating BODY `state' will be
automatically saved so its available at the runtime of this stealthy function.
BODY will only be evaluated if `state' is not 'done. BODY should be designed
to be interruptable by the user, so its recommended to use for this
`phw-exit-on-input' ans `phw-throw-on-input' \(see example in
`phw-test-throw-on-input'). If BODY completes then BODY has to set `state' to
the special value 'done! If BODY has been interrupted then `state' can have an
arbitrary value which will be autom. stored and at next runtime of the
stealthy function NAME `state' will be initialized with this stored value. If
`state' is initialized with the special value 'restart then this means the
stealthy function should start from scratch because an eventually stored state
is not longer valid. If the stealthy function sets `state' to 'done then this
function will first being called after the state for this function has been
reset to something else than 'done \(mostly to 'restart)\; such a reset of the
state for a stealthy function can be done by any code and must be done via
`phw-stealthy-function-state-init'!"
  `(progn
     (unless (fboundp (quote ,name))
       (phw-stealthy-function-list-add (quote ,name))
       (phw-stealthy-function-state-alist-add (quote ,name)))
     (eval-and-compile
       (unless (fboundp (quote ,name))
         (defun ,name nil
           ,docstring
           (let ((state (phw-stealthy-function-state-get (quote ,name))))
             (unless (equal state 'done)
               ,@body)
             (phw-stealthy-function-state-set (quote ,name) state)))))))
  
(put 'defphw-stealthy 'lisp-indent-function 1)

(defvar phw-stealthy-update-running nil
  "Recursion avoidance variable for stealthy performance.")

(defphw-autocontrol/sync-function phw-stealthy-updates nil nil nil
  "Run all functions in the stealthy function list.
Each function returns 'done if it completes successfully, or something else if
interrupted by the user \(i.e. the function has been interrupted by the
user). If a function is interrupted then `phw-stealthy-function-list' is
rotated so the interrupted function is the first element so the next stealthy
run starts with this interrupted function."
  (unless phw-stealthy-update-running
    (let ((l phw-stealthy-function-list)
          (phw-stealthy-update-running t)
          ;; necessary because timers set this locally to t to prevent
          ;; timer-actions from being quitted by C-g. Our potentially long
          ;; lasting actions must be quit-able!
          (inhibit-quit nil))
      (while (and l (equal 'done (funcall (car l))))
        (setq l (cdr l)))
      ;; if l is nil this means all functions have successfully completed -
      ;; otherwise we ensure that next time we start with the interrupted
      ;; function.
      (when l
        (setq phw-stealthy-function-list
              (phw-rotate phw-stealthy-function-list (car l)))))))

;
;; generation of nodes rsp. of attributes of nodes

(defun phw-generate-node-name (text-name first-chars icon-name name-of-buffer)
  "Generate a new name from TEXT-NAME by adding an appropriate image according
to ICON-NAME to the first FIRST-CHARS of TEXT-NAME. If FIRST-CHARS is < 0 then
a string with length abs\(FIRST-CHARS) is created, the image is applied to
this new string and this \"image\"-string is added to the front of TEXT-NAME.
If no image can be found for ICON-NAME then the original TEXT-NAME is
returned. NAME-OF-BUFFER is the name of the tree-buffer where the resulting
node-name will be displayed.

If an image is added then two text-properties are added to the FIRST-CHARS of
the returned string: 'tree-buffer-image-start which holds 0 as value and
'tree-buffer-image-length which holds the absolute value of FIRST-CHARS
value."
  (let ((image nil)
        (ret nil))
    (with-current-buffer name-of-buffer
      (setq image (and icon-name (tree-buffer-find-image icon-name)))
      (setq ret
            (if image
                (if (> first-chars 0)
                    (tree-buffer-add-image-icon-maybe
                     0 first-chars text-name image)
                  (concat (tree-buffer-add-image-icon-maybe
                           0 (- first-chars)
                           (make-string (- first-chars) ? ) image)
                          text-name))
              text-name)))
    ret))


(silentcomp-provide 'phw-common-browser)

;;; phw-common-browser.el ends here
