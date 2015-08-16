;;; phw-examples.el --- examples for using PHW with Elisp

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2002

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id$

;;; Commentary:
;;
;; Contains the code for some examples mentioned in the online-help.
;; This is a full working layout-example to demonstrate how to program
;; complete new special windows/buffers, add them to a layout and synchronize
;; it with the edit-window of PHW.
;;
;; To test this example just do:
;; 1. Start PHW 
;; 2. Load phw-examples.el into (X)Emacs: (require 'phw-examples)
;; 3. Call `phw-show-layout-help' and insert "example-layout1" as layout name
;;    to see the outline of the test layout and get information about the
;;    special windows of this layout.
;; 4. Call `phw-examples-activate'.
;; 5. Play around with the new layout and test it.
;; 6. Call `phw-examples-deactivate'.
;;
;; The intention of this example is to be a skeleton and pattern for other
;; packages which want to use the layout-engine of PHW do display their own
;; information. For example graphical debuggers (like JDEbug of JDEE) could be
;; made this way.

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.


;;; Code

;; ---------------------------------------------------------------------------
;; --- Some requirements we always need if using the PHW layout-engine -------
;; ---------------------------------------------------------------------------

(require 'phw-util)
(require 'phw-layout)
(require 'phw-common-browser)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

;; ---------------------------------------------------------------------------
;; --- Code for the bufferinfo buffer ----------------------------------------
;; ---------------------------------------------------------------------------

;; Normally we should have some options the user can customize

;; Lets define an own customize-group
(defgroup phw-examples nil
  "Settings for the bufferinfo example in the Emacs code browser."
  :group 'phw-examples-bufferinfo
  :prefix "phw-")

;; The following three options are typical for a special PHW-buffer which
;; should be synchronized with current buffer in the edit-area.

;; The :type of the first two options is essential and MUST NOT be defined
;; different, because the macro `defphw-autocontrol/sync-function' and the
;; function `phw-activate-phw-autocontrol-function' expects exactly this
;; option-type!

;; An own hook running after synchronizing is not essential but mostly useful
;; for users who wants to do some own stuff.

(defcustom phw-examples-bufferinfo-buffer-sync 'basic
  "*Synchronize the bufferinfo buffer automatically with current edit buffer.

If 'always then the synchronization takes place always a buffer changes in the
edit window, if nil then never. If a list of major-modes then only if the
`major-mode' of the new buffer belongs NOT to this list.

If the special value 'basic is set then PHW uses the setting of the option
`phw-basic-buffer-sync'.

IMPORTANT NOTE: Every time the synchronization is done the hook
`phw-bufferinfo-buffer-sync-hook' is evaluated."
  :group 'phw-examples-bufferinfo
  :type '(radio :tag "Synchronize PHWs example bufferino buffer"
                (const :tag "use basic value" :value basic)
                (const :tag "Always" :value always)
                (const :tag "Never" nil)
                (repeat :tag "Not with these modes"
                        (symbol :tag "mode"))))

(defcustom phw-examples-bufferinfo-buffer-sync-delay 'basic
  "*Time Emacs must be idle before the bufferinfo-buffer is synchronized.
Synchronizing is done with the current source displayed in the edit window. If
nil then there is no delay, means synchronization takes place immediately. A
small value of about 0.25 seconds saves CPU resources and you get even though
almost the same effect as if you set no delay.

If the special value 'basic is set then PHW uses the setting of the option
`phw-basic-buffer-sync-delay'"
  :group 'phw-analyse
  :type '(radio (const :tag "use basic value" :value basic)
                (const :tag "No synchronizing delay" :value nil)
                (number :tag "Idle time before synchronizing" :value 2))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'phw-minor-mode)
                            (featurep 'phw-examples)
                            phw-minor-mode)
                       (phw-activate-phw-autocontrol-function
                        value 'phw-examples-bufferinfo-buffer-sync))))
  :initialize 'custom-initialize-default)

(defcustom phw-examples-bufferinfo-buffer-sync-hook nil
  "Hook run at the end of `phw-examples-bufferinfo-buffer-sync'.
See documentation of `phw-examples-bufferinfo-buffer-sync' for conditions when
synchronization takes place and so in turn these hooks are evaluated.

Preconditions for such a hook:
- Current buffer is the buffer of the currently selected
  edit-window.
- The bufferinfo-buffer is displayed in a visible window of the
  phw-frame \(so no check for visibilty of the bufferinfo-buffer in
  the phw-frame is necessary in a hook function)

Postcondition for such a hook:
Point must stay in the same edit-window as before evaluating the hook.

Important note: If the option
`phw-examples-bufferinfo-buffer-sync' is not nil the function
`phw-examples-bufferinfo-buffer-sync' is running either every
time Emacs is idle or even after every command \(see
`phw-examples-bufferinfo-buffer-sync-delay'). So if the
bufferinfo-buffer is displayed in a window of the phw-frame \(see
preconditions above) these hooks can be really called very often!
Therefore each function of this hook should/must check in an
efficient way at beginning if its task have to be really
performed and then do them only if really necessary! Otherwise
performance of Emacs could slow down dramatically!"
  :group 'phw-analyse
  :type 'hook)

;; --------------- internals for bufferinfo-buffer -------------------


(defconst phw-examples-bufferinfo-buffer-name " *PHW buffer info*")
(defvar phw-examples-bufferinfo-last-file-buffer nil)

;; Two helper functions for displaying infos in a special buffer

(defun phw-examples-print-file-attributes (buffer filename)
  "Insert in buffer BUFFER some file-information about FILENAME."
  (phw-with-readonly-buffer buffer
    (erase-buffer)
    (insert (format "Bufferinfo for %s:\n\n" filename))
    (let* ((attributes (file-attributes filename))
           (type (format "Type: %s\n" (cond ((null (nth 0 attributes))
                                             "File")
                                            ((equal (nth 0 attributes) t)
                                             "Directory")
                                            ((stringp (nth 0 attributes))
                                             (concat "Link to "
                                                     (nth 0 attributes))))))
           (size (format "Size: %d\n" (nth 7 attributes)))
           (modes (format "Modes: %s\n" (nth 8 attributes))))
      (insert type size modes))))


(defun phw-examples-print-non-filebuffer (buffer buffer-name)
  "Insert in buffer BUFFER a small message for buffer with name BUFFER-NAME."
  (phw-with-readonly-buffer buffer
    (erase-buffer)
    (insert (format "Bufferinfo for buffer %s\n\n" buffer-name))
    (insert "This is a not a filebuffer, so there are no\n")
    (insert "informations available.")))

;; IMPORTANT: The main synchronizing function must be defined with the macro
;; `defphw-autocontrol/sync-function'!

(defphw-autocontrol/sync-function phw-examples-bufferinfo-buffer-sync
    phw-examples-bufferinfo-buffer-name phw-examples-bufferinfo-buffer-sync t
  "Synchronizes the buffer-info buffer with current source if changed.
Can be called interactively but normally this should not be necessary because
it will be called autom. by the internal synchronizing mechanism of PHW."

  ;; The macro `defphw-autocontrol/sync-function' does a lot for our
  ;; conveniance:
  
  ;; 1) here we can be sure that the buffer with name
  ;; `phw-examples-bufferinfo-buffer-name' is displayed in a window of
  ;; `phw-frame' because the macro `defphw-autocontrol/sync-function'
  ;; encapsulates automatically the following code within
  ;; `phw-do-if-buffer-visible-in-phw-frame' and this macro binds locally the
  ;; variables visible-buffer and visible-window: visible-window:=
  ;; (get-buffer-window phw-examples-bufferinfo-buffer-name) visible-buffer:=
  ;; (get-buffer phw-examples-bufferinfo-buffer-name)

  ;; 2) The macro `defphw-autocontrol/sync-function' automatically takes care of
  ;; the setting of option `phw-examples-bufferinfo-buffer-sync' and runs the
  ;; following code only when the related conditions are true

  ;; 3) The generated function has one optional argument FORCE which can be used
  ;; in the code below.
  
  ;; 4) The macro `defphw-autocontrol/sync-function' makes this synchronizing
  ;; function interactive

  ;; For details please read the documentation of
  ;; `defphw-autocontrol/sync-function'!

  ;; synchronize only when point stays in one of the edit-window.
  (when (phw-point-in-edit-window-number)

    ;; we need the file-name of indirect-buffers too (if the base-buffer is a
    ;; file-buffer), therefore we use `phw-buffer-file-name' (see the docstring
    ;; of this function)
    (let ((filename (phw-buffer-file-name (current-buffer))))
    
      (if (and filename (phw-buffer-or-file-readable-p filename))

          ;; synchronizing for real filesource-buffers or indirect buffers of
          ;; real file buffers

            ;; Let us be smart: We synchronize only if sourcebuffer has changed
            ;; or if the argument FORCE is not nil
            (when (or force
                      (not (equal (current-buffer)
                                  phw-examples-bufferinfo-last-file-buffer)))
              ;; set new last-file-buffer so we can check next time if changed
              (setq phw-examples-bufferinfo-last-file-buffer (current-buffer))
              ;; we display the file-infos for current source-buffer
              (phw-examples-print-file-attributes visible-buffer filename))
        
        ;; what should we do for non file buffers like help-buffers etc...
        (setq phw-examples-bufferinfo-last-file-buffer nil)
        (phw-examples-print-non-filebuffer visible-buffer
                                           (buffer-name (current-buffer)))))

    ;; Now lets run the hooks in `phw-examples-bufferinfo-buffer-sync-hook'
    (run-hooks 'phw-examples-bufferinfo-buffer-sync-hook)))


;; Two conveniance-commands for the user

(defun phw-maximize-bufferinfo-window ()
  "Maximize the bufferinfo-window.
I.e. delete all other PHW-windows, so only one PHW-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works
also if the PHW-analyse-window is not visible in current layout."
  (interactive)
  (phw-maximize-phw-buffer phw-examples-bufferinfo-buffer-name t))

(defun phw-goto-bufferinfo-window ()
  "Make the bufferinfo-window the current window."
  (interactive)
  (phw-goto-phw-window phw-examples-bufferinfo-buffer-name))


;; The "window-dedicator"-function for the bufferinfo-buffer. See
;; `defphw-window-dedicator-to-phw-buffer' for an explanation.

(defphw-window-dedicator-to-phw-buffer phw-examples-set-bufferinfo-buffer
    phw-examples-bufferinfo-buffer-name nil
  "Set the buffer in the current window to the bufferinfo-buffer and make this
window dedicated for this buffer. Makes the buffer read-only."
  ;; activating the synchronization of the bufferinfo-window:
  ;; `phw-activate-phw-autocontrol-function' takes care of the possible
  ;; settings in `phw-examples-bufferinfo-buffer-sync-delay'. Therefore we do
  ;; it here because then changes in phw-examples-bufferinfo-buffer-sync-delay
  ;; are taken into account each time the bufferinfo buffer is set in the
  ;; layout (after each hiding/showing the phw-window, each redrawing the
  ;; layout deactivating/activating PHW)
  (phw-activate-phw-autocontrol-function phw-examples-bufferinfo-buffer-sync-delay
                                         'phw-examples-bufferinfo-buffer-sync)
  
  (switch-to-buffer (get-buffer-create phw-examples-bufferinfo-buffer-name))
  (setq buffer-read-only t))

;; ---------------------------------------------------------------------------
;; --- Code for the action buffer --------------------------------------------
;; ---------------------------------------------------------------------------


(defconst phw-examples-action-buffer-name " *PHW action buffer*")
(defvar phw-examples-action-buffer-keymap nil)



;; Two helper functions for creating a read-only buffer with a special local
;; key-map.

(defun phw-examples-insert-text-in-action-buffer (text)
  "Insert TEXT at point and make it highlight-able for mouse-movement over the
text."
  (let ((p (point)))
    (insert text)
    (put-text-property p (+ p (length text)) 'mouse-face 'highlight)))

(defun phw-examples-action-buffer-create ()
  "Return the action-buffer with name `phw-examples-action-buffer-name' If
the buffer does not exist it will be created. The buffer is read only,
contains two buttons \[prior] and \[next] and mouse-2 calls
`phw-examples-action-buffer-clicked'."
  (save-excursion
    (if (get-buffer phw-examples-action-buffer-name)
        (get-buffer phw-examples-action-buffer-name)
      (let ((nop (function (lambda() (interactive)))))
        (set-buffer (get-buffer-create phw-examples-action-buffer-name))

        ;; we setup a local key-map
        
        (make-local-variable 'phw-examples-action-buffer-keymap)
        (setq phw-examples-action-buffer-keymap (make-sparse-keymap))
        
        ;; define mouse-2 with `phw-examples-action-buffer-clicked'
        (define-key phw-examples-action-buffer-keymap
          (if phw-running-xemacs '(button2) [down-mouse-2])
          'phw-examples-action-buffer-clicked)

        ;; nop operations for the other mouse-2 operations with Emacs
        (define-key phw-examples-action-buffer-keymap [mouse-2] nop)
        (define-key phw-examples-action-buffer-keymap [double-mouse-2] nop)
        (define-key phw-examples-action-buffer-keymap [triple-mouse-2] nop)
        
        (use-local-map phw-examples-action-buffer-keymap)

        ;; insert the action buttons [prior] and [next] and make it read-only

        (phw-with-readonly-buffer (current-buffer)
         (erase-buffer)
         (phw-examples-insert-text-in-action-buffer "[prior]")
         (insert "\n")
         (phw-examples-insert-text-in-action-buffer "[next]")
         (insert "\n"))
        
        (current-buffer)))))



;; The function which performs the actions in the action-buffer

(defun phw-examples-action-buffer-clicked (e)
  "Perform the right action for the mouse-click.
If the user clicks onto \[prior] the buffer in the edit-window is scrolled up,
if clicks onto \[next] the buffer in the edit-window is scrolled down.
Otherwise nothing will be done."
  (interactive "e")
  (mouse-set-point e)
  (let ((line (phw-buffer-substring (phw-line-beginning-pos) (phw-line-end-pos))))
    (save-match-data
      (cond ((string-match "prior" line)
             (phw-select-edit-window)
             (call-interactively 'scroll-down))
            ((string-match "next" line)
             (phw-select-edit-window)
             (call-interactively 'scroll-up))
            (t nil)))))


;; Two conveniance-commands for the user

(defun phw-maximize-action-window ()
  "Maximize the action-window.
I.e. delete all other PHW-windows, so only one PHW-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works
also if the PHW-analyse-window is not visible in current layout."
  (interactive)
  (phw-maximize-phw-buffer phw-examples-action-buffer-name t))

(defun phw-goto-action-window ()
  "Make the action-window the current window."
  (interactive)
  (phw-goto-phw-window phw-examples-action-buffer-name))


;; The "window-dedicator"-function for the action-buffer. See
;; `defphw-window-dedicator-to-phw-buffer' for an explanation.

(defphw-window-dedicator-to-phw-buffer phw-examples-set-action-buffer
    phw-examples-action-buffer-name nil
  "Set the buffer in the current window to the action-buffer and make this
window dedicated for this buffer."
  (switch-to-buffer (buffer-name (phw-examples-action-buffer-create))))

;; ---------------------------------------------------------------------------
;; --- The layout definition with a bufferinfo- and a action-buffer -----------
;; ---------------------------------------------------------------------------


(phw-layout-define "example-layout1" top
  "This function creates the following layout:

   -------------------------------------------------------
   |Bufferinfo for <filename>:            |[prior]       |
   |Type: file                            |[next]        |
   |Size: 23456                           |              |
   |Modes: rw-rw-rw-                      |              |
   |-----------------------------------------------------|
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   |                    Edit                             |
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place.

The top-left window always displays informations about the current buffer in
the selected edit-window. This window demonstrates how autom. synchronizing
a special window/buffer of a layout with current edit-window.

The top-right window offers two buttons which can be used with the middle
mouse-button to scroll the edit-window. This is not very senseful but it
demonstrates how to drive the edit-window with actions performed in a special
window/buffer of a layout."

  ;; dedicating the bufferinfo window to the bufferinfo-buffer
  (phw-examples-set-bufferinfo-buffer)
  
  ;; creating the action-window
  (phw-split-hor 0.75)
  
  ;; dedicating the action window to the action-buffer
  (phw-examples-set-action-buffer)

  ;; selecting the edit-window
  (select-window (next-window)))


;; ---------------------------------------------------------------------------
;; --- (De)activating the new layout and the synchronization -----------------
;; ---------------------------------------------------------------------------



;; Code for saving and restoring the state before activation

(defvar phw-examples-preact-layout nil)
(defvar phw-examples-preact-windows-height nil)
(defvar phw-examples-preact-compile-window-height nil)
(defun phw-examples-preactivation-state(action)
  (cond ((equal action 'save)
         (setq phw-examples-preact-layout
               phw-layout-name
               phw-examples-preact-windows-height
               phw-windows-height
               phw-examples-preact-compile-window-height
               phw-compile-window-height))
        ((equal action 'restore)
         (setq phw-layout-name
               phw-examples-preact-layout
               phw-windows-height
               phw-examples-preact-windows-height
               phw-compile-window-height
               phw-examples-preact-compile-window-height))))



;; Activation of the example. Because a set of new special windows integrated
;; in a new layout is often just the GUI of a complete tool (like a graphical
;; debugger) we demonstrate here the complete activation and deactivation of
;; such a tool or at least of the tool-GUI. We decide that the GUI of our
;; example tool needs a compile-window with height 5 lines and the height of
;; the special windows "row" on top should be exactly 6 lines (normally width
;; and height of the special windows should be a fraction of the frame, but
;; here we use 6 lines; You can change the code below to use a frame-fraction
;; of 0.2 instead of 6 hard lines, just try it!

(defun phw-examples-activate ()
  "Activate the new layout \"example-layout1\".
Set `phw-compile-window-height' to 5 and `phw-windows-height' to 6. The
preactivation-state is saved and will be restored by
`phw-examples-deactivate'."
  (interactive)

  (assert (featurep 'phw) nil
          "PHW must be loaded!")
  (assert phw-minor-mode nil
          "PHW must be activated!")
  (assert (equal (selected-frame) phw-frame) nil
          "The PHW-frame must be selected!")
  (assert (not (phw-string= phw-layout-name "example-layout1")) nil
          "The examples-layout1 is already active!")
  
  ;; saving the state
  (phw-examples-preactivation-state 'save)

  ;; switch to our prefered layout
  (setq phw-windows-height 6)
  (setq phw-compile-window-height 8)
  (let ((phw-change-layout-preserves-compwin-state nil))
    ;; activating the synchronization of the bufferinfo-window is done in the
    ;; dedicator-function (see `phw-examples-set-bufferinfo-buffer' for the
    ;; reason). So the synchronizing will be activated implicitly with the
    ;; layout-switch because this redraws the layout and this calls all
    ;; dedicator-functions.
    (phw-layout-switch "example-layout1")))


;; Deactivation of the example

(defun phw-examples-deactivate ()
  "Deactivate the new layout \"example-layout1\".
Stops `phw-examples-bufferinfo-buffer-sync' and restore the state
as before activation."
  (interactive)

  (assert (featurep 'phw) nil
          "PHW must be loaded!")
  (assert phw-minor-mode nil
          "PHW must be activated!")
  (assert (equal (selected-frame) phw-frame) nil
          "The PHW-frame must be selected!")
  (assert (phw-string= phw-layout-name "example-layout1") nil
          "The example-layout1 is not active!")
  
  (phw-stop-autocontrol/sync-function 'phw-examples-bufferinfo-buffer-sync)
  
  (phw-examples-preactivation-state 'restore)
  
  (phw-layout-switch phw-layout-name))
  

;; ---------------------------------------------------------------------------
;; --- Providing the examples ------------------------------------------------
;; ---------------------------------------------------------------------------

(provide 'phw-examples)

;; phw, 2015, 2015-examples.el ends here
