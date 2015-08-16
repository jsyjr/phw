;;; phw-upgrade.el --- Upgrade an old phw-version to the latest one

;; Copyright (C) 2000 - 2015 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Ryan Ware,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;;         Ryan Ware <ryan.r.ware@intel.com>
;; Maintainer: Ryan Ware <ryan.r.ware@intel.com>
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
;; This file upgrades an old PHW-version best possible to the latest one.
;;
;; What is the intention of this library:
;;
;; Big packages like PHW will be enhanced and developed continuously so
;; sometimes a new version must be released. Such packages offer in general a
;; lot of customizable options so probably some of these options change the
;; type or are renamed because the old type and/or name of the option makes no
;; sense in the new release.
;;
;; Especially options which have changed the type of their value are now a
;; problem for the user which want to upgrade to the latest PHW-version: If
;; the user has saved a certain value for option X in its .emacs-file but the
;; type of this saved value doesn't match the new defined type in the
;; defcustom-form after an PHW-upgrade then there can occur serious problems
;; like PHW can not be started anymore or even Emacs can not be started
;; without errors.
;;
;; Until now there was only one way to fix these problems: The user must
;; manually edit his .emacs-file and remove all entries for options which have
;; now another type. After this and after restarting Emacs the new
;; default-values of the type-changed options in the new PHW-release are
;; active and the user can go on using Emacs and PHW. But this approach to fix
;; the incompatible-option-problem has two serious drawbacks:
;; 1. The user must manually edit the customize-section in his .emacs-file.
;;    This should normally not be done and if then only by old-handed
;;    Emacs-users.
;; 2. The customized value of the option X in the old-release (with the old
;;    type) is lost because after removing the related entry from the
;;    .emacs-file the new default-value is active, so the user must
;;    re-customize the option X.
;;
;; OK, this is one half of the option-upgrade-problem but a new PHW-release
;; can also rename a option from name X to name Y because the new name Y makes
;; much more sense and/or is more mnemonic. If only the name has changed but
;; not the type this is not a serious problem like above but also annoying
;; because the customized value of the old-option X takes no effect in the new
;; release but instead the default-value of the new-option Y is now active.
;; But nevertheless this problem has the drawback number 2 (see above).
;;
;; The last category of upgrade-problems is a renamed option which has also
;; changed its type.
;;
;; phw-upgrade.el is the solution for all these problems:

;; - It checks all customized values of all PHW-options if they are still
;;   type-compatible. If not then it tries to upgrade the old-value to the new
;;   value-type and if this is not possible then it resets the option to the
;;   new default value and store it via customize in the .emacs-file (or in
;;   any file which is used for customized options).
;; - It offers a special constant `phw-upgradable-option-alist' which allows
;;   the PHW-maintainers to define special transformings for renamed options
;;   so even the value of an old-option X can be savely transformed to the
;;   new-option Y and the old setting is not lost.
;;
;; All these checks and transformings are done at beginning of activating PHW.
;; If PHW has recognized incompatible or renamed options it does its
;; upgrading/reseting-job so all PHW-options have correct types so PHW can
;; start correct. After PHW is started it displays a list of all upgraded or
;; reseted option with their old and new values.
;;
;; How does this library work:
;;
;; The important functions are `phw-check-not-compatible-options' and
;; `phw-upgrade-not-compatible-options':
;;
;; The former one checks if all customized values of PHW-options have still
;; correct type. If not the incompatible options and their old values are
;; stored in an alist `phw-not-compatible-options'. Only this function is
;; allowed to changed this alist!!
;;
;; The latter one processes now this alist and looks for every incompatible
;; option if there is an entry in `phw-upgradable-option-alist'. If yes then a
;; special value-transforming is tried by `phw-option-upgrade'. If no or if
;; the special transforming has been failed for any reason then it resets the
;; option to the default-value of current active PHW-version and save it via
;; `customize-save-variable'.
;;
;; So if the PHW-maintainers define no special transforming in the alist
;; `phw-upgradable-option-alist' for a re-typed option X then all incompatible
;; options are at least reset to their current default-value and therefore PHW
;; can start correct.
;;
;; But there is another function `phw-upgrade-renamed-options': This function
;; processes the alist `phw-upgradable-option-alist' and call for every
;; element-key (an old option-symbol) of this alist `phw-option-upgrade' but
;; only if this element-key is not also contained in the alist
;; `phw-not-compatible-options' because in this case this option has been
;; already be upgraded/reseted by `phw-upgrade-not-compatible-options' (see
;; above).
;;
;; So the calling sequence of these three functions must be:
;; 1. `phw-check-not-compatible-options'
;; 2. `phw-upgrade-not-compatible-options'
;;    `phw-upgrade-renamed-options' or vice versa.
;; 
;; There are also two interactive commands:
;; - `phw-display-upgraded-options' displays a temp. buffer with all upgraded
;;   or reseted PHW-options with their old and new values.
;; - `phw-upgrade-options': Does all necessary beginning with the
;;   incompatibility-check and ending with the display of the options.
;;
;; What must an PHW-maintainer do:
;;
;; + If he wants only a save and correct PHW-start with the new release:
;;   NOTHING
;; + If he wants to preserve best possible the customized values of now
;;   type-incompatible and/or renamed options:
;;   - Adding entries to the alist `phw-upgradable-option-alist' and
;;   - Defining suitable transforming-functions for every of these options.
;;   See the comment of `phw-upgradable-option-alist'.

;;; History
;;
;; For the ChangeLog of this file see the git-repository. For a complete
;; history of the PHW-package see the file NEWS.


;;; Code

(eval-when-compile
  (require 'silentcomp))

;; IMPORTANT: The version-number is auto-frobbed from the Makefile. Do not
;; change it here!
;; TODO: Makefile frobbing broken

(defconst phw-version "2.50"
  "Current PHW version.")

(eval-when-compile
  (require 'cl))

(require 'phw-util)

(silentcomp-defun widget-convert)
(silentcomp-defun phw-find-optionsym-for-tree-buffer-name)

;; -------------------------------------------------------------------------
;; define in this defconst all important NEWS which a user should know after
;; upgrading to the new version.
;; -------------------------------------------------------------------------

;; Each NEWS-string should be a one-liner shorter than 70 chars
(defconst phw-upgrade-news
  '(("2.50" . ("PHW now requires full CEDET being installed (at least 2.0)."
	       "By default PHW now utilizes CEDET distributed as part of Emacs."
	       "PHW now requires Emacs 24.4 (though previous versions may work."
               "This release includes numerous fixes related to moving to the latest Emacs"
               ))
    ("2.40" . ("PHW now requires full CEDET being installed (at least 1.0pre6)."
               "More user-responsible buffer-parsing based on the idle-mechanism of semantic."
               "PHW is able to work with indirect buffers it the base-buffer is filebased."
               "The history can now be bucketized, see new `phw-history-make-buckets'."
               "New faces `phw-history-bucket-node-face', `phw-history-dead-buffer-face', `phw-history-indirect-buffer-face'."
               "Sticky parwnt-node for all PHW-windows of type tree-buffer."
               "New support for Git and Monotone as version-control systems."
               "New command `phw-goto-window-edit-by-smart-selection'."
               "New command `phw-goto-window-phw-by-smart-selection'."
               "New option `phw-ignore-pop-up-frames'."
               "Full compatibility with Emacs 22 and 23 - see NEWS file."
               "Better compatibility with CEDET 1.0preX and semantic 2.0preX - see NEWS file."
               "The internal download-feature of PHW has been completely disabled."
               ))
    ("2.32" . ("New PHW-window (tree-buffer) for the semantic-analyser."
               "New PHW-window for displaying definition for current symbol at point."
               "Up- and down-arrow are now also smart in the tree-buffers."
               "Much better maximizing/minimizing of the phw-tree-windows."
               "New option `phw-maximize-next-after-maximized-select'."
               "`phw-truncate-lines' has been renamed to `phw-tree-truncate-lines'"))
    ("2.30" . ("Support for displaying the VC-state in the tree-buffers; see NEWS." ;;
               "PHW is now capable of handling remote paths (e.g. TRAMP-paths)"
               "Precisely expanding of current node via popup-menu of the methods-buffer."
               "Time consuming tasks are performed stealthy; see `phw-stealthy-tasks-delay'"))
    ("2.27" . ("Much saver resizing-mechanism for permanent compile-window. See NEWS."))
    ("2.26" . ("Some regexp-options has been changed to regexp-list-options. See NEWS."
               "New option `phw-history-exclude-file-regexps'."
               "`phw-expand-methods-nodes' works for non-semantic-buffers too."
               "Readonly-sourcefiles are display in a different face."))
    ("2.25" . ("`phw-sort-history-items' has been renamed to `phw-history-sort-method'"
               "New options `phw-sources-sort-ignore-case' and `phw-history-sort-ignore-case'"
               "New icons for parent-display in the Methods-buffer"))
    ("2.24" . ("New \"current-type\"-filter for the Methods-buffer"
               "Now directories are prescanned for emptyness"))
    ("2.23" . ("New cedet1.0beta2 is supported."
               "Distinction between functions and function-prototypes in the Methods-buffer"
               "The command `phw-toggle-layout' now has a prefix-argument"
               "Default tag-filters for certain files which are applied automatically"
               "Double-clicking the mouse-1-button now works with integrated speedbar"
               "A new hook `phw-speedbar-before-activate-hook'"))
    ("2.22" . ("New nifty feature for filtering the tags displayed in the Methods-buffer"
               "Much smarter mechanism to highlight the current tag in the methods-buffer"
               "New option `phw-auto-expand-tag-tree-collapse-other'."
               "Fixed a bug preventing the native Windows-port of XEmacs from working."))
    ("2.21" . ("Advice for `balance-windows' so only the edit-windows are balanced."
               "Gnus, BBDB, VM, Xrefactory etc. work even when PHW-windows are visible."
               "Commands using `Electric-pop-up-window' now work correctly with PHW."
               "Fixed some annoying bugs and one fatal bug."))
    ("2.20" . ("Fixed a bug preventing tree-buffers with expand-symbol \'before\' to work"
               "'phw-major-modes-\(de)activate' replaced by `phw-major-modes-show-or-hide'"
               "New keybinding for the online-help: [C-c . h]"
               "The edit-area can be splitted in more than 2 windows."
               "`phw-other-window-jump-behavior' renamed in `phw-other-window-behavior'"
               "New option `phw-maximize-phw-window-after-selection'"
               "popup-menus of the tree-buffers can be used with the tmm-library"
               "New option `phw-change-layout-preserves-compwin-state'"
               "`delete-window' and `delete-other-windows' handle the compile-window"
               "Support of the default modeline-mechanisms for deleting other windows"))
    ("2.11" . ("Using semanticdb to jump to type-tags defined in other files"))
    ("2.01" . ("Support for semantic 2.0"
               "The tree-buffers can be displayed graphically with images"
               "Popup-menus of the tree-buffers support submenus"
               "The sources- and the history-buffer can be filtered"
               "Ediff runs per default in the PHW-frame"))
    ("1.96" . ("PHW can work together with the window-managers escreen and winring"
               "Much better support of the PHW-compile-window"))))



;; ----------------------------------------------------------------------
;; define in this defconst all options which should be upgraded
;; ----------------------------------------------------------------------

(defconst phw-upgradable-option-alist
  '((phw-compile-window-temporally-enlarge . (phw-compile-window-temporally-enlarge
                                              phw-upgrade-compile-window-temporally-enlarge))
    (phw-hide-phw-windows-hook . (phw-hide-phw-windows-before-hook identity))
    (phw-show-phw-windows-hook . (phw-show-phw-windows-before-hook identity))
    (phw-layout-nr . (phw-layout-name phw-upgrade-layout-nr))
    (phw-toggle-layout-sequence . (phw-toggle-layout-sequence
                                   phw-upgrade-toggle-layout-sequence))
    (phw-cache-directory-contents . (phw-cache-directory-contents
                                     phw-upgrade-cache-directory-contents))
    (phw-layout-always-operate-in-edit-window . (phw-layout-always-operate-in-edit-window
                                                 phw-upgrade-alway-operate-in-edit-window))
    (phw-truncate-lines . (phw-tree-truncate-lines phw-upgrade-truncate-lines))
    (phw-mode-line-prefixes . (phw-mode-line-prefixes
                               phw-upgrade-mode-line-prefixes))
    (phw-mode-line-data . (phw-mode-line-data
                               phw-upgrade-mode-line-data))
    (phw-use-speedbar-for-directories . (phw-use-speedbar-instead-native-tree-buffer
                                         phw-upgrade-use-speedbar-for-directories))

    (phw-directories-menu-user-extension . (phw-directories-menu-user-extension
                                            phw-upgrade-directories-menu-ext))
    (phw-sources-menu-user-extension . (phw-sources-menu-user-extension
                                        phw-upgrade-sources-menu-ext))
    (phw-methods-menu-user-extension . (phw-methods-menu-user-extension
                                        phw-upgrade-methods-menu-ext))
    (phw-history-menu-user-extension . (phw-history-menu-user-extension
                                        phw-upgrade-history-menu-ext))
    (phw-bucket-token-display . (phw-bucket-node-display identity))
    (phw-auto-expand-token-tree . (phw-auto-expand-tag-tree identity))
    (phw-font-lock-tokens . (phw-font-lock-tags identity))
    (phw-layout-window-sizes . (phw-layout-window-sizes phw-upgrade-layout-window-sizes))
    (phw-token-jump-sets-mark . (phw-tag-jump-sets-mark identity))
    (phw-token-display-function . (phw-tag-display-function phw-upgrade-token-display-function))
    (phw-type-token-display . (phw-type-tag-display phw-upgrade-type-token-display))
    (phw-post-process-semantic-tokenlist . (phw-post-process-semantic-taglist
                                            phw-upgrade-post-process-semantic-tokenlist))
    (phw-show-only-positioned-tokens . (phw-show-only-positioned-tags identity))
    (phw-show-tokens . (phw-show-tags phw-upgrade-show-tags))
    (phw-show-tags . (phw-show-tags phw-upgrade-show-tags))
    (phw-highlight-token-with-point . (phw-highlight-tag-with-point identity))
    (phw-highlight-token-with-point-delay . (phw-highlight-tag-with-point-delay identity))
    (phw-token-visit-post-actions . (phw-tag-visit-post-actions
                                     phw-upgrade-token-visit-post-actions))
    (phw-token-header-face . (phw-tag-header-face
                              phw-upgrade-token-header-face))
    (phw-post-process-semantic-taglist . (phw-post-process-semantic-taglist
                                          phw-upgrade-post-process-semantic-taglist))
    (phw-primary-mouse-jump-destination . (phw-mouse-click-destination identity))
    (phw-split-edit-window . (phw-split-edit-window-after-start phw-upgrade-split-edit-window))
    (phw-sort-history-items . (phw-history-sort-method phw-upgrade-sort-history-items))
    (phw-other-window-jump-behavior . (phw-other-window-behavior phw-upgrade-other-window-jump-behavior))
    (phw-excluded-directories-regexp . (phw-excluded-directories-regexps
                                        phw-upgrade-excluded-directories-regexp))
    (phw-source-file-regexps . (phw-source-file-regexps
                                phw-upgrade-source-file-regexps))
    (phw-exclude-parents-regexp . (phw-exclude-parents-regexps
                                   phw-upgrade-exclude-parents-regexp))
    (phw-auto-expand-tag-tree-collapse-other . (phw-auto-expand-tag-tree-collapse-other
                                                phw-upgrade-auto-expand-tag-tree-collapse-other))
    (phw-prescan-directories-for-emptyness . (phw-prescan-directories-for-emptyness
                                              phw-upgrade-prescan-directories-for-emptyness))
    (phw-sources-perform-read-only-check . (phw-sources-perform-read-only-check
                                            phw-upgrade-sources-perform-read-only-check))
    (phw-vc-enable-support . (phw-vc-enable-support
                              phw-upgrade-vc-enable-support))
    (phw-tree-image-icons-directories . (phw-tree-image-icons-directories
                                         phw-upgrade-tree-image-icons-directories))
    (phw-tree-RET-selects-edit-window . (phw-tree-do-not-leave-window-after-select
                                         phw-upgrade-tree-RET-selects-edit-window))
    (phw-grep-find-function . (phw-grep-recursive-function identity))
    )
  "Alist of all options which should be upgraded for current PHW-version.
There are several reasons why an option should be contained in this alist:
a) An old option has just be renamed in current-PHW version but has still the
   same type of value so the new option should get the value of the old one.
b) An old option has changed its type and we try to transform the old-typed
   value to the new type.
c) An old option has be renamed and also changed its type so we try to
   transform the value of the old option to the type of the new option and set
   the new option to this transformed value.

If an old option has changed its type and we can not savely transform the
old-value to the new type then this option should NOT be contained in this
alist! Such an option is auto. reset to the current default-value by
`phw-upgrade-not-compatible-options'!

Every element of this alist has the following form:
The car is the old option symbol and the cdr is a 2-element-list with:
1. elem: The new option symbol \(can be equal with the old option symbol, see
   b) above)
2. elem: A function which converts the value of the old option to the new
   option. If the type of the options is identical \(i.e. only the option name
   has been changed, see a) above) then this function should be `identity'
   otherwise a function which gets one argument \(the value of the old option)
   and returns either a corresponding value for the new option with the new
   correct type or the symbol 'phw-no-upgrade-conversion if no correct
   conversion can be performed! Maybe the function `phw-option-get-value' can
   be helpful within such a transforming-function.")

;; ----------------------------------------------------------------------
;; define here all necessary upgrade functions
;; ----------------------------------------------------------------------

;; upgrading phw-compile-window-temporally-enlarge
(defun phw-upgrade-compile-window-temporally-enlarge (old-val)
  (case old-val
    ((t after-compilation) 'after-display)
    ((nil) nil)
    ((after-selection both) old-val)
    (otherwise 'phw-no-upgrade-conversion)))

;; upgrading phw-window-sync
(defun phw-upgrade-window-sync (old-val)
  (if (equal old-val t)
      (phw-option-get-value 'phw-window-sync 'standard-value)
    nil))

(defun phw-upgrade-layout-window-sizes (old-val)
  (phw-option-get-value 'phw-layout-window-sizes 'standard-value))

;; upgrading old layout-numbers (PHW <= 1.80) to new layout-names (PHW
;; >= 1.90)
(defun phw-upgrade-layout-nr2name (number)
  (let ((number-name-alist '((nil . "left8")
                             (0 . "left1")
                             (1 . "left2")
                             (2 . "left3")
                             (3 . "left4")
                             (4 . "left5")
                             (5 . "right1")
                             (6 . "left6")
                             (7 . "top1")
                             (8 . "left7")
                             (9 . "left8")
                             (10 . "top2")
                             (11 . "left9")
                             (12 . "left10")
                             (13 . "left11")
                             (14 . "left12")
                             (15 . "left13")
                             (16 . "left14")
                             (17 . "left15")
                             (18 . "leftright1")
                             (19 . "leftright2")
                             (20 . "speedbar1"))))
    (cdr (assoc number number-name-alist))))

(defun phw-upgrade-layout-nr (old-val)
  (let ((name (phw-upgrade-layout-nr2name old-val)))
    (if (stringp name)
        name
      'phw-no-upgrade-conversion)))

(defun phw-upgrade-toggle-layout-sequence (old-val)
  (mapcar (function (lambda (elem)
                      (phw-upgrade-layout-nr2name elem)))
          old-val))

(defun phw-upgrade-use-speedbar-for-directories (old-val)
  (if old-val
      'dir))

(defun phw-upgrade-major-modes-activate (old-val)
  (if (not (listp old-val))
      old-val
    (let ((l (copy-tree old-val)))
      (dolist (elem l)
        (if (and (consp elem)
                 (integerp (cdr elem)))
            (setcdr elem (phw-upgrade-layout-nr2name (cdr elem)))))
      l)))

(defun phw-upgrade-cache-directory-contents (old-val)
  (mapcar (function (lambda (elem)
                      (cons (nth 0 elem) (nth 1 elem))))
          old-val))

(defun phw-upgrade-truncate-lines (old-val)
  (cond ((equal t old-val)
         '(phw-directories-buffer-name
           phw-sources-buffer-name
           phw-methods-buffer-name
           phw-history-buffer-name))
        ((equal nil old-val)
         nil)
        ((listp old-val)
         (let ((new-list nil))
           (if (nth 0 old-val)
               (setq new-list (cons 'phw-directories-buffer-name new-list)))
           (if (nth 1 old-val)
               (setq new-list (cons 'phw-sources-buffer-name new-list)))
           (if (nth 2 old-val)
               (setq new-list (cons 'phw-methods-buffer-name new-list)))
           (if (nth 3 old-val)
               (setq new-list (cons 'phw-history-buffer-name new-list)))
           new-list))
        (t
         '(phw-directories-buffer-name
           phw-sources-buffer-name
           phw-methods-buffer-name
           phw-history-buffer-name))))

(defun phw-upgrade-alway-operate-in-edit-window (old-val)
  (let ((l (copy-tree old-val)))
    (setq l (delete 'switch-to-buffer-other-window l))
    l))

(defun phw-upgrade-mode-line-prefixes (old-val)
  (list (cons 'phw-directories-buffer-name
              (nth 0 old-val))
        (cons 'phw-sources-buffer-name
              (nth 1 old-val))
        (cons 'phw-methods-buffer-name
              (nth 2 old-val))
        (cons 'phw-history-buffer-name
              (nth 3 old-val))))

(defun phw-upgrade-mode-line-data (old-val)
  (list (cons 'phw-directories-buffer-name
              (if (equal (nth 0 old-val) 'selected)
                  'sel-dir
                (nth 0 old-val)))
        (cons 'phw-sources-buffer-name
              (if (equal (nth 1 old-val) 'selected)
                  'sel-dir
                (nth 1 old-val)))
        (cons 'phw-methods-buffer-name
              (if (equal (nth 2 old-val) 'selected)
                  'sel-source
                (nth 2 old-val)))
        (cons 'phw-history-buffer-name
              (nth 3 old-val))))

(defun phw-upgrade-menu-extension (old-list)
  (mapcar (function (lambda (i)
                      (reverse i)))
          old-list))

(defun phw-upgrade-directories-menu-ext (old-val)
  (append (phw-upgrade-menu-extension old-val)
          (phw-option-get-value 'phw-directories-menu-user-extension
                                'standard-value)))

(defun phw-upgrade-sources-menu-ext (old-val)
  (append (phw-upgrade-menu-extension old-val)
          (phw-option-get-value 'phw-sources-menu-user-extension
                                'standard-value)))

(defun phw-upgrade-methods-menu-ext (old-val)
  (append (phw-upgrade-menu-extension old-val)
          (phw-option-get-value 'phw-methods-menu-user-extension
                                'standard-value)))

(defun phw-upgrade-history-menu-ext (old-val)
  (append (phw-upgrade-menu-extension old-val)
          (phw-option-get-value 'phw-history-menu-user-extension
                                'standard-value)))

(defun phw-upgrade-token-display-function (old-val)
  (let ((l (copy-tree old-val))
        (mapping-list
         '((semantic-name-nonterminal                  . phw--semantic-format-tag-name)
           (semantic-abbreviate-nonterminal            . phw--semantic-format-tag-abbreviate)
           (semantic-summarize-nonterminal             . phw--semantic-format-tag-summarize)
           (semantic-prototype-nonterminal             . phw--semantic-format-tag-prototype)
           (semantic-concise-prototype-nonterminal     . phw--semantic-format-tag-concise-prototype)
           (semantic-uml-abbreviate-nonterminal        . phw--semantic-format-tag-uml-abbreviate)
           (semantic-uml-prototype-nonterminal         . phw--semantic-format-tag-uml-prototype)
           (semantic-uml-concise-prototype-nonterminal . phw--semantic-format-tag-uml-concise-prototype)
           (semantic-prin1-nonterminal                 . phw--semantic-format-tag-prin1)
           (phw-name-nonterminal                  . phw-format-tag-name)
           (phw-abbreviate-nonterminal            . phw-format-tag-abbreviate)
           (phw-summarize-nonterminal             . phw-format-tag-summarize)
           (phw-prototype-nonterminal             . phw-format-tag-prototype)
           (phw-concise-prototype-nonterminal     . phw-format-tag-concise-prototype)
           (phw-uml-abbreviate-nonterminal        . phw-format-tag-uml-abbreviate)
           (phw-uml-prototype-nonterminal         . phw-format-tag-uml-prototype)
           (phw-uml-concise-prototype-nonterminal . phw-format-tag-uml-concise-prototype)
           (phw-prin1-nonterminal                 . phw-format-tag-prin1))))
    (mapc (function (lambda (e)
                      (if (assoc (cdr e) mapping-list)
                          (setcdr e (cdr (assoc (cdr e) mapping-list))))))
          l)
    l))


(defun phw-upgrade-type-token-display (old-val)
  (let ((val-copy (copy-tree old-val))
        (mapping-list
         '((phw-type-token-class-face . phw-type-tag-class-face)
           (phw-type-token-interface-face . phw-type-tag-interface-face)
           (phw-type-token-struct-face . phw-type-tag-struct-face)
           (phw-type-token-typedef-face . phw-type-tag-typedef-face)
           (phw-type-token-enum-face . phw-type-tag-enum-face)
           (phw-type-token-group-face . phw-type-tag-group-face))))
    (mapc (function (lambda (e)
                      (dolist (l (cdr e))
                        (if (assoc (nth 2 l) mapping-list)
                            (phw-set-elt l 2
                                         (cdr (assoc (nth 2 l) mapping-list)))))))
          val-copy)
    val-copy))

(defun phw-upgrade-post-process-semantic-tokenlist (old-val)
  (let ((val-copy (copy-tree old-val))
        (mapping-list
         '((phw-group-function-tokens-with-parents . phw-group-function-tags-with-parents))))
    (mapc (function (lambda (e)
                      (if (assoc (cdr e) mapping-list)
                          (setcdr e (cdr (assoc (cdr e) mapping-list))))))
          val-copy)
    val-copy))

(defun phw-upgrade-token-visit-post-actions (old-val)
  (let ((val-copy (copy-tree old-val))
        (mapping-list
         '((phw-token-visit-highlight-token-header . phw-tag-visit-highlight-tag-header)
           (phw-token-visit-smart-token-start . phw-tag-visit-smart-tag-start)
           (phw-token-visit-recenter . phw-tag-visit-recenter)
           (phw-token-visit-recenter-top . phw-tag-visit-recenter-top)
           (phw-token-visit-goto-doc-start . phw-tag-visit-goto-doc-start)
           (phw-token-visit-narrow-token . phw-tag-visit-narrow-tag))))
    (mapc (function (lambda (e)
                      (dotimes (i (length (cdr e)))
                        (if (assoc (nth i (cdr e)) mapping-list)
                            (phw-set-elt (cdr e) i
                                         (cdr (assoc (nth i (cdr e))
                                                     mapping-list)))))))
          val-copy)
    val-copy))

(defun phw-upgrade-token-header-face (old-val)
  (if (equal old-val 'phw-token-header-face)
      'phw-tag-header-face
    old-val))

(defun phw-upgrade-post-process-semantic-taglist (old-val)
  (let ((l (copy-tree old-val)))
    (dolist (elem l)
      (if (cdr elem)
          (setcdr elem (list (cdr elem)))))
    l))

(defun phw-upgrade-split-edit-window (old-val)
  (if (equal old-val t)
      'before-activation
    old-val))

(defun phw-upgrade-other-window-jump-behavior (old-val)
  (if (equal old-val 'all)
      'all
    (phw-option-get-value 'phw-other-window-behavior
                          'standard-value)))

(defun phw-upgrade-show-tags (old-val)
  (phw-option-get-value 'phw-show-tags
                        'standard-value))

(defun phw-upgrade-sort-history-items (old-val)
  (if old-val phw-sources-sort-method))

(defun phw-upgrade-excluded-directories-regexp (old-val)
  (list old-val))

(defun phw-upgrade-source-file-regexps (old-val)
  (let ((l (copy-tree old-val)))
    (dolist (elem l)
      (setcdr elem (list (list (cadr elem)) (list (caddr elem)))))
    l))
    
(defun phw-upgrade-exclude-parents-regexp (old-val)
  (if old-val (list old-val)))

(defun phw-upgrade-auto-expand-tag-tree-collapse-other (old-val)
  (if old-val
      'only-if-on-tag
    nil))

(defun phw-upgrade-prescan-directories-for-emptyness (old-val)
  (if old-val 'unless-remote nil))

(defun phw-upgrade-sources-perform-read-only-check (old-val)
  (if old-val 'unless-remote nil))
  
(defun phw-upgrade-vc-enable-support (old-val)
  (if old-val 'unless-remote nil))

(defun phw-upgrade-tree-image-icons-directories (old-val)
  (let ((l (copy-tree old-val)))
    (cons (nth 0 l)
          (delq nil (list (if (nth 1 l)
                              (cons 'phw-directories-buffer-name
                                    (nth 1 l)))
                          (if (nth 2 l)
                              (cons 'phw-sources-buffer-name
                                    (nth 2 l)))
                          (if (nth 3 l)
                              (cons 'phw-methods-buffer-name
                                    (nth 3 l)))
                          (if (nth 4 l)
                              (cons 'phw-history-buffer-name
                                    (nth 4 l))))))))

(defun phw-upgrade-tree-RET-selects-edit-window (old-val)
  (delq nil (mapcar (lambda (b)
                      (and (not (phw-member-of-symbol/value-list
                                 (symbol-value b)
                                 old-val))
                           b))
                    '(phw-directories-buffer-name
                      phw-sources-buffer-name
                      phw-methods-buffer-name
                      phw-history-buffer-name
                      phw-analyse-buffer-name))))
    

;; ----------------------------------------------------------------------
;; internal functions. Don't change anything below this line
;; ----------------------------------------------------------------------

(defgroup phw-upgrade-internal nil
  "Only Internal setting for the PHW upgrade-mechanism - no user-options!"
  :group 'phw-general
  :prefix "phw-")

(defcustom phw-options-version phw-version
  "*DO NOT CUSTOMIZE THIS VALUE - IT IS ONLY FOR INTERNAL USAGE!"
  :group 'phw-upgrade-internal
  :type 'string)

(defun phw-custom-file-writeable-p ()
  "Returns not nil if and only if the custom-file is writable for PHW, which
means it is neither a bytecompiled-file nor a read-only-file."
  (let ((file (phw-custom-file)))
    (and file
         (not (equal (file-name-extension file) "elc"))
         (file-writable-p file))))

(defun phw-customize-save-variable (option value)
  ;; because the adviced version of `custom-save-all' do only all the special
  ;; needed things if `phw-minor-mode' is on we must temporally set here this
  ;; variable to not nil because at that time this function is called this
  ;; variable is maybe still nil.
  (let ((phw-minor-mode t))
    (if (phw-custom-file-writeable-p)
        (customize-save-variable option value)
      (customize-set-variable option value))))

(defun phw-customize-set-variable (option value)
  (customize-set-variable option value))

(defun phw-option-set-default (option)
  "Save the PHW-option OPTION with current default value."
  (phw-customize-set-variable option
                              (phw-option-get-value option 'standard-value)))

(defun phw-option-upgrade (old-option)
  "Upgrade the old PHW-option OLD-OPTION if the following conditions are ALL
true:
1. OLD-OPTION is the key of an element of `phw-upgradable-option-alist'
2. 'saved-value of OLD-OPTION is not nil
3. Either
   + the new-option from `phw-upgradable-option-alist' has the same name
     as OLD-OPTION and
   + the type of the value of OLD-OPTION is not compatible with the current
     type of OLD-OPTION \(this prevents from doing an upgrade twice!)
   or
   + OLD-OPTION is not a valid option in current PHW and
   + The related new-option `phw-upgradable-option-alist' is not already
     customized, i.e. the 'saved-value of new-option is nil.

If all conditions are true then the value of OLD-OPTION is transformed by the
transforming-function of the related element of `phw-upgradable-option-alist'
to the correct new type and then the related new option is saved with this new
value.

Return nil if no upgrade is necessary because at least one of the conditions
above is not true. Returns the transformed value of OLD-OPTION or
'phw-no-upgrade-conversion in form of a list, to distinguish a transformed
value nil from the nil-result which indicates that no upgrade was necessary
\(see above). This means the \"real\" new value is the car of this
result-list!"
  (let ((upgrade-elem (cdr (assoc old-option phw-upgradable-option-alist)))
        new-value)
    ;; check if an upgrade is necessary or allowed
    (when (and upgrade-elem
               (or (and (equal old-option (nth 0 upgrade-elem))
                        (not (phw-option-compatible-p old-option)))
                   (and (not (member old-option phw-all-options))
                        (null (get (nth 0 upgrade-elem) 'saved-value))))
               (get old-option 'saved-value))
      ;; try to transform the old-value in the new type.
      (setq new-value
            (condition-case nil
                (funcall (nth 1 upgrade-elem)
                         (phw-option-get-value old-option 'saved-value))
              (error 'phw-no-upgrade-conversion)))
      (when (not (equal new-value 'phw-no-upgrade-conversion))
        ;; the old-value has been transformed successfully into the new type
        ;; so we can save it.
        (phw-customize-set-variable (nth 0 upgrade-elem) new-value))
      ;; we return the value of the transforming-function even if it is
      ;; 'phw-no-upgrade-conversion!
      (list new-value))))

(defun phw-option-compatible-p (option)
  "Return not nil only if the type of the value of OPTION is compatible with
its current defcustom-definition."
  (require 'cus-edit)
  (widget-apply (widget-convert (get option 'custom-type))
                :match (symbol-value option)))

(defvar phw-old-phw-version nil
  "Only not nil if PHW has upgraded the options to a newer options-version
after an PHW-upgrade.")

(defun phw-options-version=phw-version-p ()
  "Return not nil if the saved value of `phw-options-version' is equal to
`phw-version'."
  (equal (phw-option-get-value 'phw-options-version 'saved-value)
         phw-version))

(defun phw-store-current-options-version ()
  (when (not (phw-options-version=phw-version-p))
    (setq phw-old-phw-version (phw-option-get-value 'phw-options-version
                                                    'saved-value))
    (phw-customize-save-variable 'phw-options-version phw-version)))
  

(defvar phw-not-compatible-options nil
  "This variable is only set by `phw-check-not-compatible-options'! It is an
alist with car is the symbol of an incompatible option and the cdr is the not
compatible value of this option.
This option is evaluated by `phw-upgrade-not-compatible-options' and
`phw-display-upgraded-options'.")


(defvar phw-all-options nil)

(defun phw-get-all-phw-options ()
  (or phw-all-options
      (mapatoms
       (lambda (symbol)
         (when (and (save-match-data (string-match "phw-" (symbol-name symbol)))
                    (get symbol 'custom-type))
           (setq phw-all-options (cons symbol phw-all-options)))))))

(defun phw-check-not-compatible-options ()
  "Check for all PHW-options if their current value is compatible to the
defined type. If not store it in `phw-not-compatible-options'."
  (setq phw-not-compatible-options nil)

  ;; get all options of PHW
  (phw-get-all-phw-options)
  
  ;; check if all current values of PHW options match their types. Add not
  ;; matching options to `phw-not-compatible-options'.
  (dolist (option phw-all-options)
    (require 'cus-edit)
    (unless (phw-option-compatible-p option)
      (setq phw-not-compatible-options
            (cons (cons option
                        (symbol-value option))
                  phw-not-compatible-options)))))

(defun phw-upgrade-not-compatible-options ()
  "Upgrade all not anymore compatible options of `phw-not-compatible-options'.
If such an option is contained in `phw-upgradable-option-alist' then try to
perform a special upgrade with `phw-option-upgrade'. If no special upgrade is
done then the option is reset to the default-value of current PHW-version."
  ;; For every not compatible option perform an upgrade
  (let ((is-not-a-downgrade
         (not (phw-package-version-list<
               (phw-package-version-str2list phw-version)
               (phw-package-version-str2list phw-options-version)))))
    (dolist (option phw-not-compatible-options)
      ;; if the incompatible option is not upgraded by `phw-option-upgrade'
      ;; then we reset it to the standard-value of current PHW-version. If we
      ;; make a downgrade we always reset to the default!
      (let ((upgrade-result
             (if is-not-a-downgrade (phw-option-upgrade (car option)))))
        (when (or (null upgrade-result) ;; no upgrade necessary or allowed
                  ;; the upgrade has been tried but has failed.
                  (equal (car upgrade-result) 'phw-no-upgrade-conversion))
          (phw-option-set-default (car option)))))))
    

(defvar phw-renamed-options nil)

(defun phw-upgrade-renamed-options ()
  "Upgrade all renamed options of `phw-upgradable-option-alist' and store
every option in `phw-renamed-options' if at least an upgrade was tried \(see
`phw-option-upgrade').

Note: This function upgrades only the renamed but not the incompatible options
\(i.e. only the type but not the name of the option has changed) of
`phw-upgradable-option-alist' because the latter ones will be upgraded by
`phw-upgrade-not-compatible-options'!"
  (setq phw-renamed-options nil)
  (when (not (phw-package-version-list<
              (phw-package-version-str2list phw-version)
              (phw-package-version-str2list phw-options-version)))
    (phw-get-all-phw-options)
    (dolist (option phw-upgradable-option-alist)
      ;; perform only an upgrade if the option is not contained in
      ;; `phw-not-compatible-options' too because then PHW has auto.
      ;; recognized that this option is not compatible and the upgrade (or
      ;; reset) is performed by `phw-upgrade-not-compatible-options'!
      (when (not (assoc (car option) phw-not-compatible-options))
        (let ((new-value-list (phw-option-upgrade (car option))))
          ;; if an upgrade was tried then store the option in
          ;; `phw-renamed-options'.
          (when (and new-value-list
                     (not (equal (car new-value-list)
                                 'phw-no-upgrade-conversion)))
            (setq phw-renamed-options
                  (cons (list (car option)
                              (phw-option-get-value (car option) 'saved-value)
                              (car (cdr option))
                              (car new-value-list))
                        phw-renamed-options))))))))

(require 'wid-edit)
(silentcomp-defvar widget-button-keymap)
(silentcomp-defvar widget-keymap)

(defvar phw-upgrade-button-keymap
  (let (parent-keymap mouse-button1 keymap)
    (if phw-running-xemacs
        (setq parent-keymap widget-button-keymap
              mouse-button1 [button1])
      (setq parent-keymap widget-keymap
            mouse-button1 [down-mouse-1]))
    (setq keymap (copy-keymap parent-keymap))
    (define-key keymap mouse-button1 #'widget-button-click)
    keymap)
  "Keymap used inside buttons.")


(defun phw-not-compatible-or-renamed-options-detected ()
  (or phw-not-compatible-options phw-renamed-options))

(defun phw-upgrade-make-copy-of-custom-file ()
  "Make a backup of the file returned by `phw-custom-file' in the same directory."
  (when (phw-custom-file-writeable-p)
    (let* ((file (phw-custom-file))
           (backup-file-base (format "%s.before_phw_%s" file phw-version))
           (backup-file backup-file-base)
           (i 0))
      (while (file-exists-p backup-file)
        (setq i (1+ i))
        (setq backup-file (format "%s__%d" backup-file-base i)))
      (copy-file file backup-file))))
      

(defun phw-display-upgraded-options ()
  "Display a information-buffer which options have been upgraded or reset.
Offers two buttons where the user can decide if the upgraded options should
also being saved by PHW for future settings or if the buffer should be
killed.

If saving is possible this command display where the options would be saved.
It is that file Emacs uses to save customize-settings. This file is
\"computed\" from the settings in `custom-file' and `user-init-file' \(see the
documentation of these variables).

PHW automatically makes a backup-file of that file which will be modified by
storing the upgraded rsp. renamed PHW-options. This backup file gets a unique
name by adding a suffix \".before_phw_<version>\" to the name of the modified
file. If such a file already exists PHW adds a unique number to the end of the
filename to make the filename unique. This is a safety mechanism if something
fails during storing the upgraded options, so you never lose the contents of
your customization-file!"
  (interactive)
  (if (phw-not-compatible-or-renamed-options-detected)
      (progn
        (with-current-buffer (get-buffer-create "*PHW upgraded options*")
          (switch-to-buffer (current-buffer))
          (kill-all-local-variables)
          (let ((inhibit-read-only t))
            (erase-buffer))
          (if (not (phw-custom-file-writeable-p))
              (progn
                (widget-insert "Emacs can not save the upgraded incompatible options (s.b.) because that file\n")
                (widget-insert "specified for storing all customizations (see documentation of the option\n")
                (widget-insert "`custom-file') because the file")
                (widget-insert (if (phw-custom-file)
                                   (concat (phw-custom-file) " is not writeable by Emacs!")
                                 " does either not exist or Emacs has been\nstarted with -q (in the latter case Emacs prevents from writing in the\ncustomizations-file)!\n"))
                (widget-insert "\nPlease restart Emacs with a writeable custom- or init-file or without -q\nso the new option-values can be stored!\n\n"))
            (when (not (get 'phw-display-upgraded-options
                            'phw-upgrades-saved))
              (widget-insert (format "Click on [Save] to save all changed options (s.b.) into %s.\n"
                                     (phw-custom-file)))
              (widget-insert (format "This makes a backup of this file uniquely named with a suffix .before_phw_%s.\n\n"
                                     phw-version))))
          (widget-insert "Click on [Close] to kill this buffer (do this also after clicking [Save]).\n\n")
          (when phw-not-compatible-options
            (widget-insert "The values of the following options are incompatible with current type.\nPHW has tried to transform the old-value to the new type. In cases where\nthis was not possible PHW has reset to the current default-value.")
            (widget-insert "\n\n"))
          (dolist (option phw-not-compatible-options)
            (let ((option-name (symbol-name (car option)))
                  (old-value (cdr option))
                  (new-value (symbol-value (car option))))
              (widget-insert (concat "+ Option:   " option-name))
              (widget-insert "\n")
              (widget-insert (concat "  Old value: "
                                     (if (and (not (equal old-value nil))
                                              (not (equal old-value t))
                                              (or (symbolp old-value)
                                                  (listp old-value)))
                                         "'")
                                     (prin1-to-string old-value)))
              (widget-insert "\n")
              (widget-insert (concat "  New value: "
                                     (if (and (not (equal new-value nil))
                                              (not (equal new-value t))
                                              (or (symbolp new-value)
                                                  (listp new-value)))
                                         "'")
                                     (prin1-to-string new-value)))
              (widget-insert "\n\n")))
          (when phw-renamed-options
            (widget-insert "The following options are not longer valid and have now new names. PHW has\ntried to transform the old value to the new option. In cases where this\nwas not possible the current default value is active!")
            (widget-insert "\n\n"))
          (dolist (option phw-renamed-options)
            (let ((old-option-name (symbol-name (nth 0 option)))
                  (old-value (nth 1 option))
                  (new-option-name (symbol-name (nth 2 option)))
                  (new-value (nth 3 option)))
              (widget-insert (concat "+ Old option: " old-option-name))
              (widget-insert "\n")
              (widget-insert (concat "  Old value:  "
                                     (if (and (not (equal old-value nil))
                                              (not (equal old-value t))
                                              (or (symbolp old-value)
                                                  (listp old-value)))
                                         "'")
                                     (prin1-to-string old-value)))
              (widget-insert "\n")
              (widget-insert (concat "  New option: " new-option-name))
              (widget-insert "\n")
              (widget-insert (concat "  New value:  "
                                     (if (equal new-value 'phw-no-upgrade-conversion)
                                         ;; we print the new default value.
                                         (prin1-to-string (symbol-value (nth 2 option)))
                                       (concat (if (and (not (equal new-value nil))
                                                        (not (equal new-value t))
                                                        (or (symbolp new-value)
                                                            (listp new-value)))
                                                   "'")
                                               (prin1-to-string new-value)))))
              (if (equal new-value 'phw-no-upgrade-conversion)
                  (widget-insert "\n  (The old value couldn't be transformed - this is the current default!)"))
              (widget-insert "\n\n")))
          (widget-insert "If the new values are not what you want please re-customize!")
          (widget-insert "\n\n")
          (widget-insert "For a list of the most important NEWS call `phw-display-news-for-upgrade'!\n\n")
          (widget-insert "\n")
          (when (phw-custom-file-writeable-p)
            (when (not (get 'phw-display-upgraded-options
                            'phw-upgrades-saved))
              ;; Insert the Save button
              (widget-create 'push-button
                             :button-keymap phw-upgrade-button-keymap ; XEmacs
                             :keymap phw-upgrade-button-keymap ; Emacs
                             :notify (lambda (&rest ignore)
                                       (if (get 'phw-display-upgraded-options
                                                'phw-upgrades-saved)
                                           (phw-info-message "Upgraded options are already saved!")
                                         (phw-upgrade-make-copy-of-custom-file)
                                         (dolist (option phw-not-compatible-options)
                                           (phw-customize-save-variable
                                            (car option) (symbol-value (car option))))
                                         (dolist (option phw-renamed-options)
                                           (phw-customize-save-variable
                                            (nth 2 option)
                                            (symbol-value (nth 2 option))))
                                         ;; store the information that the
                                         ;; upgradings have already been saved now
                                         (put 'phw-display-upgraded-options
                                              'phw-upgrades-saved t)
                                         (phw-store-current-options-version)
                                         (phw-info-message "Upgraded options saved!")))
                             "Save")
              (widget-insert " ")))
          ;; Insert the Cancel button
          (widget-create 'push-button
                         :button-keymap phw-upgrade-button-keymap ; XEmacs
                         :keymap phw-upgrade-button-keymap ; Emacs
                         :notify (lambda (&rest ignore)
                                   (kill-buffer (current-buffer)))
                         "Close")
          (widget-setup)
          (goto-char (point-min)))
        t)
    ;; now we display only the choice to save the phw-options-version but only
    ;; if phw-options-version != phw-version and (either the command is called
    ;; interactively or first-time called by program)
    (when (and (or (phw-interactive-p)
                   (not (get 'phw-display-upgraded-options
                         'phw-options-version-save-displayed)))
               (not (phw-options-version=phw-version-p)))
      (put 'phw-display-upgraded-options 'phw-options-version-save-displayed t)
      (with-current-buffer (get-buffer-create "*PHW upgraded options*")
        (switch-to-buffer (current-buffer))
        (kill-all-local-variables)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (widget-insert "There are no incompatible or renamed options. Your settings are correct.\n")
        (widget-insert (format "But PHW must store that the phw-settings are uptodate with %s.\n\n"
                               phw-version))
        (if (not (phw-custom-file-writeable-p))
            (progn
              (widget-insert (format "Emacs can not save the value of `phw-options-version' (%s) in that file\n" phw-options-version))
              (widget-insert "specified for storing all customizations (see documentation of the option\n")
              (widget-insert "`custom-file') because the file")
              (widget-insert (if (phw-custom-file)
                                 (concat (phw-custom-file) " is not writeable by Emacs!")
                               " does either not exist or Emacs has been\nstarted with -q (in the latter case Emacs prevents from writing in the\ncustomizations-file)!\n"))
              (widget-insert "\nPlease restart Emacs with a writeable custom- or init-file or without -q\nso the value of `phw-options-version' (s.a.) can be stored!\n\n"))
          (widget-insert (format "Click on [Save] to save `phw-options-version' (%s) into %s.\n"
                                 phw-options-version (phw-custom-file)))
          (widget-insert (format "This makes a backup of this file unique named with a suffix .before_phw_%s.\n\n"
                                 phw-version)))
        (widget-insert "Click on [Close] to kill this buffer (do this also after clicking [Save]).\n\n")
        (widget-insert "For a list of the most important NEWS call `phw-display-news-for-upgrade'!\n\n")
        (widget-insert "\n")
        (when (phw-custom-file-writeable-p)
          ;; Insert the Save button
          (widget-create 'push-button
                         :button-keymap phw-upgrade-button-keymap ; XEmacs
                         :keymap phw-upgrade-button-keymap ; Emacs
                         :notify (lambda (&rest ignore)
                                   (phw-upgrade-make-copy-of-custom-file)
                                   (phw-store-current-options-version)
                                   (phw-info-message "phw-options-version saved!"))
                         "Save")
          (widget-insert " "))
        ;; Insert the Close button
        (widget-create 'push-button
                       :button-keymap phw-upgrade-button-keymap ; XEmacs
                       :keymap phw-upgrade-button-keymap ; Emacs
                       :notify (lambda (&rest ignore)
                                 (kill-buffer (current-buffer)))
                       "Close")
        (widget-setup)
        (goto-char (point-min))))
    nil))

(defun phw-display-news-for-upgrade (&optional full-news)
  "Display the most important NEWS after an PHW-upgrade.
If you call this function but no PHW-upgrade has been performed before
starting PHW then nothing is display unless FULL-NEWS is not nil.

If FULL-NEWS is not nil then the NEWS-file is displayed in another window."
  (interactive "P")
  (if full-news
      (find-file-other-window (concat phw-phw-dir "NEWS"))
    (if (and phw-old-phw-version
             (or (not (get 'phw-display-news-for-upgrade
                           'phw-news-for-upgrade-displayed))
                 (phw-interactive-p)))
        (progn
          (with-output-to-temp-buffer "*News for the new PHW-version*"
            (princ (format "You have upgraded PHW from version %s to %s.\n\n"
                           phw-old-phw-version phw-version))
            (princ "Here are the most important NEWS:\n\n")
            (mapc (function (lambda (version)
                              (if (phw-package-version-list<
                                   (phw-package-version-str2list phw-old-phw-version)
                                   (phw-package-version-str2list (car version)))
                                  (dolist (news (cdr version))
                                    (princ (concat "* " news "\n"))))))
                  phw-upgrade-news)
            (princ "\nFor more details see the attached NEWS-file."))
          ;; We want this being displayed only once
          (put 'phw-display-news-for-upgrade 'phw-news-for-upgrade-displayed t))
      (message "There are no NEWS to display."))))
    
  
(defun phw-upgrade-options ()
  "Check for all PHW-options if the current value is compatible to the type.
If not upgrade it to the new type or reset it to the default-value of current
PHW. Try also to upgrade renamed options. Displays all upgraded or reset
options with their old \(before the upgrade/reset) and new values."
  (interactive)
  (phw-check-not-compatible-options)
  (phw-upgrade-not-compatible-options)
  (phw-upgrade-renamed-options)
  (phw-display-upgraded-options))

;; ----------------------------------------------------------------------
;; all needs for the requirements check
;; ----------------------------------------------------------------------

;; we need the min and max version of cedet and the list of missing libraries
;; of cedet (if there are any)
(require 'phw-cedet-wrapper)

(defvar phw-all-requirements-available nil)

(defun phw-check-requirements ()
  "Ensure that if all requirements of PHW are fulfilled.

Currently this is a check if the right `cedet-version is loaded."
  ;; we do not support (X)Emacs 18, 19 or 20!
  (when phw-running-unsupported-emacs
    (phw-error "Sorry, but PHW requires an (X)Emacs-version >= 21!"))

  (when phw-regular-xemacs-package-p
    (phw-error "Sorry, but PHW is currently not runnable as XEmacs-package. Install \"by hand\"."))

  (when phw-cedet-missing-libraries
    (phw-error "PHW is missing the libs %s of CEDET - check the CEDET-installation/setup!"
               phw-cedet-missing-libraries))

  (when (and (or (not (boundp 'phw-version-check)) phw-version-check)
             (not phw-all-requirements-available))
    (let ((cedet-required-version-str-min (phw-package-version-list2str
                                           phw-cedet-required-version-min))
          (version-error nil))
      ;; check if cedet-version is correct
      ;; And no longer check against a Maximum version
      (when (or (not (boundp 'cedet-version))
                (phw-package-version-list<
                 (phw-package-version-str2list cedet-version)
                 phw-cedet-required-version-min))
        (setq version-error (concat "cedet ["
                                    cedet-required-version-str-min
                                    "]")))
      (if (null version-error)
          ;; this is the only place where this variable is set
          (setq phw-all-requirements-available t)
        (phw-error "PHW can only be used with %s! Please install it and restart Emacs!"
                   version-error))))
  (when phw-all-requirements-available
    (message "All requirements for PHW %s fulfilled - Enjoy it!" phw-version)))


(defun phw-package-version-str2list (ver-str)
  "Convert the version-str VER-STR to the internal version-list format with
the following elements of the version-list:
1. Major-version
2. Minor-version
3. 0 = alpha, 1 = beta, 2 = pre, 3 = nothing \(e.g. \"1.4\"), 4 = . \(e.g. \"1.4.3\"
4. Subversion after the alpha, beta, pre or .

Return nil if ver-str has not the required syntax:
<major>.<minor>\[.|pre|beta|alpha]\[<sub-stable/pre/beta/alpha-version>]"
  (let ((str ver-str))
    (save-match-data 
      (if (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)\\(pre\\|beta\\|alpha\\|\\.\\)?\\([0-9]+\\)?$" str)
          (list (string-to-number (match-string 1 str))
                (string-to-number (match-string 2 str))
                (if (phw-string= (match-string 3 str) "alpha")
                    0
                  (if (phw-string= (match-string 3 str) "beta")
                      1
                    (if (phw-string= (match-string 3 str) "pre")
                        2
                      (if (phw-string= (match-string 3 str) ".")
                          4
                        3))))
                (if (match-string 4 str)
                    (string-to-number (match-string 4 str))
                  0))))))


(defun phw-package-version-list< (ver1 ver2)
  "Return non-nil if VER1 is less than VER2."
  (let ((v1-0 (nth 0 ver1))
	(v1-1 (nth 1 ver1))
	(v1-2 (nth 2 ver1))
	(v1-3 (nth 3 ver1))
	(v2-0 (nth 0 ver2))
	(v2-1 (nth 1 ver2))
	(v2-2 (nth 2 ver2))
	(v2-3 (nth 3 ver2)))
    (or (< v1-0 v2-0)
        (and (= v1-0 v2-0)
             (< v1-1 v2-1))
        (and (= v1-0 v2-0)
             (= v1-1 v2-1)
             (< v1-2 v2-2))
        (and (= v1-0 v2-0)
             (= v1-1 v2-1)
             (= v1-2 v2-2)
             (< v1-3 v2-3)))))

(defun phw-package-version-string< (ver1-str ver2-str)
  "Return non nil if VER-STR1 is logically less then VER-STR2."
  (let ((ver1 (phw-package-version-str2list ver1-str))
        (ver2 (phw-package-version-str2list ver2-str)))
    (phw-package-version-list< ver1 ver2)))

(defun phw-package-version-list2str (ver)
  "Complementary function to `phw-package-version-str2list'."
  (concat (number-to-string (nth 0 ver))
          "."
          (number-to-string (nth 1 ver))
          (case (nth 2 ver)
            (0 "alpha")
            (1 "beta")
            (2 "pre")
            (4 ".")
            (otherwise ""))
          (if (and (not (= (nth 2 ver) 3))
                   (not (= (nth 3 ver) 0)))
              (number-to-string (nth 3 ver))
            "")))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: remove from texi the whole
;; download stuff inkl. in the command ssection phw-download-*

(silentcomp-provide 'phw-upgrade)

;;; phw-upgrade.el ends here
