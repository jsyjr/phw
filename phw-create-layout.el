;;; phw-create-layout.el --- creating new layouts

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
;; Contains code for easy creating new layouts

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.


;;; Code

(eval-when-compile
  (require 'silentcomp))

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(require 'phw-mode-line)
(require 'phw-util)
(require 'phw-compilation)

;; XEmacs stuff
(silentcomp-defvar vertical-divider-map)
(silentcomp-defvar modeline-map)
;; Emacs 21.X stuff
(silentcomp-defvar auto-hscroll-mode)
(silentcomp-defvar before-make-frame-hook)
(silentcomp-defvar after-make-frame-functions)
;; First loaded during activated PHW
(silentcomp-defvar phw-buildin-layouts)

(defgroup phw-create-layout nil
  "Settings for creating new PHW-layouts."
  :group 'phw-layout
  :prefix "phw-create-layout-")

(defcustom phw-create-layout-file "~/.phw-user-layouts.el"
  "*File where all layouts created by `phw-create-new-layout' are stored."
  :group 'phw-create-layout
  :type 'file)

(defcustom phw-create-layout-frame-width 140
  "*Frame width of the layout creation frame."
  :group 'phw-create-layout
  :type 'integer)

(defcustom phw-create-layout-frame-height 51
  "*Frame height of the layout creation frame."
  :group 'phw-create-layout
  :type 'integer)


;; internal vars and consts


(defconst phw-create-layout-buf-prefix " *PHW-LC-")
(defconst phw-create-layout-frame-name "Creation of a new PHW-layout")
(defconst phw-create-layout-all-buf-types
  '("directories" "history" "methods" "sources" "speedbar" "analyse" "symboldef"))

(defconst phw-create-layout-help-text-left-right
  "
 PHW layout creation mode
 ========================

 This is the help-screen of this mode. The window
 displaying this help text is called the edit-window
 which is neither selectable nor delete-able nor
 split-able in this mode.

 <left/right/up/down-arrow>: Moving around in current
 window C-n, C-p: Go to next/previous window (beside
 the edit-window)

 C-s: Split current window. You will be asked:
      - If \"vertical\" or \"horizontal\" split
      - How to split: \"at-point\", \"half\" or
        \"other\" (i.e. you can specify any fraction
        between 0.1 and 0.9)
      - Which type the current window should be
        \(see description of C-t below).
 C-u: Unsplit, ie. delete current window
 C-t: Give the current window a built-in type
      (\"directories\", \"sources\", \"methods\",
      \"history\" etc.) or any arbitrary user-defined
      type (\"other\"). See the Online-manual!

 C-c: Cancel layout creation. This does not save the
      layout. Deletes this frame.
 C-q: Save current defined layout and quit the layout
      creation. You will be asked for a layout-name.
      With TAB-completion you can get the names already
      in use. You have to choose a new name!
      Deletes this frame.

 There are NO other commands or keys available. ALL
 other keys are disabled in this mode!
")

(defconst phw-create-layout-help-text-top
  " PHW layout creation mode
 ========================

 This is the help-screen of this mode. The window displaying this help text is called
 the edit-window which is neither selectable nor delete-able nor split-able in this mode.

 <left/right/up/down-arrow>: Moving around in current window
 C-n, C-p: Go to next/previous window (beside the edit-window)

 C-s: Split current window. You will be asked:
      - If \"vertical\" or \"horizontal\" split
      - How to split: \"at-point\", \"half\" or \"other\" (i.e. you can specify any
        fraction between 0.1 and 0.9)
      - Which type the current window should be \(see description of C-t below).
 C-u: Unsplit, ie. delete current window
 C-t: Give the current window a built-in type (\"directories\", \"sources\", \"methods\",
      \"history\" etc.) or any arbitrary user-defined type (\"other\").

 C-c: Cancel layout creation. This does not save the layout. Deletes this frame.
 C-q: Save current defined layout and quit the layout creation. You will be asked for a
      layout-name. With TAB-completion you can get the names already in use.
      You have to choose a new name! Deletes this frame.

 There are NO other commands or keys available. ALL other keys are disabled in this mode!
")

(defconst phw-create-layout-file-header
     "

;;; Commentary:

;; This file contains all user-defined PHW-layouts created by the command
;; `phw-create-new-layout'.

;; !!! DO NOT EDIT THIS FILE MANUALLY - IT IS GENERATED BY PHW !!!

")


(defvar phw-create-layout-frame nil)
(defvar phw-create-layout-edit-window nil)

(defvar phw-create-layout-old-global-map nil)
(defvar phw-create-layout-old-minor-mode-map-alist nil)
(defvar phw-create-layout-old-hscroll nil)
(defvar phw-create-layout-old-debug-on-error nil)
(defvar phw-create-layout-old-frame nil)
(defvar phw-create-layout-old-vertical-div-map nil)
(defvar phw-create-layout-old-modeline-map nil)
(defvar phw-create-layout-old-after-frame-h nil)
(defvar phw-create-layout-old-before-frame-h nil)

(defvar phw-create-layout-generated-lisp nil)
(defvar phw-create-layout-gen-counter 0)

(defvar phw-create-layout-buf-types nil)

;; can be 'left, 'right, 'top or 'left-right
(defvar phw-create-layout-type 'left)

(defun phw-create-layout-initilize ()
  (setq phw-create-layout-buf-types
        (phw-copy-list phw-create-layout-all-buf-types))
  (setq phw-create-layout-frame nil)
  (setq phw-create-layout-edit-window nil)
  (setq phw-create-layout-old-global-map nil)
  (setq phw-create-layout-old-minor-mode-map-alist nil)
  (setq phw-create-layout-old-hscroll nil)
  (setq phw-create-layout-old-frame nil)

  (if phw-running-xemacs
      (progn
        (setq phw-create-layout-old-vertical-div-map nil)
        (setq phw-create-layout-old-modeline-map nil))
    (setq phw-create-layout-old-after-frame-h nil)
    (setq phw-create-layout-old-before-frame-h nil))
  
  (setq phw-create-layout-generated-lisp nil)
  (setq phw-create-layout-gen-counter 0))

(defvar phw-create-layout-frame-deleted nil)

(defadvice delete-frame (before phw-create-layout)
  "Ensure calling `phw-create-layout-cancel' during deleting the
layout-creation frame. Does nothing for any other other frame!"
  (let ((frame (or (ad-get-arg 0) (selected-frame))))
    (when (phw-string= (phw-frame-parameter frame 'name)
                       phw-create-layout-frame-name)
      (setq phw-create-layout-frame-deleted t)
      (phw-create-layout-cancel))))

(defadvice delete-frame (after phw-create-layout)
  "Ensures correct deleting of the layout-creation frame. Does nothing for any
other other frame!"
  (when phw-create-layout-frame-deleted
    (setq phw-create-layout-frame-deleted nil)
    (phw-activate))
  (ad-disable-advice 'delete-frame 'after 'phw-create-layout)
  (ad-activate 'delete-frame))
  

(defun phw-create-layout-frame-ok ()
  "Return not nil if current frame is the `phw-create-layout-frame'"
  (and phw-create-layout-frame
       (frame-live-p phw-create-layout-frame)
       (equal (selected-frame) phw-create-layout-frame)))

(defun phw-create-layout-cancel (&rest ignore)
  "Cancel layout-creation without saving the layout."
  (interactive)
  (when (phw-create-layout-frame-ok)
    (phw-create-layout-clear-all (phw-interactive-p))
    (message "PHW Layout Creation canceled - the layout is not saved!")
    (and (phw-interactive-p) (phw-activate))))

(defun phw-create-layout-clear-all (&optional delete-frame)
  "Resets all stuff to state before `phw-create-new-layout' was called. If
DELETE-FRAME is not nil then the new created frame will be deleted and the
`phw-create-layout-old-frame' will be selected."
  ;; disabling the advice
  (ad-disable-advice 'delete-frame 'before 'phw-create-layout)
  (ad-activate 'delete-frame)
  ;; killing all white-space-filled layout-buffers
  (save-match-data
    (dolist (b (buffer-list phw-create-layout-frame))
      (if (string-match "^ \\*PHW-LC-" (buffer-name b))
          (kill-buffer b))))
  ;; restore the global-map
  (if (keymapp phw-create-layout-old-global-map)
      (use-global-map phw-create-layout-old-global-map))
  ;; restore the minor-mode-maps
  (if phw-create-layout-old-minor-mode-map-alist
      (setq minor-mode-map-alist
            phw-create-layout-old-minor-mode-map-alist))
  ;; restore horiz. scrolling
  (unless phw-running-xemacs
    (setq auto-hscroll-mode phw-create-layout-old-hscroll))
  ;; for XEmacs restore these maps
  (if phw-running-xemacs
      (progn
        (setq vertical-divider-map phw-create-layout-old-vertical-div-map)
        (setq modeline-map phw-create-layout-old-modeline-map))
    ;; before and after making frame stuff
    (setq before-make-frame-hook phw-create-layout-old-before-frame-h)
    (setq after-make-frame-functions phw-create-layout-old-after-frame-h))
  ;; restore old debug-on-error
  (setq debug-on-error phw-create-layout-old-debug-on-error)
  ;; delete the layout-frame and select the phw-create-layout-old-frame
  (when delete-frame
    (when (and phw-create-layout-old-frame
               (frame-live-p phw-create-layout-old-frame))
      (raise-frame phw-create-layout-old-frame)
      (select-frame phw-create-layout-old-frame))
    (when (and phw-create-layout-frame
               (frame-live-p phw-create-layout-frame))
      (ad-with-originals 'delete-frame
        (delete-frame phw-create-layout-frame))))
  (setq phw-create-layout-frame nil))

(defun phw-create-layout-save-and-quit (&rest ignore)
  "Quit the PHW Layout creation and save the defined layout."
  (interactive)
  (when (phw-create-layout-frame-ok)
    (if (phw-create-layout-ready-for-save-p)
        (let ((delete-frame (phw-interactive-p)))
          ;; if an error occurs during `phw-create-layout-save-layout' or the
          ;; user hits C-q we must clean the layout creation stuff!
          (unwind-protect
              (phw-create-layout-save-layout)
            ;; clean the layout creation stuff
            (phw-create-layout-clear-all delete-frame)
            (message "PHW Layout Creation finished.")
            (phw-activate)))
      (phw-error "You must give every PHW-tree-window a type (use C-t)!"))))


(defun phw-create-layout-ready-for-save-p ()
  "Returns only not nil if all windows in current layout have a type."
  (let ((save-p t))
    (save-excursion
      (dolist (win (phw-window-list (selected-frame) 0))
        (unless (equal win phw-create-layout-edit-window)
          (set-buffer (window-buffer win))
          (setq save-p (phw-create-layout-buffer-type)))))
    save-p))


(defmacro phw-create-layout-insert-line (line)
  "Insert LINE in current-buffer and adds a newline."
  `(progn
     (insert ,line)
     (insert "\n")))

(defun phw-create-layout-insert-file-header ()
  (insert (format ";;; %s --- user defined PHW-layouts" ;;
                  (phw-file-name-nondirectory phw-create-layout-file)))
  (insert phw-create-layout-file-header))

(defun phw-create-layout-save-layout ()
  "Saves current layout in `phw-create-layout-file'."
  ;; make edit-window the current selected window
  (phw-create-layout-select-edit-window)
  ;; we need the reversed sequence of the generated code
  (setq phw-create-layout-generated-lisp
        (nreverse phw-create-layout-generated-lisp))
  ;; ensure we have load all layouts defined until now
  (phw-load-layouts)
  ;; now we have the create-code in the right sequence so we can save the new
  ;; layout in the user-layout file
  (let ((layout-name ""))
    ;; a repeat...until-loop
    (while (progn
             ;;the while body
             (setq layout-name
                   (phw-choose-layout-name (phw-available-layouts-of-type nil)
                                           nil))
             ;; the while condition
             (phw-available-layouts-member-p layout-name)))
    (with-temp-file (expand-file-name phw-create-layout-file)
      (erase-buffer)
      (if (file-readable-p (expand-file-name phw-create-layout-file))
          (insert-file-contents (expand-file-name phw-create-layout-file))
        (phw-create-layout-insert-file-header))
      (goto-char (point-max))
      ;; insert header of the layout-define macro
      (phw-create-layout-insert-line
       (format "(phw-layout-define \"%s\" %s nil"
               layout-name
               (symbol-name phw-create-layout-type)))
      ;; insert all the generated layout-code of the new layout
      (dolist (line phw-create-layout-generated-lisp)
        (phw-create-layout-insert-line
         (format "  %s" line)))
      ;; close the new layout-function
      (phw-create-layout-insert-line "  )"))
    ;; now we load the new layout
    (load-file (expand-file-name phw-create-layout-file))
    (message "The new layout is saved in %s, loaded and available!"
             phw-create-layout-file)))

(defun phw-create-layout-gen-lisp (lisp-statement)
  (setq phw-create-layout-generated-lisp
        (cons lisp-statement phw-create-layout-generated-lisp)))

(defun phw-create-layout-split-ver (&optional fraction)
  (let ((factor (or fraction
                    (/ (float (count-lines (window-start) (point)))
                       (float (- (phw-window-full-height) 2))))))
    (phw-split-ver factor t)
    (phw-create-layout-gen-lisp `(phw-split-ver ,factor t))
    factor))

(defun phw-create-layout-split-hor (&optional fraction)
  (let ((factor (or fraction
                    (/ (float (- (point) (phw-line-beginning-pos)))
                       (float (- (window-width) 3))))))
    (phw-split-hor factor t)
    (phw-create-layout-gen-lisp `(phw-split-hor ,factor t))
    (beginning-of-line)
    factor))

(defun phw-create-layout-add-to-buf-types (type)
  (when (and (stringp type)
             (member type phw-create-layout-all-buf-types))
    (add-to-list 'phw-create-layout-buf-types type)
    (setq phw-create-layout-buf-types
          (sort phw-create-layout-buf-types 'phw-string<))))

(defun phw-create-layout-remove-from-buf-type (type)
  (when (stringp type)
    (setq phw-create-layout-buf-types
          (sort (delete type phw-create-layout-buf-types) 'phw-string<))))

(defun phw-create-layout-buffer-type ()
  (get-text-property (point-min) 'phw-create-layout-type))

(defun phw-create-layout-buffer-factor ()
  (get-text-property (point-min) 'phw-create-layout-factor))

(defun phw-create-layout-set-buffer-type (type)
  (let ((buffer-read-only nil))
    (put-text-property (point-min) (1+ (point-min)) 'phw-create-layout-type
                       type)))

(defun phw-create-layout-set-buffer-factor (factor)
  (let ((buffer-read-only nil))
    (put-text-property (point-min) (1+ (point-min)) 'phw-create-layout-factor
                       factor)))


(defun phw-create-layout-gen-lisp-for-buffer-type (type)
  (let ((func-sym (intern (format "phw-set-%s-buffer" type))))
    (phw-create-layout-gen-lisp
     `(if (fboundp (quote ,func-sym))
          (,func-sym)
        (phw-set-default-phw-buffer)))))
  

(defun phw-create-layout-set-buffer-to-type (&optional type)
  "Give current PHW-buffer a type."
  (interactive)
  (when (phw-create-layout-frame-ok)
    ;; adding the old buffer type to the available-list
    (phw-create-layout-add-to-buf-types (or type
                                            (phw-create-layout-buffer-type)))
    (let ((new-type (or (and (stringp type) type)
                        (phw-query-string "Type of current PHW-tree-buffer:"
                                          phw-create-layout-buf-types
                                          "Insert the buffer type"))))
      ;; removing the new buffer type from the available-list
      (phw-create-layout-remove-from-buf-type new-type)
      (phw-mode-line-set (buffer-name (current-buffer))
                         (selected-frame)
                         (concat "PHW " new-type) nil t)
      ;; setting the new buffer type in the buffer itself
      (phw-create-layout-set-buffer-type new-type)
      (when (phw-interactive-p)
        (phw-create-layout-gen-lisp-for-buffer-type new-type)
        (phw-create-layout-next-window))
      new-type)))

(defun phw-create-layout-select-edit-window ()
  (let ((counter 0))
    (while (not (equal (selected-window) phw-create-layout-edit-window))
      (other-window 1)
      (setq counter (1+ counter)))
    (phw-create-layout-gen-lisp `(dotimes (i ,counter)
                                   (other-window 1)
                                   (if (equal (selected-window)
                                              phw-compile-window)
                                       (other-window 1))))))

(defun phw-create-layout-split ()
  "Split current window."
  (interactive)
  (when (phw-create-layout-frame-ok)
    ;; splitting
    (let* ((old-buf-type (phw-create-layout-buffer-type))
           (split-choices (if (equal phw-create-layout-type 'top)
                              '("horizontal" "vertical")
                            '("vertical" "horizontal")))
           (split-type (phw-query-string "Split type:" split-choices))
           (split-method
            (phw-query-string "Split method:"
                              '("at-point" "half")
                              "Insert a fraction between 0.1 and 0.9"))
           (fraction (cond ((phw-string= split-method "at-point")
                            nil)
                           ((phw-string= split-method "half")
                            0.5)
                           ((floatp (string-to-number split-method))
                            (string-to-number split-method))
                           (t 0.5)))
           (real-split-factor
            (if (phw-string= split-type "horizontal")
                (phw-create-layout-split-hor fraction)
              (phw-create-layout-split-ver fraction))))
      ;; creating new fitting buffers
      (phw-create-layout-new-buffer)
       (save-excursion
         (save-selected-window
           (select-window (next-window))
           (phw-create-layout-new-buffer)))
      ;; asking for the buffer type
      (phw-create-layout-set-buffer-factor real-split-factor)
      (phw-create-layout-gen-lisp-for-buffer-type
       (phw-create-layout-set-buffer-to-type old-buf-type))
      (phw-create-layout-next-window))))

(defun phw-create-layout-forward-char ()
  "Move one character forward."
  (interactive)
  (when (phw-create-layout-frame-ok)
    (unless (> (- (point) (phw-line-beginning-pos)) (- (window-width)
                                                       (if phw-running-xemacs
                                                           3
                                                         2)))
      (call-interactively 'forward-char))))

(defun phw-create-layout-next-window ()
  "Go to the next window.
This command always goes to the next special PHW-window, i.e. it never selects
the edit-window."
  (interactive)
  (when (phw-create-layout-frame-ok)
    (let ((steps (if (equal (next-window) phw-create-layout-edit-window) 2 1)))
      (other-window steps)
      (phw-create-layout-gen-lisp `(dotimes (i ,steps)
                                     (other-window 1)
                                     (if (equal (selected-window)
                                                phw-compile-window)
                                         (other-window 1)))))))

(defun phw-create-layout-previous-window ()
  "Go to the previous window.
This command always goes to the PHW-window preceding current window, i.e. it
never selects the edit-window."
  (interactive)
  (when (phw-create-layout-frame-ok)
    (let ((steps (if (equal (previous-window (selected-window) 0)
                            phw-create-layout-edit-window)
                     -2 -1)))
      (other-window steps)
      (phw-create-layout-gen-lisp `(dotimes (i ,(abs steps))
                                     (other-window -1)
                                     (if (equal (selected-window)
                                                phw-compile-window)
                                         (other-window -1)))))))

(defun phw-create-layout-delete-window ()
  "Delete current window."
  (interactive)
  (when (phw-create-layout-frame-ok)
    (unless (or (equal (selected-window) phw-create-layout-edit-window)
                (= (length (phw-window-list nil 0))
                   (if (equal phw-create-layout-type 'left-right) 3 2)))
      (if (and (member phw-create-layout-type '(right left-right))
               (equal (previous-window (selected-window) 0)
                      phw-create-layout-edit-window)
               (> (nth 0 (phw-window-edges (next-window))) (nth 0 (phw-window-edges)))
               (= (nth 3 (phw-window-edges phw-create-layout-edit-window))
                  (nth 3 (phw-window-edges))))
          ;; In exactly this window context we can not delete the current
          ;; window because otherwise the edit-window would enlarge and the
          ;; wrong window would be deleted!
          (phw-error "This window can not be deleted! Delete another one.")
        ;; add the buffer type of the deleted window to the available-list
        (phw-create-layout-add-to-buf-types (phw-create-layout-buffer-type))
        (kill-buffer (current-buffer))
        (delete-window)
        (phw-create-layout-gen-lisp '(delete-window))
        (if (equal (selected-window) phw-create-layout-edit-window)
            (phw-create-layout-previous-window))
        ;; add the buffer type of the new bigger window to the available-list
        (phw-create-layout-add-to-buf-types (phw-create-layout-buffer-type))
        (kill-buffer (current-buffer))
        (phw-create-layout-new-buffer)))))

(defvar phw-create-layout-mode-map nil
  "`phw-create-layout-mode' key-map.")

(if phw-create-layout-mode-map
    ()
  (setq phw-create-layout-mode-map (make-sparse-keymap))
;;  (suppress-key-map phw-create-layout-mode-map t)

  ;; for minibuffer insertion we need the following
  (dotimes (i 26)
    (define-key phw-create-layout-mode-map
      (string (+ i 97)) 'self-insert-command))

  (dotimes (i 26)
    (define-key phw-create-layout-mode-map
      (string (+ i 65)) 'self-insert-command))

  (dotimes (i 10)
    (define-key phw-create-layout-mode-map
      (string (+ i 48)) 'self-insert-command))

  (define-key phw-create-layout-mode-map "." 'self-insert-command)
  (define-key phw-create-layout-mode-map "-" 'self-insert-command)
  
  (if phw-running-xemacs
      (define-key phw-create-layout-mode-map (kbd "<BS>")
        'delete-backward-char)
    (define-key phw-create-layout-mode-map (kbd "<DEL>")
      'backward-delete-char-untabify))

  (define-key phw-create-layout-mode-map (kbd "C-q")
    'phw-create-layout-save-and-quit)
  (define-key phw-create-layout-mode-map (kbd "C-c")
    'phw-create-layout-cancel)
  (define-key phw-create-layout-mode-map (kbd "C-u")
    'phw-create-layout-delete-window)
  (define-key phw-create-layout-mode-map (kbd "C-s") 'phw-create-layout-split)
  (define-key phw-create-layout-mode-map (kbd "C-t")
    'phw-create-layout-set-buffer-to-type)
  (define-key phw-create-layout-mode-map (kbd "<left>") 'backward-char)
  (define-key phw-create-layout-mode-map (kbd "<right>")
    'phw-create-layout-forward-char)
  (define-key phw-create-layout-mode-map (kbd "<up>") 'previous-line)
  (define-key phw-create-layout-mode-map (kbd "<down>") 'next-line)
  (define-key phw-create-layout-mode-map (kbd "C-n")
    'phw-create-layout-next-window)
  (define-key phw-create-layout-mode-map (kbd "C-p")
    'phw-create-layout-previous-window)
  (set-keymap-parent phw-create-layout-mode-map nil))


(defun phw-create-layout-new-buffer (&optional do-not-fill)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer (generate-new-buffer phw-create-layout-buf-prefix))
  (erase-buffer)
  (unless do-not-fill
    (dotimes (i (phw-window-full-height))
      (insert
       (format "%s\n"
               (make-string (- (window-width)
                               (if phw-running-xemacs 3 1))
                            ?\ )))))
  (goto-char (point-min))
  (phw-create-layout-mode)
  (set-window-dedicated-p (selected-window) t))



(defun phw-create-layout-mode ()
  "Major mode for creating new PHW-layouts."
  (setq major-mode 'phw-create-layout-mode)
  (setq mode-name "PHW Create-Layout")
  (use-local-map phw-create-layout-mode-map)
  (make-local-variable 'buffer-read-only)
  (phw-mode-line-set (buffer-name (current-buffer))
                     (selected-frame) "" nil t)
  (setq buffer-read-only t))

(defun phw-create-layout-init-layout (&optional new)
  (delete-other-windows)
  (phw-create-layout-new-buffer)
  (when new
    (setq phw-create-layout-type (intern (phw-query-string
                                          "Location of the the PHW-tree-windows:"
                                          '("left" "right" "top" "left-right")))))
  (case phw-create-layout-type
    (left
     (phw-split-hor phw-windows-width))
    (right
     (phw-split-hor (- phw-windows-width) t))
    (top
     (phw-split-ver phw-windows-height))
    (otherwise
     (phw-split-hor (- (* 0.667 phw-windows-width)) t)
     (phw-split-hor (* 0.667 phw-windows-width) nil t)))
  ;; we set the buffer in the big edit-window
  (phw-create-layout-new-buffer t)
  ;; now we insert the help in the edit-window
  (let ((buffer-read-only nil))
    (insert (if (equal phw-create-layout-type 'top)
                phw-create-layout-help-text-top
              phw-create-layout-help-text-left-right)))
  (setq phw-create-layout-edit-window (selected-window))
  (phw-mode-line-set (buffer-name (current-buffer))
                     (selected-frame) "   PHW edit-window" nil t)
  ;; The edit window must not be dedicated
  (set-window-dedicated-p (selected-window) nil)
  ;; we set the buffer for the (currently unsplitted) PHW-window
  (other-window 1)
  (phw-create-layout-new-buffer)
  ;; for the left-right type we have to set the other column too
  (when (equal phw-create-layout-type 'left-right)
    (other-window 1)
    (phw-create-layout-new-buffer)))


(defun phw-create-layout-make-frame ()
  "Create a new frame for the layout creation process and return it."
  (if phw-running-xemacs
      (make-frame `((name . ,phw-create-layout-frame-name)
                    (minibuffer . t)
                    (user-position . t)
                    (width . ,phw-create-layout-frame-width)
                    (height . ,phw-create-layout-frame-height)
                    (default-toolbar-visible-p . nil)
                    (left-toolbar-visible-p . nil)
                    (right-toolbar-visible-p . nil)
                    (top-toolbar-visible-p . nil)
                    (bottom-toolbar-visible-p . nil)
                    (default-gutter-visible-p . nil)
                    (left-gutter-visible-p . nil)
                    (right-gutter-visible-p . nil)
                    (top-gutter-visible-p . nil)
                    (bottom-gutter-visible-p . nil)
                    (has-modeline-p . t)
                    (use-left-overflow . nil)
                    (vertical-scrollbar-visible-p . nil)
                    (horizontal-scrollbar-visible-p . nil)
                    (use-right-overflow . nil)
                    (menubar-visible-p . nil)))
    (make-frame `((name . ,phw-create-layout-frame-name)
                  (minibuffer . t)
                  (user-position . t)
                  (width . ,phw-create-layout-frame-width)
                  (height . ,phw-create-layout-frame-height)
                  (vertical-scroll-bars . nil)
                  (horizontal-scroll-bars . nil)
                  (tool-bar-lines . 0)
                  (menu-bar-lines . 0)))))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Wir m￼ssen ev. PHW vorher
;; deaktivieren, da sonst ein 2. PHW-menu entsteht. Beim C-c oder C-q eben
;; dann wieder aktivieren.
(defun phw-create-new-layout ()
  "Start interactively layout creating."
  (interactive)

  (phw-deactivate)
  (phw-create-layout-initilize)

  ;; before- and after make frame stuff
  (when (not phw-running-xemacs)
    (setq phw-create-layout-old-after-frame-h after-make-frame-functions)
    (setq after-make-frame-functions nil)
    (setq phw-create-layout-old-before-frame-h before-make-frame-hook)
    (setq before-make-frame-hook nil))
    
  ;; saving old frame
  (setq phw-create-layout-old-frame (selected-frame))

  ;; creating new frame
  (setq phw-create-layout-frame (phw-create-layout-make-frame))
  (raise-frame phw-create-layout-frame)
  (select-frame phw-create-layout-frame)
  (ad-enable-advice 'delete-frame 'before 'phw-create-layout)
  (ad-enable-advice 'delete-frame 'after 'phw-create-layout)
  (ad-activate 'delete-frame)

  ;; global map
  (setq phw-create-layout-old-global-map (current-global-map))
  (use-global-map phw-create-layout-mode-map)

  ;; minor-modes map
  (setq phw-create-layout-old-minor-mode-map-alist minor-mode-map-alist)
  (setq minor-mode-map-alist nil)

  ;; horiz. scrolling
  (unless phw-running-xemacs
    (setq phw-create-layout-old-hscroll auto-hscroll-mode)
    (setq auto-hscroll-mode nil))

  ;; for XEmacs modeline- and vertical-divider maps
  (when phw-running-xemacs
    (setq phw-create-layout-old-vertical-div-map vertical-divider-map)
    (setq vertical-divider-map nil)
    (setq phw-create-layout-old-modeline-map modeline-map)
    (setq modeline-map nil))

  ;; debug on error
  (setq phw-create-layout-old-debug-on-error debug-on-error)
  (setq debug-on-error nil)

  (phw-create-layout-init-layout t))


(defun phw-delete-new-layout ()
  "Select a layout-name and delete this layout.
This means the layout-definition is removed from the file
`phw-create-layout-file' and the layout-function and associated aliases are
unbound."
  (interactive)
  ;; ensure we have load all layouts defined until now
  (phw-load-layouts)
  (let ((new-layout-list
         (sort (phw-set-difference (phw-available-layouts-of-type nil)
                                   (mapcar (function (lambda (elem)
                                                       (car elem)))
                                           phw-buildin-layouts)
                                   'member)
               'phw-string<))
        (layout-name nil))
    (if (= (length new-layout-list) 0)
        (phw-error "There are no layouts to delete!")
      (setq layout-name (phw-choose-layout-name new-layout-list t)))
    (with-temp-file (expand-file-name phw-create-layout-file)
      (erase-buffer)
      (if (file-readable-p (expand-file-name phw-create-layout-file))
          (insert-file-contents (expand-file-name phw-create-layout-file))
        (phw-error "This layout is not defined in %s!" phw-create-layout-file))
      (goto-char (point-min))
      (if (re-search-forward (concat "^(phw-layout-define +"
                                     "\"" layout-name "\".+$")
                             nil t)
          (progn
            ;; Deleting the layout definition in the file
            ;; `phw-create-layout-file'.
            (beginning-of-line)
            (delete-region (match-beginning 0)
                           (progn
                             (forward-sexp)
                             (point)))
            (kill-line)
            ;; undefining the function and aliases.
            (phw-layout-undefine layout-name))
        (phw-error "This layout is not defined in %s!" phw-create-layout-file)))))

(defun phw-create-layout-debug ()
  "Debugging command for the PHW-developers."
  (interactive)
  (message "Layout-Debug: Type: %s, Factor: %s"
           (phw-create-layout-buffer-type)
           (phw-create-layout-buffer-factor)))

;; Klaus Berndl <klaus.berndl@sdm.de>: Cause of the magic autostart stuff of
;; the advice-package we must disable at load-time all these advices!!
;; Otherwise would just loading phw (not activating!) activate each advice
;; AFTER the FIRST usage of our advices!!
(ad-disable-advice 'delete-frame 'after 'phw-create-layout)
(ad-disable-advice 'delete-frame 'before 'phw-create-layout)
(ad-activate 'delete-frame)


(silentcomp-provide 'phw-create-layout)

;; phw-help.el ends here
