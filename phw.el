;;; phw.el --- a code browser for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2015 John S. Yates, Jr.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Ancestry
;;
;; This package is an outgrowth (or perhaps more accurately a reduction) of ECB,
;; an elaborate emacs code browsing package with impressive window management
;; capabilities.  My interest was only in ECB's ability to provide what it
;; termed a "persistent compile window".  This was a horizontal window spanning
;; the edit area along with a set of mechanisms for ensuring that certain
;; buffers consistently got displayed in this window.  The collection of such
;; buffers was always more than just the output of compilation activities.
;; Hence I have renamed that window the Persistent Horizontal Window (PHW).

(require 'cl-macs) ; for cl-loop
(require 'compile) ; for compilation-buffer-p

;;====================================================
;; Notes on overrides
;;====================================================

; other-window:
; - Skips PHW by setting PHW's 'no-other-window property to t.

;;====================================================
;; Customization
;;====================================================

(defgroup phw nil
  "Navigate a frame with a persistent horizontal window."
  :group 'convenience
  :group 'environment
  :group 'windows
  :prefix "phw-")

(defcustom phw-common-prefix "C-,"
  ""
  :group 'phw
  :type 'key-sequence
;  :set (function phw--create-keymap)
)

(defcustom phw-window-at-top-of-frame nil
  "Non-nil positions the persistent horizontal window at top of frame."
  :group 'phw
  :type 'boolean)

(defcustom phw-edit-selected-PHW-max 10
  ""
  :group 'phw
  :type 'integer)

(defcustom phw-PHW-selected-edit-min 20
  ""
  :group 'phw
  :type 'integer)

(defcustom phw-display-in-PHW-major-modes
  '(compilation-mode
    shell-mode)
  "Major-modes to be displayed in the persistent horizontal window.
Any buffer for which `derived-mode-p' returns non-nil for any mode
in this list will be displayed in the PHW."
  :group 'phw
  :type '(repeat (symbol :tag "Major-mode name")))

(defcustom phw-display-in-PHW-predicates
  '(comint-check-proc
    compilation-buffer-p)
  "A list of buffer predicates.
Any buffer for which one of the predicates in this list returns
non-nil will be displayed in the PHW."
  :group 'phw
  :type '(repeat (symbol :tag "Buffer predicate")))

(defcustom phw-display-in-PHW-buffer-names
  `("*Apropos*"
    "*apropos-toc*"
    "*Backtrace*"
    "*bsh*"
    "*buffer-selection*"
    "*Calculator*"
    "*Compile-log*"
    "*Completions*"
    "*Help*"
    "*Messages*"
    "*Occur*"
    "*shell*"
    "*vc*"
    "*vc-diff*")
  "Buffer names to be displayed in the persistent horizontal window.
Any successful match will cause a buffer to be displayed in the PHW."
  :group 'phw
  :type '(repeat (string :tag "Buffer name")))

(defcustom phw-display-in-PHW-buffer-regexs
  `("\\*[cC]ompilation.*\\*"
    "\\*i?grep.*\\*"
    "\\*magit: .*\\*")
  "Buffer regexs to be displayed in the persistent horizontal window.
Any successful match will cause a buffer to be displayed in the PHW."
  :group 'phw
  :type '(repeat (string :tag "Buffer name regex")))


(defgroup phw-hooks nil
  "Hooks to influence PHW mode."
  :group 'phw
  :prefix "phw-")


;;====================================================
;; State variables
;;====================================================

(defvar phw--debug nil)

;; Global
(defvar phw--window-PHW nil
  "Window object of _live_ persistent window else nil.")
(defvar phw--MR-window-edit nil
  "Window object of most recent edit (non-phw) window.")
(defvar phw--MR-window-selected nil
  "Window object of most recently selected window.")
(defvar phw--MR-buffer-selected nil
  "Most recently selected buffer.")
(defvar phw--window-sides-slots nil
  "Save window-sides-slots from mode enable time.")

(defvar phw--keymap '(keymap)
  "Keymap when phw-mode is enable.")

;; Buffer local
(defvar-local phw--window nil
  "A buffer's window binding.  It contains either nil (no
binding) or a window object.  A non-live window object is
equivalent to nil.  The system endeavors to display each buffers
in the window to which it is bound.")

;;====================================================
;; Activaton and deactivation
;;====================================================

(defun phw--active (activate)
  "Activate (display) or deactivate (retract) the PHW."
  (unless (window-live-p phw--window-PHW)
    (setq phw--window-PHW nil))
  (cond
   ((and activate (not phw--window-PHW))
    (phw--make-active))
   ((not activate)
    (phw--make-inactive)))
  (force-mode-line-update t))

(defun phw--make-inactive ()
  "Effect globals changes necessary to make phw-mode inactive.
This may be to allow something like ediff or many-windows gdb
full access to the frame or it may be because phw-mode is being
disabled.  This function is idem potent: calling it repeatedly
should have no ill-effects.  Never call this function directly.
Always use phw--active."
;;  (remove-hook 'buffer-list-update-hook 'phw--on-window-change)
  (remove-hook 'post-command-hook 'phw--selected-window-adjust-height)

  ;; Remove our display action.
  (setq display-buffer-base-action nil)
  ;; Restore original window-sides-slots
  (setq window-sides-slots phw--window-sides-slots)
  (setq phw--window-sides-slots nil)
  ;; Destroy the PHW
  (when (window-live-p phw--window-PHW)
    (set-window-parameter phw--window-PHW 'delete-window nil)
    (delete-window phw--window-PHW))
  (setq phw--window-PHW nil))

(defun phw--make-active ()
  "Effect globals changes necessary to make phw-mode active.
Never call this function directly.  Always use phw--active."
  ;; Save window-sides-slots and install our version
  (let ((slots window-sides-slots))
    (unless phw--window-sides-slots
      (setq phw--window-sides-slots slots))
    (setq window-sides-slots
          (list (nth 0 slots)
                (if phw-window-at-top-of-frame 1 (nth 1 slots))
                (nth 2 slots)
                (if phw-window-at-top-of-frame (nth 3 slots) 1))))
  ;; Note currently selected window as MR-edit
  (setq phw--MR-window-edit (selected-window))
  (setq phw--MR-window-selected phw--MR-window-edit)
  (setq phw--MR-buffer-selected (current-buffer))

  ;; Create the PHW and establish important parameters
  (let ((phw (split-window (frame-root-window)
                           (- phw-edit-selected-PHW-max)
                           (if phw-window-at-top-of-frame 'above 'below))))
    (setq phw--window-PHW phw)
    (window-preserve-size phw t t)
    (set-window-parameter phw 'window-side
                          (if phw-window-at-top-of-frame 'top 'bottom))
    (set-window-parameter phw 'no-other-window t)
    (set-window-parameter phw 'delete-window
                          (lambda (_win)
                            (error "delete-window: Cannot delete phw-mode's persistent horizontal window"))))

  ;; Establish a base action to direct some buffers to PHW
  (setq display-buffer-base-action '(phw--display-window . nil))

  (add-hook 'post-command-hook 'phw--selected-window-adjust-height)

;;  (add-hook 'buffer-list-update-hook 'phw--on-window-change)
  )

;;====================================================
;; Interactive commands and trampoline templates
;;====================================================

(defun phw-goto-window-template ()
  "Move focus to a window based on triggering key sequence's final key.

This function is merely a trampoline for calling `phw-goto-window'.
`phw--window-targets' tabulates the available trigger keys and their
corresponding focus targets.  Only when called interactively will
`last-command-event' contain an appropriate value to guide execution."
  (interactive)
  (phw-goto-window))

(defun phw-goto-window ()
  (interactive)
  (let ((dst-win (phw--window-target-from-key)))
    (when dst-win
      (select-window dst-win))))

(defun phw-exchange-windows-template ()
  "Exchange window contents based on triggering key sequence's final key.
Display the current window's buffer in the target window and the target
window's buffer in the current window.  Move focus to the target window.

This function is merely a trampoline for calling `phw-exchange-windows'.
`phw--window-targets' tabulates the available trigger keys and their
corresponding focus targets.  Only when called interactively will
`last-command-event' contain an appropriate value to guide execution."
  (interactive)
  (phw-exchange-windows))

(defun phw--move-window-update-binding (win buf)
  ""
  (set-window-buffer win buf)
  (with-current-buffer buf
    (setq-local phw--window win)))

(defun phw-exchange-windows ()
  (interactive)
  (let ((dst-win (phw--window-target-from-key)))
    (when dst-win
      (let* ((dst-buf (window-buffer dst-win))
             (src-win (selected-window))
             (src-buf (window-buffer src-win)))
        (phw--move-window-update-binding dst-win src-buf)
        (phw--move-window-update-binding src-win dst-buf)
        (select-window dst-win)))))

;;====================================================
;; Command key to window target mapping
;;====================================================

(defvar phw--window-targets
  '(("0"   . "PHW")
    ("1"   . "edit-window-1")
    ("2"   . "edit-window-2")
    ("3"   . "edit-window-3")
    ("4"   . "edit-window-4")
    ("5"   . "edit-window-5")
    ("6"   . "edit-window-6")
    ("7"   . "edit-window-7")
    ("8"   . "edit-window-8")
    ("9"   . "edit-window-9")
    ("f"   . "edit-window-next")
    ("b"   . "edit-window-prev")
    ("n"   . "edit-window-next")
    ("p"   . "edit-window-prev")
    (","   . "PHW-or-edit-window")
    ("C-," . "PHW-or-edit-window")
    ("."   . "edit-window-ring-pop")
    ("C-." . "edit-window-ring-pop"))
 "Map a triggering key sequence's final event to a window target suffix.
Each entry is a cons (EVENT . TARGET) where EVENT is a string representing
a triggering key sequence's final event and TARGET is a string describing
the associated target window.  During keymap construction `phw--keymap-add'
uses TARGET as a suffix to create aliases for phw's generic commands."
)

(defun phw--window-target-from-key ()
  "Map a triggering key sequence's final event to a live window object.
This function implements an analysis parallel to `phw--window-targets'."
  (let ((selected (selected-window))
        (event last-command-event)
        target)
    ; Using 0 as the MINIBUFFER argument in calls to next-window
    ; and previous-window enforces minibuffer exclusion.
    (cond
     ((= event ?0)                      ; PHW
      (setq target phw--window-PHW))
     ((<= ?1 event ?9)                  ; edit window N
      (setq target phw--window-PHW)
      (cl-loop repeat (- event ?0) do
            (setq target (next-window target 0))))
     ((or (= event ?f)                  ; edit window next
     	  (= event ?n))
      (setq target phw--MR-window-edit)
      (cl-loop do
            (setq target (next-window target 0))
            while (or (not (window-live-p target))
                      (eq target phw--window-PHW))))
     ((or (= event ?b)                  ; edit window prev
     	  (= event ?p))
      (setq target phw--MR-window-edit)
      (cl-loop do
            (setq target (previous-window target 0))
            while (or (not (window-live-p target))
                      (eq target phw--window-PHW))))
     ((or (= event ?\,)                 ; PHW or edit
	  (= event (elt (kbd "C-,") 0)))
      (setq target (if (eq selected phw--window-PHW)
                       phw--MR-window-edit
                     phw--window-PHW)))
     ((or (= event ?\.)                 ; edit window ring pop
     	  (= event (elt (kbd "C-.") 0)))
      -1)
     (t error "Unknown buffer key (%s)" event))
    target))

;;====================================================
;; Construct keymap and trampoline functions
;;====================================================

(defun phw--keymap-add-verb-group (cmd &optional verb)
  "Augment `phw--keymap' with VERB bindings invoking CMD trampolines."
  (let* ((name (symbol-name cmd))
         (prefix (substring name 0 (string-match "window" name)))
         (body (symbol-function (intern (concat name "-template")))))
    (dolist (elt phw--window-targets)
      (let ((alias (intern (concat prefix (cdr elt))))
            (keyseq (kbd (concat phw-common-prefix " " verb " " (car elt)))))
        (fset alias body)
        (define-key phw--keymap keyseq alias)))))

(defun phw--create-keymap ()
  "Create phw-mode's keymap using current `phw-common-prefix'."
  (setq phw--keymap (make-sparse-keymap))
  (phw--keymap-add-verb-group 'phw-goto-window)
  (phw--keymap-add-verb-group 'phw-exchange-windows "x"))

;; Perform construction during load.
(phw--create-keymap)

;;====================================================
;; Window selection
;;====================================================

(defun phw--display-window (buffer _alist)
  "Return a window object if BUFFER should be displayed there else nil.
If BUFFER is bound to a live window then return that.  Otherwise if
BUFFER satisfies one of the various PHW criteria (its major-mode
is derived from a mode in `phw-display-in-PHW-major-modes', one of
the `phw-display-in-PHW-predicates' returns true, it name is equal
to one in `phw-display-in-PHW-buffer-names' or matches a regex in
`phw-display-in-PHW-buffer-regexs') then return the PHW.

This function gets registered as the `display-buffer-base-action'.
Currently the implementation ignores the contents of ALIST."
  (with-current-buffer buffer
    (let ((phw phw--window-PHW)
          (win phw--window)
          (bname (buffer-name)))
      (unless (and win (window-live-p win))
        (setq win nil))
;      (message "--1--: buffer %s, mode %s, win %s" buffer major-mode win)
      (setq win (catch 'window
                  (cond
                   (win ; bound to a live window
;                    (message "--2--: already bound")
                    (throw 'window win))
                   ((not (window-live-p phw)) ; ?no PHW?
;                    (message "--3--: no live PHW!")
                    (throw 'window nil))
                   (t
                    (when (derived-mode-p phw-display-in-PHW-major-modes)
;                      (message "--4--: major mode => PHW")
                      (throw 'window phw))
                    (dolist (predicate phw-display-in-PHW-predicates)
                      (when (and (fboundp predicate) (funcall predicate buffer))
;                        (message "--5--: buffer satisfies some predicate")
                        (throw 'window phw)))
                    (dolist (name phw-display-in-PHW-buffer-names)
                      (when (eq t (compare-strings name nil nil bname nil nil nil))
;                        (message "--6--: buffer name == string")
                        (throw 'window phw)))
                    (dolist (regex phw-display-in-PHW-buffer-regexs)
                      (save-match-data
                        (when (string-match regex bname)
;                          (message "--7--: buffer name matches regex")
                          (throw 'window phw))))))))
      (setq-local phw--window win)
      (when win
        (window--display-buffer buffer win 'reuse))
;      (message "--9--: return %s: %s" (phw-window-ordinal win) win)
      win)))

;;====================================================
;; Window tracking and resizing in post-command-hook
;;====================================================

(defun phw--selected-window-adjust-height ()
  "A post-command-hook function to adjust the PHW's height."
  (let ((win (selected-window)))
    (unless (minibuffer-window-active-p win)
      (let ((buf (current-buffer)))
        (unless (and (eq win phw--MR-window-selected)
                     (eq buf phw--MR-buffer-selected))
          (let ((phw phw--window-PHW)
                height)
            (message "win %s, buf %s, phw %s" win buf phw)
            (save-window-excursion
              (maximize-window)
              (setq height (window-body-height)))
            (cond
             ((eq win phw)
              (let ((point-saved (point)))
                (setq height (- height phw-PHW-selected-edit-min))
                (goto-char (point-min))
                (when (pos-visible-in-window-p (point-max))
                  (setq height (min height (count-lines (point-min) (point-max)))))
                (goto-char point-saved))
              (message "PHW : %s height %s adjust %s" win height (- height (window-body-height)))
              )
             (t
              (setq phw--MR-window-edit win)
              (setq height (- height phw-edit-selected-PHW-max))
              (message "EDIT: %s height %s adjust %s" win height (- height (window-body-height)))
              ))
            (window-resize win (- height (window-body-height)))
            (setq phw--MR-window-selected win)
            (setq phw--MR-buffer-selected buf)))))))

;;====================================================
;; Mode line stuff
;;====================================================

(defun phw-window-ordinal (&optional window)
  "Return a string representing WINDOW's navigation ordinal.
This ordinal is WINDOW's position in the host frame's windows
list when counting from the PHW."
  (let ((target (window-normalize-window window t))
        (win phw--window-PHW)
        (ordinal 0))
    (when (and win target)
      (cl-loop until (eq win target) do
            (when (window-live-p win)
              (setq ordinal (1+ ordinal)))
            (setq win (next-window win 0)))
      (number-to-string ordinal))))

;; (defun phw-window-ordinal (window)
;;   "Debuggind: return ID of selected window as a string"
;;   (let ((txt (substring (format "%s" window) (length "#<window "))))
;;     (substring txt 0 (string-match " " txt))))

;;====================================================
;; Minor mode delcaration
;;====================================================

;;;###autoload
(define-minor-mode phw-mode nil  ; let d-m-m supply doc stri-
  :group 'phw
  :global t
  :keymap phw--keymap
  (phw--active phw-mode))

;;====================================================
;; Wrap up
;;====================================================

(provide 'phw)
