;;; phw.el --- Persistent Horizontal Window management -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016 John S. Yates, Jr.

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
;; buffers consistently got displayed in that window.  In my use the collection
;; of such buffers was always more than just the output of compilations.  Hence
;; I have renamed that window the Persistent Horizontal Window (PHW).
;;
;; TODO:
;; - window prompting:
;;   . grey out (https://github.com/abo-abo/ace-window)
;;   . big digits (https://github.com/dimitri/switch-window)
;;   . highlight window ordinal in non-active mode lines
;; - highlight fringe (https://github.com/jwiegley/alert/blob/master/alert.el)
;;     (copy-face 'fringe 'phw--saved-fringe-face)
;;     (copy-face 'phw--saved-fringe-face 'fringe)
;;     (set-face-background 'fringe <configured highlight color>)

(require 'cl-macs) ; for cl-loop
(require 'compile) ; for compilation-buffer-p
(require 'dash)
(require 'view)

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

(defcustom phw-prefix-key "C-,"
  ""
  :group 'phw
  :type 'key-sequence
;  :set (function phw--create-keymap)
)

(defcustom phw-window-at-top-of-frame nil
  "Non-nil positions the persistent horizontal window at top of frame.
Disconcerting without being able to position the minibuffer there too."
  :group 'phw
  :type 'boolean)

(defcustom phw-keep-windows-balanced t
  "Attempt to keep windows balance following splits and deletes."
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
;    "\\*magit: .*\\*"
    "\\*magit-[^:]*-popup\\*")
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

;; Buffer local
(defvar-local phw--buffer-binding nil
  "A buffer's window binding.  It contains either nil (no
binding) or a window object.  A non-live window object is
equivalent to nil.  The system endeavors to display each buffers
in the window to which it is bound.")


;;====================================================
;; Other variables
;;====================================================

(defvar phw--log nil
  "Buffer for logging output")


;;====================================================
;; Mode line
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


;;====================================================
;; Logging
;;====================================================

(defun phw--show-buffer (buf)
  "Return a string with BUF's window binding and name."
  (with-current-buffer buf
    (let ((bnd phw--buffer-binding))
      (format "[%s: #%s%s]"
              buf
              (if phw--buffer-binding (phw-window-ordinal phw--buffer-binding) "_")
              (if (and bnd (window-live-p bnd) (window-dedicated-p bnd)) " - dedicated" "")))))

(defun phw--show-state ()
  "Return a string with selected-window, current-buffer and MR windows"
  (format "state:{ current= <#%s: %s>, MR selected= <#%s: %s>, MR edit= #%s }"
             (phw-window-ordinal (selected-window)) (current-buffer)
             (phw-window-ordinal phw--MR-window-selected) phw--MR-buffer-selected
             (phw-window-ordinal phw--MR-window-edit)))

(defun phw--log-append (str)
  "Append the supplied string to the log buffer"
    (with-current-buffer phw--log
      (goto-char (point-max))
      (insert str)
      (set-buffer-modified-p nil)))

(defun phw--log-show (win)
  ""
    (with-current-buffer phw--log
      (setq-local phw--buffer-binding win)
      (let ((prot (window-dedicated-p win)))
        (set-window-dedicated-p win nil)
        (unwind-protect
            (set-window-buffer win phw--log)
          (set-window-dedicated-p win prot)
        (select-window win t)))))

(defun phw-log ()
  "Pop up a display of windows, their buffers and history lists."
  (interactive)
  (phw--log-append
   (format "
post-command-hook:
  %S
window-configuration-change-hook:
  %S
%s

" post-command-hook window-configuration-change-hook (phw--show-state)))

  (let ((win phw--window-PHW))
    (cl-loop with win = phw--window-PHW for ordinal by 1 do
             (phw--log-append
              (format "#%d: %s  %s\n  Prev:\n" ordinal win
                      (phw--show-buffer (window-buffer win))))
             (cl-loop for (buf) in (window-prev-buffers win) do
                      (phw--log-append (concat "    " (phw--show-buffer buf) "\n")))
             (phw--log-append (format "  Next:\n"))
             (cl-loop for buf in (window-next-buffers win) do
                      (phw--log-append (concat "    " (phw--show-buffer buf) "\n")))
             (setq win (next-window win 0))
             until (eq phw--window-PHW win)) ; win is now PHW
    (phw--log-append (format "\n"))
    (phw--log-show win)))

(defun phw--drop-all-buffer-bindings ()
  "Remove all buffer "
  (interactive)
  (cl-loop for buf in (buffer-list) do
           (with-current-buffer buf
             (kill-local-variable 'phw--buffer-binding))))

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
  (remove-hook 'post-command-hook 'phw--post-command)
  (remove-hook 'window-configuration-change-hook
               'phw--window-configuration-change)
  ;; Remove our display action.
  (setq display-buffer-base-action nil)
  ;; Restore original window-sides-slots
  (setq window-sides-slots phw--window-sides-slots)
  (setq phw--window-sides-slots nil)
  ;; Destroy the PHW
  (let ((phw phw--window-PHW))
    (when (window-live-p phw)
      (set-window-dedicated-p phw nil)
      (set-window-parameter phw 'delete-window nil)
      (delete-window phw)))
  (setq phw--window-PHW nil))

(defun phw--make-active ()
  "Effect globals changes necessary to make phw-mode active.
Never call this function directly.  Always use phw--active."

  ;; Make sure we have a buffer to collect logging info
  (let ((log (get-buffer-create "*PHW Log*")))
    (setq phw--log log)
    (with-current-buffer log
;      (view-mode 1)
      (setq-local buffer-undo-list t)
      (erase-buffer)))

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
    (set-window-buffer phw phw--log)
    (set-window-dedicated-p phw t)
    (window-preserve-size phw t t)
    (set-window-parameter phw 'window-side
                          (if phw-window-at-top-of-frame 'top 'bottom))
    (set-window-parameter phw 'no-other-window t) ; other-window skip PHW
    (set-window-parameter phw 'split-window       ; reuse PHW unsplit
                          (lambda (_win _size _side) phw--window-PHW))
    (set-window-parameter phw 'delete-window
                          (lambda (_win)
                            (error "delete-window: Cannot delete phw-mode's persistent horizontal window")))
    (with-selected-window phw
      (switch-to-buffer "*Messages*")))

  ;; Establish a base action to direct some buffers to PHW
  (setq display-buffer-base-action '(phw--choose-window . nil))

  (add-hook 'post-command-hook 'phw--post-command)
  (add-hook 'window-configuration-change-hook
            'phw--window-configuration-change))


;;====================================================
;; Buffer to window binding
;;====================================================

(defun phw--cleanse-window-history (buf win)
  "Cleanse BUF and any dead or unbound buffers from WIN's history list."
  (set-window-prev-buffers
   win
   (-keep
    (lambda (elt)
      (let ((pbuf (car elt)))
        (when (and                       ; preserve so long as
               (buffer-live-p pbuf)      ;   live
               (not (eq pbuf buf))       ;   not BUF
               (with-current-buffer pbuf ;   bound to WIN
                 (and phw--buffer-binding (eq phw--buffer-binding win))))
          elt)))
    (window-prev-buffers win))))

(defun phw--switch-to-previous-buffer (buf win purge)
  "Replace BUF by displaying a previous buffer from WIN's history.
If PURGE is j-nil exclude BUF from WIN's history.  BUF and WIN are
actually convenience args as they must be current."
  (phw--log-append
   (format "  switch #%s to prev%s\n" (phw-window-ordinal win)
           (if purge (format "; purge %s" buf) "")))
  ;; If BUF bound to window other than WIN then cleanse that window.
  (let ((bound-window phw--buffer-binding))
    (when (and bound-window (not (eq bound-window win)))
      (phw--cleanse-window-history buf bound-window)))
  ;; If BUF is not to be purged then bind it to WIN
  (when (null purge)
    (setq-local phw--buffer-binding win))
  ;; Cleanse WIN's history possibly excluding BUF
  (phw--cleanse-window-history (if purge buf nil) win)
  ;; Pass purge as bury-or-kill
  (switch-to-prev-buffer win purge))

(defun phw--move-from-to (buf from to)
  "Purge BUF from FROM's window & history; display & select BUF in TO.
BUF and FROM are actually convenience args as they must be current."
  (phw--log-append (format " move %s from #%s to #%s\n"
                           buf (phw-window-ordinal from) (phw-window-ordinal to)))
  (phw--switch-to-previous-buffer buf from t)
  (select-window to)
  (with-current-buffer (window-buffer)
    (when (null phw--buffer-binding)
      (setq-local phw--buffer-binding to)))
  (set-window-buffer to buf)
  (setq-local phw--buffer-binding to))


;;====================================================
;; Interactive commands and trampoline templates
;;====================================================

(defun phw-select-window-template ()
  "Move focus to a window based on triggering key sequence's final key.

This function is merely a trampoline for calling `phw-select-window'.
`phw--window-targets' tabulates the available trigger keys and their
corresponding focus targets.  Only when called interactively will
`last-command-event' contain an appropriate value to guide execution."
  (interactive)
  (phw-select-window))

(defun phw-select-window ()
  (interactive)
  (let ((dst-win (phw--window-target-from-key)))
    (when dst-win
      (phw--log-append (format "KB: select #%s\n" (phw-window-ordinal dst-win)))
      (select-window dst-win))))

(defun phw-move-buffer-to-window-template ()
  "Move current buffer based on triggering key sequence's final key.
Display the current buffer in the target window.  Focus remains with
that buffer.

This is merely a trampoline for calling `phw--move-buffer-to-windows'.
`phw--window-targets' tabulates the available trigger keys and their
corresponding focus targets.  Only when called interactively will
`last-command-event' contain an appropriate value to guide execution."
  (interactive)
  (phw-move-buffer-to-window))

(defun phw-move-buffer-to-window ()
  (interactive)
  (let ((dst-win (phw--window-target-from-key)))
    (when dst-win
      (phw--log-append (format "KB: move to #%s\n" (phw-window-ordinal dst-win)))
      (phw--move-from-to (current-buffer) (selected-window) dst-win))))

(defun phw-kill-buffer-in-windows-template ()
  "Kill buffer in window based on triggering key sequence's final key.
Focus remains in the current window.

This is merely a trampoline for calling `phw-kill-buffer-in-windows'.
`phw--window-targets' tabulates the available trigger keys and their
corresponding focus targets.  Only when called interactively will
`last-command-event' contain an appropriate value to guide execution."
  (interactive)
  (phw-kill-buffer-in-windows))

(defun phw-kill-buffer-in-windows ()
  (interactive)
  (let ((original (selected-window))
        (kill (window-buffer (phw--window-target-from-key)))
        (win phw--window-PHW))
    (phw--log-append (format "KB: kill #%s\n" (phw-window-ordinal kill)))
    (cl-loop do
          (select-window win)
          (let ((buf (window-buffer win)))
            (when (eq buf kill)
              (phw--switch-to-previous-buffer buf win t)))
          (setq win (next-window win 0))
          until (eq win phw--window-PHW))
    (kill-buffer kill)
    (select-window original t)))

(defun phw-exchange-windows-template ()
  "Exchange window contents based on triggering key sequence's final key.
Display the current window's buffer in the target window and the target
window's buffer in the current window.  The previously current buffer
remains current though now displayed in a newly selected window..

This function is merely a trampoline for calling `phw-exchange-windows'.
`phw--window-targets' tabulates the available trigger keys and their
corresponding focus targets.  Only when called interactively will
`last-command-event' contain an appropriate value to guide execution."
  (interactive)
  (phw-exchange-windows))

(defun phw-exchange-windows ()
  (interactive)
  (let ((dst-win (phw--window-target-from-key)))
    (when dst-win
      (phw--log-append (format "KB: exchange with #%s\n" (phw-window-ordinal dst-win)))
      (let* ((dst-buf (window-buffer dst-win))
             (src-win (selected-window))
             (src-buf (current-buffer)))
        (with-current-buffer dst-buf
          (phw--move-from-to dst-buf dst-win src-win)
        (with-current-buffer src-buf
          (phw--move-from-to src-buf src-win dst-win)))))))


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
;; Window selection
;;====================================================

(defun phw--choose-window (buffer _alist)
  "Return a window object if BUFFER should be displayed there else nil.
If BUFFER is bound to a live window then return that.  Otherwise if
BUFFER satisfies one of the various PHW criteria (its major-mode is
derived from a mode in `phw-display-in-PHW-major-modes', one of the
`phw-display-in-PHW-predicates' returns true, its name is equal to
one in `phw-display-in-PHW-buffer-names' or its name matches a regex
in `phw-display-in-PHW-buffer-regexs') then return the PHW.

This function gets registered as the `display-buffer-base-action'.
Currently the implementation ignores the contents of ALIST."
  (with-current-buffer buffer
    (let ((bname (buffer-name))
          (win phw--buffer-binding)
          (phw phw--window-PHW))
      (setq win (catch 'window
                  (cond
                   ((and win (window-live-p win)) ; bound to a live window
                    (phw--log-append (format "    (choose-window %s) => #%s: bound\n" buffer (phw-window-ordinal win)))
                    (throw 'window win))
                   ((not (window-live-p phw)) ; ?no PHW?
                    (error "(choose-window %s) PHW does not exist!" buffer))
                   (t
                    (when (derived-mode-p phw-display-in-PHW-major-modes)
                      (phw--log-append (format "    (choose-window %s) => PHW: major mode\n" buffer))
                      (throw 'window phw))
                    (dolist (predicate phw-display-in-PHW-predicates)
                      (when (and (fboundp predicate) (funcall predicate buffer))
                        (phw--log-append (format "    (choose-window %s) => PHW: predicate satisfied\n" buffer))
                        (throw 'window phw)))
                    (dolist (name phw-display-in-PHW-buffer-names)
                      (when (eq t (compare-strings name nil nil bname nil nil nil))
                        (phw--log-append (format "    (choose-window %s) => PHW: string match\n" buffer))
                        (throw 'window phw)))
                    (dolist (regex phw-display-in-PHW-buffer-regexs)
                      (save-match-data
                        (when (string-match regex bname)
                          (phw--log-append (format "    (choose-window %s) => PHW: regex match\n" buffer))
                          (throw 'window phw))))
                    (phw--log-append (format "    (choose-window %s) => nil\n" buffer))
                    (throw 'window nil))))))))


;;====================================================
;; Construct keymap and trampoline functions
;;====================================================

(defvar phw--keymap '(keymap)
  "Keymap when phw-mode is enable.")

(defvar phw--keymap-prefix nil
  "Key containing only PHW mode's prefix and \"C-h\".")


(defun phw--keymap-add-verb-group (cmd &optional verb)
  "Augment `phw--keymap' with VERB bindings invoking CMD trampolines."
  (let* ((name (symbol-name cmd))
         (prefix (substring name 0 (string-match "window" name)))
         (body (symbol-function (intern (concat name "-template")))))
    (dolist (elt phw--window-targets)
      (let ((alias (intern (concat prefix (cdr elt))))
            (keyseq (kbd (concat phw-prefix-key " " verb " " (car elt)))))
        (fset alias body)
        (define-key phw--keymap keyseq alias)))))

(defun phw--create-keymaps ()
  "Create phw-mode's keymap using current `phw-prefix-key'.
Decomposes the map seen by \"C-h k\" into phw--map-prefix (to enable
displaying prompts) and phw--map-continuation (to complete decoding)."
  (setq phw--keymap (make-sparse-keymap))

  (define-key phw--keymap (kbd (concat phw-prefix-key " l")) 'phw-log)
  (define-key phw--keymap (kbd (concat phw-prefix-key " D")) 'phw--drop-all-buffer-bindings)

  (phw--keymap-add-verb-group 'phw-select-window)
  (phw--keymap-add-verb-group 'phw-move-buffer-to-window "m")
  (phw--keymap-add-verb-group 'phw-kill-buffer-in-windows "k")
  (phw--keymap-add-verb-group 'phw-exchange-windows "x")

  (setq phw--keymap-prefix (make-sparse-keymap))
  (define-key phw--keymap-prefix (kbd phw-prefix-key) 'phw--caught-prefix)
  (define-key phw--keymap-prefix (kbd "C-h"         ) 'phw--caught-prefix))

;; Perform construction during load.
(phw--create-keymaps)

;;====================================================
;; Minor mode delcaration
;;====================================================

;; Positioned here to avoid compilation warnings
;;;###autoload
(define-minor-mode phw-mode nil  ; let d-m-m supply doc stri-
  :group 'phw
  :global t
  :keymap phw--keymap
  (phw--active phw-mode))


;;====================================================
;; Window tracking and resizing via hook functions
;;====================================================

(defvar phw--use-full-keymap nil
  "Flag to request use of the full keymap for one command cycle.
At all other times phw--kepmap-prefix is in effect.")

(defvar phw--window-buffer-updates nil
  "")

(defun phw--caught-prefix ()
  "On C-h or prefix key setup single command use of full keymap."
  (interactive)
  (setq phw--use-full-keymap t)
  (setq unread-command-events (list last-command-event))
  (when (eq last-command-event (aref (kbd phw-prefix-key) 0))
    (add-hook 'pre-command-hook 'phw--pre-command-drop-prompt)))

(defun phw--pre-command-drop-prompt ()
  "Resume using prefix map following single command use of full keymap."
  (remove-hook 'pre-command-hook 'phw--pre-command-drop-prompt))

(defun phw--post-command ()
  "A post-command-hook function: reset keymap, apply updates casuing
buffers to be displayed in windows, and adjust the PHW's height.
Also handles setting-up the appropriate keymap for every command cycle
and moving reassigned buffers to their intended windows."
  (cond
   (phw--use-full-keymap                ; One time use of full map
    (setq phw--use-full-keymap nil))
   (t                                   ; Otherwise default to prefix map
    (set-transient-map phw--keymap-prefix)))

  (let ((updates phw--window-buffer-updates))
    (when updates
      (setq phw--window-buffer-updates nil)
      (let ((hook-funcs window-configuration-change-hook))
        (setq window-configuration-change-hook nil)
        (set-window-dedicated-p phw--window-PHW nil)
        (unwind-protect
            (cl-loop for (win . buf) in updates do
                     (condition-case err
                         (set-window-buffer win buf)
                       (error "!ERROR: phw--post-command (updating window buffers): %s" err)))
          (set-window-dedicated-p phw--window-PHW t)
          (setq window-configuration-change-hook hook-funcs)))))

  (let ((win (selected-window)))
    (unless (minibuffer-window-active-p win)
      (let ((buf (current-buffer)))
        (unless (and (eq win phw--MR-window-selected)
                     (eq buf phw--MR-buffer-selected))
          (with-demoted-errors "phw--post-command error: %S"
            (let ((phw phw--window-PHW)
                  height)
              (phw--log-append (format "  PCH - %s\n" (phw--show-state)))
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
                (phw--log-append (format "  PCH - PHW : %s height %s adjust %s\n" win height (- height (window-body-height))))
                )
               (t
                (setq phw--MR-window-edit win)
                (setq height (- height phw-edit-selected-PHW-max))
                (phw--log-append (format "  PCH - EDIT: %s height %s adjust %s\n" win height (- height (window-body-height))))
                ))
              (window-resize win (- height (window-body-height)))
              (setq phw--MR-window-selected win)
              (setq phw--MR-buffer-selected buf))))))))

(defun phw--append-update (update-win update-buf)
  "Append an update unless the same update already appears
in the queue and no interfering updates appear later."
  (let ((match nil))
    (cl-loop for (win . buf) in phw--window-buffer-updates do
             (cond
              ((eq update-win win)
               (setq match (eq update-buf buf)))
              ((eq update-buf buf)
               (setq match nil))))
    (unless match
      (let ((update (list (cons update-win update-buf))))
        (setq phw--window-buffer-updates
              (if phw--window-buffer-updates
                  (nconc phw--window-buffer-updates update)
                update))))))

(defun phw--window-configuration-change ()
  "Bind a buffer to its current (or possibly reassigned) window.
Reassignment is handled by queuing changes to our post-command"
  (let* ((cur-buf (current-buffer))
         (bnd-win phw--buffer-binding)
         (old-win (selected-window))
         (new-win (display-buffer cur-buf)))
    (when (null new-win) ;; If current buffer has no explicit new binding then reuse selected
      (setq new-win old-win))
    (when (not (eq bnd-win new-win))
      (phw--log-append (format "  WCC - rebind buffer %s: #%s -> #%s\n"
                               cur-buf
                               (phw-window-ordinal bnd-win)
                               (phw-window-ordinal new-win)))
      ;; Bind current buffer to chosen window
      (setq-local phw--buffer-binding new-win))
    (when (not (eq new-win old-win))
      ;; Record an update to display it in its new window
      (phw--append-update new-win (current-buffer))
      ;; Record an update to display the "revealed" buffer in the old window
      (let ((prev (window-prev-buffers old-win)))
        (when prev
          (let ((prev-buf (caar prev)))
            (with-current-buffer prev-buf
              (setq-local phw--buffer-binding old-win))
            (phw--append-update old-win prev-buf))))
      (phw--log-append "  WCC - queued updates:")
      (cl-loop for update in phw--window-buffer-updates do
               (phw--log-append (format " [#%s: %s]" (phw-window-ordinal (car update)) (cdr update))))
      (phw--log-append "\n"))))


;;====================================================
;; Echo area
;;====================================================

(advice-add 'display-message-or-buffer :around #'my/advise-display-message-or-buffer)

(defun my/advise-display-message-or-buffer (_msg &optional _bnam _ntw _frm)
  "Direct any output greater than one line to the PHW."
  (let ((resize-mini-windows (if phw-mode nil resize-mini-windows)))))


;;====================================================
;; Window balancing
;;====================================================

(defun phw--balance-windows ()
  "Rebalance window sizes while protecting PHW's height"
  (when (and phw-mode phw-keep-windows-balanced)
    (let ((parent (window-parent)))
      (when (and phw--window-PHW (not (eq parent (frame-root-window))))
        (window-preserve-size phw--window-PHW nil t)
        (balance-windows parent)
        (window-preserve-size phw--window-PHW nil nil)))))

(defun phw--advise-split-window (&optional _win size _side _pxl)
  "Balance windows following a split if no explicit size was given."
  (unless size
    (phw--balance-windows)))

(advice-add 'split-window :after #'phw--advise-split-window)

(defun phw--advise-delete-window (&optional _win)
  "Balance windows following a deletion."
  (phw--balance-windows))

(advice-add 'delete-window :after #'phw--advise-delete-window)


;;====================================================
;; Wrap up
;;====================================================

(provide 'phw)
