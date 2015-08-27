;;; phw.el --- a code browser for Emacs

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

;;====================================================
;; Notes on overrides
;;====================================================

; other-window:
; - Skips PHW by setting PHW's 'no-other-window property to t.

;;====================================================
;; Define global minor mode and customization
;;====================================================

;;;###autoload
(define-minor-mode phw-mode nil  ; let d-m-m supply doc string
  :group 'phw
  :global t
  (phw--activate phw-mode))

(defgroup phw nil
  "Navigate a frame with a persistent horizontal window."
  :group 'convenience
  :group 'environment
  :group 'windows
  :prefix "phw-")

(defcustom phw-common-prefix "C-,"
  ""
  :group 'phw
  :type 'key-sequence)

(defcustom phw-window-at-top-of-frame nil
  "Non-nil to position persistent horizontal window at top of frame."
  :group 'phw
  :type 'boolean)

(defcustom phw-window-lines 10
  ""
  :group 'phw
  :type 'integer)

(defcustom phw-minor-mode-text " PHW"
  "String to display in the mode line when PHW minor mode is active.
\(When the string is not empty, make sure that it has a leading space.)

Because for PHW it is quite obvious if it is active or not when the
PHW-windows are visible this text is only display in the modeline if the
PHW-windows are hidden."
  :group 'phw
  :type 'string)


(defgroup phw-hooks nil
  "Hooks to influence PHW mode."
  :group 'phw
  :prefix "phw-")


;;====================================================
;; Required minor mode definitions
;;====================================================

(defvar phw-mode-map nil
  "Internal key-map for PHW minor mode.")

(defcustom phw-key-map
  '(phw-common-prefix
    . (
       ; Buffer display size control
       (t "/"  phw-toggle-fit-on-focus)

       ; Rotate buffers within a window
       (t "n"  phw-goto-window)
       (t "p"  phw-goto-window)

       ; Move focus to different windows (select)
       (t ","  phw-goto-window)
       (t "C-,"  phw-goto-window)
       (t "."  phw-goto-window)
       (t "C-."  phw-goto-window)
       (t "f"  phw-goto-window)
       (t "b"  phw-goto-window)
       (t "0"  phw-goto-window)
       (t "1"  phw-goto-window)
       (t "2"  phw-goto-window)
       (t "3"  phw-goto-window)
       (t "4"  phw-goto-window)
       (t "5"  phw-goto-window)
       (t "6"  phw-goto-window)
       (t "7"  phw-goto-window)
       (t "8"  phw-goto-window)
       (t "9"  phw-goto-window)

       ; Move current buffer and focus to a new window
       (t "m," phw-dwim-move-buffer-to-edit-window-or-phw)
       (t "m." phw-dwim-move-buffer-to-edit-window-pop)
       (t "mf" phw-move-buffer-to-edit-window-forward)
       (t "mb" phw-move-buffer-to-edit-window-backward)
       (t "m0" phw-move-buffer-phw)
       (t "m1" phw-move-buffer-to-edit-window-N)
       (t "m2" phw-move-buffer-to-edit-window-N)
       (t "m3" phw-move-buffer-to-edit-window-N)
       (t "m4" phw-move-buffer-to-edit-window-N)
       (t "m5" phw-move-buffer-to-edit-window-N)
       (t "m6" phw-move-buffer-to-edit-window-N)
       (t "m7" phw-move-buffer-to-edit-window-N)
       (t "m8" phw-move-buffer-to-edit-window-N)
       (t "m9" phw-move-buffer-to-edit-window-N)

       ; Exchange buffers between source and destination window.
       ; Focus follows current buffer.
       (t "x," phw-dwim-exchange-buffers-edit-window-and-phw)
       (t "x." phw-dwim-exchange-buffers-edit-window-pop)
       (t "xf" phw-exchange-buffers-edit-window-forward)
       (t "xb" phw-exchange-buffers-edit-window-backward)
       (t "x0" phw-exchange-buffers-phw)
       (t "x1" phw-exchange-buffers-edit-window-N)
       (t "x2" phw-exchange-buffers-edit-window-N)
       (t "x3" phw-exchange-buffers-edit-window-N)
       (t "x4" phw-exchange-buffers-edit-window-N)
       (t "x5" phw-exchange-buffers-edit-window-N)
       (t "x6" phw-exchange-buffers-edit-window-N)
       (t "x7" phw-exchange-buffers-edit-window-N)
       (t "x8" phw-exchange-buffers-edit-window-N)
       (t "x9" phw-exchange-buffers-edit-window-N)

       ))
  "*Specifies all key-bindings for the PHW minor mode key-map."
  :group 'phw
  :type '(cons (choice :tag "Common prefix"
                       (const :tag "No common prefix" :value nil)
                       (string :tag "Common prefix" :value phw-prefix))
               (repeat :tag "Key-bindings"
                       (list :tag "Key-definition"
                             (boolean :tag "o Use common prefix" :value t)
                             (string :tag "o Key")
                             (function :tag "o Function or lambda-expression"
                                       :value nil))))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   ;; make a mode-map and save it
                   (setq phw-mode-map
                         (let ((km (make-sparse-keymap))
                               (val-list (cl-copy-list (cdr value)))
                               keq-string)
                           (dolist (elem val-list)
                             (setq key-string (concat (if (nth 0 elem) (eval (car value)))
                                                      " " (nth 1 elem)))
                             (define-key km (read-kbd-macro key-string) (nth 2 elem)))
                           km))
                   ;; Add/update minor-mode and its map in alists.
                   (add-minor-mode 'phw-mode
                                   'phw-minor-mode-text phw-mode-map)
                   ;; PHW minor mode doesn't work w/ Desktop restore.
                   (when (boundp 'desktop-minor-mode-handlers)
                     (add-to-list 'desktop-minor-mode-handlers
                                  (cons 'phw-mode 'ignore))))))

;;====================================================
;; Interactive operations
;;====================================================

;;;###autoload
(defun phw-goto-window ()
  ""
  (interactive)
  (let ((win (phw--window-from-keys)))
    (unless (eq win phw--window-persistent)
      (setq phw--window-last-edit win))
    (when phw--debug
      (message "Return %s, Selected %s, Last-edit %s"
               win
               (selected-window)
               phw--window-last-edit))
    (select-window win)))

;;====================================================
;; Internals
;;====================================================

(defvar phw--debug nil)

(defvar phw--window-persistent nil
  "Window object of _live_ persistent window else nil.")
(defvar phw--window-last-edit nil
  "Window object of last non-persistent window.")
(defvar phw--window-sides-slots nil
  "Save window-sides-slots from mode enable time.")

(defun phw--activate (activate)
  "Activate (display) or deactivate (retract) the PHW."
  (unless (window-live-p phw--window-persistent)
    (setq phw--window-persistent nil))
  (cond
   ((and phw--window-persistent (not activate))
    ; Restore window-sides-slots
    (setq window-sides-slots phw--window-sides-slots)
    (setq phw--window-sides-slots nil)
    ; Destroy the PHW
    (delete-window phw--window-persistent)
    (setq phw--window-persistent nil)
    )
   ((and activate (not phw--window-persistent))
    ; Save window-sides-slots and install our version
    (let ((slots window-sides-slots))
      (unless phw--window-sides-slots
        (setq phw--window-sides-slots slots))
      (setq window-sides-slots
            (list (nth 0 slots)
                  (if phw-window-at-top-of-frame 1 (nth 1 slots))
                  (nth 2 slots)
                  (if phw-window-at-top-of-frame (nth 3 slots) 1))))
    ; Note currently selected window as last-edit
    (setq phw--window-last-edit (selected-window))
    ; Create the PHW and establish important parameters
    (setq phw--window-persistent
          (split-window (frame-root-window)
                        (- phw-window-lines)
                        (if phw-window-at-top-of-frame 'above 'below)
                        ))
    (set-window-parameter phw--window-persistent
                          'window-side
                          (if phw-window-at-top-of-frame 'top 'bottom))
    (set-window-parameter phw--window-persistent
                          'no-other-window t)))
  (force-mode-line-update t))

(defun phw--window-from-keys ()
  "From triggering key sequence's final event return a window object.
Mappings are:
  '0'   : PHW
  '1'   : edit window 1
  '2'   : edit window 2
  '3'   : edit window 3
  '4'   : edit window 4
  '5'   : edit window 5
  '6'   : edit window 6
  '7'   : edit window 7
  '8'   : edit window 8
  '9'   : edit window 9
  'f'   : next edit window
  'b'   : previous edit window
  ','   : alternate between PHW and current edit window
  'C-,' : alternate between PHW and current edit window
  '.' ' : backwards through ring of recent edit windows
  'C-.' : backwards through ring of recent edit windows"
  (let ((selected (selected-window))
        (event last-command-event)
        target)
    (unless (eq selected phw--window-persistent)
      (setq phw--window-last-edit selected))

    ; Using 0 as the MINIBUFFER argument in calls to next-window and
    ; previous-window is necessary to guarantee minibuffer exclusion.
    (cond
     ((= event ?0)                      ; PHW
      (setq target phw--window-persistent))

     ((<= ?1 event ?9)                  ; edit window N
      (setq target phw--window-persistent)
      (loop repeat (- event ?0) do
            (setq target (next-window target 0))))

     ((= event ?f)                      ; next edit window
      (setq target phw--window-last-edit)
      (loop do
            (setq target (next-window target 0))
            while (or (not (window-live-p target))
                      (eq target phw--window-persistent))))

     ((= event ?b)                      ; previous edit window
      (setq target phw--window-last-edit)
      (loop do
            (setq target (previous-window target 0))
            while (or (not (window-live-p target))
                      (eq target phw--window-persistent))))

     ((or (= event ?\,)                 ; alternate PHW and edit
	  (= event (elt (kbd "C-,") 0)))
      (setq target (if (eq selected phw--window-persistent)
                       phw--window-last-edit
                     phw--window-persistent)))

     ((or (= event ?\.)                 ; pop to previous edit
     	  (= event (elt (kbd "C-.") 0)))
      -1)

     (t error "Unknown buffer key (%s)" event))
    target))

(defun phw-window-ordinal (&optional window)
  ""
  (let ((target (window-normalize-window window t))
        (win phw--window-persistent)
        (ordinal 0))
    (if (not (and win target))
        nil
      (loop until (eq win target) do
            (setq ordinal (1+ ordinal))
            (setq win (next-window win 0)))
      ordinal)))


;;====================================================
;; PHW byte-compilation
;;====================================================

(defun phw-compile-file-if-necessary (file &optional force)
  "Compile the PHW-file FILE if necessary. This is done if FORCE is not nil or
FILE.el is newer than FILE.elc or if FILE.elc doesn't exist."
  (let ((elc-file (concat (phw-file-name-sans-extension file) ".elc")))
    (if (or force
	    (not (phw-file-exists-p elc-file))
	    (file-newer-than-file-p file elc-file))
        (byte-compile-file file))))

;;;###autoload
(defun phw-byte-compile (&optional force-all)
  "Byte-compiles the PHW package.
This is done for all lisp-files of PHW if FORCE-ALL is not nil or for each
lisp-file FILE.el which is either newer than FILE.elc or if FILE.elc doesn't
exist."
  (interactive "P")
  (phw-check-requirements)
  (let ((files (phw-directory-files (phw-file-name-directory (locate-library "phw"))
                                    t)))
    (save-excursion
      (dolist (file files)
	(if (save-match-data
              (and (string-match "\\(phw.*\\)\\.el$" file)
                   (not (string-match "phw-autoloads" file))))
            (phw-compile-file-if-necessary file force-all))))))


(provide 'phw)
