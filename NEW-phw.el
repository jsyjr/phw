;;; phw.el --- an emacs Persistent Horizontal Window


;; ====== Made mechanics ===============================================

;;;###autoload
(defun phw-minor-mode (&optional arg)
  "Toggle PHW minor mode.
With prefix argument ARG, turn on if positive, otherwise off. Return non-nil
if the minor mode is enabled.

\\{phw-mode-map}"
  (interactive "P")
  (let ((new-state (if (null arg)
                       (not phw-minor-mode)
                     (> (prefix-numeric-value arg) 0))))
    (if new-state
        (phw-activate-internal)
      (phw-deactivate-internal)))

  phw-minor-mode)

(defun phw-add-to-minor-modes ()
  "Does all necessary to add PHW as a minor mode with current values of
`phw-mode-map' and `phw-minor-mode-text'."
  ;; PHW minor mode doesn't work w/ Desktop restore.
  ;; This line will disable this minor mode from being restored
  ;; by Desktop.
  (when (boundp 'desktop-minor-mode-handlers)
    (add-to-list 'desktop-minor-mode-handlers
		 (cons 'phw-minor-mode 'ignore)))
  (add-minor-mode 'phw-minor-mode
                  'phw-minor-mode-text phw-mode-map))

(defvar phw-minor-mode nil
  "Do not set this variable directly. Use `phw-minor-mode'.!")

;;defvar-local
(defvar phw-minor-mode-text nil
  "[ph|eN]/[MIN|FLX|MAX]")

(defvar phw-mode-map nil
  "Internal key-map for PHW minor mode.")

;; ====== Customization ================================================

(defgroup phw nil
  "Settings for the Persistent Horizontal Window."
  :prefix "phw-")

(defcustom phw-common-prefix "C-."
  ""
  :group 'phw
  :type 'key-sequence)

(defcustom phw-key-map
  '(phw-common-prefix
    . ((t "n"  phw-window-display-next)
       (t "p"  phw-window-display-previous)

       (t "."  phw-dwim-goto-edit-window-or-phw)
       (t "f"  phw-goto-edit-window-forward)
       (t "b"  phw-goto-edit-window-backward)
       (t "1"  phw-goto-edit-window-1-or-display-next)
       (t "2"  phw-goto-edit-window-2-or-display-next)
       (t "3"  phw-goto-edit-window-3-or-display-next)
       (t "4"  phw-goto-edit-window-4-or-display-next)
       (t "5"  phw-goto-edit-window-5-or-display-next)
       (t "6"  phw-goto-edit-window-6-or-display-next)
       (t "7"  phw-goto-edit-window-7-or-display-next)
       (t "8"  phw-goto-edit-window-8-or-display-next)
       (t "9"  phw-goto-edit-window-9-or-display-next)

       (t "m." phw-dwim-move-buffer-to-edit-window-or-phw)
       (t "mf" phw-move-buffer-to-edit-window-forward)
       (t "mb" phw-move-buffer-to-edit-window-backward)
       (t "m1" phw-move-buffer-to-edit-window-1)
       (t "m2" phw-move-buffer-to-edit-window-2)
       (t "m3" phw-move-buffer-to-edit-window-3)
       (t "m4" phw-move-buffer-to-edit-window-4)
       (t "m5" phw-move-buffer-to-edit-window-5)
       (t "m6" phw-move-buffer-to-edit-window-6)
       (t "m7" phw-move-buffer-to-edit-window-7)
       (t "m8" phw-move-buffer-to-edit-window-8)
       (t "m9" phw-move-buffer-to-edit-window-9)

       (t "x." phw-dwim-exchange-buffers-edit-window-and-phw)
       (t "xf" phw-exchange-buffers-edit-window-forward)
       (t "xb" phw-exchange-buffers-edit-window-backward)
       (t "x1" phw-exchange-buffers-edit-window-1)
       (t "x2" phw-exchange-buffers-edit-window-2)
       (t "x3" phw-exchange-buffers-edit-window-3)
       (t "x4" phw-exchange-buffers-edit-window-4)
       (t "x5" phw-exchange-buffers-edit-window-5)
       (t "x6" phw-exchange-buffers-edit-window-6)
       (t "x7" phw-exchange-buffers-edit-window-7)
       (t "x8" phw-exchange-buffers-edit-window-8)
       (t "x9" phw-exchange-buffers-edit-window-9)

       ))

  "*Specifies all key-bindings for the PHW minor-mode key-map.

You MUST change this option via customize to take effect!

The value is a cons-cell where the car is a common-prefix key for all the
key-bindings. The cdr is a list of key-bindings each of them a list again. A
key-binding has the following form:

  '\(<common-prefix-flag> <keysequence> <function>) where

<common-prefix-flag>: If t then the common-prefixkey defined as car of the
                      value \(see above) is used.
<keysequence>: If the common prefixkey is used then the final key-binding is the
               concatenation of the common-prefixkey \(see above) and this
               keysequence.
<function>: The function to bind to the key.  This can also be a
            lambda-expression.

All keysequences must be inserted as a string and must follow the syntax needed
by `read-kbd-macro' or `kbd'. This means you can insert the key in the same
manner \"C-h k\" displays keysequences. Here is the summary of the syntax:

Text is divided into \"words \" separated by whitespace. Except for the words
described below, the characters of each word go directly as characters of the
keysequence. The whitespace that separates words is ignored. Whitespace in the
macro must be written explicitly, as in \"C-c SPC\".

  * The special words RET, SPC, TAB, DEL, LFD, ESC, and NUL represent special
   control characters. The words must be written in uppercase.

  * A word in angle brackets, e.g., <return>, <down>, <left> or <f1>, represents
    a function key. \(Note that in the standard configuration, the function
    key <return> and the control key RET are synonymous.). You can use angle
    brackets on the words RET, SPC, etc., but they are not required there.

  * Keys can be written by their ASCII code, using a backslash followed by up
    to six octal digits. This is the only way to represent keys with codes
    above \377.

  * One or more prefixes M- \(meta), C- \(control), S- \(shift), A- \(alt),
    H- \(hyper), and s- \(super) may precede a character or key notation. For
    function keys, the prefixes may go inside or outside of the brackets:
    C-<down> = <C-down>. The prefixes may be written in any order: M-C-x =
    C-M-x.
    Prefixes are not allowed on multi-key words, e.g., C-abc, except that the
    Meta prefix is allowed on a sequence of digits and optional minus sign:
    M--123 = M-- M-1 M-2 M-3.

  * The `^' notation for control characters also works:  ^M = C-m."
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
                   ;; add the minor-mode and and the minor-mode-map to the
                   ;; alists if not already contained. In this case just
                   ;; replace the values in the alists
                   (phw-add-to-minor-modes))))

(defun phw-activate-internal ()
  ""
  (setq phw-minor-mode-text " PHW")
  (setq phw-minor-mode t))

(defun phw-deactivate-internal ()
  ""
  (setq phw-minor-mode-text nil)
  (setq phw-minor-mode nil))
