;;; phw.el --- a code browser for Emacs

;; Copyright (C) 2000-2015 Jesper Nordenberg,
;;                         Klaus Berndl,
;;                         Kevin A. Burton,
;;                         John S. Yates, Jr.,
;;                         Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: John S. Yates, Jr. <john@yates-sheets.org>
;; Keywords: window, tools

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

;;; Ancestry
;;
;; This package is an outgrowth (or perhaps more accurately a reduction)
;; of ECB, an elaborate emacs code browsing package with impressive
;; window management capabilities.  My interest was only in ECB's
;; ability to provide what it termed a "persistent compile window".
;; This was a horizontal window spanning the edit area along with a set
;; of mechanisms for ensuring that certain buffers consistently got
;; displayed in this window.  The collection of such buffers was always
;; more than just the output of compilation activities.  Hence I have
;; renamed that window the Persistent Horizontal Window (PHW).


;; We need this libraries already here if we miss some requirements
(require 'phw-util)

;; rest of phw loads
(require 'phw-layout)
(require 'phw-compilation)
(require 'phw-compatibility)
(require 'phw-autogen)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

;;====================================================
;; Variables
;;====================================================
(defvar phw-major-mode-selected-source nil
  "Major-mode of currently selected source.")

(defvar phw-minor-mode nil
  "Do not set this variable directly. Use `phw-activate' and
`phw-deactivate' or `phw-minor-mode'.!")

(defvar phw-activated-window-configuration nil
  "Window configuration used after the PHW is activated.")

;;====================================================
;; Customization
;;====================================================

(defgroup phw nil
  "Manage a persistent horizontal window."
  :group 'windows
  :prefix "phw-")

(defgroup phw-hooks nil
  "Hooks to influence PHW mode."
  :group 'phw
  :prefix "phw-")

(defcustom phw-auto-activate t
  "*Automatically startup PHW when Emacs starts up.
This should only be true if you always want to run `phw-activate'."
  :group 'phw
  :type 'boolean)

(defcustom phw-minor-mode-text " PHW"
  "*String to display in the mode line when PHW minor mode is active.
\(When the string is not empty, make sure that it has a leading space.)

Because for PHW it is quite obvious if it is active or not when the
PHW-windows are visible this text is only display in the modeline if the
PHW-windows are hidden."
  :group 'phw
  :type 'string)

(defcustom phw-run-ediff-in-phw-frame t
  "*Run ediff-sessions in the same frame as PHW is running.
If not nil then PHW ensures that ediff runs in the same frame as PHW and PHW
restores exactly the \"before-ediff\"-window-layout after quiting ediff. If
nil then ediff decides in which frame it will run - depending on the current
window-layout \(e.g. if the phw-windows are currently hidden) this can be the
phw-frame but this can also be a newly created frame or any other frame."
  :group 'phw
  :type 'boolean)


(defcustom phw-activate-before-layout-draw-hook nil
  "*Hook run at the end of activating PHW by `phw-activate'.
These hooks run after all the internal setup process but directly before\(!)
drawing the layout specified in `phw-layout' \(means before dividing the frame
into several windows). A senseful using of this hook can be maximizing the
Emacs-frame for example, because this should be done before the layout is
drawn because PHW computes the size of the PHW-windows with the current frame
size! If you need a hook-option for the real end of the activating process
\(i.e. after the layout-drawing) look at `phw-activate-hook'.

IMPORTANT: The difference between this hook and
`phw-redraw-layout-before-hook' is that the latter one is evaluated always
before the layout is redrawn \(for example after calling `phw-redraw-layout')
whereas the former one \(this hook) is only evaluated exactly once during the
activation-process of PHW. So during the activation process there is the
following sequence of hooks:
1. 'phw-activate-before-layout-draw-hook' \(this one)
2. `phw-redraw-layout-before-hook'
3. <Drawing the layout>
4. `phw-redraw-layout-after-hook'
5. `phw-activate-hook'"
  :group 'phw-hook
  :type 'hook)


(defcustom phw-before-activate-hook nil
  "*Hook run at the beginning of activating PHW by `phw-activate'.
These hooks run before any other tasks of the activating process are
performed. If any of these hooks returns nil then PHW will not be activated!

This can be used to check some conditions and then only start PHW if all
conditions are true. For example a function could be added which returns only
nil if Gnus is running. Then calling `phw-activate' or `phw-minor-mode' will
only start PHW if Gnus is not already running."
  :group 'phw-hook
  :type 'hook)


(defcustom phw-activate-hook nil
  "*Hook run at the end of activating PHW by `phw-activate'.
These hooks run at the real end of the activating process, means after the
layout has been drawn!. If you need hooks which are run direct before the
layout-drawing look at `phw-activate-before-layout-draw-hook'."
  :group 'phw-hook
  :type 'hook)

(defcustom phw-deactivate-hook nil
  "*Hook run at the end of deactivating PHW by `phw-deactivate'.
These hooks run before the phw-layout is cleared!"
  :group 'phw-hook
  :type 'hook)

(defcustom phw-before-deactivate-hook nil
  "*Hook run at the beginning of deactivating PHW by `phw-deactivate'.
These hooks run before any other tasks of the deactivating process are
performed. If any of these hooks returns nil then PHW will not be deactivated!
See also `phw-before-activate-hook'."
  :group 'phw-hook
  :type 'hook)


;;====================================================
;; Internals
;;====================================================

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

(defvar phw-mode-map nil
  "Internal key-map for PHW minor mode.")

(defcustom phw-common-prefix "C-,"
  ""
  :group 'phw
  :type 'key-sequence)

(defcustom phw-key-map
  '(phw-common-prefix
       ; Rotate buffers within a window
    . ((t "n"  phw-window-display-next)
       (t "p"  phw-window-display-previous)

       ; Buffer display size control
       (t "/"  phw-toggle-fit-on-focus)

       ; Move focus to different windows (select)
       (t ","  phw-dwim-goto-edit-window-or-phw)
       (t "."  phw-goto-most-recent-window)
       (t "f"  phw-goto-edit-window-forward)
       (t "b"  phw-goto-edit-window-backward)
       (t "1"  phw-goto-edit-window-N)
       (t "2"  phw-goto-edit-window-N)
       (t "3"  phw-goto-edit-window-N)
       (t "4"  phw-goto-edit-window-N)
       (t "5"  phw-goto-edit-window-N)
       (t "6"  phw-goto-edit-window-N)
       (t "7"  phw-goto-edit-window-N)
       (t "8"  phw-goto-edit-window-N)
       (t "9"  phw-goto-edit-window-N)

       ; Move current buffer and focus to a new window
       (t "m," phw-dwim-move-buffer-to-edit-window-or-phw)
       (t "mf" phw-move-buffer-to-edit-window-forward)
       (t "mb" phw-move-buffer-to-edit-window-backward)
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
       (t "xf" phw-exchange-buffers-edit-window-forward)
       (t "xb" phw-exchange-buffers-edit-window-backward)
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

(defvar phw-current-window 0)
(defvar phw-current-edit 1)

(defun phw-window-from-keys ()
  "Return a window spec from last element of triggering key sequence.
Mappings are:
  '1'..'9'  : edit window N
  ',' 'C-,' : alternate between PHW and current edit window
  '.' 'C-.' : previous in edit windows ring
  'f'       : next edit window
  'b'       : previous edit window"
  (let* ((keys (this-command-keys-vector))
         (len  (length keys))
         (last (elt keys (1- len))))
    (cond
     ((< ?0 last (1+ ?9))               ; edit window N
      (- last ?0))
     ((or (= last ?\,)                  ; alternate PHW and edit
	  (= last (elt (kbd "C-,") 0)))
      (if (zerop phw-current-window)
	  phw-current-edit
	0))
     ((or (= last ?\.)                  ; pop to previous edit
     	  (= last (elt (kbd "C-.") 0)))
      -1)
     ((= last ?f)                       ; next edit window
      -2)
     ((= last ?b)                       ; previous edit window
      -3)
     (t error "Unknown buffer key (%s)" last))))

;;;###autoload
(defun phw-goto-window ()
  ""
  (interactive)
  (let ((target (phw-window-from-keys)))
    (when (<= 0 target)
      (setq phw-current-window target)
      (when (< 0 target)
        (setq phw-current-edit target)))
    (message "Target edit window: %s  (current %s; edit %s)" target phw-current-window phw-current-edit)))


;;;###autoload
(defvar phw-last-window-config-before-deactivation nil
  "Contains the last `current-window-configuration' directly before
PHW has been deactivated. Do not set this variable!")

(defun update-autocontrol (func pre add)
  "Add or remove FUNC from various command hooks.
If ADD is non-nil then add the function to the appropriate hooks
otherwise remove it.  If PRE is non-null then the hooks are
`pre-command-hook' and `phw-pre-command-hooks' otherwise they are
`post-command-hook' and `phw-post-command-hooks'."
  (cond (add (cond (pre
                    (add-hook    'pre-command-hook       func)
                    (add-hook    'phw-pre-command-hooks  func))
                   (nil
                    (add-hook    'post-command-hook      func)
                    (add-hook    'phw-post-command-hooks func))))
        (nil (cond (pre
                    (remove-hook 'pre-command-hook       func)
                    (remove-hook 'phw-pre-command-hooks  func))
                   (nil
                    (remove-hook 'post-command-hook      func)
                    (remove-hook 'phw-post-command-hooks func))))))

(defun update-hook (add)
  (if add (function add-hook) (function remove-hook)))

(defun update-all-hooks (add)
  (update-autocontrol 'phw-layout-pre-command-hook     t   add)
  (update-autocontrol 'phw-layout-post-command-hook    nil add)
  (update-autocontrol 'phw-handle-major-mode-visibilty nil add))

(defun phw-activate ()
  "Activates PHW and creates the special buffers for the choosen layout.
For the layout see `phw-layout-name'. This function raises always the
PHW-frame if called from another frame. This is the same as calling
`phw-minor-mode' with a positive argument."
  (interactive)
  (phw-minor-mode 1))

(defun phw-clean-up-after-activation-failure (msg err)
  "Complete cleanup of all PHW-setups and report an error with message MSG."
  (let ((phw-minor-mode t))
    (phw-deactivate-internal t))
  (setq phw-minor-mode nil)
  (force-mode-line-update t)
  (error "PHW: %s (error-type: %S, error-data: %S)" msg (car err) (cdr err)))

(defun phw-activate-internal ()
  "Activates the PHW and creates all the buffers and draws the PHW-screen
with the actually chosen layout \(see `phw-layout-name'). This function raises
always the PHW-frame if called from another frame."
  (when (or (null phw-frame)
            (not (frame-live-p phw-frame)))
    (setq phw-frame (selected-frame)))

  (if phw-minor-mode
      (when (and (not (equal (selected-frame) phw-frame))
                 (or (equal phw-activation-selects-phw-frame-if-already-active t)
                     (and (equal phw-activation-selects-phw-frame-if-already-active 'ask)
                          (y-or-n-p "PHW is already active in another frame. Select it? "))))
        (phw-select-phw-frame))

    (let ((debug-on-error debug-on-error))
      ;; we activate only if all before-hooks return non nil
      (when (run-hook-with-args-until-failure 'phw-before-activate-hook)

        (condition-case err-obj
            (progn

              ;; first initialize the whole layout-engine
              (phw-initialize-layout)

              ;; enable permanent advices - these advices will never being
              ;; deactivated after first activation of PHW unless
              ;; `phw-split-edit-window-after-start' is not 'before-activation
              ;; (see `phw-deactivate-internal')
              (phw-enable-advices 'phw-permanent-adviced-layout-functions)

              ;; enable advices for not supported window-managers
;;              (phw-enable-advices 'phw-winman-not-supported-function-advices)

              ;; enable advices for the compatibility with other packages
;;              (phw-enable-advices 'phw-compatibility-advices)

              ;; set the phw-frame
              (let ((old-phw-frame phw-frame))
                (if phw-new-phw-frame
                    (progn
                      (run-hooks 'phw-activate-before-new-frame-created-hook)
                      (setq phw-frame (make-frame))
                      (put 'phw-frame 'phw-new-frame-created t))
                  (setq phw-frame (selected-frame))
                  (put 'phw-frame 'phw-new-frame-created nil))
                ;; If PHW is acivated in a frame unequal to that frame which was
                ;; the phw-frame at last deactivation then we initialize the
                ;; `phw-edit-area-creators'.
                (if (not (equal phw-frame old-phw-frame))
                    (phw-edit-area-creators-init)))
              (raise-frame phw-frame)
              (select-frame phw-frame)

              (phw-enable-own-temp-buffer-show-function t)

              ;; now we can activate PHW
              (update-all-hooks t)

              ;; ediff-stuff; we operate here only with symbols to avoid bytecompiler
              ;; warnings
              (phw-activate-ediff-compatibility)
              )
          (error
           ;;          (backtrace)
           (phw-clean-up-after-activation-failure
            "Errors during the basic setup of PHW." err-obj)))

        (condition-case err-obj
            ;; run personal hooks before drawing the layout
            (run-hooks 'phw-activate-before-layout-draw-hook)
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the hooks of phw-activate-before-layout-draw-hook."
            err-obj)))

        (setq phw-minor-mode t)

        ;; now we draw the screen-layout of PHW.
        (condition-case err-obj
            ;; now we draw the layout chosen in `phw-layout'. This function
            ;; activates at its end also the adviced functions if necessary!
            ;; Here the directories- and history-buffer will be updated.
            (let ((phw-redraw-layout-quickly nil)
                  (use-last-win-conf (and phw-last-window-config-before-deactivation
                                          (equal phw-split-edit-window-after-start
                                                 'before-deactivation)
                                          (not (phw-window-configuration-invalidp
                                                phw-last-window-config-before-deactivation)))))
              (phw-enable-temp-buffer-shrink-to-fit phw-compile-window-height)
              (if use-last-win-conf
                  (setq phw-edit-area-creators
                        (nth 4 phw-last-window-config-before-deactivation)))

              (phw-redraw-layout-full 'no-buffer-sync
                                      nil
                                      (and use-last-win-conf
                                           (nth 6 phw-last-window-config-before-deactivation))
                                      (and use-last-win-conf
                                           (nth 5 phw-last-window-config-before-deactivation)))

              ;; if there was no compile-window before deactivation then we have
              ;; to hide the compile-window after activation
              (if (and use-last-win-conf
                       (null (nth 2 phw-last-window-config-before-deactivation)))
                  (phw-toggle-compile-window -1))

              (when (member phw-split-edit-window-after-start
                            '(vertical horizontal nil))
                (delete-other-windows)
                (case phw-split-edit-window-after-start
                  (horizontal (split-window-horizontally))
                  (vertical (split-window-vertically))))
              )
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the layout setup of PHW." err-obj))
          )

        (condition-case err-obj
            ;; we run any personal hooks
            (run-hooks 'phw-activate-hook)
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the hooks of phw-activate-hook." err-obj)))

        (setq phw-minor-mode t)
        (message "The PHW is now activated.")

        ;; if we activate PHW first time then we display the node "First steps" of
        ;; the online-manual
        (ignore-errors
          (when (null phw-source-path)
            (let ((phw-show-help-format 'info))
              (phw-show-help)
              (Info-goto-node "First steps"))))

        (phw-enable-advices 'phw-layout-basic-adviced-functions)

        (condition-case err-obj
            ;;now take a snapshot of the current window configuration
            (setq phw-activated-window-configuration
                  (current-window-configuration))
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the snapshot of the windows-configuration." err-obj)))
        )))
  phw-minor-mode)


;;;###autoload
(defun phw-deactivate ()
  "Deactivates the PHW and kills all PHW buffers and windows."
  (interactive)
  (phw-minor-mode 0))

(defun phw-deactivate-internal (&optional run-no-hooks)
  "Deactivates the PHW and kills all PHW buffers and windows."
  (unless (not phw-minor-mode)

    (when (or run-no-hooks
              (run-hook-with-args-until-failure 'phw-before-deactivate-hook))

      (setq phw-last-window-config-before-deactivation
            (current-window-configuration))

      ;; deactivating the adviced functions
      (dolist (adviced-set-elem phw-adviced-function-sets)
        ;; Note: as permanent defined advices-sets are not disabled here!
        (phw-disable-advices (car adviced-set-elem)))

      (phw-enable-own-temp-buffer-show-function nil)

      (phw-enable-temp-buffer-shrink-to-fit nil)

      (update-all-hooks nil)

      ;; ediff-stuff
      (phw-deactivate-ediff-compatibility)

      ;; run any personal hooks
      (unless run-no-hooks
        (run-hooks 'phw-deactivate-hook))

      ;; clear the phw-frame. Here we try to preserve the split-state after
      ;; deleting the PHW-screen-layout.
      (when (frame-live-p phw-frame)
        (raise-frame phw-frame)
        (select-frame phw-frame)
        (condition-case oops
            (let* ((config (phw-window-configuration-data))
                   (window-before-redraw (nth 0 config))
                   (pos-before-redraw (nth 1 config))
                   (edit-win-data-before-redraw (nth 2 config))
                   (edit-win-list-after-redraw nil))
              ;; first we make all windows of the PHW-frame not dedicated and
              ;; then we delete all PHW-windows
              (phw-select-edit-window)

              ;; deletion of all windows. (All other advices are already
              ;; disabled!)
              (phw-with-original-permanent-layout-functions
               (delete-other-windows))

              ;; some paranoia....
              (set-window-dedicated-p (selected-window) nil)

              ;; now we restore the edit-windows as before the deactivation
              ;; (All other advices are already disabled!)
              (if (= (length edit-win-data-before-redraw)
                     (phw-edit-area-creators-number-of-edit-windows))
                  (phw-with-original-permanent-layout-functions
                   (phw-restore-edit-area))
                (phw-edit-area-creators-init))

              (setq edit-win-list-after-redraw (phw-canonical-edit-windows-list))

              ;; a safety-check if we have now at least as many windows as
              ;; edit-windows before deactivation. If yes we restore all
              ;; window-data as before deactivation.
              (when (= (length edit-win-list-after-redraw)
                       (length edit-win-data-before-redraw))
                (dotimes (i (length edit-win-data-before-redraw))
                  (let ((win (nth i edit-win-list-after-redraw))
                        (data (nth i edit-win-data-before-redraw)))
                    (set-window-buffer win (nth 0 data))
                    (set-window-start win (nth 1 data))
                    (set-window-point win (nth 2 data))
                    (if (> (length edit-win-list-after-redraw) 1)
                        (phw-set-window-size win (nth 3 data)))
                    )))

              ;; at the end we always stay in that window as before the
              ;; deactivation.
              (when (integerp window-before-redraw)
                (phw-select-edit-window window-before-redraw))
              ;; if we were in an edit-window before deactivation let us go to
              ;; the old place
              (when pos-before-redraw
                (goto-char pos-before-redraw)))
          (error
           ;; in case of an error we make all windows not dedicated and delete
           ;; at least all other windows
           (phw-warning "phw-deactivate-internal (error-type: %S, error-data: %S)"
                        (car oops) (cdr oops))
           (ignore-errors (delete-other-windows))))

        (if (get 'phw-frame 'phw-new-frame-created)
            (ignore-errors (delete-frame phw-frame t))))

      (phw-initialize-layout)

      ;; we do NOT disable the permanent-advices of
      ;; `phw-permanent-adviced-layout-functions' unless the user don't want
      ;; preserving the split-state after reactivating PHW.
      (when (not (equal phw-split-edit-window-after-start 'before-activation))
        (phw-disable-advices 'phw-permanent-adviced-layout-functions t)
        (phw-edit-area-creators-init))

      (setq phw-activated-window-configuration nil)

      (setq phw-minor-mode nil)
      ))

  (if (null phw-minor-mode)
      (message "The PHW is now deactivated."))
  phw-minor-mode)

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

  (force-mode-line-update t)

  phw-minor-mode)


;; PHW byte-compilation

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

(defun phw-auto-activate-hook()
  "If necessary, run `phw-activate' when Emacs is started."
  (when phw-auto-activate
    (phw-activate)))

(add-hook 'emacs-startup-hook 'phw-auto-activate-hook)


;; Klaus Berndl <klaus.berndl@sdm.de>: Cause of the magic autostart stuff of
;; the advice-package we must disable at load-time all these advices!!
;; Otherwise would just loading phw (not activating!) activate each advice
;; AFTER the FIRST usage of our advices!!

(dolist (adviced-set-elem phw-adviced-function-sets)
  (phw-disable-advices (car adviced-set-elem) t))

(provide 'phw)