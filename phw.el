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

;; $Id$

;;; Commentary:
;;
;; PHW stands for "Persistent Horizontal Window".
;;
;;   +------------------------------------------------+
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                  Edit-area                     |
;;   |    (can be split into several edit-windows)    |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   +------------------------------------------------+
;;   |                                                |
;;   |          Persistent Horizontal Window          |
;;   |                                                |
;;   +------------------------------------------------+
;;

;;; Installation
;;
;; To use the Emacs code browser add the PHW files to your load path and add the
;; following line to your .emacs file:
;;
;; If you want autoloading PHW after first start:
;;
;;    (require 'phw-autoloads)
;;
;; or if you want loading the complete PHW:
;;
;;    (require 'phw)
;;
;; Optional: You can byte-compile PHW with `phw-byte-compile' after the
;;           PHW-package is loaded

;;; Activation

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

;;; Code:

;; We need this libraries already here if we miss some requirements
(require 'phw-util)

;; rest of phw loads
(require 'phw-layout)
(require 'phw-compilation)
(require 'phw-autogen)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

;;====================================================
;; Variables
;;====================================================
(defvar phw-major-mode-selected-source nil
  "Major-mode of currently selected source.")

(defvar phw-item-in-tree-buffer-selected nil
  "Only true if any item in any tree-buffer has been selected in recent
command.")

;; Klaus Berndl <klaus.berndl@sdm.de>: FRAME-LOCAL
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

(defun phw-kill-buffer-hook ()
  "Function added to the `kill-buffer-hook' during PHW activation.
  It does several tasks:
- Depending on the value in `phw-kill-buffer-clears-history' the corresponding
  entry in the history-buffer is removed.
- Clearing the method buffer if a file-buffer has been killed.
- The entry of the removed file-buffer is removed from `phw-tag-tree-cache'."
  (let* ((curr-buf (current-buffer))
         (buffer-file (phw-fix-filename (phw-buffer-file-name curr-buf))))
    ;; this prevents automatically from killing one of the phw-buffers because
    ;; these ones are never releated to file!
    (when buffer-file
      ;; 1. clearing the history if necessary
      (phw-history-kill-buffer-clear curr-buf)

      ;; 2. clearing the method buffer if a file-buffer is killed
      (phw-rebuild-methods-buffer-with-tagcache nil nil t)

      ;; 3. removing the file-buffer from `phw-tag-tree-cache'. Must be done
      ;;    after 2. because otherwise a new element in the cache would be
      ;;    created again by `phw-rebuild-methods-buffer-with-tagcache'.
      (phw-clear-tag-tree-cache (buffer-name curr-buf)))
    (when (member curr-buf (phw-get-current-visible-phw-buffers))
      (phw-error "Killing an special PHW-buffer is not possible!"))))


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

;;;###autoload
(defun phw-activate ()
  "Activates PHW and creates the special buffers for the choosen layout.
For the layout see `phw-layout-name'. This function raises always the
PHW-frame if called from another frame. This is the same as calling
`phw-minor-mode' with a positive argument."
  (interactive)
  (phw-minor-mode 1))

(defun phw-activate-internal ()
  "Activates the PHW and creates all the buffers and draws the PHW-screen
with the actually chosen layout \(see `phw-layout-name'). This function raises
always the PHW-frame if called from another frame."

  (phw-activate--impl)
  phw-minor-mode)


(defvar phw-upgrade-check-done nil)

(defun phw-clean-up-after-activation-failure (msg err)
  "Complete cleanup of all PHW-setups and report an error with message MSG."
  (let ((phw-minor-mode t))
    (phw-deactivate-internal t))
  (setq phw-minor-mode nil)
  (if phw-running-xemacs
      (phw-redraw-modeline t)
    (force-mode-line-update t))
  (error "PHW %s: %s (error-type: %S, error-data: %S)" phw-version msg
         (car err) (cdr err)))

(defvar phw-last-window-config-before-deactivation nil
  "Contains the last `phw-current-window-configuration' directly before
PHW has been deactivated. Do not set this variable!")

(defvar phw-temporary-changed-emacs-variables-alist nil
  "Internal alist which stores old values of emacs variables/options which
have to be changed during running PHW. Use only `phw-modify-emacs-variable'
for modifying this alist.")

(defun phw-modify-emacs-variable (var action &optional new-value)
  "Stores or restores the old value of the Emacs-variable symbol VAR.
VAR has to be a bound symbol for a variable. ACTION is either 'store or
'restore. The optional arg NEW-VALUE is only used when ACTION is 'store and is
that value VAR should be set to. After calling with ACTION is 'restore the
value of VAR is as before storing a NEW-VALUE for variable-symbol VAR."
  (case action
    (store
     (or (phw-find-assoc var phw-temporary-changed-emacs-variables-alist)
         (progn
           (setq phw-temporary-changed-emacs-variables-alist
                 (phw-add-assoc (cons var (symbol-value var))
                                phw-temporary-changed-emacs-variables-alist))
           (set var new-value))))
    (restore
     (let ((elem (phw-find-assoc var phw-temporary-changed-emacs-variables-alist)))
       (when elem
         (set var (cdr elem))
         (setq phw-temporary-changed-emacs-variables-alist
               (phw-remove-assoc var phw-temporary-changed-emacs-variables-alist)))))))

(defun autocontrol-hook (func pre add)
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

(defun phw-activate--impl ()
  "See `phw-activate'.  This is the implementation of PHW activation."
  (when (or (null phw-frame) (not (frame-live-p phw-frame)))
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

        ;; temporary changing some emacs-vars
        (when (< max-specpdl-size 3000)
          (phw-modify-emacs-variable 'max-specpdl-size 'store 3000))
        (when (< max-lisp-eval-depth 1000)
          (phw-modify-emacs-variable 'max-lisp-eval-depth 'store 1000))
        (when (and phw-running-xemacs
                   (boundp 'progress-feedback-use-echo-area))
          (phw-modify-emacs-variable 'progress-feedback-use-echo-area 'store t))

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

              (phw-activate-phw-autocontrol-function phw-compilation-update-idle-time
                                                     'phw-compilation-buffer-list-changed-p)
              (phw-activate-phw-autocontrol-function 'post
                                                     'phw-layout-post-command-hook)
              (phw-activate-phw-autocontrol-function 'pre
                                                     'phw-layout-pre-command-hook)
              (phw-activate-phw-autocontrol-function 0.5
                                                     'phw-repair-only-phw-window-layout)
              (phw-activate-phw-autocontrol-function 'post
                                                     'phw-handle-major-mode-visibilty)
              (add-hook 'after-save-hook 'phw-update-methods-after-saving)
              (add-hook 'kill-buffer-hook 'phw-kill-buffer-hook)

              ;; after adding all idle-timers and post- and pre-command-hooks we
              ;; activate the monitoring
              (phw-activate-phw-autocontrol-function 1 'phw-monitor-autocontrol-functions)

              ;; We activate the stealthy update mechanism
              (phw-stealthy-function-state-init)
              (phw-activate-phw-autocontrol-function phw-stealthy-tasks-delay
                                                      'phw-stealthy-updates)

              ;; running the compilation-buffer update first time
              (phw-compilation-buffer-list-init)

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

              ;; now we synchronize all PHW-windows
              (phw-window-sync)

              ;; now update all the PHW-buffer-modelines
              (phw-mode-line-format)
              )
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the layout setup of PHW." err-obj))
          )

        (condition-case err-obj
            (let ((edit-window (car (phw-canonical-edit-windows-list))))
              (when (and phw-display-default-dir-after-start
                         (null (phw-buffer-file-name
                                (window-buffer edit-window))))
                (phw-set-selected-directory
                 (phw-fix-filename (with-current-buffer (window-buffer edit-window)
                                     default-directory)))))
          (error
           (phw-clean-up-after-activation-failure
            "Errors during setting the default directory." err-obj)))

        (condition-case err-obj
            ;; we run any personal hooks
            (run-hooks 'phw-activate-hook)
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the hooks of phw-activate-hook." err-obj)))

        (condition-case err-obj
            ;; enable mouse-tracking for the phw-tree-buffers; we do this after
            ;; running the personal hooks because if a user putﾴs activation of
            ;; follow-mouse.el (`turn-on-follow-mouse') in the
            ;; `phw-activate-hook' then our own PHW mouse-tracking must be
            ;; activated later. If `turn-on-follow-mouse' would be activated
            ;; after our own follow-mouse stuff, it would overwrite our
            ;; mechanism and the show-node-name stuff would not work!
            (if (phw-show-any-node-info-by-mouse-moving-p)
                (tree-buffer-activate-follow-mouse))
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the mouse-tracking activation." err-obj)))

        (setq phw-minor-mode t)
        (message "The PHW is now activated.")

        (condition-case err-obj
            ;; now we display all `phw-not-compatible-options' and
            ;; `phw-renamed-options'
            (if (and phw-auto-compatibility-check
                     (or (phw-not-compatible-or-renamed-options-detected)
                         (not (phw-options-version=phw-version-p))))
                ;; we must run this with an idle-times because otherwise these
                ;; options are never displayed when Emacs is started with a
                ;; file-argument and PHW is automatically activated. I this
                ;; case the buffer of the file-argument would be displayed
                ;; after the option-display and would so hide this buffer.
                (phw-run-with-idle-timer 0.25 nil 'phw-display-upgraded-options)
              (phw-display-news-for-upgrade))
          (error
           (phw-clean-up-after-activation-failure
            "Error during the compatibility-check of PHW." err-obj)))

        ;; if we activate PHW first time then we display the node "First steps" of
        ;; the online-manual
        (ignore-errors
          (when (null phw-source-path)
            (let ((phw-show-help-format 'info))
              (phw-show-help)
              (Info-goto-node "First steps"))))

        ;; display tip of the day if `phw-tip-of-the-day' is not nil
        (ignore-errors
          (phw-show-tip-of-the-day))

        (phw-enable-advices 'phw-layout-basic-adviced-functions)

        (condition-case err-obj
            ;;now take a snapshot of the current window configuration
            (setq phw-activated-window-configuration
                  (phw-current-window-configuration))
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the snapshot of the windows-configuration." err-obj)))
        ))))


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
            (phw-current-window-configuration))

      ;; deactivating the adviced functions
      (dolist (adviced-set-elem phw-adviced-function-sets)
        ;; Note: as permanent defined advices-sets are not disabled here!
        (phw-disable-advices (car adviced-set-elem)))

      (phw-enable-own-temp-buffer-show-function nil)

      (phw-enable-temp-buffer-shrink-to-fit nil)

      (phw-stop-all-autocontrol/sync-functions)
      (remove-hook 'after-save-hook 'phw-update-methods-after-saving)
      (remove-hook 'kill-buffer-hook 'phw-kill-buffer-hook)

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
              (phw-make-windows-not-dedicated phw-frame)

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
           (ignore-errors (phw-make-windows-not-dedicated phw-frame))
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

      ;; we can safely do the kills because killing non existing buffers
      ;; doesnﾴt matter. We kill these buffers because some customize-options
      ;; takes only effect when deactivating/reactivating PHW, or to be more
      ;; precise when creating the tree-buffers again.
      (dolist (tb-elem (phw-phw-buffer-registry-name-list 'only-tree-buffers))
        (tree-buffer-destroy tb-elem))
      (phw-phw-buffer-registry-init)

      (setq phw-activated-window-configuration nil)

      (setq phw-minor-mode nil)

      ;; restoring the value of temporary modified vars
      (phw-modify-emacs-variable 'max-specpdl-size 'restore)
      (phw-modify-emacs-variable 'max-lisp-eval-depth 'restore)
      (when (and phw-running-xemacs
                 (boundp 'progress-feedback-use-echo-area))
        (phw-modify-emacs-variable 'progress-feedback-use-echo-area 'restore))))


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

  (if phw-running-xemacs
      (phw-redraw-modeline t)
    (force-mode-line-update t))

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
              (and (string-match "\\(silentcomp\\|tree-buffer\\|phw.*\\)\\.el$" file)
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
