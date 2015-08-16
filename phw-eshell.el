;;; phw-eshell.el --- eshell integration for the PHW.

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2001

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

;; This package provides eshell integration for the PHW.  This basically allows
;; you to jump to the eshell in the compilation window, sync up the current
;; eshell with the current PHW buffer and run commands without getting in the
;; way.
;;
;; It provides the following features:
;;
;; - ability to jump to the eshell buffer within the compilation window ( C-.e )
;;   If the eshell isn't running it will be started
;;
;; - expands the compilation window when you run commands.  So for example it
;;   allows you to view the eshell in minimized mode and then when you run 'ls'
;;   the window automatically expands.
;;
;; - Synchronizes the current directory of the eshell with the current buffer
;;   of the either the edit-window or the phw-windows.
;;
;; - Provides smart window layout of the eshell buffer.  This makes sure that
;;   the eshell is taking up the exact amount of space and that nothing is
;;   hidden.
;; 
;; The goal is to make it easy to jump to a command prompt to run OS level
;; commands.  
;; 
;; If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History:

;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.

;;; Design:
;;
;; Syncing the current buffer with the eshell is done two ways.  If the buffer
;; is visible in a window, we always resync.  If it is not visible then
;; phw-eshell-goto-eshell will sync up when the user goes to the eshell
;; buffer.
;;
;; Integrating of eshell is mostly done by advicing the command `eshell' which
;; uses the mechanism of the display-buffer (adviced version).


;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'phw-util)
(require 'phw-compilation)
(require 'phw-common-browser)

(silentcomp-defvar eshell-buffer-name)
(silentcomp-defun eshell)
(silentcomp-defun eshell/cd)
(silentcomp-defun eshell-send-input)
(silentcomp-defun eshell-bol)

(defgroup phw-eshell nil
  "Settings for eshell integration within the PHW."
  :group 'phw
  :prefix "phw-eshell-")

(defcustom phw-eshell-enlarge-when-eshell t
  "*Enlarge the compile-window if it is selected by `eshell'.
This takes only effect if the command `eshell' is called!"
  :group 'phw-eshell
  :type 'boolean)

(defcustom phw-eshell-fit-window-to-command-output t
  "*Fit the compile-window after an eshell-command to the output.
This is done by the function `phw-eshell-fit-window-to-output' which is added
to `eshell-post-command-hook' ie. which is running autom. after each
eshell-command."
  :group 'phw-eshell
  :type 'boolean)

(defcustom phw-eshell-auto-activate nil
  "*Startup the eshell and display it in the compile-window.
If current layout does not display a compile-window \(see
`phw-compile-window-height') then nothing is done."
  :group 'phw-eshell
  :type 'boolean)

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: was phw-eshell-synchronize -->
;; rename in texi and also to phw-upgrade (also with value-upgrade!)
(defcustom phw-eshell-buffer-sync 'basic
  "*Synchronize eshell with the default-directory of current source-buffer.

This option takes only effect if a permanant compile-window is used in the
current layout.

If 'always then the synchronization takes place always a buffer
changes in the edit window and if after this the
default-directory of the new edit-buffer is different from the
default-directory of the current eshell-buffer. If value is nil
then never a synchronization will take place. If a list of
major-modes then only if the `major-mode' of the new buffer
belongs NOT to this list.

If the special value 'basic is set then PHW uses the setting of the option
`phw-basic-buffer-sync'."
  :group 'phw-eshell
  :type '(radio :tag "Synchronize the eshell if in compile-window."
                (const :tag "Use basic value" :value basic)
                (const :tag "Always" :value always)
                (const :tag "Never" nil)
                (repeat :tag "Not with these modes"
                        (symbol :tag "mode"))))
    

(defcustom phw-eshell-buffer-sync-delay 'basic
  "*Time Emacs must be idle before the eshell-buffer of PHW is synchronized.
Synchronizing is done with the current source displayed in the edit window. If
nil then there is no delay, means synchronization takes place immediately. A
small value of about 0.25 seconds saves CPU resources and you get even though
almost the same effect as if you set no delay.

If the special value 'basic is set then PHW uses the setting of the option
`phw-basic-buffer-sync-delay'."
  :group 'phw-eshell
  :type '(radio (const :tag "Use basic value" :value basic)
                (const :tag "No synchronizing delay" :value nil)
                (number :tag "Idle time before synchronizing" :value 2))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'phw-minor-mode)
                            phw-minor-mode)
                       (phw-activate-phw-autocontrol-function
                        value 'phw-analyse-buffer-sync))))
  :initialize 'custom-initialize-default)
  


(defvar phw-eshell-pre-command-point nil
  "Point in the buffer we are at before we executed a command.")

(defvar phw-eshell-buffer-list nil
  "List of eshell-buffers created until now.
Background: `eshell' creates new eshell-buffers with `generate-new-buffer' if
called with an prefix arg!")


(defphw-advice-set phw-eshell-adviced-functions
  "These functions of eshell are adviced if ehsell is active during PHW is
active.")

(defphw-advice eshell around phw-eshell-adviced-functions
  "Ensure that ehsell is running in the PHW-compile-window if any."
  ;; we tell PHW to handle the eshell-buffers as compilation-buffers so they
  ;; will be displayed in the compile-window (if any). We must add this as
  ;; regexp because ehsell can open new eshell-buffers with a name created by
  ;; generate-new-buffer-name! This approach is not completely save because if
  ;; a users changes `eshell-buffer-name' during acivated PHW we get not
  ;; informed about this and maybe we can handle the new buffer-name of eshell
  ;; not as compilation-buffer. But we have no other chance: Adding the return
  ;; value of `eshell' in the advice to `phw-compilation-buffer-names-internal'
  ;; does not help because `eshell' uses the new buffer-name already for
  ;; `pop-to-buffer'. So an after advice would add the new buffer-name to late
  ;; and a before-advice does not know the new-buffer name. The only way would
  ;; be to reimplement the whole `eshell'-code in an around advice but this is
  ;; not related to the benefit. IMO is it very improbably that a user changes
  ;; `eshell-buffer-name' at all...
  (let ((new-elem (cons (concat ".*"
                                (regexp-quote eshell-buffer-name)
                                ".*")
                        t)))
    (if phw-compile-window-height
        (progn
          (add-to-list 'phw-compilation-buffer-names-internal new-elem)
          (add-to-list 'phw-compilation-major-modes-internal 'eshell-mode))
      ;; if we have no persistent compile-window we do not handle eshell autom.
      ;; as compilation-buffer. If the user wants this then he has to modify
      ;; `phw-compilation-buffer-names' and/or `phw-compilation-major-modes'.
      ;; Therefore we remove the new-elem here from the internal lists.
      (setq phw-compilation-buffer-names-internal
            (delete new-elem phw-compilation-buffer-names-internal))
      (setq phw-compilation-major-modes-internal
            (delete 'eshell-mode phw-compilation-major-modes-internal))))
  
  ;; maybe we have to auto toggle our compile-window if temporally hidden
  (when (equal 'hidden (phw-compile-window-state))
    (phw-layout-debug-error "eshell around-advice: comp-win will be toggled.")
    (phw-toggle-compile-window 1))

  (phw-activate-phw-autocontrol-function phw-eshell-buffer-sync-delay
                                         'phw-eshell-buffer-sync)
  ;; some hooks
  (add-hook 'eshell-post-command-hook 'phw-eshell-recenter)
  (add-hook 'eshell-post-command-hook 'phw-eshell-fit-window-to-output)
  (add-hook 'eshell-pre-command-hook 'phw-eshell-precommand-hook)
  (add-hook 'window-size-change-functions 'phw-eshell-window-size-change)

  ;; run `eshell' --------------------------------------------
  (phw-eshell-save-buffer-history
   ad-do-it)
  ;; ---------------------------------------------------------

  ;; some post processing

  ;; add the buffer of the buffer used/created by `eshell' to
  ;; `phw-eshell-buffer-list'
  (add-to-list 'phw-eshell-buffer-list ad-return-value)

  (when phw-eshell-enlarge-when-eshell
    (phw-toggle-compile-window-height 1))

  ;;always recenter because if the point is at the top of the eshell buffer
  ;;and we switch to it the user is not going to be able to type a command
  ;;right away.
  (phw-eshell-recenter)  
  
  ;;sync to the current buffer
  (phw-eshell-buffer-sync))
  
  

(defun phw-eshell-activate-integration ()
  "Does all necessary to activate the eshell-integration. But this doesn not
load or activate eshell - it just prepares PHW to work perfectly with eshell."
  (phw-enable-advices 'phw-eshell-adviced-functions))

(defun phw-eshell-deactivate-integration ()
  (phw-disable-advices 'phw-eshell-adviced-functions)
  (phw-stop-autocontrol/sync-function 'phw-eshell-buffer-sync)
  (remove-hook 'eshell-post-command-hook 'phw-eshell-recenter)
  (remove-hook 'eshell-post-command-hook 'phw-eshell-fit-window-to-output)
  (remove-hook 'eshell-pre-command-hook 'phw-eshell-precommand-hook)
  (remove-hook 'window-size-change-functions 'phw-eshell-window-size-change))

(defphw-autocontrol/sync-function phw-eshell-buffer-sync nil phw-eshell-buffer-sync t
  "Synchronize the eshell with the directory of current source-buffer.
This is only done if the eshell is currently visible in the compile-window of
PHW and if either this function is called interactively or
`phw-eshell-buffer-sync' is not nil."
  (when (and (equal (selected-frame) phw-frame)
             (phw-compile-window-live-p)
             (phw-point-in-edit-window-number))
    (let* ((my-eshell-buffer
            ;; nil or a living eshell-buffer in the phw-compile-window
            (car (member (window-buffer phw-compile-window)
                         phw-eshell-buffer-list)))
           (my-reference-directory default-directory)
           (my-eshell-directory (and (bufferp my-eshell-buffer)
                                     (with-current-buffer my-eshell-buffer
                                       default-directory))))
      (when (and (bufferp my-eshell-buffer)
                 (stringp my-reference-directory)
                 (stringp my-eshell-directory)
                 (not (phw-string= (phw-fix-filename my-reference-directory)
                                   (phw-fix-filename my-eshell-directory))))
        (phw-eshell-save-buffer-history
         (with-current-buffer my-eshell-buffer
           ;; make sure we have a clean eshell-command-line
           (goto-char (point-max))
           (eshell-bol)
           (delete-region (point) (point-at-eol))
           ;;change the directory without showing the cd command
           (eshell/cd my-reference-directory))
           
         ;;execute the command
         (save-selected-window
           (select-window phw-compile-window)
           (eshell-send-input)))
        
        (phw-eshell-recenter)
        
        ;; we need to make sure that that the eshell buffer isn't at the
        ;; top of the buffer history list just because we implicitly
        ;; changed its directory and switched to it. It might not be a
        ;; good idea in the long term to put it all the way at the end of
        ;; the history list but it is better than leaving it at the top.
        (bury-buffer eshell-buffer-name)))))

(defmacro phw-eshell-save-buffer-history (&rest body)
  "Protect the buffer-list so that the eshell buffer name is not placed early
in the buffer list or at all if it currently doesn't exist."
  (let ((eshell-buffer-list (make-symbol "my-buffer-list")))
    `(let ((,eshell-buffer-list (phw-frame-parameter (selected-frame)
                                                     'buffer-list)))
       (unwind-protect
           (progn
             ,@body)
         (modify-frame-parameters nil (list (cons 'buffer-list
                                                  ,eshell-buffer-list)))))))

(defun phw-eshell-recenter(&optional display-errors)
  "Recenter the eshell window so that the prompt is at the buffer-end."
  (interactive (list t))

  (if (and (equal (selected-frame) phw-frame)
           (phw-compile-window-live-p)
           ;; the buffer in the phw-compile-window is a living eshell-buffer
           (member (window-buffer phw-compile-window)
                   phw-eshell-buffer-list))
      (save-selected-window
        (select-window phw-compile-window)
        (goto-char (point-max))
        (recenter -2))
    (when display-errors
      (phw-error "Eshell not running or compile-window not visible!"))))

(defun phw-eshell-precommand-hook ()
  ;;use the eshell-pre-command-hook to set the point.
  (setq phw-eshell-pre-command-point (point)))


(defun phw-eshell-fit-window-to-output()
  "Fit window of eshell to the output of last command. This function is added
to `eshell-post-command-hook' and only called there. This function tries to
fit the height of the compile-window best to the last command-output. The
algorithm fit the window to the height of the last command-output but do not
enlarge the compile-window over half of the frame-height and also not below
`phw-compile-window-height' (in lines)."
  (when (and (equal (selected-frame) phw-frame)
             (phw-compile-window-live-p)
             ;; the buffer in the phw-compile-window is a living eshell-buffer
             (member (window-buffer phw-compile-window)
                     phw-eshell-buffer-list))

    ;; fit the window to the height of the last command-output but do not
    ;; enlarge the compile-window over half of the frame-height and also not
    ;; below `phw-compile-window-height' (in lines).
    (when (and phw-eshell-fit-window-to-command-output
               (integer-or-marker-p phw-eshell-pre-command-point))
      (let* ((compile-window-height-lines
              (phw-normalize-number phw-compile-window-height
                                    (1- (frame-height))))
             (phw-enlarged-compilation-window-max-height
              (max (min (with-current-buffer (window-buffer phw-compile-window)
                          ;; we want to see the old command line too and 2
                          ;; must be added because we have a modeline and one
                          ;; empty line cause of the (recenter -2) in
                          ;; `phw-eshell-recenter'. For XEmacs it would be
                          ;; better to check if a horiz. scrollbar is used.
                          ;; This causes the one line more we need for XEmacs
                          (+ (if phw-running-xemacs 4 3)
                             (count-lines phw-eshell-pre-command-point
                                          (point))))
                        (/ (1- (frame-height)) 2))
                   compile-window-height-lines)))
        (phw-toggle-compile-window-height 1)
        (phw-eshell-recenter))
        
      ;;reset
      (setq phw-eshell-pre-command-point nil))))


(defun phw-eshell-auto-activate-hook()
  "Activate the eshell when PHW is activated. See `phw-eshell-auto-activate'."
  (when phw-eshell-auto-activate
    (ignore-errors (eshell))))

(defun phw-eshell-window-size-change(frame)
  "Called when we change window sizes so that the eshell can resize."
  (when (and phw-minor-mode
             (equal frame phw-frame))
    (ignore-errors (phw-eshell-recenter))))

(add-hook 'phw-activate-hook 'phw-eshell-auto-activate-hook)

(silentcomp-provide 'phw-eshell)

;;; phw, 2015, 2015-eshell.el ends here
