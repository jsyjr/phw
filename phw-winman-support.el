;; phw-winman-support.el - support of several window managers

;; Copyright (C) 2000 - 2005, 2015 Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools, escreen, winring
;; Created: 2003

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

;;; Commentary
;;
;; This library contains support for several window-managers so they interact
;; well with PHW. Currently the following window-managers are supported by PHW:
;; - winring.el: Written by Barry A. Warsaw <bwarsaw@python.org>, get it from
;;   http://www.python.org/emacs/winring
;; - escreen.el: Written by Noah Friedman <friedman@splode.com>, get it from
;;   http://www.splode.com/~friedman/software/emacs-lisp/#ui
;;
;; Note: With one of these window-managers installed and active you can run
;; applications like Gnus, VM or BBDB in the same frame as PHW! Just use
;; different window-configurations (winring.el) or escreens (escreen.el) for
;; PHW and the other applications. Especially with winring.el you can give
;; every configuration a descriptive name like "PHW" or "Gnus" ; afterwards
;; you can jump to a window-configuration by name!


;;; Installation and enabling
;;
;; This library is installed autom. with PHW. But every support must be
;; enabled explicitly:
;; - winring: Call `phw-winman-winring-enable-support'. This *must* be done
;;   *before* the first call to any winring-command, so also before calling
;;   `winring-initialize'!
;; - escreen: Call `phw-winman-escreen-enable-support'. This *must* be done
;;   *before* the first call to any escreen-command, so also before calling
;;   `escreen-install'!
;;
;; You can also put into your .emacs:
;; (phw-winman-winring-enable-support) or/and
;; (phw-winman-escreen-enable-support)

;;; Deinstallation
;;
;; Just run `phw-winman-escreen-disable-support' rsp.
;; `phw-winman-winring-disable-support'.


;;; Usage
;;
;; After enabling the support of one of the supported window-managers just go
;; on as described in the commentary or introduction of the respective
;; library-file(s) of the window-manager. Here is a short description:

;; - winring: Run `winring-initialize'. If PHW is active then the resulting
;;   window-configuration is the PHW-window-configuration. Otherwise you can
;;   create the PHW-window-configuration when you first time call
;;   `winring-new-configuration' with name equal to `phw-winman-winring-name'.
;;   In general you can run all commands of the winring-library. If you jump
;;   to the PHW-window-configuration then PHW will be autom. activated and if
;;   you leave the PHW-window-configuration then PHW will autom. deactivated.

;; - escreen: Run `escreen-install' (deactivates PHW if currently
;;   running), `escreen-create-screen', `escreen-goto-screen' etc. The latter
;;   ones activate autom. PHW if creating or selecting the escreen with number
;;   `phw-escreen-number' (default = 1) and deactivate PHW autom. if leaving
;;   the PHW-escreen.


;;; BUGS
;;
;; Currently not known


;; Thanks to Johann "Myrkraverk" Oskarsson <myrkraverk@users.sourceforge.net>
;; for the first trigger for this support-library. He has suggested to
;; integrate PHW with escreen.


;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'phw-util)
(require 'phw-common-browser)

(silentcomp-defvar escreen-current-screen-number)


(defgroup phw-winman-support nil
  "Settings for supporting several window-managers.
Currently winring and escreen are supported."
  :group 'phw
  :prefix "phw-winman-")

(defcustom phw-winman-escreen-number 1
  "*Number of the escreen which is reserved for PHW.
If you go to the escreen with this number you go always to the escreen with
activated PHW. All other escreen-numbers are escreens with deactivated PHW!"
  :group 'phw-winman-support
  :group 'phw-most-important
  :type 'integer)

(defcustom phw-winman-winring-name "PHW"
  "*Name of the winring-window-configuration reserved for PHW.
If you go to the window-configuration with this name you go always to the
window-configuration with activated PHW. All other window-configuration are
configurations with deactivated PHW!"
  :group 'phw-winman-support
  :group 'phw-most-important
  :type 'string)

;; support for the library escreen.el ----------------------------------------

(defphw-advice-set phw-winman-escreen-adviced-functions
  "These functions of escreen are adviced if escreen is active during PHW is
active. This advice is a permanent advice set!"
  t)


(defun phw-winman-escreen-enable-support ()
  "Load the escreen-library and enable the PHW-support for it.
This does not install or activate escreen! For this you have still to call
`escreen-install'! For further documentation about escreen see the file
escreen.el!"
  (interactive)
  (if (locate-library "escreen")
      (condition-case nil
          (progn
            (require 'escreen)
            (phw-enable-advices 'phw-winman-escreen-adviced-functions)
            (add-hook 'escreen-goto-screen-hook
                      'phw-winman-escreen-goto-escreen-hook)
            (phw-info-message "Support for escreen enabled."))
        (error
         (phw-winman-escreen-disable-support)
         (phw-error "The escreen-support can not be properly installed!")))
    (phw-error "The library escreen.el can not be found!")))


(defun phw-winman-escreen-disable-support ()
  "Disable the escreen-support of PHW."
  (interactive)
  (phw-disable-advices 'phw-winman-escreen-adviced-functions t)
  (when (featurep 'escreen)
    (remove-hook 'escreen-goto-screen-hook
                 'phw-winman-escreen-goto-escreen-hook)))
    

(defun phw-winman-escreen-goto-escreen-hook ()
  "Activate PHW if we go to the escreen with number `phw-escreen-number'."
  (if (and (boundp 'phw-minor-mode)
           (not phw-minor-mode)
           (= escreen-current-screen-number
              phw-winman-escreen-number))
      (let ((phw-split-edit-window-after-start 'before-deactivation))
        (phw-activate))))

(defphw-advice escreen-save-current-screen-configuration
    before
    phw-winman-escreen-adviced-functions
  "escreen can only handle screen-configurations if PHW is deactivated. This
is because PHW handles its window-creation completely by itself and because it
uses dedicated windows. So we deactivate PHW before running this function."
  (if (and (boundp 'phw-minor-mode)
           phw-minor-mode
           (equal phw-frame (selected-frame)))
      (let ((phw-split-edit-window-after-start 'before-deactivation))
        (phw-deactivate))))

;; support for the library winring.el ---------------------------------------

(defphw-advice-set phw-winman-winring-adviced-functions
  "These functions of winring are adviced if winring is active during PHW is
active.  This advice is a permanent advice set!"
  t)

(defun phw-winman-winring-enable-support ()
  "Load the winring-library and enable the PHW-support for it.
This does not install or activate winring! For this you have still to call
`winring-initialize'! For further documentation about winring see the file
winring.el!"
  (interactive)
  (if (locate-library "winring")
      (condition-case nil
          (progn
            (require 'winring)
            (phw-enable-advices 'phw-winman-winring-adviced-functions)
            (phw-info-message "Support for winring enabled."))
        (error
         (phw-winman-winring-disable-support)
         (phw-error "The winring-support can not be properly installed!")))
    (phw-error "The library winring.el can not be found!")))

(defun phw-winman-winring-disable-support ()
  "Disable the winring-support of PHW."
  (interactive)
  (phw-disable-advices 'phw-winman-winring-adviced-functions t))


(defvar phw-winman-winring-phw-frame nil
  "Frame for which the PHW-window-configuration was set first time.")

(defphw-advice winring-set-name after phw-winman-winring-adviced-functions
  "Store frame if name is equal with `phw-winman-winring-name' and activate
PHW if we set the name `phw-winman-winring-name'."
  ;; Because this is an after advice of winring-name-of-current returns here
  ;; already the new name!
  (when (phw-string= (winring-name-of-current) phw-winman-winring-name)
    ;; we do this only the first time
    (when (null phw-winman-winring-phw-frame)
      (setq phw-winman-winring-phw-frame
            (or (ad-get-arg 1) (selected-frame))))
    ;; now we activate PHW if necessary
    (when (and (boundp 'phw-minor-mode)
               (not phw-minor-mode)
               (equal (or (ad-get-arg 1)
                          (selected-frame)) phw-winman-winring-phw-frame))
      (let ((phw-split-edit-window-after-start 'before-deactivation))
        (phw-activate)))))

(defphw-advice winring-duplicate-configuration before phw-winman-winring-adviced-functions
  "Prevent the PHW-window-configuration from being duplicated."
  (if (phw-string= (winring-name-of-current) phw-winman-winring-name)
      (phw-error "The PHW-window-configuration can not be duplicated!")))

(defphw-advice winring-restore-configuration before phw-winman-winring-adviced-functions
  "Deactivates PHW if the PHW-window-configuration is active."
  (if (and (phw-string= (winring-name-of-current) phw-winman-winring-name)
           (boundp 'phw-minor-mode)
           phw-minor-mode)
      (let ((phw-split-edit-window-after-start 'before-deactivation))
        (phw-deactivate))))
  

(defphw-advice winring-save-current-configuration before phw-winman-winring-adviced-functions
  "winring can only handle window-configurations if PHW is deactivated. This
is because PHW handles its window-creation completely by itself and because it
uses dedicated windows. So we deactivate PHW before running this function."
  (if (and (boundp 'phw-minor-mode)
           phw-minor-mode
           (equal phw-frame (selected-frame)))
      (let ((phw-split-edit-window-after-start 'before-deactivation))
        (phw-deactivate))))

  
(defphw-advice winring-initialize after phw-winman-winring-adviced-functions
  "If PHW is active when winring is initialized then this initial
window-configuration gets always the name `phw-winman-winring-name'."
  (if (and (boundp 'phw-minor-mode)
           phw-minor-mode
           (equal phw-frame (selected-frame)))
      (winring-set-name phw-winman-winring-name)))


;; not supported window-managing functions------------------------------------

(defphw-advice-set phw-winman-not-supported-function-advices
  "These function will be adviced so an error is reported when executed in the
phw-frame. This advice is a permanent advice set!"
  t)

(defphw-advice winner-mode before phw-winman-not-supported-function-advices
  "Prevents `winner-mode' from being activated for the PHW-frame."
  (if (equal (selected-frame) phw-frame)
      (phw-error "Can't use winner-mode functions in the phw-frame.")))

(defphw-advice winner-redo before phw-winman-not-supported-function-advices
  "Prevents `winner-redo' from being used within the PHW-frame."
  (if (equal (selected-frame) phw-frame)
      (phw-error "Can't use winner-mode functions in the phw-frame.")))

(defphw-advice winner-undo before phw-winman-not-supported-function-advices
  "Prevents `winner-undo' from being used within the PHW-frame."
  (if (equal (selected-frame) phw-frame)
      (phw-error "Can't use winner-mode functions in the phw-frame.")))

(when-phw-running-xemacs
  (defphw-advice push-window-configuration before phw-winman-not-supported-function-advices
    (if (and (equal (selected-frame) phw-frame)
             (phw-interactive-p))
        (phw-error "Can't use interactive push-window-configuration in the phw-frame.")))

  (defphw-advice pop-window-configuration before phw-winman-not-supported-function-advices
    (if (and (equal (selected-frame) phw-frame)
             (phw-interactive-p))
        (phw-error "Can't use interactive pop-window-configuration in the phw-frame.")))
  
  (defphw-advice unpop-window-configuration before phw-winman-not-supported-function-advices
    (if (and (equal (selected-frame) phw-frame)
             (phw-interactive-p))
        (phw-error "Can't use interactive unpop-window-configuration in the phw-frame.")))
  )

;; we disable all advices per default.

(phw-disable-advices 'phw-winman-winring-adviced-functions t)
(phw-disable-advices 'phw-winman-escreen-adviced-functions t)
(phw-disable-advices 'phw-winman-not-supported-function-advices t)

(silentcomp-provide 'phw-winman-support)

;;; phw-winman-support.el ends here
