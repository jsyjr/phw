;;; phw-multiframe.el --- 

;; $Id$

;; Copyright (C) 2000-2003, 2015 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: 
;; Version: 1.0.0

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is am implementation of multiple frame support for the PHW.  The
;; original design was fairly easy to implement and since it will be a long time
;; until PHW 2.0 it seems obvious that this should be done as a temporary
;; solution.
;;
;; In order to use this just create a new frame and run 'phw-activate.  You can
;; create as many frames and run PHW in any of them.
;;

;;; Notes:
;;
;; Because PHW is now global it is never really deactivated.  You can deactivate
;; PHW in a frame if you want but the advice will still be around.
;;
;; You have a separate PHW methods, directory, and source buffer for each PHW
;; frame.
;;

;;; Install:
;;
;; Place a (require 'phw-multiframe) at the end of your normal PHW
;; initialization

;;; TODO:
;;
;; - Should we have a separeate speedbar buffer?  What about eshell?
;;
;; - I should allocate my own phw-compile-window for each frame.
;;
;; - PHW deactivation isn't currently supported.
;;
;; - Make sure I don't have any hooks that might run on deleted buffers.
;;
;; - Make sure we clean up when a frame is deleted.
;;
;; - Is it possible to migrate some of this code into default-frame-alist
;; instead of using a hook?

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; Code:

;;make certain variables frame local

(defvar phw-multiframe-variables (list 'phw-last-edit-window-with-point
                                       'phw-edit-window
                                       'phw-compile-window
                                       'phw-frame
                                       'phw-windows-hidden
                                       'phw-toggle-layout-state
                                       'phw-minor-mode
                                       'phw-activated-window-configuration)
  "List of phw variables that are required to be nil in new frames and frame local.")

(defun phw-multiframe-make-frame-hook(frame)
  "Create a hook so that we can enable the default variables within new frames."
  (interactive
   (list
    (selected-frame)))

  ;;make variables frame local in this frame.

  ;;reset everything to the default value?
  
  (dolist(variable phw-multiframe-variables)
    (set-frame-parameter nil frame (list (cons variable nil)))
    (modify-frame-parameters frame (list (cons variable nil))))

  ;;phw-eshell-buffer-name ?
  ;;phw-speedbar-buffer-name ?

  ;;set PHW special buffer names

  (phw-multiframe-setup-buffer-name 'phw-methods-buffer-name " *PHW Methods <%s>*")
  (phw-multiframe-setup-buffer-name 'phw-history-buffer-name " *PHW History <%s>*")
  (phw-multiframe-setup-buffer-name 'phw-sources-buffer-name " *PHW Sources <%s>*")
  (phw-multiframe-setup-buffer-name 'phw-directories-buffer-name " *PHW Directories <%s>*")

  ;;eshell support
  (when (and (featurep 'eshell)
             (featurep 'phw-eshell))
    
    (phw-multiframe-setup-buffer-name 'phw-eshell-buffer-name " *eshell <%s>*")
    (phw-multiframe-setup-buffer-name 'eshell-buffer-name " *eshell <%s>*"))

  ;;speedbar support
  (when (and (featurep 'speedbar)
             (featurep 'phw-speedbar))
    
    ;;fix speedbar by binding the given speedbar frame value with the current frame
      
    (mapc (lambda(sframe)
	    (when (boundp sframe)
                (set-frame-parameter nil frame (list (cons sframe frame))))
                (modify-frame-parameters frame (list (cons sframe frame))))
            '(speedbar-frame speedbar-attached-frame dframe-attached-frame))
      
    ;;setup speedbar with a new buffer

    (let((new-phw-speedbar-buffer-name nil))
    
      (setq new-phw-speedbar-buffer-name (phw-multiframe-setup-buffer-name 'phw-speedbar-buffer-name " SPEEDBAR <%s>"))

      (set-frame-parameter nil frame (list (cons 'speedbar-buffer
						 (get-buffer-create new-phw-speedbar-buffer-name))))
      (modify-frame-parameters frame (list (cons 'speedbar-buffer
                                                 (get-buffer-create new-phw-speedbar-buffer-name)))))))

(defun phw-multiframe-setup-buffer-name(variable buffer-format-name)
  "Given a variable name such as 'phw-methods-buffer-name and a format such as
'*PHW Methods <%s>*' we will register a new buffer mapping with the current
frame.  When complete return the new buffer name."

  (let((new-buffer-name (format buffer-format-name
                                (format-time-string "%s"))))
    (with-no-warnings
      (set-frame-parameter nil frame (list (cons variable new-buffer-name))))
  
    (with-no-warnings
      (modify-frame-parameters frame (list (cons variable new-buffer-name))))

    new-buffer-name))

(with-no-warnings
  (defun phw-deactivate-internal ()
    "Deactivates the PHW and kills all PHW buffers and windows."
    (unless (not phw-minor-mode)
      
      (setq phw-minor-mode nil))
    (message "The PHW is now deactivated.")
    phw-minor-mode))

(defun phw-multiframe-activate-hook()
  "Hook to run to initialize multiframe support"

  ;;disable PHW frame management for this frame
  (ad-deactivate 'delete-frame)

  ;;now make sure that the buffer being displayed in the edit window isn't a
  ;;compilation buffer.  (NOTE: I actually think this should be a standard part
  ;;of the PHW)
  (phw-multiframe-edit-window-non-compilation-buffer))

(defun phw-multiframe-edit-window-non-compilation-buffer()
  "Go through the buffer list making the edit window a non compilation buffer."
  (interactive)
  
  (let((buffer-list (buffer-list))
       (index 0))

    (while (and (or (phw-compilation-buffer-p (window-buffer phw-edit-window))
                    (null (buffer-file-name (window-buffer phw-edit-window))))
                (< index (length buffer-list)))

      (set-window-buffer phw-edit-window (nth index buffer-list))
      
      (setq index (1+ index)))))

;;this needs to happen last and it should be the last hook
(add-hook 'phw-activate-hook 'phw-multiframe-activate-hook t)

;;we need to modify frame parameters for new frames
(add-hook 'after-make-frame-functions 'phw-multiframe-make-frame-hook)

(provide 'phw-multiframe)

;;; phw-multiframe.el ends here
