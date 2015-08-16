;;; phw-buffertab.el --- 

;; $Id$

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
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

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; Code:

;;; TODO: make sure we don't do this TOO many times.
;;
;; - we need to define a property with 'local-map set correctly.
;;
;; - write a function that generates a popup menu
;;
;; - the popup menu should allow the user to check a buffer view to set it as
;;   the default when opening new files.
;;
;;   - is this possible?  I think it might but I would need to set it up
;;   correctly.

(eval-when-compile
  (require 'silentcomp))

(require 'phw-compilation)

(with-no-warnings
  (defface phw-buffertab-primary-face '((t (:bold t :foreground "black")))
    "Face used to highlight the annotation lines to the left of the annotate buffer."
    :group 'phw))

(with-no-warnings
  (defface phw-buffertab-secondary-face '((t (:bold nil :foreground "black")))
    "Face used to highlight the annotation lines to the left of the annotate buffer."
    :group 'phw))

(with-no-warnings
  (defface phw-buffertab-secondary-mouse-face '((t (:bold nil :foreground "black" :italic t)))
    "Face used to highlight the annotation lines to the left of the annotate buffer."
    :group 'phw))

(with-no-warnings
  (defcustom phw-buffertab-map
    (let ((map (make-sparse-keymap)))
      (define-key map [header-line down-mouse-2] 'phw-buffertab-popup-menu)
      map)
    "Key map used for buffertab navigation"
    :group 'phw))

(defun phw-buffertab-popup-menu()
  "Popup a menu for selecting an PHW buffer."
  (interactive)

  (let((menu (phw-buffertab-make-menu "Compilation Buffers")))

    (popup-menu menu)))

(defun phw-buffertab-make-menu(name)
  "Make a menu for use on the buffertab."

  (let((menu (list 'keymap name)))

    (dolist(entry (phw-compilation-get-buffers))

      (add-to-list 'menu (cons (list (car entry) (car entry))
                               'switch-to-buffer)) t)

    (pp menu)
    
    menu))

(defun phw-buffertab-setup-header()
  ""
  (interactive)

  (with-no-warnings
    (let ((phw-prefix "   PHW: " ))
      (with-current-buffer (get-buffer phw-speedbar-buffer-name)
	;;FIXME: figure out what modeline tab to use
	(setq header-line-format (concat phw-prefix "/ " (buffer-name)" "))

	(add-text-properties 0 (length phw-prefix)
			     (list 'face 'phw-buffertab-primary-face)
			     header-line-format)

	(add-text-properties (1+ (length phw-prefix)) (length header-line-format)
			     (list 'face 'phw-buffertab-secondary-face
				   'mouse-face 'phw-buffertab-secondary-mouse-face
				   'local-map 'phw-buffertab-map)
			     header-line-format)))))

(silentcomp-provide 'phw-buffertab)

;;; phw, 2015, 2015-buffertab.el ends here
