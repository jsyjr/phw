;;; phw-compatibility.el --- PHW-compatibility for other packages

;; Copyright (C) 2000 - 2015 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Ryan Ware,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;;         Ryan Ware <ryan.r.ware@intel.com>
;; Maintainer: Ryan Ware <ryan.r.ware@intel.com>
;; Keywords: browser, code, programming, tools
;; Created: 2004

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
;; Contains compatibility-code for other-packages.
;;
;; Whenever commands of other packages are not fully compatible with PHW then
;; this library should contain the necessary code to make it fully compatible
;; - or at least working acceptable.
;;
;; This file is part of the PHW package which can be found at:
;; http://phw.sourceforge.net

(require 'phw-util)
(require 'phw-layout)

;; To add compatibilty code for packages just do:
;;
;; 1. Add the needed advice(s) to `phw-compatibility-advices'
;; 2. Add the advice-code below.
;;
;; All advices of `phw-compatibility-advices' will be autom. enabled when PHW
;; starts and autom. disabled when PHW shuts down. No advice is enabled just
;; by loading the PHW-library!

(defphw-advice-set phw-compatibility-advices
  "Contains all advices needed for package-compatibility.")

;; package bs.el ----------------------------------------------------------

(defphw-advice bs-show before phw-compatibility-advices
  "Ensures `bs-show' works well when called from another window as an
edit-window. Does nothing if called in another frame as the `phw-frame'."
  (when (equal (selected-frame) phw-frame)
    (unless (phw-point-in-edit-window-number)
      (phw-select-edit-window))
    ;; now we handle if bs-show should always display in the compile-window
    (let ((my-bs-buffer (get-buffer-create "*buffer-selection*")))
      ;; phw-compilation-buffer-p needs a living buffer!
      (when (and (phw-compilation-buffer-p my-bs-buffer)
                 phw-compile-window-height)
        (display-buffer (buffer-name my-bs-buffer))))))

;; package electric.el ------------------------------------------------------


(defphw-advice one-window-p around phw-always-disabled-advices
  "If called for the `phw-frame' it only returns not nil if there is exactly
one edit-window. Neither the phw-windows nor the compile-window nor the
minibuffer-window are considered. This adviced version of `one-window-p' is
not for direct usage therefore it's added to `phw-always-disabled-advices' and
therefore it's always disabled\; use the macro `phw-with-phw-advice' instead
if you need this adviced version of `one-window-p'!"
  (if (and phw-minor-mode
           (equal (selected-frame) phw-frame))
      (setq ad-return-value
            (= (length (phw-canonical-edit-windows-list)) 1))
    ad-do-it))

(defphw-advice Electric-pop-up-window around phw-compatibility-advices
  "Ensures that the electric-* commands \(e.g. `electric-buffer-list') work
well with PHW. If BUFFER is a \"compilation-buffer\" in the sense of
`phw-compilation-buffer-p' then BUFFER will be displayed in the compile-window
of PHW - if there is any. If the compile-window is temporally hidden then the
BUFFER is displayed in an edit-window!"
  (if (and phw-minor-mode
           (equal (selected-frame) phw-frame))
      (if (and (phw-compilation-buffer-p (ad-get-arg 0))
               (equal (phw-compile-window-state) 'visible))
          (pop-to-buffer (ad-get-arg 0))
        (let ((phw-compilation-buffer-names nil)
              (phw-compilation-major-modes nil)
              (phw-compilation-predicates nil))
          (phw-with-phw-advice 'one-window-p 'around
            ad-do-it)))
    ad-do-it))

(defphw-advice electric-command-history before phw-compatibility-advices
  "Ensures that the electric-* commands work well with PHW."
  (when (and phw-minor-mode
             (equal (selected-frame) phw-frame)
             (phw-point-in-dedicated-special-buffer))
    (phw-select-edit-window)))

(defphw-advice electric-buffer-list before phw-compatibility-advices
  "Ensures that the electric-* commands work well with PHW."
  (when (and phw-minor-mode
             (equal (selected-frame) phw-frame)
             (phw-point-in-dedicated-special-buffer))
    (phw-select-edit-window)))

(defphw-advice electric-buffer-list after phw-compatibility-advices
  "Ensures that the electric-* commands work well with PHW."
  (when (and phw-minor-mode
             (equal (selected-frame) phw-frame))
    (if (phw-buffer-obj "*Buffer List*")
        (bury-buffer (phw-buffer-obj "*Buffer List*")))))

;; package master.el ---------------------------------------------------------

;; The adviced version of switch-to-buffer-other-window can redraw the layout
;; (e.g. if the buffer in the compile-window is the slave and the
;; compile-window has been made visible), so <window> in the code below can be
;; a destroyed window-object! we have to prevent from this (e.g. by selecting
;; the window before by number).
(defphw-advice master-says around phw-compatibility-advices
  "Makes the function compatible with PHW."
  (if (or (not phw-minor-mode)
          (not (equal (selected-frame) phw-frame)))
      (phw-with-original-basic-functions ad-do-it)
    (if (null (buffer-live-p (phw-buffer-obj master-of)))
        (error "Slave buffer has disappeared")
      (let ((window  (selected-window))
            (point-loc (phw-where-is-point))
            (p (point)))
        (if (not (eq (window-buffer window) (phw-buffer-obj master-of)))
            (switch-to-buffer-other-window master-of))
        (if (ad-get-arg 0)
            (condition-case nil
                (apply (ad-get-arg 0) (ad-get-arg 1))
              (error nil)))
        (select-window (case (car point-loc)
                         (phw
                          (phw-get-phw-window-by-number (cdr point-loc)))
                         (edit
                          (phw-get-edit-window-by-number (cdr point-loc)))
                         (compile
                          phw-compile-window)
                         (minibuf
                          (minibuffer-window phw-frame))
                         (other-dedicated
                          (phw-get-window-by-number (cdr point-loc)))))
        (goto-char (point))))))

;; package scroll-all.el --------------------------------------------------

(defphw-advice scroll-all-function-all around phw-compatibility-advices
  "Make it compatible with PHW."
  (if (or (not phw-minor-mode)
          (not (equal (selected-frame) phw-frame)))
      (phw-with-original-basic-functions ad-do-it)
    (let (;; This runs the `other-window'-calls in the body in the right mode
          (phw-other-window-behavior 'only-edit))
      ;; return the current number of edit-windows if point is in an edit-window
      ;; and always return 1 if point is not in an edit-window.
      (cl-flet ((count-windows (&optional minibuf)
                            (if (phw-point-in-edit-window-number)
                                (length (phw-canonical-edit-windows-list))
                              1)))
        ad-do-it))))


;; package tmm.el --------------------------------------------------------

;; Klaus Berndl <klaus.berndl@sdm.de>: We can not use our
;; Electric-pop-up-window advice instaed of this advice because otherwise
;; some commands of the popup-menus of the phw-buffers would not work - this
;; comes from the save-window-excursion in the the tmm.
(defphw-advice tmm-prompt around phw-compatibility-advices
  "Make it compatible with PHW."
  (if (or (not phw-minor-mode)
          (not (equal (selected-frame) phw-frame)))
      (phw-with-original-basic-functions ad-do-it)
    ;; we set temporally `phw-other-window-behavior' to a function which
    ;; always selects the "next" window after the
    ;; `phw-last-edit-window-with-point'
    (let ((phw-other-window-behavior
           (lambda (win-list edit-win-list phw-win-list comp-win
                             mini-win point-loc nth-win)
             (phw-next-listelem edit-win-list
                                phw-last-edit-window-with-point)))
          ;; we must not handle the tmm-stuff as compilation-buffer
          (phw-compilation-buffer-names nil)
          (phw-compilation-major-modes nil)
          (phw-compilation-predicates nil))
      ad-do-it)))

;; ediff-stuff ---------------------------------------------------------------

(defvar phw-before-ediff-window-config nil)

;; We must not add this function to `ediff-before-setup-windows-hook' because
;; this hook is called very often - see docu. The hook
;; `ediff-before-setup-hook' is called only once - so it can be used to store
;; window-configs!
(defun phw-ediff-before-setup-hook ()
  "Special phw-setup before starting ediff."
  (if (and phw-minor-mode
           (equal (selected-frame) phw-frame))
      (progn
        (setq phw-before-ediff-window-config (current-window-configuration))
        (if phw-run-ediff-in-phw-frame
            ;; !!!! we must delete all PHW-windows and the compile-window so
            ;; ediff can manage the whole phw-frame concerning its windows!
            ;; This is the reason why we can advice `ediff-setup-windows' so
            ;; it runs with all original layout basic functions (especially
            ;; delete-other-window is necessary!)
            (progn
              (phw-toggle-phw-windows -1)
              (phw-toggle-compile-window -1))
          (if (not (phw-windows-all-hidden))
              (delete-other-windows (car (phw-canonical-edit-windows-list))))))
    (setq phw-before-ediff-window-config nil)))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Should we add this function to
;; `ediff-suspend-hook' too?! We should add something but this functions is
;; not perfectly....in general suspending ediff need some work here...
(defun phw-ediff-quit-hook ()
  "Added to the end of `ediff-quit-hook' during PHW is activated. It
does all necessary after finishing ediff."
  (when phw-minor-mode
    (if (and (not (equal (selected-frame) phw-frame))
             (y-or-n-p
              "Ediff finished. Do you want to delete the extra ediff-frame? "))
        (delete-frame (selected-frame) t))
    (select-frame phw-frame)
    (when phw-before-ediff-window-config
      (phw-set-window-configuration phw-before-ediff-window-config)
      (setq phw-before-ediff-window-config nil))))

(defun phw-activate-ediff-compatibility ()
  (if (boundp 'ediff-quit-hook)
      (put 'ediff-quit-hook 'phw-ediff-quit-hook-value
           ediff-quit-hook))
  (add-hook 'ediff-quit-hook 'ediff-cleanup-mess)
  (add-hook 'ediff-quit-hook 'phw-ediff-quit-hook t)
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: suspending ediff and
  ;; especially reactivating does currently not really work well...
  ;; (add-hook 'ediff-suspend-hook 'phw-ediff-quit-hook t)
  (add-hook 'ediff-before-setup-hook
            'phw-ediff-before-setup-hook))

(defun phw-deactivate-ediff-compatibility ()
  (if (get 'ediff-quit-hook 'phw-ediff-quit-hook-value)
      (setq ediff-quit-hook (get 'ediff-quit-hook
                                 'phw-ediff-quit-hook-value))
    (remove-hook 'ediff-quit-hook 'phw-ediff-quit-hook))
  (remove-hook 'ediff-before-setup-hook
               'phw-ediff-before-setup-hook))


;; view-stuff --------------------------------------------------------------------

;; The code of the view-package of GNU Emacs has to be advices when the
;; view-buffer is displayed in the compile-window of PHW.
;; The much simpler view-mechanism of XEmacs (view-less.el) should work out of
;; the box.

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: for Emacs 22 we need also a
;; workaround when no compile-window is active because the return-to-alist
;; stuff in Emacs 22 is not really smart and does not work with layout like
;; PHW - with Emacs 23 it works perfectly without a compile-window.
;; In Emacs 22 the return-to-alist contains in case of a window-layout with
;; more than two windows quit-window which switches the buffer after quiting
;; (which is not what we want); with deactivated PHW and more than 2 windows
;; the same dump behavior - but unfortunatelly PHW contains always more than
;; two windows (at least in by far most cases), so current view-mode-exit
;; stuff wil not work with Emacs 22 and PHW ==> we simplify the logic in the
;; following case:
;; - Emacs 22 is running and
;; - there is no compile-window:
;; - current buffer is in view-mode
;; then we just delete the current-window (for which view-mode-exit is called)
;; and select phw-last-edit-window-with-point (hmm, is this possible - this
;; one will be the deleted one...how to go back to that window we have to go
;; back??

(defphw-advice view-mode-exit around phw-compatibility-advices
  "Makes view-mode compatible with PHW.

If there is no compile-window \(i.e. the buffer with view-mode is not
displayed in the special compile-window of PHW) then nothing special is done
but the original `view-mode-exit' is performed.

If the view-buffer is displayed in the compile-window \(i.e. this function is
called from within the compile-window) then the whole window-management stuff
of view-mode is disabled only `view-no-disable-on-exit' is taken into acount.

The compile-window will be shrinked down with
`phw-toggle-compile-window-height' and the last edit-window with point will be
selected afterwards."
  (if (and (boundp 'phw-minor-mode)
           phw-minor-mode
           (eq (selected-frame) phw-frame)
           (eq (selected-window) phw-compile-window))
      (when view-mode
        (or view-no-disable-on-exit
            (view-mode-disable))
        ;;          (when (ad-get-arg 1) ;; = exit-action
        ;;            (setq view-exit-action nil)
        ;;            (funcall (ad-get-arg 1) (current-buffer)))
        (force-mode-line-update)
        (phw-toggle-compile-window-height -1)
        (select-window phw-last-edit-window-with-point))
    ad-do-it))


;; not yet done ----------------------------------------------------------------

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>:
;; *** The new package gdb-ui.el provides an enhanced graphical interface to
;; GDB. You can interact with GDB through the GUD buffer in the usual way, but
;; there are also further buffers which control the execution and describe the
;; state of your program. It separates the input/output of your program from
;; that of GDB and watches expressions in the speedbar. It also uses features
;; of Emacs 21 such as the display margin for breakpoints, and the toolbar.
;; This is new in Emacs 21.4 so maybe we have to make it compatible with PHW!
;; But maybe this could be hard because AFAIK gdb-ui.el uses dedicated
;; windows!




;; we disable the advices at load-time
(phw-disable-advices 'phw-compatibility-advices t)

(provide 'phw-compatibility)

;;; phw-compatibility.el ends here
