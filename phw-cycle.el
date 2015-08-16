;;; phw-cycle.el --- cycle buffers through phw windows.

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2002

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

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History:

;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.

;;; TODO:
;;
;; - What is the pattern we should use for cycling through other windows?
;;
;;   - phw-cycle-through-X-buffers (select the next X buffer)
;;   - phw-cycle-switch-to-X-buffer (set the X buffer using completion)
;;
;; - How do we setup the menubar?
;;
;;          - PHW
;;                Cycle
;;                     - Forward Compilation Buffer
;;                     - Set Compilation Buffer
;;
;; - What do we use for key bindings?
;;
;; - We need an easier way to setup completion and a better way to get the
;;   index.
;;
;; - If possible, try to put fit the buffer so that the end of buffer is at the
;; end of the window... if necessary.

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'phw-compilation)
(require 'phw-layout)

(defgroup phw-cycle nil
  "Setting for cycling through misc PHW buffers."
  :group 'phw
  :prefix "phw-cycle-")


(defun phw-cycle-through-compilation-buffers(&optional choose-buffer)
  "Cycle through all compilation buffers currently open.
The choosen compilation buffer is displayed within the compilation window
`phw-compile-window' \(if this window doesn't exist then an error is
displayed). If the currently opened buffer within the compilation window is
not a compilation buffer, we jump to the first compilation buffer. If not we
try to loop through all compilation buffers. If we hit the end we go back to
the beginning.

If CHOOSE-BUFFER is not nil then the user will be prompted for the
compilation-buffer to switch to.

Afterwards always the compile-window of PHW is selected."

  (interactive "P")
  (if (not (numberp phw-compile-window-height))
      (phw-error "This command needs a persistent compile window!")
    (if choose-buffer
        (switch-to-buffer (completing-read "PHW compilation buffer: "
                                           (phw-compilation-get-buffers)))
      
      (let* ((compilation-buffers (phw-compilation-get-buffers))
             ;; This works even if phw-compile-window is nil or not alive
             ;; (means temporally hidden) --> then current-buffer is the
             ;; buffer of the currently selected window!
             (current-buffer (or (and (phw-compile-window-live-p)
                                      (window-buffer phw-compile-window))
                                 (current-buffer)))
             (current-buffer-name (buffer-name current-buffer))
             (current nil)
             (index nil))
        (when (null compilation-buffers)
          (phw-error "No compilation buffers available."))
        
        (if (not (phw-compilation-buffer-p current-buffer))
            ;;if the current buffer is not a compilation buffer, goto the first
            ;;compilation buffer.
            
            (phw-cycle-set-compilation-buffer 0 compilation-buffers)
          
          ;;else... we need to determine what buffer to display.
          
          (setq current (assoc current-buffer-name compilation-buffers))
          
          (setq index (cdr current))
          
          (if (= (1+ index) (length compilation-buffers))
              ;;go back to the first buffer.
              (phw-cycle-set-compilation-buffer 0 compilation-buffers)
            (phw-cycle-set-compilation-buffer (1+ index)
                                              compilation-buffers)))))))
  

(defun phw-cycle-set-compilation-buffer(index compilation-buffers)
  "Set the buffer in the compilation window."

  (let ((buffer-name (car (nth index compilation-buffers))))
    (switch-to-buffer buffer-name)))


(silentcomp-provide 'phw-cycle)

;;; phw, 2015, 2015-cycle.el ends here
