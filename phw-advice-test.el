;;; phw-advice-test.el --- test-lib for the advice backbone of PHW

;; Copyright (C) 2000 - 2009, 2015 Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author:  Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2009

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
;; Contains a test-suite for the advice-backbone of PHW.
;;
;; This file is part of the PHW package which can be found at:
;; http://phw.sourceforge.net

;;; Usage
;;
;; 1. Load this library into Emacs
;; 2. Call M-x phw-test-with-original-advice-set
;; 3. Compare the Output in the message-buffer with the expected output at the
;;    end of this file
;; 4. If there are differences then send this output to
;;    the phw-mailing-list (see Info-manual)

(eval-when-compile
  (require 'silentcomp))

(require 'phw-util)
(require 'phw-common-browser)

(defphw-advice-set phw-advice-test-set
  "An advice-set only for testing the advice-mechanism of PHW")

(defun phw-advice-test-defun-1 ()
  (message "I'm the ORIGINAL function phw-advice-test-defun-1"))

(defphw-advice phw-advice-test-defun-1 around phw-advice-test-set
  "An advice"
  (message "I'm the AROUND advice of phw-advice-test-defun-1"))

(defun phw-advice-test-defun-2 ()
  (message "I'm the ORIGINAL function phw-advice-test-defun-2"))

(defphw-advice phw-advice-test-defun-2 before phw-advice-test-set
  "An advice"
  (message "I'm the BEFORE advice of phw-advice-test-defun-2"))

(defphw-advice phw-advice-test-defun-2 after phw-advice-test-set
  "An advice"
  (message "I'm the AFTER advice of phw-advice-test-defun-2"))

(defun phw-advice-test-defun-3 ()
  (message "I'm the ORIGINAL function phw-advice-test-defun-3"))

(defphw-advice phw-advice-test-defun-3 around phw-always-disabled-advices
  "An always disabled advice"
  (message "I'm the AROUND advice of (the always disabled) phw-advice-test-defun-3"))

(defphw-advice phw-advice-test-defun-3 after phw-always-disabled-advices
  "An always disabled advice"
  (message "I'm the AFTER advice of (the always disabled) phw-advice-test-defun-3"))

(defun phw-advice-test-defun-4 ()
  (message "I'm the ORIGINAL function phw-advice-test-defun-4"))

(defphw-advice phw-advice-test-defun-4 around phw-always-disabled-advices
  "An always disabled advice"
  (message "I'm the AROUND advice of (the always disabled) phw-advice-test-defun-4"))

(defun phw-test-with-original-advice-set ()
  (interactive)
  (let ((phw-advices-debug-error t))
    (unwind-protect
        (progn
          (message "!!! BEGIN phw-test-with-original-advice-set !!!!")
          (phw-enable-advices 'phw-advice-test-set)
          (phw-disable-advices 'phw-always-disabled-advices)
          (phw-advice-test-defun-1)
          (phw-advice-test-defun-2)
          (phw-advice-test-defun-3)
          (phw-advice-test-defun-4)
          (phw-with-original-adviced-function-set 'phw-advice-test-set
            (phw-advice-test-defun-1)
            (message "LOC-0.1 phw-test-with-original-advice-set")
            (phw-with-phw-advice 'phw-advice-test-defun-3 'around
              (phw-advice-test-defun-1)
              (phw-advice-test-defun-2)
              (phw-advice-test-defun-3)
              (phw-advice-test-defun-4)
              (phw-with-phw-advice 'phw-advice-test-defun-3 'around
                (phw-advice-test-defun-3)
                (phw-advice-test-defun-4)
                (phw-with-phw-advice 'phw-advice-test-defun-3 'after
                  (phw-advice-test-defun-3)
                  (phw-advice-test-defun-4)
                  (phw-with-phw-advice 'phw-advice-test-defun-4 'around
                    (phw-advice-test-defun-3)
                    (phw-advice-test-defun-4))
                  (message "LOC-0.2 phw-test-with-original-advice-set")
                  (phw-advice-test-defun-3)
                  (phw-advice-test-defun-4))
                (message "LOC-0.3 phw-test-with-original-advice-set")
                (phw-advice-test-defun-3)
                (phw-advice-test-defun-4))
              (message "LOC-0.4 phw-test-with-original-advice-set")
              (phw-advice-test-defun-3)
              (phw-advice-test-defun-4))
            (phw-advice-test-defun-2)
            (phw-advice-test-defun-3)
            (phw-advice-test-defun-4)
            (message "LOC-1 phw-test-with-original-advice-set")
            (phw-with-original-adviced-function-set 'phw-advice-test-set
              (phw-advice-test-defun-1)
              (phw-advice-test-defun-2))
            (message "LOC-2 phw-test-with-original-advice-set")
            (phw-advice-test-defun-1)
            (phw-advice-test-defun-2)            
            (message "LOC-3 phw-test-with-original-advice-set")
            (phw-with-original-adviced-function-set 'phw-advice-test-set
              (phw-advice-test-defun-1)
              (phw-advice-test-defun-2)
              (message "LOC-4 phw-test-with-original-advice-set")
              (phw-with-original-adviced-function-set 'phw-advice-test-set
                (phw-advice-test-defun-1)
                (phw-advice-test-defun-2)))            
            (message "LOC-5 phw-test-with-original-advice-set")
            )
          (phw-advice-test-defun-1)
          (phw-advice-test-defun-2)
          (message "LOC-6 phw-test-with-original-advice-set"))
      (phw-disable-advices 'phw-advice-test-set)
      (phw-advice-test-defun-1)
      (phw-advice-test-defun-2)
      (phw-advice-test-defun-3)
      (phw-advice-test-defun-4)
      (message "!!! END phw-test-with-original-advice-set !!!!"))))


;; expected output:

;;  !!! BEGIN phw-test-with-original-advice-set !!!!
;;  PHW 2.33: debug enabling the advice-set: phw-advice-test-set
;;  PHW 2.33: debug enabling of 'after' advice phw-advice-test-defun-2 
;;  PHW 2.33: debug enabling of 'before' advice phw-advice-test-defun-2 
;;  PHW 2.33: debug enabling of 'around' advice phw-advice-test-defun-1 
;;  PHW 2.33: debug disabling the advice-set: phw-always-disabled-advices
;;  PHW 2.33: debug disabling of 'around' advice phw-advice-test-defun-4 
;;  PHW 2.33: debug disabling of 'after' advice phw-advice-test-defun-3 
;;  PHW 2.33: debug disabling of 'around' advice phw-advice-test-defun-3 
;;  PHW 2.33: debug disabling of 'around' advice count-windows 
;;  PHW 2.33: debug disabling of 'around' advice one-window-p 
;;  PHW 2.33: debug disabling of 'around' advice walk-windows 
;;  PHW 2.33: debug calling of 'around' advice phw-advice-test-defun-1 
;;  I'm the AROUND advice of phw-advice-test-defun-1
;;  PHW 2.33: debug calling of 'before' advice phw-advice-test-defun-2 
;;  I'm the BEFORE advice of phw-advice-test-defun-2
;;  I'm the ORIGINAL function phw-advice-test-defun-2
;;  PHW 2.33: debug calling of 'after' advice phw-advice-test-defun-2 
;;  I'm the AFTER advice of phw-advice-test-defun-2
;;  I'm the ORIGINAL function phw-advice-test-defun-3
;;  I'm the ORIGINAL function phw-advice-test-defun-4
;;  PHW 2.33: debug with original advice-set: phw-advice-test-set - ENTRY
;;  PHW 2.33: debug disabling the advice-set: phw-advice-test-set
;;  PHW 2.33: debug disabling of 'after' advice phw-advice-test-defun-2 
;;  PHW 2.33: debug disabling of 'before' advice phw-advice-test-defun-2 
;;  PHW 2.33: debug disabling of 'around' advice phw-advice-test-defun-1 
;;  I'm the ORIGINAL function phw-advice-test-defun-1
;;  LOC-0.1 phw-test-with-original-advice-set
;;  PHW 2.33: debug with always disabled phw-advice: around phw-advice-test-defun-3 - ENTRY
;;  PHW 2.33: debug enabling of 'around' advice phw-advice-test-defun-3 
;;  I'm the ORIGINAL function phw-advice-test-defun-1
;;  I'm the ORIGINAL function phw-advice-test-defun-2
;;  PHW 2.33: debug calling of 'around' advice phw-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) phw-advice-test-defun-3
;;  I'm the ORIGINAL function phw-advice-test-defun-4
;;  PHW 2.33: debug with always disabled phw-advice: around phw-advice-test-defun-3 - ENTRY
;;  PHW 2.33: debug calling of 'around' advice phw-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) phw-advice-test-defun-3
;;  I'm the ORIGINAL function phw-advice-test-defun-4
;;  PHW 2.33: debug with always disabled phw-advice: after phw-advice-test-defun-3 - ENTRY
;;  PHW 2.33: debug enabling of 'after' advice phw-advice-test-defun-3 
;;  PHW 2.33: debug calling of 'around' advice phw-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) phw-advice-test-defun-3
;;  PHW 2.33: debug calling of 'after' advice phw-advice-test-defun-3 
;;  I'm the AFTER advice of (the always disabled) phw-advice-test-defun-3
;;  I'm the ORIGINAL function phw-advice-test-defun-4
;;  PHW 2.33: debug with always disabled phw-advice: around phw-advice-test-defun-4 - ENTRY
;;  PHW 2.33: debug enabling of 'around' advice phw-advice-test-defun-4 
;;  PHW 2.33: debug calling of 'around' advice phw-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) phw-advice-test-defun-3
;;  PHW 2.33: debug calling of 'after' advice phw-advice-test-defun-3 
;;  I'm the AFTER advice of (the always disabled) phw-advice-test-defun-3
;;  PHW 2.33: debug calling of 'around' advice phw-advice-test-defun-4 
;;  I'm the AROUND advice of (the always disabled) phw-advice-test-defun-4
;;  PHW 2.33: debug disabling of 'around' advice phw-advice-test-defun-4 
;;  PHW 2.33: debug with always disabled phw-advice: around phw-advice-test-defun-4 - EXIT
;;  LOC-0.2 phw-test-with-original-advice-set
;;  PHW 2.33: debug calling of 'around' advice phw-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) phw-advice-test-defun-3
;;  PHW 2.33: debug calling of 'after' advice phw-advice-test-defun-3 
;;  I'm the AFTER advice of (the always disabled) phw-advice-test-defun-3
;;  I'm the ORIGINAL function phw-advice-test-defun-4
;;  PHW 2.33: debug disabling of 'after' advice phw-advice-test-defun-3 
;;  PHW 2.33: debug with always disabled phw-advice: after phw-advice-test-defun-3 - EXIT
;;  LOC-0.3 phw-test-with-original-advice-set
;;  PHW 2.33: debug calling of 'around' advice phw-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) phw-advice-test-defun-3
;;  I'm the ORIGINAL function phw-advice-test-defun-4
;;  PHW 2.33: debug with always disabled phw-advice: around phw-advice-test-defun-3 - EXIT
;;  LOC-0.4 phw-test-with-original-advice-set
;;  PHW 2.33: debug calling of 'around' advice phw-advice-test-defun-3 
;;  I'm the AROUND advice of (the always disabled) phw-advice-test-defun-3
;;  I'm the ORIGINAL function phw-advice-test-defun-4
;;  PHW 2.33: debug disabling of 'around' advice phw-advice-test-defun-3 
;;  PHW 2.33: debug with always disabled phw-advice: around phw-advice-test-defun-3 - EXIT
;;  I'm the ORIGINAL function phw-advice-test-defun-2
;;  I'm the ORIGINAL function phw-advice-test-defun-3
;;  I'm the ORIGINAL function phw-advice-test-defun-4
;;  LOC-1 phw-test-with-original-advice-set
;;  PHW 2.33: debug with original advice-set: phw-advice-test-set - ENTRY
;;  I'm the ORIGINAL function phw-advice-test-defun-1
;;  I'm the ORIGINAL function phw-advice-test-defun-2
;;  PHW 2.33: debug with original advice-set: phw-advice-test-set - EXIT
;;  LOC-2 phw-test-with-original-advice-set
;;  I'm the ORIGINAL function phw-advice-test-defun-1
;;  I'm the ORIGINAL function phw-advice-test-defun-2
;;  LOC-3 phw-test-with-original-advice-set
;;  PHW 2.33: debug with original advice-set: phw-advice-test-set - ENTRY
;;  I'm the ORIGINAL function phw-advice-test-defun-1
;;  I'm the ORIGINAL function phw-advice-test-defun-2
;;  LOC-4 phw-test-with-original-advice-set
;;  PHW 2.33: debug with original advice-set: phw-advice-test-set - ENTRY
;;  I'm the ORIGINAL function phw-advice-test-defun-1
;;  I'm the ORIGINAL function phw-advice-test-defun-2
;;  PHW 2.33: debug with original advice-set: phw-advice-test-set - EXIT
;;  PHW 2.33: debug with original advice-set: phw-advice-test-set - EXIT
;;  LOC-5 phw-test-with-original-advice-set
;;  PHW 2.33: debug enabling the advice-set: phw-advice-test-set
;;  PHW 2.33: debug enabling of 'after' advice phw-advice-test-defun-2 
;;  PHW 2.33: debug enabling of 'before' advice phw-advice-test-defun-2 
;;  PHW 2.33: debug enabling of 'around' advice phw-advice-test-defun-1 
;;  PHW 2.33: debug with original advice-set: phw-advice-test-set - EXIT
;;  PHW 2.33: debug calling of 'around' advice phw-advice-test-defun-1 
;;  I'm the AROUND advice of phw-advice-test-defun-1
;;  PHW 2.33: debug calling of 'before' advice phw-advice-test-defun-2 
;;  I'm the BEFORE advice of phw-advice-test-defun-2
;;  I'm the ORIGINAL function phw-advice-test-defun-2
;;  PHW 2.33: debug calling of 'after' advice phw-advice-test-defun-2 
;;  I'm the AFTER advice of phw-advice-test-defun-2
;;  LOC-6 phw-test-with-original-advice-set
;;  PHW 2.33: debug disabling the advice-set: phw-advice-test-set
;;  PHW 2.33: debug disabling of 'after' advice phw-advice-test-defun-2 
;;  PHW 2.33: debug disabling of 'before' advice phw-advice-test-defun-2 
;;  PHW 2.33: debug disabling of 'around' advice phw-advice-test-defun-1 
;;  I'm the ORIGINAL function phw-advice-test-defun-1
;;  I'm the ORIGINAL function phw-advice-test-defun-2
;;  I'm the ORIGINAL function phw-advice-test-defun-3
;;  I'm the ORIGINAL function phw-advice-test-defun-4
;;  !!! END phw-test-with-original-advice-set !!!!

(silentcomp-provide 'phw-advice-test)

;;; phw, 2015-advice-test.el ends here

