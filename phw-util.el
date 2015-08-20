;;; phw-util.el --- utility functions for PHW

;; Copyright (C) 2000-2015   Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2000

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
;; Contains misc utility functions for PHW.
;;
;; This file is part of the PHW package which can be found at:
;; http://phw.sourceforge.net

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.

;;; Code:

;;; ----- Some constants -----------------------------------

;;;###autoload
(defconst phw-running-xemacs nil)

(defconst phw-running-gnu-emacs (not phw-running-xemacs))

(defconst phw-running-unsupported-emacs (condition-case nil
                                            (<= emacs-major-version 20)
                                          (error t))
  "True if running XEmacs or Gnu Emacs < 21.")

(defconst phw-running-gnu-emacs-version-22 (and phw-running-gnu-emacs
                                                (>= emacs-major-version 22))
  "True if running Gnu Emacs >= version 22")

(defconst phw-running-gnu-emacs-version-23 (and phw-running-gnu-emacs
                                                (>= emacs-major-version 23))
  "True if running Gnu Emacs >= version 23")

(defconst phw-temp-dir
  (file-name-as-directory
   (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP")
       (if (eq system-type 'windows-nt) "c:/temp/" "/tmp/")))
  "A directory where PHW can store temporary files.")

(defconst phw-phw-dir
  (expand-file-name (file-name-directory (locate-library "phw"))))
(defconst phw-semantic-dir
  (if (locate-library "semantic")
      (expand-file-name (file-name-directory (locate-library "semantic")))))

(defconst phw-phw-parent-dir (expand-file-name (concat phw-phw-dir "../")))

;; image support possible with current Emacs setup?
;; This will first checked at activation-time of PHW because otherwise usage
;; of emacs --deamon could fail...

(defvar phw-images-can-be-used nil
  "INTERNAL - DO NOT USE AND CHANGE!")
(defvar phw-images-can-be-used-init-p nil
  "INTERNAL - DO NOT USE AND CHANGE!")

(defun phw-images-can-be-used ()
  "Not nil if images can be used with current Emacs setup."
  (if phw-images-can-be-used-init-p
      phw-images-can-be-used
    (setq phw-images-can-be-used-init-p t)
    (setq phw-images-can-be-used
          (and (or (fboundp 'defimage)
                   (fboundp 'make-image-specifier))
               (if (fboundp 'display-images-p)
                   (display-images-p)
                 window-system)))))


;;; ----- Tracing ------------------------------------------

;; we use the trace.el library!

;;; ----- Compatibility between GNU Emacs and XEmacs -------

;; miscellaneous differences

(defmacro when-phw-running-xemacs (&rest body)
  "Evaluates BODY when `phw-running-xemacs' is true. Use this macro when you
want the BODY being parsed by semantic!. If not use the variable
`phw-running-xemacs'."
  `(when phw-running-xemacs
     ,@body))

(defmacro when-phw-running-emacs (&rest body)
  "Evaluates BODY when `phw-running-gnu-emacs' is false. Use this
macro when you want the BODY being parsed by semantic!. If not
use the form \(unless phw-running-xemacs)."
  `(when phw-running-gnu-emacs
     ,@body))

(defmacro when-phw-running-emacs-22 (&rest body)
  "Evaluates BODY when `phw-running-gnu-emacs-version-22' is
true. Use this macro when you want the BODY being parsed by
semantic!. If not use the form \(when phw-running-gnu-emacs-version-22)."
  `(when phw-running-gnu-emacs-version-22
     ,@body))

(defmacro when-phw-running-emacs-23 (&rest body)
  "Evaluates BODY when `phw-running-gnu-emacs-version-23' is
true. Use this macro when you want the BODY being parsed by
semantic!. If not use the form \(when phw-running-gnu-emacs-version-23)."
  `(when phw-running-gnu-emacs-version-23
     ,@body))

(defun phw-event-to-key (event)
  (let ((type (event-basic-type event)))
    (case type
      ((mouse-1 mouse-2 mouse-3) 'mouse-release)
      ((down-mouse-1 down-mouse-2 down-mouse-3) 'mouse-press)
      (otherwise (event-basic-type event)))))
(defalias 'phw-facep 'facep)
(defun phw-noninteractive ()
  "Return non-nil if running non-interactively, i.e. in batch mode."
  noninteractive)
(defalias 'phw-subst-char-in-string 'subst-char-in-string)
(defalias 'phw-substring-no-properties 'substring-no-properties)
(defalias 'phw-derived-mode-p 'derived-mode-p)
(defsubst phw-count-screen-lines (&optional beg end)
  (count-screen-lines beg end))
(defalias 'phw-frame-parameter 'frame-parameter)
(defalias 'phw-line-beginning-pos 'line-beginning-position)
(defalias 'phw-line-end-pos 'line-end-position)
(defalias 'phw-bolp 'bolp)
(defalias 'phw-eolp 'eolp)
(defalias 'phw-bobp 'bobp)
(defalias 'phw-eobp 'eobp)
(defun phw-event-window (event)
  (posn-window (event-start event)))
(defun phw-event-point (event)
  (posn-point (event-start event)))
(defun phw-event-buffer (event)
  (window-buffer (phw-event-window event)))
(defun phw-window-full-width (&optional window)
  (let ((edges (window-edges window)))
    (- (nth 2 edges) (nth 0 edges))))
(defalias 'phw-window-display-height 'window-text-height)
(defalias 'phw-window-full-height 'window-height)
(defalias 'phw-frame-char-width 'frame-char-width)
(defalias 'phw-frame-char-height 'frame-char-height)
(defalias 'phw-window-edges 'window-edges)

;; overlay- and extend-stuff

(progn
  (defalias 'phw-make-overlay            'make-overlay)
  (defalias 'phw-overlay-p               'overlayp)
  (defalias 'phw-overlay-put             'overlay-put)
  (defalias 'phw-overlay-get             'overlay-get)
  (defalias 'phw-overlay-move            'move-overlay)
  (defalias 'phw-overlay-delete          'delete-overlay)
  (defalias 'phw-overlay-kill            'delete-overlay))
;; XEmacs
(defalias 'phw-make-overlay            'make-extent)
(defalias 'phw-overlay-p               'extentp)
(defalias 'phw-overlay-put             'set-extent-property)
(defalias 'phw-overlay-get             'extent-property)
(defalias 'phw-overlay-move            'set-extent-endpoints)
(defalias 'phw-overlay-delete          'detach-extent)
(defalias 'phw-overlay-kill            'delete-extent)

;; timer stuff

(progn
  (defalias 'phw-run-with-timer 'run-with-timer)
  (defalias 'phw-run-with-idle-timer 'run-with-idle-timer)
  (defalias 'phw-cancel-timer 'cancel-timer))
;; XEmacs
(defun phw-run-with-timer (secs repeat function &rest args)
  (start-itimer "phw-timer" function secs repeat
                nil (if args t nil) args))
(defun phw-run-with-idle-timer (secs repeat function &rest args)
  (start-itimer "phw-idle-timer"
                function secs (if repeat secs nil)
                t (if args t nil) args))
(defun phw-cancel-timer (timer)
  (delete-itimer timer))

;;; ----- Customize stuff ----------------------------------

(defun phw-custom-file ()
  "Filename of that file which is used by \(X)Emacs to store the
customize-options. If no custom-file can be computed or if Emacs reports an
error \(e.g. GNU Emacs complains when calling `custom-file' and Emacs has been
started with -q) nil is returned."
  (if phw-running-xemacs
      custom-file
    (require 'cus-edit)
    (ignore-errors (custom-file))))

(defun phw-option-get-value (option &optional type)
  "Return the value of a customizable PHW-option OPTION with TYPE, where TYPE
can either be 'standard-value \(the default-value of the defcustom) or
'saved-value \(the value stored persistent by the user via customize) or
'customized-value \(the value set but not saved in the customize buffer).
If TYPE is nil then the most recent set value is returned, means it
tries the customized-value, then the saved-value and then the standard-value
in exactly this sequence."
  (let ((val (car (if type
                      (get option type)
                    (or (get option 'customized-value)
                        (get option 'saved-value)
                        (get option 'standard-value))))))
    (cond ((not (listp val)) val)
          ((equal 'quote (car val)) (car (cdr val)))
;;          (t (car val)))))
          (t (eval val)))))

;;; ----- Assoc helpers ------------------------------------

(defun phw-remove-assoc (key list)
  (delete nil
          (mapcar (function (lambda (elem)
                              (if (equal (car elem) key)
                                  nil
                                elem)))
                  list)))


(defun phw-add-assoc (key-value list)
  (cons key-value list))

(defun phw-find-assoc-value (key list)
  (cdr (assoc key list)))

(defun phw-find-assoc (key list)
  (assoc key list))

;;; ----- Some function from cl ----------------------------


(defun phw-filter (seq pred)
  "Filter out those elements of SEQUENCE for which PREDICATE returns nil."
  (let ((res))
    (dolist (elem seq)
      (if (if pred (funcall pred elem) elem)
          (setq res (append res (list elem)))))
    res))

(defun phw-some (cl-pred cl-seq &rest cl-rest)
  "Return true if PREDICATE is true of any element of SEQ or SEQs.
If so, return the true (non-nil) value returned by PREDICATE."
  (if (or cl-rest (nlistp cl-seq))
      (catch 'cl-some
	(apply 'map nil
	       (function (lambda (&rest cl-x)
			   (let ((cl-res (apply cl-pred cl-x)))
			     (if cl-res (throw 'cl-some cl-res)))))
	       cl-seq cl-rest) nil)
    (let ((cl-x nil))
      (while (and cl-seq (not (setq cl-x (funcall cl-pred (pop cl-seq))))))
      cl-x)))

(defun phw-copy-list (list)
  "Return a copy of a LIST, which may be a dotted list.
The elements of the list are not copied, just the list structure itself."
  (if (fboundp 'copy-sequence)
      (copy-sequence list)
    (if (consp list)
        (let ((res nil))
          (while (consp list) (push (pop list) res))
          (prog1 (nreverse res) (setcdr res list)))
      (car list))))


(defun phw-set-difference (list1 list2 &optional test-fcn)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all items that appear in LIST1 but not LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
If TEST-FCN is not nil then it must be a function which is used to check if an
item of LIST1 is an element of LIST2. If TEST-FCN is nil then `memq' is used."
  (if (or (null list1) (null list2)) list1
    (let ((res nil))
      (while list1
        (or (if test-fcn
                (funcall test-fcn (car list1) list2)
              (memq (car list1) list2))
            (push (car list1) res))
        (pop list1))
      res)))


(defun phw-member (item list &optional test-fcn)
  "Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM. Comparison is done via `equal'
unless TEST-FCN is not nil: In this case TEST-FCN will be used to compare ITEM
with the elements of LIST. If TEST-FCN is `eq' then `memq' is called for
optimization."
  (if test-fcn
      (if (eq test-fcn 'eq)
          ;; some optimization
          (memq item list)
        (progn
          (while (and list (not (funcall test-fcn item (car list))))
            (setq list (cdr list)))
          list))
    (member item list)))

;; stolen and adapted from cl-seq.el
(defun phw-delete-duplicates (cl-seq &optional
                                     cl-test-fcn cl-start cl-end cl-from-end cl-copy)
  "Deletes duplicate elements from CL-SEQ.
Comparison is done with `equal' unless CL-TEST-FCN is not nil: In
this case TEST-FCN will be used to compare CL-ITEM with the
elements of CL-SEQ. Specifically, if two elements from the
sequence match according to the test-function \(s.a.) only the
rightmost one is retained. If CL-FROM-END is true, the leftmost
one is retained instead. If CL-START or CL-END is specified, only
elements within that subsequence are examined or removed. If
CL-COPY is nil then it destructively modifies CL-SEQ otherwise a
copy of CL-SEQ with removed duplicates is returned."
  (if (listp cl-seq)
      (let ((cl-start (or cl-start 0)))
        (if cl-from-end
            (let ((cl-p (nthcdr cl-start cl-seq))
                  cl-i)
              (setq cl-end (- (or cl-end (length cl-seq)) cl-start))
              (while (> cl-end 1)
                (setq cl-i 0)
                (while (setq cl-i (phw-position (car cl-p)
                                                (cdr cl-p)
                                                cl-test-fcn
                                                cl-i
                                                (1- cl-end)))
                  (if cl-copy (setq cl-seq (phw-copy-list cl-seq)
                                    cl-p (nthcdr cl-start cl-seq) cl-copy nil))
                  (let ((cl-tail (nthcdr cl-i cl-p)))
                    (setcdr cl-tail (cdr (cdr cl-tail))))
                  (setq cl-end (1- cl-end)))
                (setq cl-p (cdr cl-p) cl-end (1- cl-end)
                      cl-start (1+ cl-start)))
              cl-seq)
          (setq cl-end (- (or cl-end (length cl-seq)) cl-start))
          (while (and (cdr cl-seq) (= cl-start 0) (> cl-end 1)
                      (phw-position (car cl-seq)
                                    (cdr cl-seq)
                                    cl-test-fcn
                                    0
                                    (1- cl-end)))
            (setq cl-seq (cdr cl-seq) cl-end (1- cl-end)))
          (let ((cl-p (if (> cl-start 0) (nthcdr (1- cl-start) cl-seq)
                        (setq cl-end (1- cl-end) cl-start 1) cl-seq)))
            (while (and (cdr (cdr cl-p)) (> cl-end 1))
              (if (phw-position (car (cdr cl-p))
                                (cdr (cdr cl-p))
                                cl-test-fcn
                                0
                                (1- cl-end))
                  (progn
                    (if cl-copy (setq cl-seq (phw-copy-list cl-seq)
                                      cl-p (nthcdr (1- cl-start) cl-seq)
                                      cl-copy nil))
                    (setcdr cl-p (cdr (cdr cl-p))))
                (setq cl-p (cdr cl-p)))
              (setq cl-end (1- cl-end) cl-start (1+ cl-start)))
            cl-seq)))
    (let ((cl-res (phw-delete-duplicates (append cl-seq nil)
                                         cl-test-fcn
                                         cl-start
                                         cl-end
                                         cl-from-end
                                         nil)))
      (if (stringp cl-seq) (concat cl-res) (vconcat cl-res)))))

;; (phw-delete-duplicates (vector 'a 'b 'c 'd 'A 'a 'c 'e) nil nil nil t t)

;; (phw-delete-duplicates '("a" "b" "c" "d" "A" ("a" . 0) "a" ("c" . "ewfwrew") "e")
;;                           (function (lambda (l r)
;;                                       (phw-string= (if (consp l) (car l) l)
;;                                                    (if (consp r) (car r) r))
;;                                       ))
;;                           nil nil t t)

;; (remove-duplicates '("a" "b" "c" "d" "A" ("a" . 0) "a" ("c" . "ewfwrew") "e")
;;                    :test (function (lambda (l r)
;;                                      (phw-string= (if (consp l) (car l) l)
;;                                                   (if (consp r) (car r) r))
;;                                      ))
;;                    :from-end nil)

;; stolen and adapted from cl-seq.el
(defun phw-position (cl-item cl-seq &optional cl-test-fcn cl-start cl-end cl-from-end)
  "Return the position of first occurence of CL-ITEM in CL-SEQ.
Comparison is done with `equal' unless CL-TEST-FCN is not nil: In
this case TEST-FCN will be used to compare CL-ITEM with the elements
of CL-SEQ.
Return the 0-based index of the matching item, or nil if not found."
  (let ((cl-test (or cl-test-fcn 'equal)))
    (or cl-start (setq cl-start 0))
    (if (listp cl-seq)
        (let ((cl-p (nthcdr cl-start cl-seq)))
          (or cl-end (setq cl-end 8000000))
          (let ((cl-res nil))
            (while (and cl-p (< cl-start cl-end) (or (not cl-res) cl-from-end))
              (if (funcall cl-test cl-item (car cl-p))
                  (setq cl-res cl-start))
              (setq cl-p (cdr cl-p) cl-start (1+ cl-start)))
            cl-res))
      (or cl-end (setq cl-end (length cl-seq)))
      (if cl-from-end
          (progn
            (while (and (>= (setq cl-end (1- cl-end)) cl-start)
                        (not (funcall cl-test cl-item (aref cl-seq cl-end)))))
            (and (>= cl-end cl-start) cl-end))
        (while (and (< cl-start cl-end)
                    (not (funcall cl-test cl-item (aref cl-seq cl-start))))
          (setq cl-start (1+ cl-start)))
        (and (< cl-start cl-end) cl-start)))))

;; (phw-position "v" '("a" "b" "c" "d" "A" ("a" . 0) "w" "a" ("c" . "ewfwrew") "e")
;;                  (function (lambda (l r)
;;                              (phw-string= (if (consp l) (car l) l)
;;                                           (if (consp r) (car r) r))
;;                              ))
;;                  0)
;; (phw-position "d" '("a" "b" "c" "d" "e") 'phw-string= 1 4)
;; (position "d" '("a" "b" "c" "d" "e") :test 'phw-string= :start 1 :end 3)

;; (defun phw-position (seq elem &optional test-fcn)
;;   "Return the position of ELEM within SEQ counting from 0. Comparison is done
;; with `equal' unless TEST-FCN is not nil: In this case TEST-FCN will be used to
;; compare ITEM with the elements of SEQ."
;;   (if (listp seq)
;;       (let ((pos (- (length seq) (length (phw-member elem seq test-fcn)))))
;;         (if (= pos (length seq))
;;             nil
;;           pos))
;;     (catch 'found
;;       (dotimes (i (length seq))
;;         (if (funcall (or test-fcn 'equal) elem (aref seq i))
;;             (throw 'found i)))
;;       nil)))

(defun phw-set-elt (seq n val)
  "Set VAL as new N-th element of SEQ. SEQ can be any sequence. SEQ will be
changed because this is desctructive function. SEQ is returned."
  (if (listp seq)
      (setcar (nthcdr n seq) val)
    (aset seq n val))
  seq)

(defun phw-remove-elt (seq n)
  "Remove N-th element from SEQ. SEQ can be any sequence. SEQ will be
changed because this is desctructive function. SEQ is returned."
  (delq 'phw-util-remove-marker (phw-set-elt seq n 'phw-util-remove-marker)))

(defun phw-replace-first-occurence (seq old-elem new-elem)
  "Replace in SEQ the first occurence of OLD-ELEM with NEW-ELEM. Comparison is
done by `equal'. This is desctructive function. SEQ is returned."
  (let ((pos (phw-position old-elem seq)))
    (if pos
        (phw-set-elt seq pos new-elem)))
  seq)

(defun phw-replace-all-occurences (seq old-elem new-elem)
  "Replace in SEQ all occurences of OLD-ELEM with NEW-ELEM. Comparison is
done by `equal'. This is desctructive function. SEQ is returned."
  (while (phw-position old-elem seq)
    (setq seq (phw-replace-first-occurence seq old-elem new-elem)))
  seq)

(defun phw-delete-first-occurence-from-list (list elem)
  "Replace first occurence of ELEM from LIST. Comparison is done by `equal'.
This is desctructive function. LIST is returned."
  (delq 'phw-util-remove-marker
        (phw-replace-first-occurence list elem 'phw-util-remove-marker)))

(defun phw-delete-all-occurences-from-list (list elem)
  "Replace all occurences of ELEM from LIST. Comparison is done by `equal'.
This is desctructive function. LIST is returned."
  (delq 'phw-util-remove-marker
        (progn          
          (while (phw-position elem list)
            (setq list (phw-replace-first-occurence list elem
                                                    'phw-util-remove-marker)))
          list)))

(defun phw-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end."
  (if (stringp seq) (substring seq start end)
    (let (len)
      (and end (< end 0) (setq end (+ end (setq len (length seq)))))
      (if (< start 0) (setq start (+ start (or len (setq len (length seq))))))
      (typecase seq
        (list (if (> start 0) (setq seq (nthcdr start seq)))
              (if end
                  (let ((res nil))
                    (while (>= (setq end (1- end)) start)
                      (push (pop seq) res))
                    (nreverse res))
                (copy-sequence seq)))
        (otherwise (or end (setq end (or len (length seq))))
                   (let ((res (make-vector (max (- end start) 0) nil))
                         (i 0))
                     (while (< start end)
                       (aset res i (aref seq start))
                       (setq i (1+ i) start (1+ start)))
                     res))))))

(defun phw-concatenate (type &rest seqs)
  "Concatenate, into a sequence of type TYPE, the argument SEQUENCES.
TYPE can be 'string, 'vector or 'list."
  (case type
    (vector (apply 'vconcat seqs))
    (string (apply 'concat seqs))
    (list (apply 'append (append seqs '(nil))))
    (otherwise (phw-error "Not a sequence type name: %s" type))))

(defun phw-rotate (seq start-elem)
  "Rotate SEQ so START-ELEM is the new first element of SEQ. SEQ is an
arbitrary sequence. Example: \(phw-rotate '\(a b c d e f) 'c) results in \(c d
e f a b). If START-ELEM is not contained in SEQ then nil is returned."
  (let ((start-pos (phw-position start-elem seq)))
    (when start-pos
      (phw-concatenate (typecase seq
                         (list 'list)
                         (string 'string)
                         (vector 'vector))
                       (phw-subseq seq start-pos)
                       (phw-subseq seq 0 start-pos)))))

(defun phw-last (seq)
  "Return the last elem of the sequence SEQ."
  (if (listp seq)
      (car (last seq))
    (if (> (length seq) 0)
        (aref seq (1- (length seq)))
      nil)))

(defun phw-first (seq)
  "Return the first elem of the sequence SEQ."
  (if (listp seq)
      (car seq)
    (if (> (length seq) 0)
        (aref seq 0)
      nil)))
  

(defun phw-next-listelem (list elem &optional nth-next)
  "Return that element of LIST which follows directly ELEM when ELEM is an
element of LIST. If ELEM is the last element of LIST then return the first
element of LIST. If ELEM is not an element of LIST nil is returned. Elements
are compared with `equal'.

If NTH-NEXT is an integer then the NTH-NEXT element of LIST in the meaning
described above is returned, i.e. the algorithm above is applied NTH-NEXT
times. Example: Suppose LIST = '\(a b c d), ELEM is 'c and NTH-NEXT = 3 then
'b is returned - same result for NTH-NEXT = 7, 11... It works also for
negative integers, so when NTH-NEXT is -1 in the example above then 'b is
returned."
  (let ((elem-pos (phw-position elem list))
        (next (or nth-next 1)))
    (and elem-pos
         (nth (mod (+ elem-pos next)
                   (length list))
              list))))

(defun phw-aggregate-alist (alist same-predicate sort-predicate)
  "Return ALIST as a sorted, aggregated alist.

In the result all items with the same car element (according to
SAME-PREDICATE) are aggregated together.  The alist is first sorted by
SORT-PREDICATE which is called with two items of the alist and has to return
not nil if item1 should be precede item2.

Please note: SAME-PREDICATE gets the car of an item as argument, whereas
SORT-PREDICATE gets two complete items as arguments!

Example:
\(phw-aggregate-alist
 '((a . a1) (a . a2) (b . b1) (c . c3) (a . a4) (a . a3) (b . b3) (b . b2))
 (function string=)
 (lambda (item1 item2)
   (string< (symbol-name (car item1)) (symbol-name (car item2)))))
results in
\((a a1 a2 a4 a3) (b b1 b3 b2) (c c3))"
  (when (not (null alist))
    (let (same
	  tmp-old-car
	  tmp-same
	  (first-time-p t)
	  old-car)
      (nconc
       (apply #'nconc
	      (mapcar
	       (lambda (item)
		 (cond
		  (first-time-p
		   (push (cdr item) same)
		   (setq first-time-p nil)
		   (setq old-car (car item))
		   nil)
		  ((funcall same-predicate (car item) old-car)
		   (push (cdr item) same)
		   nil)
		  (t
		   (setq tmp-same same
			 tmp-old-car old-car)
		   (setq same (list (cdr item))
			 old-car (car item))
		   (list (cons tmp-old-car (nreverse tmp-same))))))
	       (sort alist (lambda (item1 item2)
			     (funcall sort-predicate
                                      item1 item2)))))
       (list (cons old-car (nreverse same)))))))

;; test
;; (phw-aggregate-alist
;;  '((a . a1) (a . a2) (b . b1) (c . c3) (a . a4) (a . a3) (b . b3) (b . b2))
;;  'string=
;;  (lambda (item1 item2)
;;    (if (string= (car item1) (car item2))
;;        (string< (symbol-name (cdr item1)) (symbol-name (cdr item2)))
;;      (string< (car item1) (car item2)))))

(defun phw-values-of-symbol/value-list (list &optional elem-accessor)
  "Return a list of values build from the members of LIST.
The result-list is a list which is build from LIST by using the
symbol-value if a list-member is a symbol and otherwise the
list-member itself.

If ELEM-ACCESSOR is a function then it is used to get that part of an elem
of LIST for which the rule above should be applied."
  (let ((elem-acc (or elem-accessor 'identity)))
    (mapcar (function (lambda (elem)
                        (let ((e (funcall elem-acc elem)))
                          (if (symbolp e)
                              (symbol-value e)
                            e))))
            list)))

;; Maybe we should enhance this docstring ;-)
(defun phw-member-of-symbol/value-list (value list &optional elem-accessor
                                              return-accessor compare-fcn)
  "Returns not nil when VALUE is a member of that list which is build from
LIST by using the symbol-value if a list-member is a symbol and otherwise the
list-member itself. If a member then the matching elem of LIST is returned.

Per default comparison between VALUE and such a list-elem is done by `equal'
unless third optional argument COMPARE-FCN is not nil: Then this function is
used.

If ELEM-ACCESSOR is a function then it is used to get that part of an elem
of LIST for which the rule above should be applied. If RETURN-ACCESSOR is a
function then it is used to get that part of that list-elem which is equal
according to the rules above."
  (let ((elem-acc (or elem-accessor 'identity))
        (return-acc (or return-accessor 'identity))
        (cmp-fcn (or compare-fcn 'equal)))
    (catch 'exit
      (dolist (elem list)
        (let ((case-fold-search t)
              (e (funcall elem-acc elem)))
          (if (funcall cmp-fcn value (if (symbolp e)
                                         (symbol-value e)
                                       e))
              (throw 'exit (funcall return-acc elem)))
          nil)))))

;;; ----- Some regexp stuff  -------------------------------

(defsubst phw-match-regexp-list (str regexp-list &optional elem-accessor
                                     return-accessor)
  "Return not nil if STR matches one of the regexps in REGEXP-LIST. If
ELEM-ACCESSOR is a function then it is used to get the regexp from the
processed elem of REGEXP-LIST. If nil the elem itself is used. If
RETURN-ACCESSOR is a function then it is used to get the object to return from
the matching elem. If nil then the matching elem itself is returned."
  (let ((elem-acc (or elem-accessor 'identity))
        (return-acc (or return-accessor 'identity)))
    (catch 'exit
      (dolist (elem regexp-list)
        (let ((case-fold-search t))
          (save-match-data
            (if (string-match (funcall elem-acc elem) str)
                (throw 'exit (funcall return-acc elem))))
          nil)))))

;;; ----- User-interaction ---------------------------------

(defun phw-confirm (text)
  (yes-or-no-p text))

;; stolen from query.el and slightly enhanced
;; This is for a small number of choices each of them a short string
(defun phw-query-string (prompt choices &optional other-prompt)
  "Prints PROMPT and returns a string which must be one of CHOICES.
CHOICES is either a list of strings whereas the first choice is the default
\(which is returned if the user simply types RET) or nil \(then only a simple
RET quits the query and returns nil). If OTHER-PROMPT is not nil and a string
then the choice \"other\" is added to CHOICES and after selecting this choice
the user is prompted with OTHER-PROMPT to insert any arbitrary string."
  (let* ((new-choices (if other-prompt
                          ;; Emacs 20.X add-to-list can not append at the end
                          (append choices (list "other"))
                        choices))
         (default (car new-choices))
         answer)
    (setq prompt (concat prompt
                         " ["
                         (if new-choices
                             (mapconcat (function (lambda (x) x))
                                        new-choices ", ")
                           "RET")
                         "] "))
    (setq new-choices (nconc (mapcar (function (lambda (x) (list x t)))
                                     new-choices)
                             '('("" t))))
    (setq answer (completing-read prompt new-choices nil t))
    (cond ((phw-string= answer "")
           (setq answer default))
          ((phw-string= answer "other")
           (setq answer (read-string (concat other-prompt ": ")))))
    answer))

;; This is for any number of string-choices without any length restriction -
;; see also `phw-query-string'.
(defun phw-offer-choices (prompt choices)
  "Prints PROMPT and returns a string which must be one of CHOICES.
CHOICES is a list of strings whereas the first choice is the default. All
choices are immediately displayed as if completion does it so a selection can
be made either with the mouse or with the keyboard."
  (let* ((minibuffer-setup-hook
          (append minibuffer-setup-hook
                  (list (lambda ()
                          (with-output-to-temp-buffer "*Completions*"
                            (display-completion-list (all-completions "" minibuffer-completion-table)))))))
         (completion-list (mapcar (function (lambda (x) (list x t)))
                                  choices))
         (answer (completing-read prompt
                                  completion-list
                                  nil t
                                  (try-completion "" completion-list))))
    (if (phw-string= answer "")
        (car choices)
      answer)))


;; phw-offer-choices-1 and phw-offer-choices-2 are two other approaches for
;; phw-offer-choices - but IMHO not as good and clean as the current one and
;; therefore not used in PHW
;; (defun phw-offer-choices-1 (prompt choices)
;;   "Prints PROMPT and returns a string which must be one of CHOICES.
;; CHOICES is a list of strings whereas the first choice is the default. All
;; choices are immediately displayed as if completion does it so a selection can
;; be made either with the mouse or with the keyboard."
;;   (let* ((minibuffer-setup-hook
;;           (append minibuffer-setup-hook
;;                   '(minibuffer-complete
;;                     minibuffer-complete
;;                     minibuffer-complete)))
;;          (answer (completing-read
;;                   prompt
;;                   (mapcar (function (lambda (x) (list x t)))
;;                           choices)
;;                   nil t)))
;;     (if (phw-string= answer "")
;;         (car choices)
;;       answer)))

;; (defun phw-offer-choices-2 (prompt choices)
;;   "Prints PROMPT and returns a string which must be one of CHOICES.
;; CHOICES is a list of strings whereas the first choice is the default. All
;; choices are immediately displayed as if completion does it so a selection can
;; be made either with the mouse or with the keyboard."
;;   ;; First we create a TAB-event
;;   (let ((event (if phw-running-xemacs
;;                    (make-event 'key-press '(key tab))
;;                  9)))
;;     ;; With these 3 TAB-events we ensure that
;;     ;; 1. The longest possible common substring is display in the minibuffer
;;     ;; 2. All possible completions are displayed
;;     (dotimes (i 3)
;;       (setq unread-command-events (cons event unread-command-events))))
;;   (let ((answer (completing-read
;;                  prompt
;;                  (mapcar (function (lambda (x) (list x t)))
;;                          choices)
;;                  nil t)))
;;     (if (phw-string= answer "")
;;         (car choices)
;;       answer)))

(defun phw-read-number (prompt &optional init-value)
  "Ask in the minibuffer for a number with prompt-string PROMPT. Optional
INIT-VALUE can be either a number or a string-representation of a number."
  (let ((init (typecase init-value
                (number (number-to-string init-value))
                (string
                 (if (phw-string= init-value "0")
                     init-value
                   (if (not (= 0 (string-to-number init-value)))
                       init-value
                     (phw-error "phw-read-number: init-value not a valid number!"))))
                (otherwise nil)))
        result)
    (while (progn
             (setq result (read-string prompt init))
             (not (or (phw-string= "0" result)
                      (not (= 0 (string-to-number result)))))))
    (string-to-number result)))

(defun phw-message-box (message-str &optional title-text button-text)
  "Display a message-box with message MESSAGE-STR and title TITLE-TEXT if
TITLE-TEXT is not nil - otherwise \"Message-box\" is used as title. The title
gets always the prefix \"PHW - \". Second optional argument BUTTON-TEXT
specifies the text of the message-box button; if nil then \"OK\" is used.

Remark: BUTTON-TEXT is currently only used with XEmacs. With GNU Emacs the
message itself is the button because GNU Emacs currently does not support
dialog-boxes very well.

If `window-system' is nil then a simple message is displayed in the echo-area."
  (let ((button (if (stringp button-text)
                    button-text
                  "OK"))
        (title (concat "PHW"
                       (if (stringp title-text)
                           (concat " - " title-text)
                         " Message"))))
    (if window-system
        (progn
          (if phw-running-xemacs
              (make-dialog-box 'question
                               :title title
                               :modal t
                               :question message-str
                               :buttons (list (vector button '(identity nil) t)))
            (x-popup-dialog t (list title (cons message-str t))))
          t)
      (message (concat title " " message-str)))))

;; some first approaches to display informations in a temp-window

;; (defvar phw-window-config-before-msg-display nil)

;; (defun phw-display-temp-message-1 (msg-title msg-content)
;;   (require 'wid-edit)
;;   (setq phw-window-config-before-msg-display
;;         (phw-current-window-configuration))
;;   (with-current-buffer (get-buffer-create msg-title)
;;     (switch-to-buffer-other-window (current-buffer))
;;     (kill-all-local-variables)
;;     (let ((inhibit-read-only t))
;;       (erase-buffer))
;;     (widget-insert msg-content)
;;     (widget-insert "\n\n")
;;     ;; Insert the Save button
;;     (widget-create 'push-button
;;                    :button-keymap phw-upgrade-button-keymap ; XEmacs
;;                    :keymap phw-upgrade-button-keymap ; Emacs
;;                    :notify (lambda (&rest ignore)
;;                              (when phw-window-config-before-msg-display
;;                                (ignore-errors
;;                                  (phw-set-window-configuration
;;                                   phw-window-config-before-msg-display))
;;                                (setq phw-window-config-before-msg-display nil)))
;;                    "OK")
;;     (widget-setup)
;;     (goto-char (point-min))))


;; (defun phw-display-temp-message-2 (msg-title msg-content)
;;   (require 'wid-edit)
;;   (setq phw-window-config-before-msg-display
;;         (phw-current-window-configuration))
;;   (with-output-to-temp-buffer msg-title
;;     (widget-insert msg-content)
;;     (widget-insert "\n\n")
;;     ;; Insert the Save button
;;     (widget-create 'push-button
;;                    :button-keymap phw-upgrade-button-keymap ; XEmacs
;;                    :keymap phw-upgrade-button-keymap ; Emacs
;;                    :notify (lambda (&rest ignore)
;;                              (when phw-window-config-before-msg-display
;;                                (ignore-errors
;;                                  (phw-set-window-configuration
;;                                   phw-window-config-before-msg-display))
;;                                (setq phw-window-config-before-msg-display nil)))
;;                    "OK")
;;     (widget-setup)
;;     (goto-char (point-min))))

;; (defvar phw-user-information-msg-buffer nil)

;; (defun phw-display-temp-message (msg-content)
;;   (require 'wid-edit)
;;   (progn
;;     (setq phw-user-information-msg-buffer
;;           (get-buffer-create "*PHW User-Information*"))
;;     (cond
;;      ((not (get-buffer-window phw-user-information-msg-buffer))
;;       (let ((split-window-keep-point nil)
;;             (window-min-height 2))
;;         ;; maybe leave two lines for our window because of the normal
;;         ;; `raised' modeline in Emacs 21
;;         ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: adjust this for Xemacs!
;;         (select-window
;;          (split-window-vertically
;;           (if (and (fboundp 'face-attr-construct)
;;                    (plist-get (face-attr-construct 'modeline) :box))
;;               -3 -2)))
;;         (switch-to-buffer phw-user-information-msg-buffer)))
;;      ((not (eq (current-buffer) phw-user-information-msg-buffer))
;;       (select-window (get-buffer-window phw-user-information-msg-buffer))))
;;     ;; insert now the msg-content
;;     (let ((inhibit-read-only t))
;;       (erase-buffer))
;;     (widget-insert msg-content)
;;     (widget-insert "\n\n")
;;     ;; Insert the Save button
;;     (widget-create 'push-button
;;                    :button-keymap phw-upgrade-button-keymap ; XEmacs
;;                    :keymap phw-upgrade-button-keymap ; Emacs
;;                    :notify (lambda (&rest ignore)
;;                              (set-buffer phw-user-information-msg-buffer)
;;                              (condition-case nil
;;                                  (while (get-buffer-window phw-user-information-msg-buffer)
;;                                    (delete-window (get-buffer-window phw-user-information-msg-buffer)))
;;                                (error nil))
;;                              (kill-buffer phw-user-information-msg-buffer)
;;                              (setq phw-user-information-msg-buffer nil))
                             
;;                    "OK")
;;     (widget-setup)
;;     ;; (setq buffer-read-only t)
;;     (message "Click [OK] or hit q for restoring previous window-layout.")))

;; ----- Information-display - errors, warnings, infos ----

(defun phw-nolog-message (&rest args)
  "Works exactly like `message' but does not log the message"
  (let ((msg (cond ((or (null args)
                        (null (car args)))
                    nil)
                   ((null (cdr args))
                    (car args))
                   (t
                    (apply 'format args)))))
    ;; Now message is either nil or the formated string.
    (if phw-running-xemacs
        ;; XEmacs way of preventing log messages.
        (if msg
            (display-message 'no-log msg)
          (clear-message 'no-log))
      ;; Emacs way of preventing log messages.
      (let ((message-log-max nil)
            (message-truncate-lines nil))
        (if msg
            (message "%s" msg)
          (message nil))))
    msg))

(defun phw-error (&rest args)
  "Signals an error but prevents it from entering the debugger. This is
useful if an error-message should be signaled to the user and evaluating
should stopped but no debugging is senseful."
  (let ((debug-on-error nil))
    (error (concat "PHW - Error: " (apply 'format args)))))

(defun phw-warning (&rest args)
  "Displays a warning."
  (message (concat "PHW - Warning: " (apply 'format args))))

(defun phw-info-message (&rest args)
  "Displays an information."
  (message (concat "PHW - Info: " (apply 'format args))))

;;; ----- Text and string-stuff ----------------------------

(defun phw-merge-face (face start end &optional text)
  "Merge FACE either to a buffer-part or to TEXT.
In both cases START and END define the region which should be
faced. The FACE is merged, i.e. the values of all face-attributes
of FACE take effect and the values of all face-attributes of the
buffer-part or TEXT which are not set by FACE are preserved.

If always returns TEXT \(if not nil then modified with FACE)."
  (if (null face)
      text
    (if phw-running-xemacs
        (put-text-property start end 'face
                           (let* ((current-face (get-text-property 0
                                                                   'face
                                                                   text))
                                  (cf
                                   (typecase current-face
                                     (phw-face (list current-face))
                                     (list current-face)
                                     (otherwise nil)))
                                  (nf
                                   (typecase face
                                     (phw-face (list face))
                                     (list face)
                                     (otherwise nil))))
                             ;; we must add the new-face in front of
                             ;; current-face to get the right merge!
                             (if (member face cf)
                                 cf
                               (append nf cf)
                               )
                             )
                           text)
      (alter-text-property start end 'face
                           (lambda (current-face)
                             (let ((cf
                                    (typecase current-face
                                      (phw-face (list current-face))
                                      (list current-face)
                                      (otherwise nil)))
                                   (nf
                                    (typecase face
                                      (phw-face (list face))
                                      (list face)
                                      (otherwise nil))))
                               ;; we must add the new-face in front of
                               ;; current-face to get the right merge!
                               (if (member face cf)
                                   cf
                                 (append nf cf))))
                           text))
    text))

(defun phw-merge-face-into-text (text face)
  "Merge FACE to the already precolored TEXT so the values of all
face-attributes of FACE take effect and but the values of all face-attributes
of TEXT which are not set by FACE are preserved.
If FACE or TEXT is nil then simply original TEXT is returned."
  (if (or (null face) (null text))
      text
    (phw-merge-face face 0 (length text) text)))

(if (fboundp 'compare-strings)
    (defalias 'phw-compare-strings 'compare-strings)
  (defun phw-compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
    "Compare the contents of two strings.
In string STR1, skip the first START1 characters and stop at END1.
In string STR2, skip the first START2 characters and stop at END2.
END1 and END2 default to the full lengths of the respective strings.

Case is significant in this comparison if IGNORE-CASE is nil.

The value is t if the strings (or specified portions) match.
If string STR1 is less, the value is a negative number N;
  - 1 - N is the number of characters that match at the beginning.
If string STR1 is greater, the value is a positive number N;
  N - 1 is the number of characters that match at the beginning."
    (or start1 (setq start1 0))
    (or start2 (setq start2 0))
    (setq end1 (if end1
                   (min end1 (length str1))
                 (length str1)))
    (setq end2 (if end2
                   (min end2 (length str2))
                 (length str2)))
    (let ((i1 start1)
          (i2 start2)
          result c1 c2)
      (while (and (not result) (< i1 end1) (< i2 end2))
        (setq c1 (aref str1 i1)
              c2 (aref str2 i2)
              i1 (1+ i1)
              i2 (1+ i2))
        (if ignore-case
            (setq c1 (upcase c1)
                  c2 (upcase c2)))
        (setq result (cond ((< c1 c2) (- i1))
                           ((> c1 c2) i1))))
      (or result
          (cond ((< i1 end1) (1+ (- i1 start1)))
                ((< i2 end2) (1- (- start1 i1)))
                (t)))
      )))

(defsubst phw-string= (str1 str2 &optional ignore-case)
  (let ((s1 (or (and (stringp str1) str1) (symbol-name str1)))
        (s2 (or (and (stringp str2) str2) (symbol-name str2))))
    (eq (phw-compare-strings s1 nil nil s2 nil nil ignore-case) t)))

(defsubst phw-string< (str1 str2 &optional ignore-case)
  (let ((s1 (or (and (stringp str1) str1) (symbol-name str1)))
        (s2 (or (and (stringp str2) str2) (symbol-name str2)))
        (result nil))
    (setq result (phw-compare-strings s1 nil nil s2 nil nil ignore-case))
    (and (numberp result) (< result 0))))

(defun phw-excessive-trim (str)
  "Return a string where all double-and-more whitespaces in STR are replaced
with a single space-character."
  (let ((s str))
    (save-match-data
      (while (string-match "[ \t][ \t]+" s)
        (setq s (concat (substring s 0 (match-beginning 0))
                        " "
                        (substring s (match-end 0))))))
    s))

;; Klaus Berndl <klaus.berndl@sdm.de>: we have to take account that GNU Emacs
;; > 21.3 has changed its split-string function! For the new split-string is
;; (cdr (split-string ...)) not nil (at least in our context below), for GNU
;; Emacs <= 21.3 nil!
(defun phw-left-trim (str)
  "Return a string stripped of all leading whitespaces of STR."
  (let ((split-result (split-string str "^[\n\t ]*")))
    (or (or (and (cdr split-result) ;; GNU Emacs > 21.3
                 (car (cdr split-result)))
            (car split-result))
        "")))

(defun phw-right-trim (str)
  "Return a string stripped of all trailing whitespaces of STR."
  (or (car (split-string str "[\n\t ]*$")) ""))

(defun phw-trim (str)
  "Applies `phw-right-trim' and `phw-left-trim' to STR."
  (phw-left-trim (phw-right-trim str)))

(defun phw-full-trim (str)
  "Applies `phw-trim' and `phw-middle-trim' to STR."
  (phw-excessive-trim (phw-trim str)))

(defun phw-fit-str-to-width (str width from)
  "If STR is longer than WIDTH then fit it to WIDTH by stripping from left or
right \(depends on FROM which can be 'left or 'right) and prepend \(rsp.
append) \"...\" to signalize that the string is stripped. If WIDTH >= length
of STR the always STR is returned. If either WIDTH or length of STR is < 5
then an empty string is returned because stripping makes no sense here."
  (let ((len-str (length str)))
    (if (>= width len-str)
        str
      (if (or (< len-str 5) ;; we want at least two characters visible of str
              (< width 5))
          ""
        (if (equal from 'left)
            (concat "..." (substring str (* -1 (- width 3))))
          (concat (substring str 0 (- width 3)) "..."))))))

;;; ----- Number-stuff -------------------------------------

(defun phw-normalize-number (value &optional ref-value)
  "Normalize VALUE in the following manner and return:
* VALUE > -1.0 and < +1.0 and REF-VALUE a number: `floor' of VALUE * REF-VALUE
* all other cases: `floor' of VALUE"
  (floor (if (and (< value 1.0)
                  (> value -1.0)
                  (numberp ref-value))
             (* ref-value value)
           value)))


;;; ----- Working-display ----------------------------------

;; code for a working display - complete stolen from the semantic-package.
;; PHW has thrown away all code which is not needed by PHW
;; The original code is written by Eric M. Ludlam <zappo@gnu.org>


;; Variables used in stages
(defvar phw-working-message nil
  "Message stored when in a status loop.")
(defvar phw-working-donestring nil
  "Done string stored when in a status loop.")
(defvar phw-working-ref1 nil
  "A reference number used in a status loop.")
(defvar phw-working-last-percent 0
  "A reference number used in a status loop.")

(defun phw-working-frame-animation-display (length number frames)
  "Manage a simple frame-based animation for working functions.
LENGTH is the number of characters left.  NUMBER is a passed in
number (which happens to be ignored.).  While coders pass t into
NUMBER, functions using this should convert NUMBER into a vector
describing how to render the done message.
Argument FRAMES are the frames used in the animation."
  (typecase number
    (vector
     (let ((zone (- (length (aref frames 0)) (length (aref number 0))
                    (length (aref number 1)))))
       (if (< (length phw-working-donestring) zone)
           (concat " " (aref number 0)
                   (make-string
                    (ceiling (/ (- (float zone)
                                   (length phw-working-donestring)) 2)) ? )
                   phw-working-donestring
                   (make-string
                    (floor (/ (- (float zone)
                                 (length phw-working-donestring)) 2)) ? )
                   (aref number 1))
         (concat " " (aref frames (% phw-working-ref1 (length frames)))
                 " " phw-working-donestring))))
    (otherwise
     (concat " " (aref frames (% phw-working-ref1 (length frames)))))))

(defvar phw-working-celeron-strings
  [ "[O     ]" "[oO    ]" "[-oO   ]" "[ -oO  ]" "[  -oO ]" "[   -oO]"
    "[    -O]" "[     O]" "[    Oo]" "[   Oo-]"  "[  Oo- ]" "[ Oo-  ]"
    "[Oo-   ]" "[O-    ]"]
  "Strings representing a silly celeron.")

(defun phw-working-celeron-display (length number)
  "Return a string displaying a celeron as things happen.
LENGTH is the amount of display that has been used.  NUMBER
is t to display the done string, or the number to display."
  (case number
    ((t)
     (phw-working-frame-animation-display length [ "[" "]" ]
					  phw-working-celeron-strings))
    ;; All the % signs because it then gets passed to message.
    (otherwise
     (phw-working-frame-animation-display length number
                                          phw-working-celeron-strings))))



(defun phw-working-dynamic-status (&optional number)
  "show the status. If NUMBER is nil, then increment a local NUMBER from 0
with each call. If it is a number or float, use it as the raw percentile."
  (let* ((n (or number phw-working-ref1))
         (m1 (funcall 'format phw-working-message))
         (m2 (phw-working-celeron-display (length m1) n)))
    (phw-nolog-message "%s%s" m1 m2)
    (setq phw-working-ref1 (1+ phw-working-ref1))))

(defmacro phw-working-status-timeout (timeout message donestr &rest forms)
  "Contain a block of code during which working status is shown.
The code may call `sit-for' or `accept-process-output', so a timer
is needed to update the message.
TIMEOUT is the length of time to wait between message updates.
MESSAGE is the message string to use and DONESTR is the completed text
to use when the functions `phw-working-status' is called from FORMS."
  (let ((current-message (make-symbol "phw-working-current-message")))
    `(let* ((,current-message (current-message))
            (phw-working-message ,message)
            (phw-working-donestring ,donestr)
            (phw-working-ref1 0)
            (time ,timeout)
            (phw-working-timer
             (phw-run-with-timer time time 'phw-working-dynamic-status)))
       (unwind-protect
           (progn ,@forms)
         (phw-cancel-timer phw-working-timer)
         (phw-working-dynamic-status t)
         (message ,current-message)))))


(defun phw-working-status-call-process
  (timeout message donestr program &optional infile buffer display &rest args)
  "Display working messages while running a process.
TIMEOUT is how fast to display the messages.
MESSAGE is the message to show, and DONESTR is the string to add when done.
CALLPROCESSARGS are the same style of args as passed to `call-process'.
The are: PROGRAM, INFILE, BUFFER, DISPLAY, and ARGS.
Since it actually calls `start-process', not all features will work.
It returns the exit-status of the called PROGRAM."
  (phw-working-status-timeout timeout message donestr
    (let* ((process-environment (cons "LC_ALL=C" process-environment))
           (proc (apply 'start-process "phw-working"
                        (if (listp buffer) (car buffer) buffer)
                        program args)))
      (set-process-sentinel proc 'list)
      (while (eq (process-status proc) 'run)
	(accept-process-output proc)
	;; accept-process-output caused my Solaris Emacs 20.3 to crash.
	;; If this is unreliable for you, use the below which will work
	;; in that situation.
	;; (if (not (sit-for timeout)) (read-event))
	)
      (process-exit-status proc))))

;;; ----- Buffers and files --------------------------------

(defsubst phw-current-line ()
  "Return the current line-number - the first line in a buffer has number 1."
  (+ (count-lines 1 (point)) (if (= (current-column) 0) 1 0)))

(defun phw-goto-line (line)
  "Goto LINE, counting from line 1 at beginning of buffer.

This function doesn't set the mark."
  ;; Move to the specified line number in that buffer.
  (save-restriction
    (widen)
    (goto-char 1)
    (if (eq selective-display t)
        (re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))))

(defmacro phw-with-readonly-buffer (buffer &rest body)
  "Make buffer BUFFER current but do not display it. Evaluate BODY in buffer
BUFFER \(not read-only an evaluation-time of BODY) and make afterwards BUFFER
read-only. Note: All this is done with `save-excursion' so after BODY that
buffer is current which was it before calling this macro."
  `(if (buffer-live-p ,buffer)
       (with-current-buffer ,buffer
         (unwind-protect
             (progn
               (setq buffer-read-only nil)
               ,@body)
           (setq buffer-read-only t)))
     (phw-error "Try to set a not existing buffer.")))

(put 'phw-with-readonly-buffer 'lisp-indent-function 1)

(defmacro phw-do-if-buffer-visible-in-phw-frame (buffer-name-symbol &rest body)
  "Evaluate BODY if a buffer is visible in the PHW-frame.

This means in fact if the following conditions are all true:
- The symbol BUFFER-NAME-SYMBOL is bound
- The value of BUFFER-NAME-SYMBOL is a name of a living buffer B
- The buffer B is visible and displayed in a window of the `phw-frame'
- PHW is active
- The current frame is the `phw-frame'
- The window of buffer B is not a window in the edit-area.
If one of these conditions is false then nothing will be done.

During the evaluation of BODY the following local variables are bound:
- visible-buffer: The buffer-object which name is the value of
  BUFFER-NAME-SYMBOL.
- visible-window: The window which displays visible-buffer"
  `(let* ((visible-buffer (if (and (boundp ,buffer-name-symbol)
                                   (stringp (symbol-value ,buffer-name-symbol)))
                              (get-buffer (symbol-value ,buffer-name-symbol))))
          (visible-window (if (bufferp visible-buffer)
                              (get-buffer-window visible-buffer))))
     (when (and phw-minor-mode
                (equal (selected-frame) phw-frame)
                visible-window
                (window-live-p visible-window)
                (not (member visible-window (phw-canonical-edit-windows-list))))
       ,@body)))
(put 'phw-do-if-buffer-visible-in-phw-frame 'lisp-indent-function 1)

(defun phw-buffer-substring (minpoint maxpoint &optional buffer)
  "Return the contents of part of BUFFER as a string.
If BUFFER is nil then the current-buffer is used. BUFFER can be a buffer-name
or a buffer-object."
  (with-current-buffer (or buffer (current-buffer))
    (buffer-substring minpoint maxpoint)))

(defun phw-delete-file (file)
  "Delete FILE if it eexists."
  (let ((exp-file (expand-file-name file)))
    (if (file-exists-p exp-file)
        (delete-file exp-file))))

(defun phw-buffer-name (buffer-or-window)
  "Return the buffer-name of BUFFER-OR-WINDOW.
BUFFER-OR-WINDOW can be a buffer-name, a buffer or a window. If a
window then the name of the buffer curently displayed in this
window is returned."
  (typecase buffer-or-window
    (string buffer-or-window)
    (buffer (buffer-name buffer-or-window))
    (window (buffer-name (window-buffer buffer-or-window)))
    (otherwise nil)))

(defun phw-buffer-obj (buffer-or-window)
  "Return the buffer-object of BUFFER-OR-WINDOW.
BUFFER-OR-WINDOW can be a buffer-name, a buffer or a window.
If a window then the buffer curently displayed in this window is returned."
  (typecase buffer-or-window
    (string (get-buffer buffer-or-window))
    (buffer buffer-or-window)
    (window (window-buffer buffer-or-window))
    (otherwise nil)))

(defun phw-buffer-local-value (sym buffer)
  "Get the buffer-local value of variable SYM in BUFFER. If there is no
buffer-local value in BUFFER then the global value of SYM is used."
  (if (fboundp 'buffer-local-value)
      (buffer-local-value sym buffer)
    (when phw-running-xemacs
      (symbol-value-in-buffer sym buffer))))
;;     (or (cdr (assoc sym (buffer-local-variables buffer)))
;;         (save-excursion
;;           (set-buffer buffer)
;;           (symbol-value sym)))))


(defun phw-file-content-as-string (file)
  "If FILE exists and is readable returns the contents as a string otherwise
return nil.
Note: No major/minor-mode is activated and no local variables are evaluated
for FILE, but proper EOL-conversion and character interpretation is done!"
  (let ((exp-filename (expand-file-name file)))
    (if (and (file-exists-p exp-filename)
             (file-readable-p exp-filename))
        (with-temp-buffer
          (insert-file-contents exp-filename)
          (buffer-string)))))

(defun phw-current-buffer-archive-extract-p ()
  "Return not nil if current buffer was extracted of an archive which is in
`tar-mode' or `archive-mode'. For this the current buffer has either to be in
minor-mode `tar-subfile-mode' or `archive-subfile-mode'."
  (or (and (boundp 'tar-subfile-mode)
           tar-subfile-mode)
      (and (boundp 'archive-subfile-mode)
           archive-subfile-mode)))

(defun phw-buffer-file-name (&optional buffer no-indirect-buffers)
  "Return filename of file represented by BUFFER.
BUFFER can also be an indirect buffer - if its base buffer points to a file
then this filename is returned.
BUFFER can be a buffer-object or a buffer-name.
If BUFFER is nil then current buffer is used.
If NO-INDIRECT-BUFFERS is not nil then for indirect buffers always nil is
returned."
  (or (buffer-file-name buffer)
      (and (not no-indirect-buffers)
           (buffer-base-buffer buffer)
           (buffer-file-name (buffer-base-buffer buffer)))))


(defun phw-buffer-or-file-readable-p (&optional filename)
  "Checks if a buffer or a file is a readable file in the sense of PHW which
means either a real physical file or an auto-extracted file from an archive.
See `phw-current-buffer-archive-extract-p'. FILENAME is either a filename or
nil whereas in the latter case the current-buffer is assumed."
  (let* ((file (or filename (phw-buffer-file-name (current-buffer)))))
    (or (and file (file-readable-p file))
        (and (not phw-running-xemacs)
             (if filename
                 (with-current-buffer (find-file-noselect filename)
                   (phw-current-buffer-archive-extract-p))
               (phw-current-buffer-archive-extract-p))))))


;;; ----- Windows ------------------------------------------

;; Emacs 20 has no window-list function and the XEmacs and Emacs 21 one has no
;; specified ordering. The following one is stolen from XEmacs and has fixed
;; this lack of a well defined order. We preserve also point of current
;; buffer! IMPORTANT: When the window-ordering is important then currently
;; these function should only be used with WINDOW = (frame-first-window
;; phw-frame)!
(defun phw-window-list (&optional frame minibuf window)
  "Return a list of windows on FRAME, beginning with WINDOW. The
windows-objects in the result-list are in the same canonical windows-ordering
of `next-window'. If omitted, WINDOW defaults to the selected window. FRAME and
WINDOW default to the selected ones. Optional second arg MINIBUF t means count
the minibuffer window even if not active. If MINIBUF is neither t nor nil it
means not to count the minibuffer even if it is active."
  ;; At least under XEmacs 21.5 there's a problem with the advice on
  ;; current-window-configuration -- that advice calls
  ;; phw-window-configuration-data, which in turn involves phw-windows-list,
  ;; which uses save-windows-excursion, which in 21.5-b28 is. . . a macro
  ;; which uses current-window-configuration!
  ;; To avoid this we run the body of this function with deactivated basic
  ;; advices of phw.
   (if (not phw-running-xemacs)
       ;; Klaus Berndl <klaus.berndl@sdm.de>: There seems to be mysterious
       ;; behavior when running our own window-list version with GNU Emacs >=
       ;; 21.3 - especially when running an igrep when the igrep-buffer is
       ;; already in another window. We can here savely use the function
       ;; `window-list' because it returns an ordered list
       (window-list frame minibuf window)
     ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: the following is needed for
     ;; XEmacs - but the best would be if we would not need
     ;; implementing window-list, means the best would be if window-list
     ;; returns an ordered list!
     (phw-with-original-basic-functions
      (setq window (or window (selected-window))
            frame (or frame (selected-frame)))
      (if (not (eq (window-frame window) frame))
          (error "Window must be on frame."))
      (let ((current-frame (selected-frame))
            (current-window (selected-window))
            (current-buf (current-buffer))
            (current-point (point))
            list)
        (unwind-protect
            (progn ;;save-window-excursion
              (select-frame frame)
              ;; this is needed for correct start-point
              (select-window window)
              (walk-windows
               (function (lambda (cur-window)
                           (if (not (eq window cur-window))
                               (setq list (cons cur-window list)))))
               minibuf
               'selected)
              ;; This is needed to get the right canonical windows-order, i.e. the
              ;; same order of windows than `walk-windows' walks through!
              (setq list (nreverse list))
              (setq list (cons window list)))
          (select-frame current-frame)
          (select-window current-window)
          (set-buffer current-buf)
          ;; we must reset the point of the buffer which was current at call-time
          ;; of this function
          (goto-char current-point))))))

(defun phw-canonical-windows-list ()
  "Return a list of all current visible windows in the `phw-frame' \(starting
from the left-most top-most window) in the order `next-window' would walk
through these windows."
  (phw-window-list phw-frame 0 (frame-first-window phw-frame)))

(defun phw-window-live-p (buffer-or-name)
  "Return not nil if buffer BUFFER-OR-NAME is displayed in an active window."
  (and buffer-or-name (window-live-p (get-buffer-window buffer-or-name))))

(defun phw-enlarge-window (window &optional val)
  "Enlarge the given window.
If VAL is nil then WINDOW is enlarged so that it is 1/2 of the current frame.
If VAL is a positive integer then WINDOW is enlarged so that its new height is
VAL lines. If VAL is > 0 and < 1 then WINDOW is enlarged so that its new
height is that fraction of the frame."
  (if (and window (window-live-p window))
      (let* ((norm-val (if val
                           (phw-normalize-number val (1- (frame-height)))
                         (/ (1- (frame-height)) 2)))
             (enlargement (- norm-val (phw-window-full-height window))))
        (save-selected-window
          (select-window window)          
          (if (> enlargement 0)
              (enlarge-window enlargement))))
    (error "Window is not alive!")))

(defun phw-window-safely-shrinkable-p (&optional window)
  "Non-nil if the WINDOW can be shrunk without shrinking other windows.
If WINDOW is nil or omitted, it defaults to the currently selected window."
  (with-selected-window (or window (selected-window))
    (let ((edges (phw-window-edges)))
      (or (= (nth 2 edges) (nth 2 (phw-window-edges (previous-window))))
	  (= (nth 0 edges) (nth 0 (phw-window-edges (next-window))))))))

(defun phw-fit-window-to-buffer (&optional window max-height min-height)
  "Make WINDOW the right height to display its contents exactly.
If WINDOW is omitted or nil, it defaults to the selected window.
If the optional argument MAX-HEIGHT is supplied, it is the maximum height
  the window is allowed to be, defaulting to the frame height.
If the optional argument MIN-HEIGHT is supplied, it is the minimum
  height the window is allowed to be, defaulting to `window-min-height'.

MAX-HEIGHT and MIN-HEIGHT can be also afraction between 0 and 1: then this is
interpreted as that fraction of the frame-height of WINDOW \(or the selected
window if WINDOW is nil).

The heights in MAX-HEIGHT and MIN-HEIGHT include the mode-line and/or
header-line."
  (interactive)

  (when (null window)
    (setq window (selected-window)))
  (when (null max-height)
    (setq max-height (frame-height (window-frame window))))

  (let* ((buf
	  ;; Buffer that is displayed in WINDOW
	  (window-buffer window))
	 (window-height
	  ;; The current height of WINDOW
	  (phw-window-full-height window)) ;; KB: was window-height
         (max-height-norm (phw-normalize-number max-height
                                                (frame-height (window-frame window))))
         (min-height-norm (and min-height
                               (phw-normalize-number min-height
                                                     (frame-height (window-frame window)))))
	 (desired-height
	  ;; The height necessary to show the buffer displayed by WINDOW
	  ;; (`count-screen-lines' always works on the current buffer).
	  (with-current-buffer buf
	    (+ (phw-count-screen-lines)
	       ;; If the buffer is empty, (count-screen-lines) is
	       ;; zero.  But, even in that case, we need one text line
	       ;; for cursor.
	       (if (= (point-min) (point-max))
		   1 0)
	       ;; For non-minibuffers, count the mode-line, if any
	       (if (and (not (window-minibuffer-p window))
			mode-line-format)
		   1 0)
	       ;; Count the header-line, if any
               (if phw-running-xemacs
                   0
                 (if header-line-format 1 0)))))
	 (delta
	  ;; Calculate how much the window height has to change to show
	  ;; desired-height lines, constrained by MIN-HEIGHT and MAX-HEIGHT.
	  (- (max (min desired-height max-height-norm)
		  (or min-height-norm window-min-height))
	     window-height))
	 ;; We do our own height checking, so avoid any restrictions due to
	 ;; window-min-height.
	 (window-min-height 1))

    ;; Don't try to redisplay with the cursor at the end
    ;; on its own line--that would force a scroll and spoil things.
    (when (with-current-buffer buf
	    (and (phw-eobp) (phw-bolp) (not (phw-bobp))))
      (set-window-point window (1- (window-point window))))

    (save-selected-window
      (select-window window)

      ;; Adjust WINDOW to the nominally correct size (which may actually
      ;; be slightly off because of variable height text, etc).
      (unless (zerop delta)
	(enlarge-window delta))

      ;; Check if the last line is surely fully visible.  If not,
      ;; enlarge the window.
      (let ((end (with-current-buffer buf
		   (save-excursion
		     (goto-char (point-max))
		     (when (and (phw-bolp) (not (phw-bobp)))
		       ;; Don't include final newline
		       (backward-char 1))
		     (when truncate-lines
		       ;; If line-wrapping is turned off, test the
		       ;; beginning of the last line for visibility
		       ;; instead of the end, as the end of the line
		       ;; could be invisible by virtue of extending past
		       ;; the edge of the window.
		       (forward-line 0))
		     (point)))))
        (unless phw-running-xemacs
          (set-window-vscroll window 0))
	(while (and (< desired-height max-height-norm)
		    (= desired-height (window-height window))
		    (not (pos-visible-in-window-p end window)))
	  (enlarge-window 1)
	  (setq desired-height (1+ desired-height)))))))

(defun phw-test-fit-window-to-buffer ()
  (interactive)
  (phw-fit-window-to-buffer
   (selected-window)
   (if (functionp temp-buffer-max-height)
       (funcall temp-buffer-max-height (current-buffer))
     temp-buffer-max-height)))

(defun phw-scroll-window (point window-start)
  "Scrolls window of current buffer. The window will start at WINDOW-START and
point will stay on POINT."
  (goto-char point)
  (set-window-start (get-buffer-window (current-buffer)) window-start))

(defun phw-window-select (buffer-or-name)
  "Select that window which displays in the `phw-frame' the buffer
BUFFER-OR-NAME which can be either a buffer-object or a buffer-name. Return
the window-object. If that buffer is not displayed in the `phw-frame' then
nothing happens and nil is returned."
  (let ((window (get-buffer-window buffer-or-name phw-frame)))
    (if window
	(select-window window)
      nil)))


;; (defmacro phw-exec-in-window (buffer-or-name &rest body)
;;   "Evaluates BODY in that window which displays the buffer BUFFER-OR-NAME
;; which can be either a buffer-object or a buffer-name. If that window is not
;; visible then BODY is not evaluated and the symbol 'window-not-visible is
;; returned. Otherwise the return value of BODY is returned. Runs encapsulated in
;; `save-selected-window' and `save-excursion'."
;;   `(save-selected-window
;;      (if (not (phw-window-select ,buffer-or-name))
;;          'window-not-visible
;;        (save-excursion
;;          (set-buffer ,buffer-or-name)
;;          ,@body))))

(defmacro phw-exec-in-window (buffer-or-name &rest body)
  "Evaluates BODY in that window which displays the buffer BUFFER-OR-NAME
which can be either a buffer-object or a buffer-name. If that window is not
visible then BODY is not evaluated and the symbol 'window-not-visible is
returned. Otherwise the return value of BODY is returned. Runs encapsulated in
`save-selected-window' and `save-excursion'."
  `(save-selected-window
     (if (not (phw-window-select ,buffer-or-name))
         'window-not-visible
       (with-current-buffer ,buffer-or-name
         ,@body))))

(put 'phw-exec-in-window 'lisp-indent-function 1)

(defun phw-make-windows-not-dedicated (&optional frame)
  "Make all windows of FRAME not dedicated."
  (mapc (function (lambda (w)
                    (set-window-dedicated-p w nil)))
        (phw-window-list (or frame (selected-frame)))))

(defun phw-set-windows-dedicated-state (buf-list state)
  "For every buffer in BUF-LIST set its windows dedicated-state to STATE if
visible in the `phw-frame'."
  (mapc (function (lambda (b)
                    (when (get-buffer-window b phw-frame)
                      (set-window-dedicated-p
                       (get-buffer-window b phw-frame) state))))
        buf-list))


(defun phw-window-in-window-list-number (win-list &optional window)
  "Return the number of WINDOW in the window-list WIN-LIST.
The left-top-most window of the frame has number 1. The other windows have
the same ordering as `other-window' would walk through the frame.

If WINDOW is nil then the currently selected window is used."
  (let ((win-number (phw-position (or window (selected-window)) win-list)))
    (if win-number (1+ win-number) nil)))

;;; ----- Time  stuff -----------------------------------------

;; next three functions stolen from gnus
(defun phw-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (nth 2 time) 0) 1000000.0)))

(defun phw-seconds-to-time (seconds)
  "Convert SECONDS (a floating point number) to an Emacs time structure."
  (list (floor seconds 65536)
	(floor (mod seconds 65536))
	(floor (* (- seconds (ffloor seconds)) 1000000))))

(defun phw-subtract-time (t1 t2)
  "Subtract two internal times and return the result as internal time."
  (let ((borrow (< (cadr t1) (cadr t2))))
    (list (- (car t1) (car t2) (if borrow 1 0))
	  (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2)))))

(defun phw-time-diff (t1 t2 &optional rounded)
  "Return the difference between time T1 and T2 in seconds \(can be a
floating-point number). If optional arg ROUNDED is not nil the result is a
rounded integer."
  (funcall (if rounded 'round 'identity)
           (phw-time-to-seconds (phw-subtract-time t1 t2))))
  
;; (let ((t1 nil)
;;       (t2 nil))
;;   (setq t1 (current-time))
;;   (sit-for 5)
;;   (setq t2 (current-time))
;;   (phw-time-diff t2 t1 t))

(defun phw-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

;;; ----- Ringstuff ----------------------------------------

(require 'ring)
(defalias 'phw-make-ring 'make-ring)
(defalias 'phw-ring-p 'ring-p)
(defalias 'phw-ring-empty-p 'ring-empty-p)
(defalias 'phw-ring-insert 'ring-insert)
(defalias 'phw-ring-ref 'ring-ref)
;; at least XEmacs does not have this function.
(defun phw-ring-elements (ring)
  "Return a list of the lements of RING."
  (mapcar #'identity (cddr ring)))

;;; ----- Menu stuff ---------------------------------------

(defvar phw-max-submenu-depth 4
  "The maximum depth of nesting submenus for the tree-buffers.")

(defun phw-create-menu-user-ext-type (curr-level max-level)
  "Creates the :type-definition for the *-menu-user-extension options.
This allows nested submenus for the popup-menus of the tree-buffers up to a
maximum level of MAX-LEVEL. CURR-LEVEL must be 1 when used in a
defcustom-clause and has to be <= MAX-LEVEL."
  (list 'repeat (delq nil
                      (list 'choice ':tag "Menu-entry" ':menu-tag "Menu-entry"
                            ':value '(ignore "")
                            (list 'const ':tag "Separator" ':value '("---"))
                            (list 'list ':tag "Menu-command"
                                  (list 'function ':tag "Function" ':value 'ignore)
                                  (list 'string ':tag "Entry-name"))
                            (if (= curr-level max-level)
                                nil
                              (list 'cons ':tag "Submenu"
                                    (list 'string ':tag "Submenu-title")
                                    (phw-create-menu-user-ext-type (1+ curr-level)
                                                                   max-level)))))))

;;; ----- byte-compiling stuff ----------------------------

(defun phw-is-byte-compiling ()
  "Return non-nil if eval'ed during compilation.  Don't use outside
`eval-when-compile'."
  (and (boundp 'byte-compile-dest-file)
       (stringp byte-compile-dest-file)))

(defun phw-load-in-progress-p ()
  load-in-progress)

;;; ----- User Interrupt handling -------------------------

;; KB: stolen from semantic.....

(defvar phw-current-input-throw-symbol nil
  "The current throw symbol for `phw-exit-on-input'.")

(defmacro phw-exit-on-input (symbol &rest forms)
  "Using SYMBOL as an argument to `throw', execute FORMS.
If FORMS includes a call to `phw-thow-on-input', then if a user presses any
key during execution, this form macro will exit with the value passed to
`phw-throw-on-input'. If FORMS completes, then the return value is the same as
`progn'."
  `(let ((phw-current-input-throw-symbol ,symbol))
     (catch ,symbol
       ,@forms)))
(put 'phw-exit-on-input 'lisp-indent-function 1)

(defmacro phw-throw-on-input (from &optional value)
  "Exit with `throw' when in `phw-exit-on-input' on user input.
FROM is an indication of where this function is called. Optional arg VALUE is
what should be thrown out and both are are combined in a cons-cell and passed
to `throw'. It is recommended to add as FROM the name of the function calling
this one or a descriptive symbol which indicates part of a code has been
interrupted..

Example: \(phw-throw-on-input 'test-inner-loop \"test\") would throw a
cons-cell \('test-inner-loop . \"test\")"
  `(when (and phw-current-input-throw-symbol
              (or (input-pending-p) (accept-process-output)))
     (throw phw-current-input-throw-symbol (cons ,from ,value))))


(defun phw-test-throw-on-input ()
  "Test that throw on input will work."
  (interactive)
  (phw-throw-on-input 'done-die)
  (message "Exit Code: %s"
	   (phw-exit-on-input 'testing
	     (let ((inhibit-quit nil)
		   (message-log-max nil))
	       (while t
		 (message "Looping ...")
                 ;; with the following line it isn't interruptable... so if
                 ;; you call a funtion which do not return you never reach the
                 ;; throw-part and no interruption takes place (could be if you
                 ;; run external processes!)
                 ;; So this mechanism is better than nothing but not really
                 ;; good... we need the `while-no-input'-macro...
                 ;;(while t nil)
		 (phw-throw-on-input 'test-inner-loop "test")
                 )
	       'exit))))

;; Compatibility Functions ---------------------------------

;; A number of functions in Gnu Emacs have been obsoleted in the
;; last few years.  To ensure compatibility with versions of
;; emacs that do not track Gnu Emacs the below functions so
;; phw is implementing compatibilty functions

;; interactive-p is obsolete as of Gnu Emacs 23.2
(defmacro phw-interactive-p (&optional kind)
  (if (or (> emacs-major-version 23)
	  (and (>= emacs-major-version 23)
	       (>= emacs-minor-version 2)))
      `(called-interactively-p ,kind)
    `(interactive-p)))

;; labels & flet is obsolete as of Gnu Emacs 24.3, so we use them but provide
;; compatibility with older versions
(unless (fboundp 'cl-labels) (fset 'cl-labels 'labels))
(unless (fboundp 'cl-flet) (fset 'cl-flet 'flet))

;; redraw-modeline is an obsolete function as of Gnu Emacs 24.3
(defmacro phw-redraw-modeline (&optional kind)
  (if (or (> emacs-major-version 24)
	  (and (>= emacs-major-version 24)
	       (>= emacs-minor-version 3)))
      `(force-mode-line-update ,kind)
    `(redraw-modeline)))

;;; ----- advice stuff -------------------------------------

(defvar phw-adviced-function-sets nil
  "A list of adviced-function sets defined with `defphw-advice-set'.
Each element is a cons-cell where car is the advice-set-var and cdr is an
indicator if the caller of `phw-with-original-adviced-function-set' is the
outmost caller.

DO NOT CHANGE THIS!")

(defvar phw-adviced-permanent-function-sets nil
  "A list of symbols, each of them an advice-set which should be permanent.
Permanent means this advice set will not be disabled during deactivation of
PHW. This variable is only set by `defphw-advice-set'.

DO NOT CHANGE THIS!")

(defvar phw-adviced-functions nil
  "A list of all advices defined with `defphw-advice'.
This list is the set union of the values of all function-sets of
`phw-adviced-function-sets'.

DO NOT CHANGE THIS!")

(defvar phw-advices-debug-error nil
  "It not nil then each advice of PHW reports when it's en/disabled or called.")

(defun phw-advices-debug-error (advice class action &rest args)
  "Run ARGS through `format' and write it to the *Messages*-buffer.
ADVICE is the adviced-function-symbol, CLASS is the advice-class \(after,
around or before) and ACTION is one of the symbols 'calling, 'enabling,
'disabling or 'reporting.

This will build up a message string like:
PHW <version>: debug <ACTION> of '<CLASS>' advice ADVICE: ARGS.
If ARGS is nil then only the message above is reported."
  (when phw-advices-debug-error
    (message (concat (format "PHW: debug %s of '%s' advice %s "
                             action
                             class
                             advice)
                     (if args
                         (apply 'format args))))))

(defmacro defphw-advice-set (advice-set docstring &optional permanent)
  "Defines an advice-set for PHW.
This defines a variable which will contain adviced functions defined by
`defphw-advice-set'. This is a set of advices which can be enabled or disabled
\"en block\" which must be done either by `phw-enable-advices',
`phw-disable-advices' or `phw-with-original-adviced-function-set'.

Before defining a new advice-set it's recommended to take a look at the value
of `phw-adviced-function-sets' if there is already a suitable advice-set.

IMPORTANT: Each advice in PHW must be defined by `defphw-advice' and must
belong to an advice-set previously defined by `defphw-advice-set'!

All advice-sets of PHW will be automatically\(!) disabled at load-time of the
phw-library and at deactivation-time of PHW. But: Enabling of a certain
advice-set must be done appropriately.

If optional argument PERMANENT is t then this advice-set will NOT be disabled
at deactivation-time of PHW! Calling `phw-disable-advices' for an advice set
defined with permanent is t will take no effect unless the optional argument
FORCE-PERMANENT of this function is set to not nil.
PERMANENT can also be a function which will be called by `phw-disable-advices'
for this advice set \(the function gets one argument: the symbol of the
advice-set) and have to return not nil if the advice-set should not be disable
by `phw-disable-advices' unless the FORCE-PERMANENT of this function is set to
not nil. 

Example:

\(defphw-advice-set phw-always-disabled-advices
  \"These advices are always disabled.\")"
  `(eval-and-compile
     (add-to-list 'phw-adviced-function-sets (cons (quote ,advice-set), nil))
     ,(if permanent
          `(add-to-list 'phw-adviced-permanent-function-sets
                        (cons (quote ,advice-set) ,permanent)))
     (defvar ,advice-set nil ,docstring)))

(put 'defphw-advice-set 'lisp-indent-function 1)

(defmacro defphw-advice (adviced-function advice-class advice-set advice-docstring &rest body)
  "Defines an advice for ADVICED-FUNCTION with ADVICE-CLASS for ADVICE-SET.
ADVICED-FUNCTION must be an advicable object \(e.g. a function, a subr
etc...). ADVICE-CLASS must be one of around, after or before. ADVICE-SET must
ba an advice-set previously defined by `defphw-advice-set'. ADVICE-DOCSTRING
ist the docstring for the advice. BODY is the program-code for the advice as
it would be written with `defadvice'.

Do not quote ADVICED-FUNCTION, ADVICE-CLASS and ADVICE-SET.

Example:

\(defphw-advice delete-frame around phw-layout-basic-adviced-functions
  \"If FRAME is equal to the PHW frame then...\"
  \(let \(\(frame \(or \(ad-get-arg 0) \(selected-frame))))
    \(if \(and phw-minor-mode
             \(equal frame phw-frame))
        \(when \(phw-confirm \"Attempt to delete the PHW-frame....Proceed? \")
	  \(phw-deactivate-internal) 
	  ad-do-it)
      ad-do-it)))"
  `(progn
     (if (assoc (quote ,advice-set) phw-adviced-function-sets)
         (add-to-list (quote ,advice-set)
                      (cons (quote ,adviced-function) (quote ,advice-class)))
       (error "The advice-set %s does not exist!"
              (symbol-name (quote ,advice-set))))
     (if (not (member (quote ,advice-class)
                      '(around after before)))
         (error "The advice-class %s is not allowed - only around, after and before!"
                (symbol-name (quote ,advice-class))))
     (add-to-list 'phw-adviced-functions (cons (quote ,adviced-function) (quote ,advice-class)))
     (eval-and-compile
       (defadvice ,adviced-function (,advice-class phw)
         ,advice-docstring
         (phw-advices-debug-error (quote ,adviced-function)
                                  (quote ,advice-class)
                                  'calling)
         ,@body))))

(put 'defphw-advice 'lisp-indent-function 3)

(defun phw-enable-phw-advice (function-symbol advice-class arg)
  "If ARG is greater or equal zero then enable the adviced version of
FUNCTION-SYMBOL. Otherwise disable the adviced version. The advice must be
defined with class ADVICE-CLASS by `defphw-advice'.

IMPORTANT: Do not use the function directly. Always use `phw-enable-advices',
`phw-disable-advices' or `phw-with-original-adviced-function-set'!."
  (if (< arg 0)
      (progn
        (ad-disable-advice function-symbol advice-class 'phw)
        (ad-activate function-symbol)
        (phw-advices-debug-error function-symbol advice-class 'disabling))
    (ad-enable-advice function-symbol advice-class 'phw)
    (ad-activate function-symbol)
    (phw-advices-debug-error function-symbol advice-class 'enabling)))
    

(defun phw-enable-advices (adviced-function-set-var)
  "Enable all advices of ADVICED-FUNCTION-SET-VAR, which must be defined by
`defphw-advice-set'."
  (if phw-advices-debug-error
      (message "PHW: debug enabling the advice-set: %s" adviced-function-set-var))
  (if (eq adviced-function-set-var 'phw-always-disabled-advices)
      (error "The advice-set phw-always-disabled-advices must not be enabled!"))
  (if (not (assq adviced-function-set-var phw-adviced-function-sets))
      (error "The adviced function set %s is not defined by defphw-advice-set!"
             (symbol-name adviced-function-set-var)))
  (dolist (elem (symbol-value adviced-function-set-var))
    (phw-enable-phw-advice (car elem) (cdr elem) 1)))
  
(defun phw-disable-advices (adviced-function-set-var &optional force-permanent)
  "Disable all advices of ADVICED-FUNCTION-SET-VAR, which must be defined by
`defphw-advice-set'

This function tests if ADVICED-FUNCTION-SET-VAR has been defined as permanent
by `defphw-advice-set'.

Calling `phw-disable-advices' for an advice set defined with
permanent t will take no effect unless the optional argument
FORCE-PERMANENT is set to not nil. If the advice set is defined as permanent
with a permanent-disable-function then this function is called with
ADVICED-FUNCTION-SET-VAR as argument; if this function returns not nil then
the adviced will be treated as permanent and will not being disabled.

If optional FORCE-PERMANENT is not nil then ADVICED-FUNCTION-SET-VAR will
be disabled regardless if permanent or not."
  (if phw-advices-debug-error
      (message "PHW: debug disabling the advice-set: %s" adviced-function-set-var))
  (if (not (assq adviced-function-set-var phw-adviced-function-sets))
      (error "The adviced function set %s is not defined by defphw-advice-set!"
             (symbol-name adviced-function-set-var)))
  (let ((permanent (if force-permanent
                       nil
                     (cdr (assq adviced-function-set-var
                                phw-adviced-permanent-function-sets)))))
    (unless (or (eq permanent t)
                (and (functionp permanent)
                     (funcall permanent adviced-function-set-var)))
      (dolist (elem (symbol-value adviced-function-set-var))
        (phw-enable-phw-advice (car elem) (cdr elem) -1)))))

;; for the outmost-caller-stuff see phw-with-original-adviced-function-set
(defmacro phw-with-phw-advice (function-symbol advice-class &rest body)
  "Evaluates BODY with the adviced version of FUNCTION-SYMBOL. The advice must
be defined by `defphw-advice' with class ADVICE-CLASS for the advice-set
`phw-always-disabled-advices'. Otherwise an error occurs. The advice is only
active during BODY.

BODY is protected by `unwind-protect' so in each case the advice
will be disabled after finishing this macro unless it is nested
within a call to this macro for the *same* FUNCTION-SYMBOL and
ADVICE-CLASS-combination! This means that the usage of this macro
is save for arbitrary nested calls, so full BODY is guaranted
being evaluated with enabled ADVICE-CLASS advice for
FUNCTION-SYMBOL.

Returns the value of BODY.

Example where this macro is used for `walk-windows' within another advice:

\(phw-with-phw-advice 'walk-windows 'around
   ad-do-it)"
  (let ((outmost-caller-p (make-symbol "outmost-caller-p")))
    ;; we have to check if we are the outmost-caller of this macro for this
    ;; adviced function AND the advice-class! different advice-classes for the
    ;; same function have to be treated differently!!
    `(let ((,outmost-caller-p (unless (member ,advice-class (get ,function-symbol 'phw-with-phw-advice))
                                (put ,function-symbol 'phw-with-phw-advice
                                     (append (list ,advice-class) (get ,function-symbol 'phw-with-phw-advice)))
                                ,advice-class)))
       (if (not (member (cons ,function-symbol ,advice-class)
                      phw-always-disabled-advices))
         (error "Advice for %s with class %s not registered in phw-always-disabled-advices!"
                (symbol-name ,function-symbol)
                (symbol-name ,advice-class)))
       (if phw-advices-debug-error
           (message "PHW: debug with always disabled phw-advice: %s %s - ENTRY" ,advice-class ,function-symbol))
       (unwind-protect
         (progn
           (when ,outmost-caller-p
             (phw-enable-phw-advice ,function-symbol ,advice-class 1))
           ,@body)
         (when ,outmost-caller-p
           ;; Only if we are the outmost caller we are allowed to disable the
           ;; enabled advice
           (put ,function-symbol 'phw-with-phw-advice
                (delete ,advice-class (get ,function-symbol 'phw-with-phw-advice)))
           (phw-enable-phw-advice ,function-symbol ,advice-class -1))
         (if phw-advices-debug-error
             (message "PHW: debug with always disabled phw-advice: %s %s - EXIT" ,advice-class ,function-symbol))))))
         
(put 'phw-with-phw-advice 'lisp-indent-function 2)

;; (insert (pp (macroexpand '(phw-with-phw-advice 'one-window-p 'around
;;                             (message "")))))

(defmacro phw-with-original-adviced-function-set (adviced-function-set-var &rest body)
  "Evaluates BODY with all adviced functions of ADVICED-FUNCTION-SET-VAR
being disabled \(means with their original definition). Restores always \(even
if an error occurs during evaluating BODY) the previous state of the adviced
functions, means it depends if the call to this macro is the outermost call:
Only if it is the outermost-call the advices of the used advice-set will be
disabled after finishing. So full BODY is guaranted being evaluated with
disabled advices of ADVICED-FUNCTION-SET-VAR.

ADVICED-FUNCTION-SET-VAR must be defined by `defphw-advice-set' and all
advices of this set must be defined by `defphw-advice'. Otherwise an error
occurs.

Example:

\(phw-with-original-adviced-function-set 'phw-layout-basic-adviced-functions
   \(do-something..))"
  (let ((outmost-caller-p (make-symbol "outmost-caller-p")))
    `(let ((,outmost-caller-p 
            (unless (equal (cdr (assq ,adviced-function-set-var phw-adviced-function-sets))
                           'outmost-caller)
              ;; if we are the outmost caller of this macro we store this
              ;; for
              ;; a) following callers
              ;; b) ourself, so we can later reset is
              (setcdr (assq ,adviced-function-set-var phw-adviced-function-sets) 'outmost-caller))
            ))
       (if phw-advices-debug-error
           (message "PHW: debug with original advice-set: %s - ENTRY" ,adviced-function-set-var))
       (unwind-protect
           (progn
             (when ,outmost-caller-p
               ;; we must force disabling permanent advice-sets too
               (phw-disable-advices ,adviced-function-set-var t))
             ,@body)
         (when ,outmost-caller-p
           ;; Only if we are the outmost caller we are allowed to re-enable the
           ;; disabled advice-set
           (setcdr (assq ,adviced-function-set-var phw-adviced-function-sets) nil)
           (phw-enable-advices ,adviced-function-set-var))
         (if phw-advices-debug-error
             (message "PHW: debug with original advice-set: %s - EXIT" ,adviced-function-set-var))))))


(put 'phw-with-original-adviced-function-set 'lisp-indent-function 1)

;; !!!!!!!!!!!!!!!! Caution !!!!!!!!!!!!!!!!!!!!!!!!!!
;; when editing that file which defines such an advice and then saving and
;; byte-compiling this file then this reactivates this advice - just a hint -
;; should be not a problem for PHW because the users should not edit the
;; phw-code ;-) But we should have this in mind!!!!!!!!!!!!!!!!!!!!!!!
(defphw-advice-set phw-always-disabled-advices
  "These advices are always disabled.
This advice-set can not be enabled by `phw-enable-advices' but such an
advice has to be activated 'on demand' by the caller. Such an advice must be
used with the macro `phw-with-phw-advice'.")

;;; ----- Provide ------------------------------------------

(provide 'phw-util)
