;;; phw-navigate.el --- Browser-navigation for PHW

;; Copyright (C) 2000 - 2005, 2015 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
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

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.


;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'phw-cedet-wrapper)
(require 'phw-util)



;;====================================================
;; 
;;====================================================

(defclass phw-dlist-node ()
  ((previous :initform nil); :protection :private)
   (next :initform nil); :protection :private)
   (data :initarg :data :initform nil); :protection :private)
   )
  "A node in a double linked list."
  )

(defun phw-dlist-node-new (data)
  (phw-dlist-node "node" :data data))

(defmethod phw-get-data ((node phw-dlist-node))
  (oref node data))

(defmethod phw-get-next ((node phw-dlist-node))
  (oref node next))

(defmethod phw-get-previous ((node phw-dlist-node))
  (oref node previous))

(defmethod phw-set-data ((node phw-dlist-node) data)
  (oset node data data))

(defmethod phw-set-next ((node phw-dlist-node) next)
  (let ((old-next (phw-get-next node)))
    (when old-next
      (oset old-next previous nil))
    (oset node next next)
    (when next
      (phw-set-previous next nil)
      (oset next previous node))))

(defmethod phw-set-previous ((node phw-dlist-node) previous)
  (let ((old-previous (phw-get-previous node)))
    (when old-previous
      (oset old-previous next nil))
    (oset node previous previous)
    (when previous
      (phw-set-next previous nil)
      (oset previous next node))))


;;====================================================
;; 
;;====================================================

(defclass phw-nav-history-item ()
  ((pos :initarg :pos :initform 0); :protection :private)
   (window-start :initarg :window-start :initform 0); :protection :private)
   )
  )
  
(defmethod phw-nav-set-pos ((item phw-nav-history-item) pos)
  (oset item pos pos))

(defmethod phw-nav-set-window-start ((item phw-nav-history-item) point)
  (oset item window-start point))

(defmethod phw-nav-get-pos ((item phw-nav-history-item))
  (oref item pos))

(defmethod phw-nav-get-window-start ((item phw-nav-history-item))
  (oref item window-start))

(defmethod phw-nav-to-string ((item phw-nav-history-item))
  (concat (int-to-string (phw-nav-get-pos item)) ":"
	  (int-to-string (phw-nav-get-window-start item))))

;; This method must return nil if saving can not be performed and otherwise
;; not nil!
(defmethod phw-nav-save ((item phw-nav-history-item))
  t)

(defmethod phw-nav-is-valid ((item phw-nav-history-item))
  t)


;;====================================================
;; 
;;====================================================

;; Klaus Berndl <klaus.berndl@sdm.de>: Changed this class from storing the
;; whole tag to storing explicitly the tag-buffer, the marker of the
;; tag-start, the marker of the tag-end. This prevents the stored
;; navigation-items from getting invalid and unusable after a full
;; semantic-reparse because such a reparse makes the overlays contained in the
;; stored tags invalid so we can not uses their informations.
(defclass phw-nav-tag-history-item (phw-nav-history-item)
  ((tag-buffer :initarg :tag-buffer :initform nil); :protection :private)
   (tag-start :initarg :tag-start :initform nil) ; :protection :private)
   (tag-end :initarg :tag-end :initform nil) ; :protection :private)
   (tag-name :initarg :tag-name :initform nil) ; :protection :private)
   (narrow :initarg :narrow :initform nil); :protection :private)
   )
  )

(defun phw-nav-tag-history-item-new (tag-name tag-buffer tag-start
                                                  tag-end &optional narrow)
  (phw-nav-tag-history-item tag-name
                              :tag-buffer tag-buffer
                              :tag-start tag-start
                              :tag-end tag-end
                              :tag-name tag-name
                              :narrow narrow))

(defmethod phw-nav-get-tag-buffer ((item phw-nav-tag-history-item))
  (oref item tag-buffer))

(defmethod phw-nav-get-tag-start ((item phw-nav-tag-history-item))
  (oref item tag-start))

(defmethod phw-nav-get-tag-end ((item phw-nav-tag-history-item))
  (oref item tag-end))

(defmethod phw-nav-get-tag-name ((item phw-nav-tag-history-item))
  (oref item tag-name))

(defmethod phw-nav-get-narrow ((item phw-nav-tag-history-item))
  (oref item narrow))

(defmethod phw-nav-goto ((item phw-nav-tag-history-item))
  (let ((tag-buffer (phw-nav-get-tag-buffer item))
        (tag-start (phw-nav-get-tag-start item))
        (tag-end (phw-nav-get-tag-end item))
        (win-start (phw-nav-get-window-start item)))
    (select-window phw-last-edit-window-with-point)
    (set-window-buffer (selected-window) tag-buffer)
    (widen)
    (goto-char tag-start)
    (when (phw-nav-get-narrow item)
      (narrow-to-region (phw-line-beginning-pos) tag-end))
    (goto-char (+ tag-start (phw-nav-get-pos item)))
    (if win-start
        (set-window-start (selected-window) (+ tag-start win-start)))))

(defmethod phw-nav-save ((item phw-nav-tag-history-item))
  "Return only nil if tag-start of ITEM points into a dead buffer. In this
case no position saving is done."
  (let ((tag-start (phw-nav-get-tag-start item)))
    (if (and tag-start (marker-buffer tag-start))
        (progn
          (with-current-buffer (marker-buffer tag-start)
            (phw-nav-set-pos item (- (point) tag-start)))
          (phw-nav-set-window-start
           item
           (if (equal (window-buffer) (marker-buffer tag-start))
               (- (window-start) tag-start)
             nil))
          t)
      nil)))

(defmethod phw-nav-to-string ((item phw-nav-tag-history-item))
  (concat (phw-nav-get-tag-name item) ":" (call-next-method)))

(defmethod phw-nav-is-valid ((item phw-nav-tag-history-item))
   (let ((tag-start (phw-nav-get-tag-start item))
         (tag-buf (phw-nav-get-tag-buffer item))
         (tag-end (phw-nav-get-tag-end item)))
     (if (and tag-start (marker-buffer tag-start)
              tag-end (marker-buffer tag-end)
              tag-buf (buffer-live-p tag-buf))
         t)))
 

;;====================================================
;; 
;;====================================================

(defclass phw-nav-file-history-item (phw-nav-history-item)
  ((file :initarg :file :initform ""); :protection :private)
   ;; the following is nil if the item does not point to an indirect-buffer
   ;; based on a file-buffer
   (indirect-buffer-name :initarg :indirect-buffer-name :initform "") ; :protection :private)
   )
  )

(defun phw-nav-file-history-item-new ()
  (let* ((file (phw-buffer-file-name))
         (ind-buffer-name (and file
                               (buffer-base-buffer)
                               (buffer-name)))
         (item (phw-nav-file-history-item (buffer-name)
                                          :file file
                                          :indirect-buffer-name ind-buffer-name)))
    (phw-nav-set-pos item (point))
    (phw-nav-set-window-start item
                              (window-start (get-buffer-window (current-buffer))))
    item))

(defmethod phw-nav-get-file ((item phw-nav-file-history-item))
  (oref item file))

(defmethod phw-nav-set-file ((item phw-nav-file-history-item) file)
  (oset item file file))

(defmethod phw-nav-get-indirect-buffer-name ((item phw-nav-file-history-item))
  (oref item indirect-buffer-name))

(defmethod phw-nav-set-indirect-buffer-name ((item phw-nav-file-history-item) indirect-buffer-name)
  (oset item indirect-buffer-name indirect-buffer-name))

(defmethod phw-nav-save ((item phw-nav-file-history-item))
  (phw-nav-set-pos item (point))
  (phw-nav-set-window-start item (window-start))
  (phw-nav-set-file item (phw-buffer-file-name))
  (phw-nav-set-indirect-buffer-name item (and (phw-buffer-file-name)
                                              (buffer-base-buffer)
                                              (buffer-name)))
  t)

(defmethod phw-nav-goto ((item phw-nav-file-history-item))
  (when (phw-nav-get-file item)
    (if (phw-nav-get-indirect-buffer-name item)
        (switch-to-buffer (phw-nav-get-indirect-buffer-name item))
      (find-file (phw-nav-get-file item)))
    (widen)
    (goto-char (phw-nav-get-pos item))
    (let ((win-start (phw-nav-get-window-start item)))
      (if win-start
          (set-window-start (selected-window) win-start)))))
  
(defmethod phw-nav-to-string ((item phw-nav-file-history-item))
  (concat (phw-nav-get-file item) "-"
          (phw-nav-get-indirect-buffer-name item)
          ":" (call-next-method)))

(defmethod phw-nav-is-valid ((item phw-nav-file-history-item))
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: for saveness we should test if
  ;; file points to a readable file - but what about remote-file (can last
  ;; long)?
  (and (phw-nav-get-file item)
       (or (null (phw-nav-get-indirect-buffer-name item))
           (phw-buffer-obj (phw-nav-get-indirect-buffer-name item)))))

;;====================================================
;; 
;;====================================================

(defvar phw-nav-first-node nil)
(setq phw-nav-first-node (phw-dlist-node-new (phw-nav-history-item "First item")))

(defvar phw-nav-current-node nil)
(setq phw-nav-current-node phw-nav-first-node)


(defun phw-nav-initialize ()
  (setq phw-nav-first-node
        (phw-dlist-node-new (phw-nav-history-item "First item")))
  (setq phw-nav-current-node phw-nav-first-node))
  

(defun phw-nav-jump-to-tag (file tag &optional narrow)
  (phw-nav-save-current)
  (find-file file)
  (phw-nav-add-item (phw-nav-tag-history-item tag narrow)))

(defun phw-nav-jump-to-file (file)
  (phw-nav-save-current)
  (find-file file)
  (phw-nav-add-item (phw-nav-file-history-item file)))

(defun phw-nav-add-item (item)
  (let ((node (phw-dlist-node-new item)))
    (phw-set-next node (phw-get-next phw-nav-current-node))
    (phw-set-next phw-nav-current-node node)
    (setq phw-nav-current-node node)))

(defun phw-nav-remove-current-node ()
  (phw-nav-remove-node phw-nav-current-node))

(defun phw-nav-remove-node (node)
  "Remove NODE and set `phw-nav-first-node' and `phw-nav-current-node' if
necessary."
  (let ((prev (phw-get-previous node))
        (next (phw-get-next node)))
    (if prev
        (phw-set-next prev (phw-get-next node)))
    (if next
        (phw-set-previous next (phw-get-previous node)))
    (if (eq node phw-nav-current-node)
        (setq phw-nav-current-node (or prev
                                       next
                                       phw-nav-first-node)))
    (if (eq node phw-nav-first-node)
        (if next
            (setq phw-nav-first-node next)
        (phw-nav-initialize)))))

(defun phw-nav-remove-invalid-nodes ()
  (let ((node phw-nav-first-node)
        (next-node nil))
    (while node
      (setq next-node (phw-get-next node))
      (if (not (phw-nav-is-valid (phw-get-data node)))
          (phw-nav-remove-node node))
      (setq node next-node))))

(defun phw-nav-save-current ()
  (while (not (phw-nav-save (phw-get-data phw-nav-current-node)))
    (phw-nav-remove-current-node)))

(defun phw-nav-goto-next ()
  "Go forward in the navigation history list."
  (interactive)
  (phw-nav-goto--internal (phw-get-next phw-nav-current-node)))

(defun phw-nav-goto-previous ()
  "Go back in the navigation history list."
  (interactive)
  (phw-nav-goto--internal (phw-get-previous phw-nav-current-node)))

(defun phw-nav-dump-history ()
  (interactive)
  (phw-nav-remove-invalid-nodes)
  (phw-nav-dump-history--internal phw-nav-first-node))

(defun phw-nav-dump-history--internal (node)
  (when node
    (insert (phw-nav-to-string (phw-get-data node)) "\n")
    (phw-nav-dump-history--internal (phw-get-next node))))

(defun phw-nav-goto--internal (node)
  (if (or (not node) (eq phw-nav-first-node node))
      (message "No more valid history items!")
    ;; before doing something we have to clear the history from now invalid
    ;; nodes means removing nodes which does not point into a live buffer
    (phw-nav-remove-invalid-nodes)
    (phw-nav-save-current)
    (setq phw-nav-current-node node)
    (phw-nav-goto (phw-get-data node))))


(silentcomp-provide 'phw-navigate)

;;; phw-navigate.el ends here
