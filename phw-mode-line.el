;;; phw-mode-line.el --- mode-line for PHW

;; Copyright (C) 2000 - 2005, 2015 Jesper Nordenberg,
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
;;
;; Contains all mode-line enhancements for PHW.

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'phw-util)
(require 'phw-face)

;; XEmacs
(silentcomp-defun phw-redraw-modeline)
(silentcomp-defun make-extent)
(silentcomp-defun set-extent-face)
(silentcomp-defun set-extent-property)
;; Emacs
(silentcomp-defun force-mode-line-update)
(silentcomp-defun propertize)

(defgroup phw-mode-line nil
  "Settings for the modelines of the PHW-tree-buffers."
  :group 'phw-general
  :prefix "phw-")

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: update the texi
(defcustom phw-mode-line-prefixes '((phw-directories-buffer-name . nil)
                                    (phw-sources-buffer-name . phw-sources-filter-modeline-prefix)
                                    (phw-methods-buffer-name . phw-methods-filter-modeline-prefix)
                                    (phw-history-buffer-name . phw-history-filter-modeline-prefix))
  "*Prefixes shown in the modelines of the special PHW-buffers.
The displayed prefix then looks like: \"[ <PREFIX>[: ]]\", means if a prefix
is defined for an special PHW-buffer then a single space is prepended and if
there is additional text to display \(e.g. the current directory in the
sources buffer, see `phw-mode-line-data') then also the string \": \" is
appended.

Everey element of this list is a cons-cell where the car is used to define a
buffer-name and the cdr to define the modeline-prefix for that buffer.

The buffer-name can either be defined as plain string or with a symbol which
contains the buffer-name as value. The latter one is recommended to define a
prefix for one of the builtin PHW-tree-buffers because then simply the related
option-symbol can be used. To add a prefix for the builtin directories
tree-buffer just set the symbol `phw-directories-buffer-name' as car.

The cdr is the prefix for a buffer and can either be a string
which used as it is or a function-symbol which is called with
three argument \(the buffer-name, the current selected directory
and the current selected source whereas the latter one is a cons
as returned by `phw-path-selected-source') and must return either
nil \(for no prefix) or a string which is then used a prefix.
Such a function can add the text-property 'help-echo to the
result-string. Then this help-string will be displayed when the
user moves the mouse over this section of the modeline.

If a special PHW-buffer should not have a prefix in its modeline then this
buffer-name should either not being added to this option or added with \"No
prefix\" \(= nil as cdr)."
  :group 'phw-mode-line
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'phw-minor-mode)
                            phw-minor-mode)
                       (phw-mode-line-format))))
  :initialize 'custom-initialize-default
  :type '(repeat (cons :tag "Prefix-definition"
                       (choice :tag "Buffer-name" :menu-tag "Buffer-name"
                               (string :tag "Buffer-name as string")
                               (symbol :tag "Symbol containing buffer-name"))
                       (choice :tag "Prefix" :menu-tag "Prefix"
                               (const :tag "No prefix" :value nil)
                               (string :tag "Prefix-string")
                               (function :tag "Compute prefix with")))))
  


(defcustom phw-mode-line-display-window-number t
  "*Display in the modeline of every special PHW-window the window-number.
The left-top-most window in a frame has the window-number 0 and all other
windows are numbered with increasing numbers in the sequence, functions like
`other-window' or `next-window' would walk through the frame.

This can be used to jump to windows by number with commands like:

  \(defun my-switch-to-window-number \(number)
    \"Switch to the nth window\"
    \(interactive \"P\")
    \(if \(integerp number)
        \(select-window \(nth number \(window-list)))))

Currently this feature is only available for GNU Emacs 21.X, because neither
GNU Emacs < 21 nor XEmacs can evaluate dynamically forms in the mode-line."
  :group 'phw-mode-line
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'phw-minor-mode)
                            phw-minor-mode)
                       (phw-mode-line-format))))
  :initialize 'custom-initialize-default
  :type 'boolean)

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: update the texi
(defcustom phw-mode-line-data '((phw-directories-buffer-name . sel-dir)
                                (phw-sources-buffer-name . sel-dir)
                                (phw-methods-buffer-name . sel-source)
                                (phw-analyse-buffer-name . sel-source)
                                (phw-history-buffer-name . "History"))
  "*Data shown in the modelines of the special PHW-buffers.
Everey element of this list is a cons-cell where the car is used to define a
buffer-name and the cdr to define the modeline-data for that buffer. For
details about how to defining a buffer-name see `phw-mode-line-prefixes' - its
completely the same.

The cdr is the data for ths modeline and can either be the symbol 'sel-dir or
'sel-source whereas the former one displays the current selected directory as
modeline-data and the latter one the current selected source-file \(without
path).

In addition to these two predefined values for every special
PHW-buffer either a simple string \(which will be displayed) or a
function can be specified which gets three args \(name of the
buffer, current selected directory and current selected source
whereas the latter one is a cons as returned by
`phw-path-selected-source') and must return a string which will
be displayed in the modeline \(or nil if no data should be
displayed). Such a function can add the text-property 'help-echo
to the result-string. Then this help-string will be displayed
when the user moves the mouse over this section of the modeline.

If a special PHW-buffer should not display special data in its modeline then
this buffer-name should either not being added to this option or added with
\"No data\" \(= nil as cdr).

The whole modeline of the special PHW-buffer consists of the prefix of
`phw-mode-line-prefixes' and the data of `phw-mode-line-data' - eventually
prepended by the window-number, see `phw-mode-line-display-window-number'."
  :group 'phw-mode-line
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'phw-minor-mode)
                            phw-minor-mode)
                       (phw-mode-line-format))))
  :initialize 'custom-initialize-default
  :type '(repeat (cons :tag "Data-definition"
                       (choice :tag "Buffer-name" :menu-tag "Buffer-name"
                               (string :tag "Buffer-name as string")
                               (symbol :tag "Symbol containing buffer-name"))
                       (choice :tag "Modeline-data" :menu-tag "Modeline-data"
                               (const :tag "No data" :value nil)
                               (const :tag "Current selected directory"
                                      :value sel-dir)
                               (const :tag "Current selected source"
                                      :value sel-source)
                               (string :tag "Data-string")
                               (function :tag "Compute data with")))))


(defun phw-mode-line-format ()
  "Update all of the modelines of each phw buffer."
  (save-excursion
    ;; update the modeline for each visible(!!) PHW-buffer
    (mapc (function
           (lambda (buffer)
             (let* ((prefix-elem (phw-some (function
                                            (lambda (p)
                                              (cond ((stringp (car p))
                                                     (if (phw-string= (car p)
                                                                      (buffer-name buffer))
                                                         (cdr p)
                                                       nil))
                                                    ((and (symbolp (car p))
                                                          (boundp (car p))
                                                          (stringp (symbol-value (car p))))
                                                     (if (phw-string= (symbol-value (car p))
                                                                      (buffer-name buffer))
                                                         (cdr p)
                                                       nil))
                                                    (t (phw-error "phw-mode-line-format: Can not get prefix-elem: %s" p)))))
                                           phw-mode-line-prefixes))
                    (prefix-str (typecase prefix-elem
                                  (null nil)
                                  (string prefix-elem)
                                  (function (funcall prefix-elem
                                                     (buffer-name buffer)
                                                     phw-path-selected-directory
                                                     (phw-path-selected-source)))))
                    (data-elem (phw-some (function
                                          (lambda (p)
                                            (cond ((stringp (car p))
                                                   (if (phw-string= (car p)
                                                                    (buffer-name buffer))
                                                       (cdr p)
                                                     nil))
                                                  ((and (symbolp (car p))
                                                        (boundp (car p))
                                                        (stringp (symbol-value (car p))))
                                                   (if (phw-string= (symbol-value (car p))
                                                                    (buffer-name buffer))
                                                       (cdr p)
                                                     nil))
                                                  (t (phw-error "phw-mode-line-format: Can not get data-elem: %s" p)))))
                                         phw-mode-line-data))
                    (data-str (cond ((equal data-elem 'sel-dir)
                                     phw-path-selected-directory)
                                    ((equal data-elem 'sel-source)
                                     (and phw-path-selected-source
                                          (phw-path-selected-source 'buffername)))
                                    ((stringp data-elem)
                                     data-elem)
                                    ((null data-elem)
                                     nil)
                                    ((functionp data-elem)
                                     (funcall data-elem
                                              (buffer-name buffer)
                                              phw-path-selected-directory
                                              (phw-path-selected-source))))))
               ;; Display a default help-echo but only if the modeline-data is
               ;; not computed by a user-function.
               (when (and (not (functionp data-elem))
                          (stringp data-str))
                 (put-text-property 0 (length data-str) 'help-echo
                                    "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu"
                                    data-str))
               ;; Now set the modeline
               (phw-mode-line-set (buffer-name buffer)
                                  phw-frame
                                  prefix-str
                                  data-str))))
          (phw-get-current-visible-phw-buffers))))


(defun phw-mode-line-make-modeline-str (str face)
  "Applies FACE to the STR. In additon it applies a help-echo to STR if STR
contains a text-property 'help-echo."
  (let ((strcp (copy-sequence str)))
    (if phw-running-xemacs
        (let ((ext (make-extent nil nil))
              (help-echo-str
               (catch 'found
                 (dotimes (i (length strcp))
                   (if (get-text-property i 'help-echo strcp)
                       (throw 'found
                              (get-text-property i 'help-echo strcp))))
                 nil)))
          (set-extent-face ext face)
          (set-extent-property ext 'help-echo help-echo-str)
          (list (cons ext strcp)))
      (list (propertize strcp 'face face)))))
                 
(defun phw-mode-line-set (buffer-name frame prefix &optional text no-win-nr)
  "Sets the mode line for a buffer. The mode line has the scheme:
\"[WIN-NR ][PREFIX[: ]][TEXT]\". WIN-NR is the number of the window which
displays BUFFER-NAME and is only displayed if the option
`phw-mode-line-display-window-number' is not nil and if NO-WIN-NR is nil. See
this option for a description of the window-number. WIN-NR will be displayed
as \"W-<number>\"."
  (when (get-buffer-window buffer-name frame)
    (let ((shown-prefix (if (stringp prefix)
                            (concat " " prefix (if (stringp text) ": " ""))
                          (if (stringp text) " " "")))
          (win-width (window-width (get-buffer-window buffer-name)))
          (available-text-width nil))
      (setq shown-prefix (phw-fit-str-to-width shown-prefix (1- win-width) 'right))
      (setq available-text-width (- win-width
                                   (+ (length shown-prefix)
                                      (if (and (not phw-running-xemacs)
                                               phw-mode-line-display-window-number
                                               (not no-win-nr))
                                          4 0))))
      (phw-mode-line-update-buffer
       buffer-name
       (list (if (and (not phw-running-xemacs)
                      phw-mode-line-display-window-number
                      (not no-win-nr))
                 ;; With :eval we must not use a list
                 '(:eval (car (phw-mode-line-make-modeline-str
                               (format " W-%d"
                                       (1- (phw-window-in-window-list-number (phw-canonical-windows-list))))
                               phw-mode-line-win-nr-face)))
               "")
             (phw-mode-line-make-modeline-str shown-prefix
                                              phw-mode-line-prefix-face)
             (phw-mode-line-make-modeline-str
              (concat (if (stringp text)
                          (phw-fit-str-to-width text
                                                available-text-width
                                                'left)))
              phw-mode-line-data-face))))))


(defun phw-mode-line-update-buffer (buffer-name new-mode-line-format)
  "Update the given buffer...."
  (if (phw-buffer-obj buffer-name)
      (with-current-buffer buffer-name
        (setq mode-line-format new-mode-line-format)
	(if phw-running-xemacs
	    (phw-redraw-modeline)
	  (force-mode-line-update)))
    (message "This buffer isn't available: %s"  buffer-name)))

(silentcomp-provide 'phw-mode-line)

;;; phw-mode-line.el end here

