;;; phw-compilation.el --- code for buffers displayed in compile-window

;; Copyright (C) 2000-2015   Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
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

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.



;;; Code:

(require 'phw-util)

(defcustom phw-compilation-buffer-names `(("*Calculator*" . nil)
                                          ("*vc*" . nil)
                                          ("*vc-diff*" . nil)
                                          ,(if phw-running-xemacs
                                               '("\\*Apropos.*\\*" . t)
                                             '("*Apropos*" . nil))
                                          ("*Occur*" . nil)
                                          ("*shell*" . nil)
                                          ("\\*[cC]ompilation.*\\*" . t)
                                          ("\\*i?grep.*\\*" . t)
                                          ("*JDEE Compile Server*" . nil)
                                          ,(if phw-running-xemacs
                                               '("\\*Help.*\\*" . t)
                                              '("*Help*" . nil))
                                          ("*Completions*" . nil)
                                          ("*Backtrace*" . nil)
                                          ("*Compile-log*" . nil)
                                          ("*bsh*" . nil)
                                          (,(if phw-running-xemacs
                                               " *Message-Log*"
                                             "*Messages*") . nil))
  "*Additional buffer names that should be displayed in the compile-window.
Buffer names can either be defined as strings or as regexps. If the
buffer-name of a buffer matches one of the defined string or regexp then it
will be displayed in the compile-window of PHW even if `compilation-buffer-p'
says nil for this buffer.

It is not recommended to add the name of eshell-buffers to this list because
PHW already handles the eshell-integration as best as possible.

See also the options `phw-compilation-major-modes' and
`phw-compilation-predicates'."
  :group 'phw-compilation
  :group 'phw-most-important
  :type '(repeat (cons (string :tag "Buffer name")
                       (boolean :tag "Handled as regexp"))))

(defvar phw-compilation-buffer-names-internal nil
  "This variable is for PHW internal use and can be used by PHW to add
buffer-names to the set specified in `phw-compilation-buffer-names'. Type is
the same as of option `phw-compilation-buffer-names'")

(defun phw-compilation-buffer-names ()
  "Return the set of buffer names which should be displayed in the
compile-window of PHW. This is a list combined of
`phw-compilation-buffer-names' and `phw-compilation-buffer-names-internal'."
  (append phw-compilation-buffer-names
          phw-compilation-buffer-names-internal))

(defun phw-compilation-registered-buffer-p (name)
  "Check if name belongs to the set of buffers returned by
`phw-compilation-buffer-names'. If yes returns NAME."
  (catch 'exit
    (dolist (b (phw-compilation-buffer-names))
      (if (null (cdr b))
          (if (phw-string= name (car b))
              (throw 'exit name))
        (save-match-data
          (if (string-match (car b) name)
              (throw 'exit name))))
      nil)))

(defcustom phw-compilation-major-modes '(compilation-mode)
  "*Additional major-mode that should be displayed in the compile-window.
All buffers of a major-mode contained in this list are displayed in the
compile-window even if `compilation-buffer-p' says nil for such a buffer.

It is not recommended to add `eshell-mode' to this list because PHW already
handles the eshell-integration as best as possible."
  :group 'phw-compilation
  :type '(repeat (symbol :tag "major-mode name")))

(defvar phw-compilation-major-modes-internal nil
  "This variable is for PHW internal use and can be used by PHW to add
major-mode symbols to the set specified in `phw-compilation-major-modes'.")

(defun phw-compilation-major-modes ()
  "Return all major-mode symbols which should be displayed in the
compile-window. This is a list combined of `phw-compilation-major-modes' and
`phw-compilation-major-modes-internal'."
  (append phw-compilation-major-modes
          phw-compilation-major-modes-internal))


(defcustom phw-compilation-predicates '(comint-check-proc)
  "*Predicates when a buffer should be treated as compilation-buffer.
Every element of this list has to be a function or lambda-expression which
gets as argument a buffer-object and which has to return not nil when this
buffer should be treated as compilation-buffer \(even if
`compilation-buffer-p' says nil) and therefore be displayed in the
compile-window of PHW \(if there is any).

In combination with the values of `phw-compilation-buffer-names' and
`phw-compilation-major-modes' PHW decides when a buffer is displayed in the
compile-window.

Default value is the function `comint-check-proc' which returns not nil when
the buffer is related to a living process."
  :group 'phw-compilation
  :type '(repeat (symbol :tag "Compilation predicate")))

(defvar phw-compilation-predicates-internal nil
  "This variable is for PHW internal use and can be used by PHW to add
predicates to the set defined in `phw-compilation-predicates'.")

(defun phw-compilation-predicates ()
  "Return all predicates which should be used to test if a buffer should be
displayed in the compile-window. This is a list combined of
`phw-compilation-predicates' and `phw-compilation-predicates-internal'."
  (append phw-compilation-predicates
          phw-compilation-predicates-internal))



(defun phw-compilation-get-buffers()
  "Get all known compilation buffer names.  See `phw-compilation-buffer-p'."

  (let((buffer-names '())
       (buffer-list (buffer-list phw-frame))
       (index 0))

    (setq buffer-list (sort buffer-list (lambda(first second)
                                          (phw-string< (buffer-name first)
                                                       (buffer-name second)))))
    (dolist(buffer buffer-list)
      (when (phw-compilation-buffer-p buffer)
        (setq buffer-names
              (append buffer-names
                      (list (cons (buffer-name buffer) index))))
        (setq index (1+ index))))

    buffer-names))


(defun phw-compilation-buffer-p (buffer-or-name)
  "Test if the given buffer BUFFER-OR-NAME should be treated as a compilation
buffer. Note that in this case we define \"compilation buffer\" as a buffer
that should ideally be displayed in the compile-window of PHW \(see
`phw-compile-window-height'). This means that in some situations this might
not be the result of a real `compile-internal'. A good example would be the
*Help* buffer.

BUFFER-OR-NAME can be the name of a living\(!) buffer or a buffer-object.

This function returns the buffer-object of BUFFER-OR-NAME - i.e.
BUFFER-OR-NAME will be treated as compilation-buffer - if:

- The name of the buffer is contained in the list returned by the function
  `phw-compilation-buffer-names' or
- the `major-mode' of the buffer is contained in the list returned by the
  function `phw-compilation-major-modes' or
- if `compilation-buffer-p' returns true or
- one of the predicates returned by `phw-compilation-predicates' returns not
  nil for the buffer.

Otherwise nil is returned.

Summary for PHW-end-users: A buffer will be treated as compilation-buffer if
either 
- `compilation-buffer-p' returns not nil, i.e. if a real compilation-buffer or
- if at least one of the options `phw-compilation-buffer-names',
  `phw-compilation-major-modes' or `phw-compilation-predicates' define the
  buffer as compilation-buffer."
  ;;determine the best valid for the buffer.
  (let ((buffer (phw-buffer-obj buffer-or-name))
        (phw-comp-predicates (phw-compilation-predicates)))
    (when buffer

      ;;test if this is a valid buffer by name.
      (if (phw-compilation-registered-buffer-p (buffer-name buffer))
          buffer
        ;;else test if this is a valid buffer by mode
        (if (with-current-buffer buffer
              (member major-mode (phw-compilation-major-modes)))
            buffer
          ;;else test if this is a regular compilation buffer
          (if (compilation-buffer-p buffer)
              buffer
            ;; we do not use run-hook-with-args-until-success because we have
            ;; to check if the functions are bound!!
            (if (dolist (p phw-comp-predicates)
                  (if (and (fboundp p) (funcall p buffer))
                      (return t)))
                buffer
              nil)))))))

;; Klaus Berndl <klaus.berndl@sdm.de>: The following mechanism is necessary to
;; avoid eating up whole CPU for updating the menu-entries for the
;; compilation-buffers. Especially if you have opened a lot of buffers this
;; can slow down Emacs/PHW dramatically. Now we add an idle-times
;; check-function `phw-compilation-buffer-list-changed-p' which checks if the
;; buffer-list has changed. If yes, then the variable
;; `phw-compilation-update-menu-p' is set to t. Only if this variable is not
;; nil the menu-bar-update-hook `phw-compilation-update-menu' updates the
;; PHW-menu.

(defvar phw-compilation-update-menu-p nil)
(defvar phw-compilation-buffer-list-cache nil)
(defvar phw-compilation-update-idle-time 0.25)

(defun phw-compilation-buffer-list-init ()
  "Initialize the compilation buffer list cache."
  (setq phw-compilation-update-menu-p nil)
  (setq phw-compilation-buffer-list-cache nil)
  (phw-compilation-buffer-list-changed-p))

(defun phw-compilation-buffer-list-changed-p ()
  "Check if current active buffer list has changed - i.e. if a new buffer has
been created or a buffer has been deleted. If yes then
`phw-compilation-update-menu-p' is set to not nil and the cache is updated."
  )

(defun phw-compilation-update-menu()
  "Create an install a menu that allows the user to navigate buffers that are
valid PHW compilation buffers. This is only done if
`phw-compilation-update-menu-p' is not nil; see
`phw-compilation-buffer-list-changed-p'. For more information about
compilation buffers see `phw-compilation-buffer-p'."

  (when phw-compilation-update-menu-p
    (let ((submenu nil)
          (buffers (phw-compilation-get-buffers)))
      (condition-case nil
          (progn
            (setq phw-compilation-update-menu-p nil)
            (dolist(buffer buffers)
              (setq submenu
                    (append submenu
                            (list (vector (car buffer)
                                          ;; switch-to-buffer-other-window is
                                          ;; ok for all situations because if
                                          ;; no compile-window it uses another
                                          ;; edit-window otherwise it uses the
                                          ;; compile-window. 
                                          `(funcall 'switch-to-buffer-other-window
                                                    ,(car buffer))
                                          :active t)))))
            
            ;;Klaus Berndl <klaus.berndl@sdm.de>: Seems not to work with
            ;;Emacs 20.X, but who cares, 20.x is outdated and not supported
            ;;anymore by PHW
            (easy-menu-change (list phw-menu-name)
                              "Compilation Buffers"
                              submenu
                              "Navigate")
            t)
        (error nil)))))
      


(provide 'phw-compilation)
