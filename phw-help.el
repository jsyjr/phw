;;; phw-help.el --- online help for PHW and bug reporting

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
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
;; Contains all online-help for PHW (stolen something from recentf.el)

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'phw-layout)
(require 'phw-util)

;; XEmacs and Emacs 20.X
(silentcomp-defvar browse-url-new-window-p)
(silentcomp-defun browse-url)
;; Emacs 21
(silentcomp-defvar browse-url-new-window-flag)
;; JDE
(silentcomp-defvar jde-version)
;; mail and reporter
(silentcomp-defun mail-subject)
(silentcomp-defun mail-text)
(silentcomp-defun reporter-submit-bug-report)

(defconst phw-help-info-start-file "phw.info")
(defconst phw-help-html-start-file "phw.html")
(defconst phw-help-info-subdir "./info-help/")
(defconst phw-help-html-subdir "./html-help/")

(defgroup phw-help nil
  "Settings for the PHW online help"
  :group 'phw)

(defcustom phw-show-help-format 'info
  "*The format `phw-show-help' shows its online help.
Allowed values are 'info \(for the Info format) and 'html \(for HTML format).
If the value is 'html then `browse-url-browser-function' says which browser is
used.

Note: If you got PHW as a standard XEmacs-package maybe the
HTML-online-documentation is not included."
  :group 'phw-help
  :group 'phw-most-important
  :type '(choice :tag "Online-help format" :menu-tag "Online-help format"
                 (const :tag "Info" :value info)
                 (const :tag "Html" :value html)))


(defcustom phw-help-info-path (concat
                               (if phw-running-xemacs
                                   (if (file-exists-p
                                        (concat phw-phw-dir
                                                phw-help-info-subdir
                                                phw-help-info-start-file))
                                       phw-help-info-subdir
                                     "../../info/")
                                 phw-help-info-subdir)
                               phw-help-info-start-file)
  "*Path where the PHW online help in info format resides.
This must be the location of the file \"phw.info\" which comes with the PHW
distribution. If is installed by unpacking the archive available on the PHW
web-site then this is the subdir `phw-help-info-subdir' of the installation
directory of PHW. If it is installed as XEmacs-package \(e.g. via the package
manager of XEmacs) then this is probably the directory \"../../info/\"
\(relativ to the Elisp directory of PHW).

The path can either be an absolute path or a path relative to the directory
where the Elisp files of PHW are.

Normally there should be no need to change this option!"
  :group 'phw-help
  :type 'file)

(defcustom phw-help-html-path
  (if (not phw-running-xemacs)
      (concat phw-help-html-subdir phw-help-html-start-file)
    (cond ((file-exists-p
            (concat phw-phw-dir
                    phw-help-html-subdir
                    phw-help-html-start-file))
           (concat phw-help-html-subdir phw-help-html-start-file))
          ((file-exists-p
            (concat phw-phw-dir
                    "../../html/"
                    phw-help-html-start-file))
           (concat "../../html/" phw-help-html-start-file))
          ((file-exists-p
            (concat phw-phw-dir
                    "../../html/phw/index.html"))
           "../../html/phw/index.html")
          (t
           (concat "../../etc/phw/html/" phw-help-html-start-file))))
  "*Path where the PHW online help in HTML format resides.
This must be the location of the file \"index.html\" which comes with the PHW
distribution. If is installed by unpacking the archive available on the PHW
web-site then this is the subdir `phw-help-html-subdir' of the installation
directory of PHW. If it is installed as XEmacs-package \(e.g. via the package
manager of XEmacs) then this is probably either the directory \"../../html/\" or
\"../../etc/phw/html/\" \(both relative to the Elisp directory of PHW).

The path can either be an absolute path or a path relative to the directory
where the Elisp files of PHW are.

Normally there should be no need to change this option!"
  :group 'phw-help
  :type 'file)


(defun phw-info (info-file &optional no-file-not-exist-err)
  "Starts `info' with INFO-FILE. If INFO-FILE does not exists then nil is
returned otherwise true. If NO-FILE-NOT-EXIST-ERR is not nil then just nil is
returned if INFO-FILE does not exist otherwise an error is reported."
  (if (file-exists-p info-file)
      (prog1 t
        (info info-file))
    (unless no-file-not-exist-err
      (phw-error "Info file %s does not exists!" info-file))
    nil))

(defun phw-browse-html-file (html-file &optional no-file-not-exist-err)
  "Opens HTML-FILE in the standard-webbrowser with `browse-url'. If INFO-FILE
does not exists then nil is returned otherwise true. If NO-FILE-NOT-EXIST-ERR
is not nil then just nil is returned if HTML-FILE does not exist otherwise an
error is reported."
  (if (file-exists-p html-file)
      (prog1 t
        (if (and (locate-library "browse-url")
                 (require 'browse-url)
                 (fboundp 'browse-url))
            (browse-url (concat "file://" html-file)
                        (if (boundp 'browse-url-new-window-flag)
                            browse-url-new-window-flag
                          browse-url-new-window-p))
          (phw-error "Function 'browse-url needed for displaying HTML!")))
    (unless no-file-not-exist-err
      (phw-error "HTML file %s does not exists!" html-file))
    nil))

;;;###autoload
(defun phw-show-help (&optional format)
  "Shows the online help of PHW in Info or HTML-format.
The format depends on the setting in `phw-show-help-format'. If called with
prefix argument, i.e. if FORMAT is not nil then the user is prompted to choose
the format of the help \(Info or Html).

If an error about not finding the needed help-file occurs please take a look
at the options `phw-help-info-start-file' and `phw-help-html-start-file'!

Note: If you got PHW as a standard XEmacs-package maybe the
HTML-online-documentation is not included."
  (interactive "P")
  (let ((f (if format
               (intern (phw-query-string "Choose format of online-help:"
                                         (if (equal 'phw-show-help-format
                                                    'html)
                                             '("info" "html")
                                           '("html" "info"))))
             phw-show-help-format))
        (info-path-abs (expand-file-name
                        (save-match-data
                          (if (or (string-match "^\\." phw-help-info-path)
                                  (string-match (concat "^"
                                                        (regexp-quote
                                                         phw-help-info-start-file))
                                                phw-help-info-path))
                              (concat phw-phw-dir phw-help-info-path)
                            phw-help-info-path))))
        (html-path-abs (expand-file-name
                        (save-match-data
                          (if (or (string-match "^\\." phw-help-html-path)
                                  (string-match (concat "^"
                                                        (regexp-quote
                                                         phw-help-html-start-file))
                                                phw-help-html-path))
                              (concat phw-phw-dir phw-help-html-path)
                            phw-help-html-path)))))
    (if (equal f 'info)
        (phw-info info-path-abs)
      (message "Opening PHW online-help in a web-browser...")
      (phw-browse-html-file html-path-abs))))


;;
;; Problem reporting functions stolen from JDEE
;;
(defvar phw-problem-report-mail-address "phw-list@lists.sourceforge.net" )

(defconst phw-problem-report-message
  "Please enter the details of your bug report here")

(defun phw-submit-problem-report()
  "Submit a problem report for the PHW to the PHW mailing-list.
This command generates in the edit-window a problem-report which contains
already the current values of all PHW options, the current backtrace-buffer if
there is any and the current message-buffer. You will be asked for a
problem-report subject and then you must insert a description of the problem.
Please describe the problem as detailed as possible!

*IMPORTANT*: Cause of extra appearance of SPAM in the mailing-lists,
SourceForge has changed its policy: Now it is only possible to post to the
mailing-list for users who have subscribed this mailing-list. So please be
aware you will not be able to send comments, bug reports and improvement
suggestions before you have subscribed the PHW-mailing-list. See the section
\"Mailing-list\" at the PHW-website at http://phw.sourceforge.net how to do
this."

  (interactive)
  (when (or phw-minor-mode
            (y-or-n-p "PHW should be active when submitting a problem-report. Force report? "))
    (if (and (equal phw-frame (selected-frame))
             (not (phw-point-in-edit-window-number)))
        (phw-select-edit-window))
    (if (not (locate-library "reporter"))
        (phw-error "You need the reporter.el package to submit a bugreport for PHW!")
      (require 'reporter)
      (progn
        (message "Preparing problem report...")
        ;;prepare the basic buffer
        (reporter-submit-bug-report
         phw-problem-report-mail-address
         (format "PHW: %s, CEDET: %s, semantic: %s, eieio: %s, speedbar: %s, JDEE: %s"
                 phw-version
                 cedet-version
                 semantic-version
                 eieio-version
                 speedbar-version
                 (if (boundp 'jde-version)
                     jde-version
                   "No JDEE"))
         (phw-problem-report-list-all-variables)
         nil
         'phw-problem-report-post-hook
         phw-problem-report-message)
        (if (equal phw-frame (selected-frame))
            (phw-redraw-layout))
        (mail-subject)
        (insert (read-string "Problem report subject: "
                             (format "PHW-%s -- " phw-version)))
        (mail-text)
        (search-forward phw-problem-report-message)
        (end-of-line)
        (message "Preparing bug report...done")))))

(defun phw-problem-report-post-hook()
  "Function run the reporter package done its work. It looks for a message- and
a backtrace-buffer and inserts the contents of that."
  (save-excursion
    (goto-char (point-min))
    ;; if the mail-packages has already inserted a signature we must not go to
    ;; the buffer-end but just before the signature
    (if (re-search-forward "^--[ \t]*$" nil t)
        (progn
          (beginning-of-line)
          (insert "\n\n\n")
          (forward-line -2))
      (goto-char (point-max))
      (insert "\n\n"))
    ;; phw-faces
    (let ((phw-face-list (delq nil (mapcar (function
                                            (lambda (f)
                                              (if (save-match-data
                                                    (string-match "^phw-"
                                                                  (symbol-name f)))
                                                  f
                                                nil)))
                                           (face-list)))))
      (insert "\n\n-----------------------------------------------------\n")
      (insert "The attributes of the PHW-faces are:\n\n")
      (dolist (f phw-face-list)
        (when f
          (insert (format "%s: %s\n"
                          (symbol-name f)
                          (funcall (if phw-running-xemacs
                                       'face-custom-attributes-get
                                     'custom-face-attributes-get)
                                   f phw-frame)))))
      (insert "\n-----------------------------------------------------\n\n"))
    (let* ((messages-buffer 
	    (get-buffer
	     (if phw-running-xemacs " *Message-Log*" "*Messages*")))
	   (backtrace-buffer (phw-buffer-obj "*Backtrace*"))
           (tag-dump-buffer (phw-buffer-obj "*phw-tag-dump*")))

      ;;insert the contents of the tag-dump buffer if it is there. 
      (insert "\n\n-----------------------------------------------------\n")
      (if tag-dump-buffer
          (progn
            (insert "The contents of the *phw-tag-dump* buffer were\n\n")
	    (insert-buffer-substring tag-dump-buffer)
            ;; we must force the mark
	    ;;(goto-char (mark t))
            (insert "\nEnd Insert *phw-tag-dump* buffer" ))
        (insert "There was no *phw-tag-dump* buffer" ))
      (insert "\n-----------------------------------------------------\n\n")

      ;;insert the contents of the trace-output buffer if it is there. 
;;       (insert "\n\n-----------------------------------------------------\n")
;;       (if tag-dump-buffer
;;           (progn
;;             (insert "The contents of the *phw-tag-dump* buffer were\n\n")
;; 	    (insert-buffer-substring tag-dump-buffer)
;;             ;; we must force the mark
;; 	    (goto-char (mark t))
;;             (insert "\nEnd Insert *phw-tag-dump* buffer" ))
;;         (insert "There was no *phw-tag-dump* buffer" ))
;;       (insert "\n-----------------------------------------------------\n\n")

      ;;insert the contents of the backtrace buffer if it is there. 
      (insert "\n\n-----------------------------------------------------\n")
      (if backtrace-buffer
          (progn
            (insert "The contents of the *Backtrace* buffer were\n\n")
	    (insert-buffer-substring backtrace-buffer)
            ;; we must force the mark
	    ;;(goto-char (mark t))
            (insert "\nEnd Insert *Backtrace* buffer" ))
        (insert "There was no *Backtrace* buffer" ))
      (insert "\n-----------------------------------------------------\n\n")

      ;;insert the contents of the messages buffer if it is there. 
      (insert "-----------------------------------------------------\n")
      (if messages-buffer
          (progn
            (insert "The contents of the *Messages* buffer were\n\n")
	    (insert-buffer-substring messages-buffer)
	    ;;(goto-char (mark t))
            (insert "\nEnd Insert *Messages* buffer" ))
        (insert "There was no *Messages* buffer" ))
      (insert  "\n-----------------------------------------------------\n\n"))))


(defun phw-problem-report-list-all-variables()
  "List all variables starting with `phw-' and some other variables which
could be interesting for support."
  (let ((emacs-vars (sort (delete nil
                                  `(pre-command-hook
                                    post-command-hook
                                    after-save-hook
                                    help-mode-hook
                                    compilation-mode-hook
                                    truncate-partial-width-windows
                                    truncate-lines
                                    ,(if (boundp 'compilation-window-height)
                                         'compilation-window-height)
                                    ,(if (boundp 'temp-buffer-max-height)
                                         'temp-buffer-max-height)
                                    auto-mode-alist
                                    ,(if (boundp 'c-mode-hook)
                                         'c-mode-hook)
                                    ,(if (boundp 'c++-mode-hook)
                                         'c++-mode-hook)
                                    ,(if (boundp 'c-mode-common-hook)
                                         'c-mode-common-hook)
                                    ,(if (boundp 'java-mode-hook)
                                         'java-mode-hook)
                                    ,(if (boundp 'jde-mode-hook)
                                         'jde-mode-hook)
                                    system-type
                                    window-system
                                    max-specpdl-size
                                    max-lisp-eval-depth
                                    ,(if (boundp 'ediff-quit-hook)
                                         'ediff-quit-hook)))
                          (function (lambda (l r)
                                      (phw-string< (symbol-name l)
                                                   (symbol-name r))))))
        (semantic-vars (sort (delete nil
                                     `(semantic-after-toplevel-cache-change-hook
                                       semantic-after-partial-cache-change-hook
                                       semantic-format-face-alist
                                       semantic-uml-colon-string
                                       semantic-orphaned-member-metaparent-type))
                             (function (lambda (l r)
                                         (phw-string< (symbol-name l)
                                                      (symbol-name r))))))
        (speedbar-vars (sort '(speedbar-dynamic-tags-function-list
                               speedbar-tag-hierarchy-method
                               speedbar-tag-group-name-minimum-length
                               speedbar-tag-split-minimum-length
                               speedbar-tag-regroup-maximum-length
                               speedbar-fetch-etags-command
                               speedbar-fetch-etags-arguments
                               speedbar-fetch-etags-parse-list)
                             (function (lambda (l r)
                                         (phw-string< (symbol-name l)
                                                      (symbol-name r))))))
        (phw-options (mapcar
                      'intern
                      (sort
                       (let (completion-ignore-case)
                         (all-completions "phw-" obarray 'user-variable-p))
                       'phw-string<)))
        (phw-internal-vars (sort '(phw-path-selected-directory
                                   phw-path-selected-source
                                   phw-use-semantic-grouping
                                   phw-autocontrol/sync-fcn-register
                                   phw-idle-timer-alist
                                   phw-post-command-hooks
                                   phw-pre-command-hooks
                                   phw-max-specpdl-size-old
                                   phw-max-lisp-eval-depth-old
                                   phw-minor-mode
                                   phw-adviced-function-sets
                                   phw-adviced-functions
                                   phw-last-window-config-before-deactivation
                                   phw-edit-area-creators
                                   phw-partial-reparse-always-full-fetch
                                   phw-stealthy-function-list
                                   phw-stealthy-function-state-alist
                                   phw-windows-hidden-state
                                   phw-toggle-layout-state
                                   phw-tree-buffer-creators
                                   phw-phw-buffer-registry
                                   phw-current-maximized-phw-buffer-name
                                   phw-special-phw-buffers-of-current-layout)
                                 (function (lambda (l r)
                                             (phw-string< (symbol-name l)
                                                          (symbol-name r)))))))
    (append emacs-vars semantic-vars speedbar-vars
            phw-internal-vars phw-options)))


(silentcomp-provide 'phw-help)

;; phw, 2015, 2015-help.el ends here
