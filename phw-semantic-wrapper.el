;;; phw-semantic-wrapper.el -- define wrappers for all semantic funcs/vars

;; Copyright (C) 2000 - 2005, 2015 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2003

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

;; This file contains wrappers for every semantic-function and -variable used
;; by PHW independent which semantic version is used. So the PHW-code is
;; independent from the fact, if semantic 2.0 offers backward-compatibility or
;; not. This library offers for each variable V of semantic a getter-function
;; named "phw--V" and for each function F an alias named "phw--F". V and F
;; follow the naming conventiones of semantic 2.0 but the resulting functions
;; always point to the correct variable or function of semantic independent
;; which semantic version is loaded. PHW only uses the functions exported from
;; phw-semantic-wrapper.el!


(require 'semantic)
(if (locate-library "semantic-ctxt")
    (progn
      (require 'semantic-ctxt)
      (require 'semantic-analyze))
  (progn
    (require 'semantic/ctxt)
    (require 'semantic/analyze)))

(defconst phw-semantic-2-loaded (string-match "^2" semantic-version))
(defconst phw-semantic-2-beta-nr (if (and phw-semantic-2-loaded
                                          (string-match "\\(beta\\|pre\\)\\([1-9]\\).*"
                                                        semantic-version))
                                     (string-to-number
                                      (match-string 2 semantic-version))
                                   -1))

(eval-when-compile
  (require 'silentcomp))

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

;; semantic 1.X does not have this
(silentcomp-defvar semanticdb-search-system-databases)
(silentcomp-defvar semantic-format-use-images-flag)
(silentcomp-defvar ezimage-use-images)

;; -- getter functions for all variables of semantic currently used by PHW ---

(defsubst phw--semantic-symbol->name-assoc-list ()
  "Return the value of `semantic-symbol->name-assoc-list'."
  (symbol-value 'semantic-symbol->name-assoc-list))

(defsubst phw--semantic-symbol->name-assoc-list-for-type-parts ()
  "Return the value of `semantic-symbol->name-assoc-list-for-type-parts'."
  (symbol-value 'semantic-symbol->name-assoc-list-for-type-parts))

(defsubst phw--semantic-format-tag-functions ()
  "Return either the value of `semantic-format-tag-functions' or
`semantic-token->text-functions' depending which semantic version is loaded."
  (if (boundp 'semantic-format-tag-functions)
      (symbol-value 'semantic-format-tag-functions)
    (symbol-value 'semantic-token->text-functions)))

(defsubst phw--semantic-orphaned-member-metaparent-type ()
  "Return the value of `semantic-orphaned-member-metaparent-type'."
  (symbol-value 'semantic-orphaned-member-metaparent-type))

(defsubst phw--semantic-uml-colon-string ()
  "Return the value of `semantic-uml-colon-string'."
  (symbol-value 'semantic-uml-colon-string))

(defsubst phw--semantic-format-face-alist ()
  "Return either the value of `semantic-format-face-alist' or
`semantic-face-alist' depending which semantic version is loaded."
  (if (boundp 'semantic-format-face-alist)
      (symbol-value 'semantic-format-face-alist)
    (symbol-value 'semantic-face-alist)))

(defsubst phw--semantic-after-toplevel-cache-change-hook ()
  "Return the hook-symbol `semantic-after-toplevel-cache-change-hook'."
  'semantic-after-toplevel-cache-change-hook)

(defsubst phw--semantic-after-partial-cache-change-hook ()
  "Return the hook-symbol `semantic-after-partial-cache-change-hook'."
  'semantic-after-partial-cache-change-hook)

(defsubst phw--semantic--before-fetch-tags-hook ()
  (if (boundp 'semantic--before-fetch-tags-hook)
      'semantic--before-fetch-tags-hook
    'semantic-before-toplevel-bovination-hook))

(defsubst phw--ezimage-use-images ()
  (if (boundp 'ezimage-use-images)
      ezimage-use-images))

(defsubst phw--semantic-format-use-images-flag ()
  (if (boundp 'semantic-format-use-images-flag)
      semantic-format-use-images-flag))

;; -- an alias for all functions of semantic currently used by PHW ---

(defconst phw--semantic-function-alist
  '((semantic-active-p                        . semantic-active-p)
    (semantic-token-function-args             . semantic-tag-function-arguments)
    (semantic-token-type-parts                . semantic-tag-type-members)
    (semantic-something-to-stream             . semantic-something-to-tag-table)
    (semantic-find-nonterminal-by-overlay     . semantic-find-tag-by-overlay)
    ;; here both functions return a list of tags!
    (semantic-find-nonterminal-by-token       . semantic-find-tags-by-class)
    (semantic-find-nonterminal-by-name        . semantic-brute-find-first-tag-by-name)
    (semantic-current-nonterminal-parent      . semantic-current-tag-parent)
    (semantic-find-nonterminal                . semantic-go-to-tag)
    (semantic-adopt-external-members          . semantic-adopt-external-members)
    (semantic-bucketize                       . semantic-bucketize)
    (semantic-clear-toplevel-cache            . semantic-clear-toplevel-cache)
    (semantic-colorize-text                   . semantic--format-colorize-text)
    (semantic-current-nonterminal             . semantic-current-tag)
    (semantic-equivalent-tokens-p             . semantic-equivalent-tag-p)
    (semantic-find-dependency                 . semantic-dependency-tag-file)
    (semantic-find-documentation              . semantic-documentation-for-tag)
    (semantic-flex-start                      . semantic-lex-token-start)
    (semantic-nonterminal-children            . semantic-tag-children-compatibility)
    (semantic-nonterminal-protection          . semantic-tag-protection)
    (semantic-overlay-live-p                  . semantic-overlay-live-p)
    (semantic-overlay-p                       . semantic-overlay-p)
    (semantic-token-buffer                    . semantic-tag-buffer)
    (semantic-token-end                       . semantic-tag-end)
    (semantic-token-extra-spec                . semantic-tag-get-attribute)
    (semantic-token-function-parent           . semantic-tag-function-parent)
    (semantic-token-get                       . semantic--tag-get-property)
    (semantic-token-name                      . semantic-tag-name)
    (semantic-token-overlay                   . semantic-tag-overlay)
    (semantic-token-overlay-cdr               . semantic--tag-overlay-cdr)
    (semantic-token-p                         . semantic-tag-p)
    (semantic-token-put                       . semantic--tag-put-property)
    (semantic-token-start                     . semantic-tag-start)
    (semantic-token-token                     . semantic-tag-class)
    (semantic-token-type                      . semantic-tag-type)
    (semantic-token-type-parent-superclass    . semantic-tag-type-superclass)
    (semantic-token-type-parent-implement     . semantic-tag-type-interfaces)
    (semantic-token-with-position-p           . semantic-tag-with-position-p)
    (semantic-analyze-current-context         . semantic-analyze-current-context)
    (semantic-analyze-possible-completions    . semantic-analyze-possible-completions)
    (semantic-get-local-arguments             . semantic-get-local-arguments)
    (semantic-analyze-token-type              . semantic-analyze-tag-type))
  "Alist where the car is a function of semantic 1.X and the cdr is the
equivalent new function of semantic 2.X. This alist should contain every
function PHW uses from the semantic library.")

(defconst phw--semantic-format-function-alist
  '((semantic-name-nonterminal                  . semantic-format-tag-name)
    (semantic-abbreviate-nonterminal            . semantic-format-tag-abbreviate)
    (semantic-summarize-nonterminal             . semantic-format-tag-summarize)
    (semantic-prototype-nonterminal             . semantic-format-tag-prototype)
    (semantic-concise-prototype-nonterminal     . semantic-format-tag-concise-prototype)
    (semantic-uml-abbreviate-nonterminal        . semantic-format-tag-uml-abbreviate)
    (semantic-uml-prototype-nonterminal         . semantic-format-tag-uml-prototype)
    (semantic-uml-concise-prototype-nonterminal . semantic-format-tag-uml-concise-prototype)
    (semantic-prin1-nonterminal                 . semantic-format-tag-prin1))
"Alist where the car is a function of semantic 1.X and the cdr is the
equivalent new function of semantic 2.X. This alist should contain every
function of `semantic-token->text-functions' (rsp. for semantic 2.X
`semantic-format-tag-functions'.")

(defconst phw--semanticdb-function-alist
  '((semanticdb-minor-mode-p             . semanticdb-minor-mode-p)
    (semanticdb-full-filename            . semanticdb-full-filename))
  "Alist where the car is a function of semanticdb 1.X and the cdr is the
equivalent new function of semanticdb 2.X. This alist should contain every
function PHW uses from the semanticdb library.")
  
;; new let us create the aliase. Each alias has the name "phw--"<function of
;; semantic 2.0>.
(dolist (f-elem (append phw--semantic-function-alist
                        phw--semantic-format-function-alist
                        phw--semanticdb-function-alist))
  (defalias (intern (concat "phw--" (symbol-name (cdr f-elem))))
    (if (fboundp (cdr f-elem))
        (cdr f-elem)
      (car f-elem))))


(defsubst phw--semantic-tag (name class &rest ignore)
  "Create a new semantic tag with name NAME and tag-class CLASS."
  (if (fboundp 'semantic-tag)
      (apply 'semantic-tag name class ignore)
    (list name class nil nil nil nil)))

(with-no-warnings(defsubst phw--semantic-tag-new-variable (name type default-value &rest attributes)
  "Create a semantic tag of class variable"
  (if (fboundp 'semantic-tag-new-variable)
      (apply 'semantic-tag-new-variable name type default-value attributes)
    (list name 'variable nil nil nil nil))))

(defsubst phw--semantic--tag-set-overlay (tag overlay)
  "Set the overlay part of TAG with OVERLAY. OVERLAY can be an overlay or an
unloaded buffer representation."
  (let ((o-cdr (phw--semantic--tag-overlay-cdr tag)))
    (setcar o-cdr overlay)))

(defsubst phw--semantic-tag-calculate-parent (tag)
  "Attempt to calculate the parent-tag of TAG."
  (if (fboundp 'semantic-tag-calculate-parent)
      (apply 'semantic-tag-calculate-parent (list tag))
    (with-current-buffer (phw--semantic-tag-buffer tag)
      (goto-char (phw--semantic-tag-start tag))
      (phw--semantic-current-tag-parent))))

(cond ((fboundp 'semantic-tag-static-p)
       (defalias 'phw--semantic-tag-static-p 'semantic-tag-static-p))
      ((fboundp 'semantic-tag-static)
       (defalias 'phw--semantic-tag-static-p 'semantic-tag-static))
      ((fboundp 'semantic-nonterminal-static)
       (defalias 'phw--semantic-tag-static-p 'semantic-nonterminal-static))
      (t
       (defsubst phw--semantic-tag-static-p (tag &optional parent)
         nil)))

(cond ((fboundp 'semantic-tag-abstract-p)
       (defalias 'phw--semantic-tag-abstract-p 'semantic-tag-abstract-p))
      ((fboundp 'semantic-tag-abstract)
       (defalias 'phw--semantic-tag-abstract-p 'semantic-tag-abstract))
      ((fboundp 'semantic-nonterminal-abstract)
       (defalias 'phw--semantic-tag-abstract-p 'semantic-nonterminal-abstract))
      (t
       (defsubst phw--semantic-tag-abstract-p (tag &optional parent)
         nil)))

(if (fboundp 'semantic-tag-prototype-p)
    (defalias 'phw--semantic-tag-prototype-p 'semantic-tag-prototype-p)
  (defsubst phw--semantic-tag-prototype-p (tag)
    (phw--semantic-tag-get-attribute tag (if (> phw-semantic-2-beta-nr 1)
                                             :prototype-flag
                                           'prototype))))

(if (fboundp 'semantic-tag-faux-p)
    (defalias 'phw--semantic-tag-faux-p 'semantic-tag-faux-p)
  (defsubst phw--semantic-tag-faux-p (tag)
    (phw--semantic-tag-get-attribute tag (if (> phw-semantic-2-beta-nr 1)
                                             :faux-flag
                                           'faux))))

(if (fboundp 'semantic-tag-function-constructor-p)
    (defalias 'phw--semantic-tag-function-constructor-p
      'semantic-tag-function-constructor-p)
  (defsubst phw--semantic-tag-function-constructor-p (tag)
    (phw--semantic-tag-get-attribute tag (if (> phw-semantic-2-beta-nr 1)
                                             :constructor-flag
                                           'constructor))))
    
(if (fboundp 'semantic-tag-function-destructor-p)
    (defalias 'phw--semantic-tag-function-destructor-p
      'semantic-tag-function-destructor-p)
  (defsubst phw--semantic-tag-function-destructor-p (tag)
    (phw--semantic-tag-get-attribute tag (if (> phw-semantic-2-beta-nr 1)
                                             :destructor-flag
                                           'destructor))))
    
    
(defsubst phw--semantic-fetch-tags (&optional check-cache)
  (if (fboundp 'semantic-fetch-tags)
      (apply 'semantic-fetch-tags nil)
    (apply 'semantic-bovinate-toplevel (list check-cache))))

(if (fboundp 'semantic-fetch-available-tags)
    (defalias 'phw--semantic-fetch-available-tags 'semantic-fetch-available-tags)
  (defsubst phw--semantic-fetch-available-tags ()
    (if (boundp 'semantic--buffer-cache)
	semantic--buffer-cache
      semantic-toplevel-bovine-cache)))

(if (fboundp 'semantic-tag-components)
    (defalias 'phw--semantic-tag-components
      'semantic-tag-components)
  (defun phw--semantic-tag-components (tag)
    (case (phw--semantic-tag-class tag)
      (type (phw--semantic-tag-type-members tag))
      (function (phw--semantic-tag-function-arguments tag))
      (otherwise nil))))

(if (fboundp 'semantic-flatten-tags-table)
    (defalias 'phw--semantic-flatten-tags-table
      'semantic-flatten-tags-table)
  (defun phw--semantic-flatten-tags-table (&optional table)
    "Flatten the tags table TABLE.
All tags in TABLE, and all components of top level tags
in TABLE will appear at the top level of list.
Tags promoted to the top of the list will still appear
unmodified as components of their parent tags."
    (let* ((table (phw--semantic-something-to-tag-table table))
           ;; Initialize the starting list with our table.
           (lists (list table)))
      (mapc (lambda (tag)
              (let ((components (phw--semantic-tag-components tag)))
                (if (and components
                         ;; unpositined tags can be hazardous to
                         ;; completion.  Do we need any type of tag
                         ;; here? - EL
                         (phw--semantic-tag-with-position-p (car components)))
                    (setq lists (cons
                                 (phw--semantic-flatten-tags-table components)
                                 lists)))))
            table)
      (apply 'append (nreverse lists))
      )))

;; Klaus Berndl <klaus.berndl@sdm.de>: Here we must make a list of tags by
;; hand for semantic 1.4!!
(if (fboundp 'semantic-find-tags-by-name)
    (defalias 'phw--semantic-find-tags-by-name
      'semantic-find-tags-by-name)
  (defsubst phw--semantic-find-tags-by-name (name &optional table)
    (list (phw--semantic-brute-find-first-tag-by-name name table))))

;;; semanticdb-API Functions
;;
;; Once you have a search result, use these routines to operate
;; on the search results at a higher level

(if (fboundp 'semanticdb-find-tags-by-name)
    (defalias 'phw--semanticdb-find-tags-by-name
      'semanticdb-find-tags-by-name)
  (defun phw--semanticdb-find-tags-by-name (name &optional path find-file-match)
    "Runs `semanticdb-find-nonterminal-by-name' with SEARCH-PARTS is nil."
    (apply 'semanticdb-find-nonterminal-by-name
           (list name path nil nil nil find-file-match))))

(if (fboundp 'semanticdb-deep-find-tags-by-name)
    (defalias 'phw--semanticdb-deep-find-tags-by-name
      'semanticdb-deep-find-tags-by-name)
  (defun phw--semanticdb-deep-find-tags-by-name (name &optional path find-file-match)
    "Runs `semanticdb-find-nonterminal-by-name' with SEARCH-PARTS is t."
    (apply 'semanticdb-find-nonterminal-by-name
           (list name path t nil nil find-file-match))))

(if (fboundp 'semanticdb-brute-deep-find-tags-by-name)
    (defalias 'phw--semanticdb-brute-deep-find-tags-by-name
      'semanticdb-brute-deep-find-tags-by-name)
  (defun phw--semanticdb-brute-deep-find-tags-by-name (name &optional
                                                            path find-file-match)
    "In semantic 1.4 all searches are brutish, so it runs just
    `semanticdb-find-nonterminal-by-name' with SEARCH-PARTS is t."
    (phw--semanticdb-deep-find-tags-by-name name path find-file-match)))


(if (fboundp 'semanticdb-strip-find-results)
    (defalias 'phw--semanticdb-strip-find-results
      'semanticdb-strip-find-results)
  (defun phw--semanticdb-strip-find-results (results)
    "Strip a semanticdb search RESULTS to exclude objects.
This makes it appear more like the results of a `semantic-find-' call."
    (apply #'append (mapcar #'cdr results))))

(if (fboundp 'semanticdb-find-result-length)
    (defalias 'phw--semanticdb-find-result-length
      'semanticdb-find-result-length)
  (defun phw--semanticdb-find-result-length (result)
    "Number of tags found in RESULT."
    (let ((count 0))
      (mapc (lambda (onetable)
              (setq count (+ count (1- (length onetable)))))
            result)
      count)))

(if (fboundp 'semanticdb-find-result-nth)
    (defalias 'phw--semanticdb-find-result-nth 'semanticdb-find-result-nth)
  (defun phw--semanticdb-find-result-nth (result n)
    "In result, return the nth search result.
This is a 0 based search result, with the first match being element 0.

The returned value is a cons cell: (TAG . TABLE) where TAG
is the tag at the nth position.  TABLE is the semanticdb table where
the TAG was found.  Sometimes TABLE can be nil."
    (let ((ans nil)
          (anstable nil))
      ;; Loop over each single table hit.
      (while (and (not ans) result)
        ;; For each table result, get local length, and modify
        ;; N to be that much less.
        (let ((ll (length (cdr (car result))))) ;; local length
          (if (> ll n)
              ;; We have a local match.
              (setq ans (nth n (cdr (car result)))
                    anstable (car (car result)))
            ;; More to go.  Decrement N.
            (setq n (- n ll))))
        ;; Keep moving.
        (setq result (cdr result)))
      (cons ans anstable))))


(silentcomp-provide 'phw-semantic-wrapper)

;;; phw-semantic-wrapper.el end here
