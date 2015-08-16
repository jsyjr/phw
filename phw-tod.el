;;; phw-tod.el --- PHW tip of the day

;; Copyright (C) 2000 - 2005, 2015 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
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
;;
;; Contains code for tips of the day

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'phw-util)

(silentcomp-defvar phw-tod-cursor)

(defcustom phw-tip-of-the-day t
  "*Show tip of the day at start time of PHW."
  :group 'phw-general
  :type 'boolean)

(defcustom phw-tip-of-the-day-file "~/.phw-tip-of-day.el"
  "*File where tip-of-the-day cursor is stored."
  :group 'phw-general
  :type 'file)

(defconst phw-tod-tip-list
  '("You can expand the PHW-methods-buffer with `phw-expand-methods-nodes' [C-c . x]."
    "You can toggle between different layouts with `phw-toggle-layout' [C-c . t]."
    "You can go back to the most recent layout with [C-u] `phw-toggle-layout' [C-u C-c . t]."
    "You can toggle displaying the PHW-windows with `phw-toggle-phw-windows' [C-c . w]."
    "You can show and hide the PHW-windows on a major-mode-basis with `phw-major-modes-show-or-hide'."
    "You can maximize a certain PHW-window either via its popup-menu or with [C-x 1] in that window."
    "You can use speedbar instead of the native tree-buffers with option `phw-use-speedbar-instead-native-tree-buffer'."
    "You can speedup access for big directories with option `phw-cache-directory-contents'."
    "You can display the online help also in HTML-format with option `phw-show-help-format'."
    "You can interactively create your own layouts with the command `phw-create-new-layout'."
    "You can start the eshell in the compile-window simply with `eshell' or [C-c . e]."
    "Use the incremental search in the methods-buffer for fast node-selecting; see `phw-tree-incremental-search'."
    "You can cycle through all currently opened \"compile-buffers\" with `phw-cycle-through-compilation-buffers'."
    "You can change the window-sizes by dragging the mouse and storing the new sizes with `phw-store-window-sizes'."
    "You can get a quick overlook of all built-in layouts with `phw-show-layout-help'."
    "Browse your sources as with a web-browser with `phw-nav-goto-next' \[C-c . n], `phw-nav-goto-previous' \[C-c . p]."
    "Customize the look\&feel of the tree-buffers with `phw-tree-expand-symbol-before' and `phw-tree-indent'."
    "Customize the contents of the methods-buffer with `phw-tag-display-function', `phw-type-tag-display', `phw-show-tags'."
    "Customize the main mouse-buttons of the tree-buffers with `phw-primary-secondary-mouse-buttons'."
    "Customize with `phw-tree-do-not-leave-window-after-select' for which tree-buffers a selection doesn't leave the window."
    "Grep a directory \(recursive) by using the popup-menu \(the right mouse-button) in the directories buffer."
    "Customize the sorting of the sources with the option `phw-sources-sort-method'."
    "Narrow the source-buffer to the selected tag in the methods-buffer with `phw-tag-visit-post-actions'."
    "Enable autom. enlarging of the compile-window by select with the option `phw-compile-window-temporally-enlarge'."
    "Customize with `phw-compile-window-temporally-enlarge' the situations the compile-window is allowed to enlarge."
    "Customize the meaning of `other-window' [C-x o] with the option `phw-other-window-behavior'."
    "Customize height and width of the PHW-windows with `phw-windows-height' and `phw-windows-width'."
    "Define with `phw-compilation-buffer-names' and `phw-compilation-major-modes' which buffers are \"compile-buffers\"."
    "Customize all faces used by PHW with the customize-groups `phw-face-options' and `phw-faces'."
    "Auto-activate eshell with the option `phw-eshell-auto-activate'."
    "Get best use of big screen-displays with leftright-layouts like \"leftright1\" or \"leftright2\"."
    "Use the POWER-click in the methods-buffer to narrow the clicked node in the edit-window."
    "Use the POWER-click in the sources- and history-buffer to get only an overlook of the source-contents."
    "Exclude not important sources from being displayed in the sources-buffer with `phw-source-file-regexps'."
    "Use left- and right-arrow for smart expanding/collapsing tree-buffer-nodes; see `phw-tree-navigation-by-arrow'." ;;
    "Add personal key-bindings to the tree-buffers with `phw-common-tree-buffer-after-create-hook'."
    "Add personal key-bindings to the directories-buffer with `phw-directories-buffer-after-create-hook'."
    "Add personal key-bindings to the sources-buffer with `phw-sources-buffer-after-create-hook'."
    "Add personal key-bindings to the methods-buffer with `phw-methods-buffer-after-create-hook'."
    "Add personal key-bindings to the history-buffer with `phw-history-buffer-after-create-hook'."
    "Pop up a menu with the right mouse-button and do senseful things in the tree-buffers."
    "Extend the builtin popup-menus to your needs - see `phw-directories-menu-user-extension'."
    "Call `phw-show-help' [C-c . o] with a prefix-argument [C-u] and choose the help-format."
    "You can change the prefix [C-c .] of all PHW-key-bindings quick and easy with `phw-key-map'."
    "Send a problem-report to the PHW-mailing-list quick and easy with `phw-submit-problem-report'."
    "Switch on/off auto. expanding of the PHW-methods-buffer with `phw-auto-expand-directory-tree'."
    "You can quickly toggle auto. expanding of the PHW-methods-buffer with `phw-toggle-auto-expand-tag-tree'."
    "Highlight current semantic-tag of the edit-buffer in the PHW-methods-buffer with `phw-highlight-tag-with-point'."
    "Apply a filter to the sources-buffer either via `phw-sources-filter' or via the popup-menu."
    "Apply a filter to the history-buffer either via `phw-history-filter' or via the popup-menu."
    "Apply tag-filters (can be layered) to the methods-buffer either via `phw-methods-filter' or via the popup-menu."
    "Use `scroll-all-mode' to scroll both edit-windows of PHW simultaneously - and no other windows are scrolled!"
    "You can toggle having a compile window with `phw-toggle-compile-window' if `phw-compile-window-height' is not nil."
    "Start PHW automatically after Emacs is started. Use option `phw-auto-activate'"
    "Maximize a tree-buffer via modeline - PHW supports the standard-mechanism of (X)Emacs for deleting other windows."
    "Easy horizontal scrolling the tree-buffers with the mouse with [M-mouse-1] and [M-mouse-3]; see `phw-tree-easy-hor-scroll'."
    "Expand and collapse very precisely the current node in a tree-buffer with commands in the popup-menu."
    "Let PHW display the version-control-state of your files in the tree-buffers. See `phw-vc-enable-support'."
    "Work with remote paths (e.g. TRAMP-, ANGE-FTP-, or EFS-paths) as with local paths in `phw-source-path'."
    "Exclude certain files from being displayed in the history-buffer. See `phw-history-exclude-file-regexps'."
    "Get the most important options of PHW at a glance by viewing the customization group \"phw-most-important\"."
    )
  "List of all available tips of the day.")




(defun phw-show-tip-of-the-day ()
  "Show tip of the day if `phw-tip-of-the-day' is not nil."
  (interactive)
  (when (or (phw-interactive-p) phw-tip-of-the-day)
    (ignore-errors (load-file phw-tip-of-the-day-file))
    (let* ((cursor (if (boundp 'phw-tod-cursor)
                       phw-tod-cursor
                     0))
           (tip (or (ignore-errors (nth cursor phw-tod-tip-list))
                    (nth 0 phw-tod-tip-list))))
      ;; show tip
      (phw-message-box tip "Tip of the day" "Close")
      ;; change cursor
      (phw-tod-move-cursor cursor))))

(defun phw-tod-move-cursor (cursor)
  (with-temp-file (expand-file-name phw-tip-of-the-day-file)
    (erase-buffer)
    (insert (format "(defvar phw-tod-cursor 0)\n(setq phw-tod-cursor %d)"
                    (if (< (1+ cursor) (length phw-tod-tip-list))
                        (1+ cursor)
                      0)))))

(silentcomp-provide 'phw-tod)

;; phw-tod.el ends here
