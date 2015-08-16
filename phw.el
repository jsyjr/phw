;;; phw.el --- a code browser for Emacs

;; Copyright (C) 2000-2015 Jesper Nordenberg,
;;                         Klaus Berndl,
;;                         Kevin A. Burton,
;;                         John S. Yates, Jr.,
;;                         Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: John S. Yates, Jr. <john@yates-sheets.org>
;; Keywords: window, tools

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
;; PHW stands for "Persistent Horizontal Window".
;;
;;   +------------------------------------------------+
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                  Edit-area                     |
;;   |   (can be splitted in several edit-windows)    |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   |                                                |
;;   +------------------------------------------------+
;;   |                                                |
;;   |          Persistent Horizontal Window          |
;;   |                                                |
;;   +------------------------------------------------+
;;

;;; Installation
;;
;; To use the Emacs code browser add the PHW files to your load path and add the
;; following line to your .emacs file:
;;
;; If you want autoloading PHW after first start:
;;
;;    (require 'phw-autoloads)
;;
;; or if you want loading the complete PHW:
;;
;;    (require 'phw)
;;
;; Optional: You can byte-compile PHW with `phw-byte-compile' after the
;;           PHW-package is loaded

;;; Activation

;;; Ancestry
;;
;; This package is an outgrowth (or perhaps more accurately a reduction)
;; of ECB, an elaborate emacs code browsing package with impressive
;; window management capabilities.  My interest was only in ECB's
;; ability to provide what it termed a "persistent compile window".
;; This was a horizontal window spanning the edit area along with a set
;; of mechanisms for ensuring that certain buffers consistently got
;; displayed in this window.  The collection of such buffers was always
;; more than just the output of compilation activities.  Hence I have
;; renamed that window the Persistent Horizontal Window (PHW).

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'info)

;; We need this libraries already here if we miss some requirements
(require 'phw-upgrade)
(require 'phw-util)

;; now we load all the cedet stuff
(require 'phw-cedet-wrapper)

;; if we miss some of the requirements we report an error.
(when phw-cedet-missing-libraries
  (if (phw-noninteractive)
      (phw-error "PHW is missing the libs %s of CEDET - check your CEDET-installation/setup!"
                 phw-cedet-missing-libraries)
    (phw-check-requirements)))

;; If we are here we can load PHW because at least we have installed and
;; loaded all required packages. The correct version will be checked
;; at start- or byte-compile-time


(message "PHW %s uses CEDET %s (contains semantic %s, eieio %s, speedbar %s)."
         phw-version
         (or (and (boundp 'cedet-version)
                  cedet-version)
             "<unknown version>")
         (or (and (boundp 'semantic-version)
                  semantic-version)
             "<unknown version>")
         (or (and (boundp 'eieio-version)
                  eieio-version)
             "<unknown version>")
         (or (and (boundp 'speedbar-version)
                  speedbar-version)
             "<unknown version>"))

;; rest of phw loads
(require 'tree-buffer)
(require 'phw-file-browser)
(require 'phw-method-browser)
(require 'phw-jde)
(require 'phw-layout)
(require 'phw-create-layout)
(require 'phw-mode-line)
(require 'phw-help)
(require 'phw-navigate)
(require 'phw-eshell)
(require 'phw-compilation)
(require 'phw-cycle)
(require 'phw-face)
(require 'phw-tod)
(require 'phw-speedbar)
(require 'phw-autogen)
(require 'phw-winman-support)
(require 'phw-compatibility)

;; add-ons
(require 'phw-analyse)
(require 'phw-symboldef)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

;; XEmacs
(silentcomp-defun phw-redraw-modeline)

;;====================================================
;; Variables
;;====================================================
(defvar phw-major-mode-selected-source nil
  "Major-mode of currently selected source.")

(defvar phw-item-in-tree-buffer-selected nil
  "Only true if any item in any tree-buffer has been selected in recent
command.")

(defun phw-initialize-all-internals (&optional no-caches)
  (phw-phw-buffer-registry-init)
  (setq phw-major-mode-selected-source nil
        phw-item-in-tree-buffer-selected nil)
  (phw-file-browser-initialize no-caches)
  (phw-method-browser-initialize no-caches))

;; Klaus Berndl <klaus.berndl@sdm.de>: FRAME-LOCAL
(defvar phw-minor-mode nil
  "Do not set this variable directly. Use `phw-activate' and
`phw-deactivate' or `phw-minor-mode'.!")

(defvar phw-activated-window-configuration nil
  "Window configuration used after the PHW is activated.")

;;====================================================
;; Customization
;;====================================================

(defgroup phw nil
  "Emacs code browser."
  :group 'tools
  :prefix "phw-")

(defgroup phw-general nil
  "General settings for the Emacs code browser."
  :group 'phw
  :prefix "phw-")

(defgroup phw-most-important nil
  "The most important settings of PHW you should know."
  :group 'phw
  :prefix "phw-")

(defcustom phw-use-recursive-edit nil
  "*Tell PHW to use a recursive edit.
If set then it can easily be deactivated by \(keyboard-escape-quit)."
  :group 'phw-general
  :type 'boolean)

(defcustom phw-auto-activate nil
  "*Automatically startup PHW when Emacs starts up.
This should only be true if you always want to run `phw-activate'."
  :group 'phw-general
  :group 'phw-most-important
  :type 'boolean)

(defcustom phw-activation-selects-phw-frame-if-already-active 'ask
  "*Trying to activate an already activated PHW selects the PHW-frame.
If t then the PHW-frame is selected, if nil then it is not. If 'ask then PHW
asks if the PHW-frame should be selected if the current-frame is not the
`phw-frame'."
  :group 'phw-general
  :type '(radio (const :tag "Select the PHW-frame" :value t)
                (const :tag "Ask if the PHW-frame should be selected" :value ask)
                (const :tag "Do not select the PHW-frame" :value nil)))

(defcustom phw-clear-caches-before-activate nil
  "*Clear all PHW internal caches before startup.
If t then PHW clears all its internal caches before starting up. Caches are
used for files- and subdirs \(see `phw-cache-directory-contents' and
`phw-cache-directory-contents-not') for semantic-tags and for the
history-filter.

This caches are completely empty at load-time of the PHW-library!

Default is nil, because is makes sense not to clear these caches at start-time
because PHW is often deacticated temporally especially in combination with
window-managers like escreen.el. In these situations the internal state of PHW
should be preserved for next activation."
  :group 'phw-general
  :type 'boolean)

(defcustom phw-stealthy-tasks-delay 1
  "*Time Emacs must be idle before PHW runs its stealthy tasks.
Currently PHW performes the following stealthy tasks:

  Prescann directories for emptyness: Prescann directories and display them as
  empty or not-empty in the directories-buffer. See the documentation of the
  option `phw-prescan-directories-for-emptyness' for a description.

  File is read only: Check if sourcefile-items of the directories- or
  sources-buffer are read-only or not. See documentation of the option
  `phw-sources-perform-read-only-check'.

  Version-control-state: Checks the version-control-state of files in
  directories which are managed by a VC-backend. See the option
  `phw-vc-enable-support'.

Here the interval is defined PHW has to be idle before starting with these
stealthy tasks. It can be a floating-point value in seconds. The value can
also be changed during running PHW."
  :group 'phw-general
  :group 'phw-most-important
  :type '(number :tag "Idle time before running stealthy tasks"
                 :value 1)
  :initialize 'custom-initialize-default
  :set (function (lambda (sym val)
                   (set sym val)
                   (phw-activate-phw-autocontrol-function
                    val 'phw-stealthy-updates))))



(defcustom phw-minor-mode-text " PHW"
  "*String to display in the mode line when PHW minor mode is active.
\(When the string is not empty, make sure that it has a leading space.)

Because for PHW it is quite obvious if it is active or not when the
PHW-windows are visible this text is only display in the modeline if the
PHW-windows are hidden."
  :group 'phw-general
  :type 'string)

(defcustom phw-auto-compatibility-check t
  "*Check at PHW-startup if all PHW-options have correct values.
If not nil then all PHW-options are checked if their current value have the
correct type. It the type is incorrect the option is either auto. upgraded to
the new type or reset to the default-value of current PHW if no upgrade is
possible. This feature can also upgrade options which are renamed in current
PHW and try to transform the old-value to the new named option. After startup
all upgraded or reset options are displayed with their old \(before
upgrade/reset) and new values. See also the commands `phw-upgrade-options' and
`phw-display-upgraded-options'. If this option is off then the user can
perform the check and reset manually with `phw-upgrade-options'."
  :group 'phw-general
  :type 'boolean)

(defcustom phw-version-check t
  "*Checks at start-time if the requirements are fulfilled.
It checks if the required versio of CEDET is installed and loaded into Emacs.

It is strongly recommended to set this option to not nil!"
  :group 'phw-general
  :type 'boolean)

(defcustom phw-debug-mode nil
  "*If not nil PHW displays debug-information in the Messages-buffer.
This is done for some critical situations concerning semantic-tags and their
overlays \(or extends for XEmacs). Normally you should not need this switched
on! But if you get errors like \"destroyed extend\" for XEmacs or
\"wrong-argument-type\" concerning overlays for GNU Emacs then you should
switch on this option and submitting a bug-report to the phw-mailing-list
\(`phw-submit-problem-report') after getting the error again!"
  :group 'phw-general
  :type 'boolean)

(defcustom phw-run-ediff-in-phw-frame t
  "*Run ediff-sessions in the same frame as PHW is running.
If not nil then PHW ensures that ediff runs in the same frame as PHW and PHW
restores exactly the \"before-ediff\"-window-layout after quiting ediff. If
nil then ediff decides in which frame it will run - depending on the current
window-layout \(e.g. if the phw-windows are currently hidden) this can be the
phw-frame but this can also be a newly created frame or any other frame."
  :group 'phw-general
  :type 'boolean)


(defcustom phw-activate-before-layout-draw-hook nil
  "*Hook run at the end of activating PHW by `phw-activate'.
These hooks run after all the internal setup process but directly before\(!)
drawing the layout specified in `phw-layout' \(means before dividing the frame
into several windows). A senseful using of this hook can be maximizing the
Emacs-frame for example, because this should be done before the layout is
drawn because PHW computes the size of the PHW-windows with the current frame
size! If you need a hook-option for the real end of the activating process
\(i.e. after the layout-drawing) look at `phw-activate-hook'.

IMPORTANT: The difference between this hook and
`phw-redraw-layout-before-hook' is that the latter one is evaluated always
before the layout is redrawn \(for example after calling `phw-redraw-layout')
whereas the former one \(this hook) is only evaluated exactly once during the
activation-process of PHW. So during the activation process there is the
following sequence of hooks:
1. 'phw-activate-before-layout-draw-hook' \(this one)
2. `phw-redraw-layout-before-hook'
3. <Drawing the layout>
4. `phw-redraw-layout-after-hook'
5. `phw-activate-hook'"
  :group 'phw-general
  :type 'hook)


(defcustom phw-before-activate-hook nil
  "*Hook run at the beginning of activating PHW by `phw-activate'.
These hooks run before any other tasks of the activating process are
performed. If any of these hooks returns nil then PHW will not be activated!

This can be used to check some conditions and then only start PHW if all
conditions are true. For example a function could be added which returns only
nil if Gnus is running. Then calling `phw-activate' or `phw-minor-mode' will
only start PHW if Gnus is not already running."
  :group 'phw-general
  :type 'hook)


(defcustom phw-activate-hook nil
  "*Hook run at the end of activating PHW by `phw-activate'.
These hooks run at the real end of the activating process, means after the
layout has been drawn!. If you need hooks which are run direct before the
layout-drawing look at `phw-activate-before-layout-draw-hook'."
  :group 'phw-general
  :type 'hook)

(defcustom phw-deactivate-hook nil
  "*Hook run at the end of deactivating PHW by `phw-deactivate'.
These hooks run before the phw-layout is cleared!"
  :group 'phw-general
  :type 'hook)

(defcustom phw-before-deactivate-hook nil
  "*Hook run at the beginning of deactivating PHW by `phw-deactivate'.
These hooks run before any other tasks of the deactivating process are
performed. If any of these hooks returns nil then PHW will not be deactivated!
See also `phw-before-activate-hook'."
  :group 'phw-general
  :type 'hook)


;;====================================================
;; Internals
;;====================================================

(defun phw-kill-buffer-hook ()
  "Function added to the `kill-buffer-hook' during PHW activation.
  It does several tasks:
- Depending on the value in `phw-kill-buffer-clears-history' the corresponding
  entry in the history-buffer is removed.
- Clearing the method buffer if a file-buffer has been killed.
- The entry of the removed file-buffer is removed from `phw-tag-tree-cache'."
  (let* ((curr-buf (current-buffer))
         (buffer-file (phw-fix-filename (phw-buffer-file-name curr-buf))))
    ;; this prevents automatically from killing one of the phw-buffers because
    ;; these ones are never releated to file!
    (when buffer-file
      ;; 1. clearing the history if necessary
      (phw-history-kill-buffer-clear curr-buf)

      ;; 2. clearing the method buffer if a file-buffer is killed
      (phw-rebuild-methods-buffer-with-tagcache nil nil t)

      ;; 3. removing the file-buffer from `phw-tag-tree-cache'. Must be done
      ;;    after 2. because otherwise a new element in the cache would be
      ;;    created again by `phw-rebuild-methods-buffer-with-tagcache'.
      (phw-clear-tag-tree-cache (buffer-name curr-buf)))
    (when (member curr-buf (phw-get-current-visible-phw-buffers))
      (phw-error "Killing an special PHW-buffer is not possible!"))))


(defun phw-window-sync ()
  "Synchronizes all special PHW-buffers with current buffer.
Depending on the contents of current buffer this command performs different
synchronizing tasks but only if PHW is active and point stays in an
edit-window.

- If current buffer is a file-buffer \(or an indirect-buffer with a
  file-buffer as base-buffer) then all special PHW-buffers are
  synchronized with current buffer.

- If current buffer is a dired-buffer then the directory- and
  the sources-tree-buffer are synchronized if visible

In addition to this all the synchronizing hooks \(e.g.
`phw-basic-buffer-sync-hook') run if the related phw-buffers are visible in an
phw-window."
  (interactive)
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: XXXXXXXXX remove the args!!!
  (phw-layout-window-sync))

(defun phw-customize ()
  "Open a customize-buffer for all customize-groups of PHW."
  (interactive)
  (phw-select-edit-window)
  (customize-group "phw"))

(defun phw-customize-most-important ()
  "Open a customize-buffer for the most important options of PHW."
  (interactive)
  (phw-select-edit-window)
  (customize-group "phw-most-important"))


;;====================================================
;; PHW minor mode: Create buffers & menus & maps
;;====================================================

(defun phw-menu-item (item)
  "Build an XEmacs compatible menu item from vector ITEM.
That is remove the unsupported :help stuff."
  (if phw-running-xemacs
      (let ((n (length item))
            (i 0)
            slot l)
        (while (< i n)
          (setq slot (aref item i))
          (if (and (keywordp slot)
                   (eq slot :help))
              (setq i (1+ i))
            (setq l (cons slot l)))
          (setq i (1+ i)))
        (apply #'vector (nreverse l)))
    item))

(defvar phw-menu-name "PHW")
(defvar phw-menu-bar
  (list
   phw-menu-name
   (phw-menu-item
    [ "Select PHW frame"
      phw-select-phw-frame
      :active (and phw-minor-mode
                   (not (equal (selected-frame) phw-frame)))
      :help "Select the PHW-frame."
      ])
   (phw-menu-item
    [ "Synchronize PHW windows"
      (phw-window-sync)
      :active (and (equal (selected-frame) phw-frame)
                   (phw-point-in-edit-window-number))
      :help "Synchronize the PHW windows with the current edit-window."
      ])
   (phw-menu-item
    [ "Update directories buffer"
      phw-update-directories-buffer
      :active (equal (selected-frame) phw-frame)
      :help "Updates the directories buffer with current disk-state"
      ])
   (phw-menu-item
    [ "Add all buffers to history"
      phw-add-all-buffers-to-history
      :active (and (equal (selected-frame) phw-frame)
                   (phw-window-live-p phw-history-buffer-name))
      :help "Add all current file-buffers to history"
      ])
   "-"
   (phw-menu-item
    [ "Rebuild methods buffer"
      phw-rebuild-methods-buffer
      :active (equal (selected-frame) phw-frame)
      :help "Rebuild the methods buffer completely"
      ])
   (phw-menu-item
    [ "Expand methods buffer"
      phw-expand-methods-nodes
      :active (equal (selected-frame) phw-frame)
      :help "Expand all nodes of a certain indent-level"
      ])
   (phw-menu-item
    [ "Toggle auto. expanding of the method buffer"
      phw-toggle-auto-expand-tag-tree
      :active (equal (selected-frame) phw-frame)
      :help "Toggle auto. expanding of the method buffer"
      ])
   "-"
   (phw-menu-item
    [ "Change layout"
      phw-change-layout
      :active (equal (selected-frame) phw-frame)
      :help "Change the layout."
      ])
   (phw-menu-item
    [ "Redraw layout"
      phw-redraw-layout
      :active (equal (selected-frame) phw-frame)
      :help "Redraw the current layout."
      ])
   (phw-menu-item
    [ "Toggle layout"
      phw-toggle-layout
      :active (and (equal (selected-frame) phw-frame)
                   (> (length phw-toggle-layout-sequence) 1))
      :help "Toggle between several layouts"
      ])
   (phw-menu-item
    [ "Toggle visibility of PHW windows"
      phw-toggle-phw-windows
      :active (equal (selected-frame) phw-frame)
      :help "Toggle the visibility of all PHW windows."
      ])
   (list
    "Layout administration"
    (phw-menu-item
     [ "Store current window-sizes"
       phw-store-window-sizes
       :active (equal (selected-frame) phw-frame)
       :help "Store current sizes of the phw-windows in current layout."
       ])
    (phw-menu-item
     [ "Restore sizes of the phw-windows"
       phw-restore-window-sizes
       :active (equal (selected-frame) phw-frame)
       :help "Restore the sizes of the phw-windows in current layout."
       ])
    (phw-menu-item
     [ "Restore default-sizes of the phw-windows"
       phw-restore-default-window-sizes
       :active (equal (selected-frame) phw-frame)
       :help "Restore the default-sizes of the phw-windows in current layout."
       ])
    "-"
    (phw-menu-item
     [ "Create new layout"
       phw-create-new-layout
       :active (equal (selected-frame) phw-frame)
       :help "Create a new PHW-layout."
       ])
    (phw-menu-item
     [ "Delete new layout"
       phw-delete-new-layout
       :active (equal (selected-frame) phw-frame)
       :help "Delete an user-created PHW-layout."
       ])
    "-"
    (phw-menu-item
     [ "Show help for a layout"
       phw-show-layout-help
       :active t
       :help "Show the documentation for a layout."
       ]))
   "-"
   (phw-menu-item
    [ "Toggle compile window"
      phw-toggle-compile-window
      :active (equal (selected-frame) phw-frame)
      :help "Toggle visibility of compile window."
      ])
   (phw-menu-item
    [ "Toggle enlarged compile window"
      phw-toggle-compile-window-height
      :active (and (equal (selected-frame) phw-frame)
                   phw-compile-window
                   (phw-compile-window-live-p))
      :help "Toggle enlarged compile window."
      ])
   "-"
   (list
    "Navigate"
    (phw-menu-item
     ["Previous \(back)"
      phw-nav-goto-previous
      :active t
      :help "Go to the previous navigation point"
      ])
    (phw-menu-item
     ["Next \(forward)"
      phw-nav-goto-next
      :active t
      :help "Go to the next navigation point"
      ]))
   (list
    "Goto window"
    (phw-menu-item
     ["Last selected edit-window"
      phw-goto-window-edit-last
      :active t
      :help "Go to the last selected edit-window"
      ])
    (phw-menu-item
     ["Edit-window 1"
      phw-goto-window-edit1
      :active t
      :help "Go to the first edit-window"
      ])
    (phw-menu-item
     ["Edit-window 2"
      phw-goto-window-edit2
      :active (phw-edit-window-splitted)
      :help "Go to the second edit-window \(if splitted\)"
      ])
    (phw-menu-item
     ["Directories"
      phw-goto-window-directories
      :active (phw-buffer-is-phw-buffer-of-current-layout-p phw-directories-buffer-name)
      :help "Go to the directories window"
      ])
    (phw-menu-item
     ["Sources"
      phw-goto-window-sources
      :active (phw-buffer-is-phw-buffer-of-current-layout-p phw-sources-buffer-name)
      :help "Go to the sources window"
      ])
    (phw-menu-item
     ["Methods and Variables"
      phw-goto-window-methods
      :active (phw-buffer-is-phw-buffer-of-current-layout-p phw-methods-buffer-name)
      :help "Go to the methods/variables window"
      ])
    (phw-menu-item
     ["History"
      phw-goto-window-history
      :active (phw-buffer-is-phw-buffer-of-current-layout-p phw-history-buffer-name)
      :help "Go to the history window"
      ])
    (phw-menu-item
     ["Analyse"
      phw-goto-window-analyse
      :active (phw-buffer-is-phw-buffer-of-current-layout-p phw-analyse-buffer-name)
      :help "Go to the analyse window"
      ])
    (phw-menu-item
     ["Speedbar"
      phw-goto-window-speedbar
      :active (and phw-use-speedbar-instead-native-tree-buffer
                   (phw-buffer-is-phw-buffer-of-current-layout-p phw-speedbar-buffer-name))
      :help "Go to the integrated speedbar window"
      ])
    (phw-menu-item
     ["Compilation"
      phw-goto-window-compilation
      :active (equal 'visible (phw-compile-window-state))
      :help "Go to the history window"
      ])
    )
   (list
    "Display window maximized"
    (phw-menu-item
     ["Directories"
      phw-maximize-window-directories
      :active (phw-buffer-is-phw-buffer-of-current-layout-p phw-directories-buffer-name)
      :help "Maximize the directories window - even if currently not visible"
      ])
    (phw-menu-item
     ["Sources"
      phw-maximize-window-sources
      :active (phw-buffer-is-phw-buffer-of-current-layout-p phw-sources-buffer-name)
      :help "Maximize the sources window - even if currently not visible"
      ])
    (phw-menu-item
     ["Methods and Variables"
      phw-maximize-window-methods
      :active (phw-buffer-is-phw-buffer-of-current-layout-p phw-methods-buffer-name)
      :help "Maximize the methods/variables window - even if currently not visible"
      ])
    (phw-menu-item
     ["History"
      phw-maximize-window-history
      :active (phw-buffer-is-phw-buffer-of-current-layout-p phw-history-buffer-name)
      :help "Maximize the history window - even if currently not visible"
      ])
    (phw-menu-item
     ["Analyse"
      phw-maximize-window-analyse
      :active (phw-buffer-is-phw-buffer-of-current-layout-p phw-analyse-buffer-name)
      :help "Maximize the analyse window - even if currently not visible"
      ])
    (phw-menu-item
     ["Speedbar"
      phw-maximize-window-speedbar
      :active (and phw-use-speedbar-instead-native-tree-buffer
                   (phw-buffer-is-phw-buffer-of-current-layout-p phw-speedbar-buffer-name))
      :help "Maximize the integrated speedbar window - even if not visible"
      ])
    )
   "-"
   (list
    "Preferences"
    (phw-menu-item
     ["Most important..."
      (customize-group "phw-most-important")
      :active t
      :help "Customize the most important options"
      ])
    (phw-menu-item
     ["All..."
      (phw-customize)
      :active t
      :help "Display all available option-groups..."
      ])
    "-"
    (phw-menu-item
     ["General..."
      (customize-group "phw-general")
      :active t
      :help "Customize general PHW options"
      ])
    (phw-menu-item
     ["Directories..."
      (customize-group "phw-directories")
      :active t
      :help "Customize PHW directories"
      ])
    (phw-menu-item
     ["Sources..."
      (customize-group "phw-sources")
      :active t
      :help "Customize PHW sources"
      ])
    (phw-menu-item
     ["Methods..."
      (customize-group "phw-methods")
      :active t
      :help "Customize PHW method display"
      ])
    (phw-menu-item
     ["History..."
      (customize-group "phw-history")
      :active t
      :help "Customize PHW history"
      ])
    (phw-menu-item
     ["Analyse..."
      (customize-group "phw-analyse")
      :active t
      :help "Customize PHW analyse ingeractor"
      ])
    (phw-menu-item
     ["Version control..."
      (customize-group "phw-version-control")
      :active t
      :help "Customize the version-control-support"
      ])
    (phw-menu-item
     ["Layout..."
      (customize-group "phw-layout")
      :active t
      :help "Customize PHW layout"
      ])
    (phw-menu-item
     ["Tree-buffer style and handling..."
      (customize-group "phw-tree-buffer")
      :active t
      :help "Customize the tree-buffers of PHW"
      ])
    (phw-menu-item
     ["Face options..."
      (customize-group "phw-face-options")
      :active t
      :help "Customize PHW faces"
      ])
    (phw-menu-item
     ["Help options..."
      (customize-group "phw-help")
      :active t
      :help "Customize options for the online help of PHW"
      ])
    (phw-menu-item
     ["PHW/eshell options..."
      (customize-group "phw-eshell")
      :active t
      :help "Customize options for the eshell integration of PHW"
      ])
    (phw-menu-item
     ["Supporting non-semantic-sources..."
      (customize-group "phw-non-semantic")
      :active t
      :help "Customize options for parsing non-semantic-sources"
      ])
    (phw-menu-item
     ["Supporting window-managers..."
      (customize-group "phw-winman-support")
      :active t
      :help "Customize options for the window-manager-support"
      ])
    )
   (list
    "Upgrade PHW"
    (phw-menu-item
     [ "Upgrade PHW-options to current PHW-version"
       phw-upgrade-options
       :active (equal (selected-frame) phw-frame)
       :help "Try to upgrade PHW-options to current PHW-version if necessary."
       ])
    )
   (list
    "Help"
    (phw-menu-item
     [ "Show Online Help"
       phw-show-help
       :active t
       :help "Show the online help of PHW."
       ])
    (phw-menu-item
     [ "PHW NEWS"
       (phw-display-news-for-upgrade t)
       :active t
       :help "Displays the NEWS-file of PHW."
       ])
    (phw-menu-item
     [ "List of most important options"
       (let ((phw-show-help-format 'info))
         (phw-show-help)
         (Info-goto-node "Most important options"))
       :active t
       :help "Displays a a list of options which you should know."
       ])
    (phw-menu-item
     [ "List of all options"
       (let ((phw-show-help-format 'info))
         (phw-show-help)
         (Info-goto-node "Option Index"))
       :active t
       :help "Displays an index of all user-options in the online-help."
       ])
    (phw-menu-item
     [ "List of all commands"
       (let ((phw-show-help-format 'info))
         (phw-show-help)
         (Info-goto-node "Command Index"))
       :active t
       :help "Displays an index of all commands in the online-help."
       ])
    (phw-menu-item
     [ "FAQ"
       (let ((phw-show-help-format 'info))
         (phw-show-help)
         (Info-goto-node "FAQ"))
       :active t
       :help "Show the FAQ of PHW."
       ])
    (phw-menu-item
     [ "Conflicts with other packages"
       (let ((phw-show-help-format 'info))
         (phw-show-help)
         (Info-goto-node "Conflicts and bugs"))
       :active t
       :help "What to do for conflicts with other packages."
       ])
    (phw-menu-item
     [ "Submit problem report"
       phw-submit-problem-report
       :active t
       :help "Submit a problem report to the PHW mailing list."
       ])
    (phw-menu-item
     [ "PHW Debug mode"
       (setq phw-debug-mode (not phw-debug-mode))
       :active t
       :style toggle
       :selected phw-debug-mode
       :help "Print debug-informations about parsing files in the message buffer."
       ])
    (phw-menu-item
     [ "PHW Layout Debug mode"
       (setq phw-layout-debug-mode (not phw-layout-debug-mode))
       :active t
       :style toggle
       :selected phw-layout-debug-mode
       :help "Print debug-informations about window-operations in the message buffer."
       ])
    "-"
    (phw-menu-item
     ["Help preferences..."
      (customize-group "phw-help")
      :active t
      :help "Customize options for the online help of PHW"
      ])
    "-"
    (concat "PHW " phw-version)
    )
   "-"
   (phw-menu-item
    [ "Deactivate PHW"
      phw-deactivate
      :active t
      :help "Deactivate PHW."
      ])
   )
  "Menu for PHW minor mode.")

(defun phw-add-to-minor-modes ()
  "Does all necessary to add PHW as a minor mode with current values of
`phw-mode-map' and `phw-minor-mode-text'."
  ;; PHW minor mode doesn't work w/ Desktop restore.
  ;; This line will disable this minor mode from being restored
  ;; by Desktop.
  (when (boundp 'desktop-minor-mode-handlers)
    (add-to-list 'desktop-minor-mode-handlers
		 (cons 'phw-minor-mode 'ignore)))
  (add-minor-mode 'phw-minor-mode
                  'phw-minor-mode-text phw-mode-map))

(defvar phw-mode-map nil
  "Internal key-map for PHW minor mode.")

(defcustom phw-key-map
  '("C-c ." . ((t "fh" phw-history-filter)
               (t "fs" phw-sources-filter)
               (t "fm" phw-methods-filter)
               (t "fr" phw-methods-filter-regexp)
               (t "ft" phw-methods-filter-tagclass)
               (t "fc" phw-methods-filter-current-type)
               (t "fp" phw-methods-filter-protection)
               (t "fn" phw-methods-filter-nofilter)
               (t "fl" phw-methods-filter-delete-last)
               (t "ff" phw-methods-filter-function)
               (t "p" phw-nav-goto-previous)
               (t "n" phw-nav-goto-next)
               (t "lc" phw-change-layout)
               (t "lr" phw-redraw-layout)
               (t "lw" phw-toggle-phw-windows)
               (t "lt" phw-toggle-layout)
               (t "s" phw-window-sync)
               (t "r" phw-rebuild-methods-buffer)
               (t "a" phw-toggle-auto-expand-tag-tree)
               (t "x" phw-expand-methods-nodes)
               (t "h" phw-show-help)
               (t "gl" phw-goto-window-edit-last)
               (t "g1" phw-goto-window-edit1)
               (t "g2" phw-goto-window-edit2)
               (t "gc" phw-goto-window-compilation)
               (t "gd" phw-goto-window-directories)
               (t "gs" phw-goto-window-sources)
               (t "gm" phw-goto-window-methods)
               (t "gh" phw-goto-window-history)
               (t "ga" phw-goto-window-analyse)
               (t "gb" phw-goto-window-speedbar)
               (t "md" phw-maximize-window-directories)
               (t "ms" phw-maximize-window-sources)
               (t "mm" phw-maximize-window-methods)
               (t "mh" phw-maximize-window-history)
               (t "ma" phw-maximize-window-analyse)
               (t "mb" phw-maximize-window-speedbar)
               (t "e" eshell)
               (t "o" phw-toggle-scroll-other-window-scrolls-compile)
               (t "\\" phw-toggle-compile-window)
               (t "/" phw-toggle-compile-window-height)
               (t "," phw-cycle-maximized-phw-buffers)
               (t "." phw-cycle-through-compilation-buffers)))

  "*Specifies all key-bindings for the PHW minor-mode key-map.
The value is a cons-cell where the car is a common-prefix key for all the
key-bindings. The cdr is a list of key-bindings each of them a list again. A
key-binding has the following form:

  '\(<common-prefix-flag> <keysequence> <function>) where

<common-prefix-flag>: If t then the common-prefixkey defined as car of the
                      value \(see above) is used.
<keysequence>: If the common prefixkey is used then the final key-binding is the
               concatenation of the common-prefixkey \(see above) and this
               keysequence.
<function>: The function to bind to the key. This can also be a
            lambda-expression .

It is highly recommended to use one of the standard keys C-c or C-x as first key
of your common-prefixkey!

You MUST change this option via customize to take effect!

All keysequences must be inserted as a string and must follow the syntax needed
by `read-kbd-macro' or `kbd'. This means you can insert the key in the same
manner \"C-h k\" displays keysequences. Here is the summary of the syntax:

Text is divided into \"words \" separated by whitespace. Except for the words
described below, the characters of each word go directly as characters of the
keysequence. The whitespace that separates words is ignored. Whitespace in the
macro must be written explicitly, as in \"C-c SPC\".

  * The special words RET, SPC, TAB, DEL, LFD, ESC, and NUL represent special
   control characters. The words must be written in uppercase.

  * A word in angle brackets, e.g., <return>, <down>, <left> or <f1>, represents
    a function key. \(Note that in the standard configuration, the function
    key <return> and the control key RET are synonymous.). You can use angle
    brackets on the words RET, SPC, etc., but they are not required there.

  * Keys can be written by their ASCII code, using a backslash followed by up
    to six octal digits. This is the only way to represent keys with codes
    above \377.

  * One or more prefixes M- \(meta), C- \(control), S- \(shift), A- \(alt),
    H- \(hyper), and s- \(super) may precede a character or key notation. For
    function keys, the prefixes may go inside or outside of the brackets:
    C-<down> = <C-down>. The prefixes may be written in any order: M-C-x =
    C-M-x.
    Prefixes are not allowed on multi-key words, e.g., C-abc, except that the
    Meta prefix is allowed on a sequence of digits and optional minus sign:
    M--123 = M-- M-1 M-2 M-3.

  * The `^' notation for control characters also works:  ^M = C-m."
  :group 'phw-general
  :group 'phw-most-important
  :type '(cons (choice :tag "Common prefix-key"
                       (const :tag "No common prefix-key" :value nil)
                       (string :tag "Prefix-key" :value "C-c ."))
               (repeat :tag "Key-bindings"
                       (list :tag "Key-definition"
                             (boolean :tag "o Use common prefix-key" :value t)
                             (string :tag "o Key")
                             (function :tag "o Function or lambda-expression"
                                       :value nil))))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   ;; make a mode-map and save it
                   (setq phw-mode-map
                         (let ((km (make-sparse-keymap))
                               (val-list (phw-copy-list (cdr value)))
                               keq-string)
                           (dolist (elem val-list)
                             (setq key-string (concat (if (nth 0 elem) (car value))
                                                      " " (nth 1 elem)))
                             (define-key km (read-kbd-macro key-string) (nth 2 elem)))
                           (easy-menu-define phw-minor-menu km
                             "PHW Minor Mode Menu" phw-menu-bar)
                           km))
                   ;; add the minor-mode and and the minor-mode-map to the
                   ;; alists if not already contained. In this case just
                   ;; replace the values in the alists
                   (phw-add-to-minor-modes))))

;;;###autoload
(defun phw-activate ()
  "Activates PHW and creates the special buffers for the choosen layout.
For the layout see `phw-layout-name'. This function raises always the
PHW-frame if called from another frame. This is the same as calling
`phw-minor-mode' with a positive argument."
  (interactive)
  (phw-minor-mode 1))

(defun phw-activate-internal ()
  "Activates the PHW and creates all the buffers and draws the PHW-screen
with the actually chosen layout \(see `phw-layout-name'). This function raises
always the PHW-frame if called from another frame."

  (if phw-use-recursive-edit
      (if phw-minor-mode
	  (progn
	    (message "PHW already activated. Drawing layout.")
	    (phw-redraw-layout))
	(catch 'exit
	  (progn
	    (phw-activate--impl)
	    (recursive-edit))
	  (phw-deactivate-internal)))
    (phw-activate--impl))
  phw-minor-mode)


(defvar phw-upgrade-check-done nil)

(defun phw-clean-up-after-activation-failure (msg err)
  "Complete cleanup of all PHW-setups and report an error with message MSG."
  (let ((phw-minor-mode t))
    (phw-deactivate-internal t))
  (setq phw-minor-mode nil)
  (if phw-running-xemacs
      (phw-redraw-modeline t)
    (force-mode-line-update t))
  (error "PHW %s: %s (error-type: %S, error-data: %S)" phw-version msg
         (car err) (cdr err)))

(defvar phw-last-window-config-before-deactivation nil
  "Contains the last `phw-current-window-configuration' directly before
PHW has been deactivated. Do not set this variable!")

(defvar phw-temporary-changed-emacs-variables-alist nil
  "Internal alist which stores old values of emacs variables/options which
have to be changed during running PHW. Use only `phw-modify-emacs-variable'
for modifying this alist.")

(defun phw-modify-emacs-variable (var action &optional new-value)
  "Stores or restores the old value of the Emacs-variable symbol VAR.
VAR has to be a bound symbol for a variable. ACTION is either 'store or
'restore. The optional arg NEW-VALUE is only used when ACTION is 'store and is
that value VAR should be set to. After calling with ACTION is 'restore the
value of VAR is as before storing a NEW-VALUE for variable-symbol VAR."
  (case action
    (store
     (or (phw-find-assoc var phw-temporary-changed-emacs-variables-alist)
         (progn
           (setq phw-temporary-changed-emacs-variables-alist
                 (phw-add-assoc (cons var (symbol-value var))
                                phw-temporary-changed-emacs-variables-alist))
           (set var new-value))))
    (restore
     (let ((elem (phw-find-assoc var phw-temporary-changed-emacs-variables-alist)))
       (when elem
         (set var (cdr elem))
         (setq phw-temporary-changed-emacs-variables-alist
               (phw-remove-assoc var phw-temporary-changed-emacs-variables-alist)))))))


(defun phw-activate--impl ()
  "See `phw-activate'.  This is the implementation of PHW activation."
  (when (or (null phw-frame) (not (frame-live-p phw-frame)))
    (setq phw-frame (selected-frame)))

  (if phw-minor-mode
      (when (and (not (equal (selected-frame) phw-frame))
                 (or (equal phw-activation-selects-phw-frame-if-already-active t)
                     (and (equal phw-activation-selects-phw-frame-if-already-active 'ask)
                          (y-or-n-p "PHW is already active in another frame. Select it? "))))
        (phw-select-phw-frame)
        (phw-update-directories-buffer))

    (let ((debug-on-error debug-on-error))
      ;; we activate only if all before-hooks return non nil
      (when (run-hook-with-args-until-failure 'phw-before-activate-hook)

        ;; temporary changing some emacs-vars
        (when (< max-specpdl-size 3000)
          (phw-modify-emacs-variable 'max-specpdl-size 'store 3000))
        (when (< max-lisp-eval-depth 1000)
          (phw-modify-emacs-variable 'max-lisp-eval-depth 'store 1000))
        (when (and phw-running-xemacs
                   (boundp 'progress-feedback-use-echo-area))
          (phw-modify-emacs-variable 'progress-feedback-use-echo-area 'store t))

        ;; checking if there are cedet or semantic-load problems
        (phw-check-cedet-load)
        (phw-check-semantic-load)

        ;; checking the requirements
        (phw-check-requirements)

        (condition-case err-obj
            (progn

              ;; initialize the navigate-library
              (phw-nav-initialize)

              ;; enable basic advices (we need the custom-save-all advice
              ;; already here! Maybe it would be better to remove this advice
              ;; from the basic-advices and add it to upgrade-advices.....)
              ;;(phw-enable-advices 'phw-layout-basic-adviced-functions)

              ;; we need the custom-all advice here!
              (phw-enable-advices 'phw-methods-browser-advices)

              ;; maybe we must upgrade some not anymore compatible or even renamed
              ;; options
              (when (and phw-auto-compatibility-check
                         (not phw-upgrade-check-done))
                (phw-check-not-compatible-options)
                (phw-upgrade-not-compatible-options)
                (phw-upgrade-renamed-options)
                (setq phw-upgrade-check-done t))

              ;; first initialize the whole layout-engine
              (phw-initialize-layout)

              ;; initialize internals
              (phw-initialize-all-internals (not phw-clear-caches-before-activate))

              ;; enable permanent advices - these advices will never being
              ;; deactivated after first activation of PHW unless
              ;; `phw-split-edit-window-after-start' is not 'before-activation
              ;; (see `phw-deactivate-internal')
              (phw-enable-advices 'phw-permanent-adviced-layout-functions)

              ;; enable advices for not supported window-managers
              (phw-enable-advices 'phw-winman-not-supported-function-advices)

              ;; enable advices for the compatibility with other packages
              (phw-enable-advices 'phw-compatibility-advices)

              ;; set the phw-frame
              (let ((old-phw-frame phw-frame))
                (if phw-new-phw-frame
                    (progn
                      (run-hooks 'phw-activate-before-new-frame-created-hook)
                      (setq phw-frame (make-frame))
                      (put 'phw-frame 'phw-new-frame-created t))
                  (setq phw-frame (selected-frame))
                  (put 'phw-frame 'phw-new-frame-created nil))
                ;; If PHW is acivated in a frame unequal to that frame which was
                ;; the phw-frame at last deactivation then we initialize the
                ;; `phw-edit-area-creators'.
                (if (not (equal phw-frame old-phw-frame))
                    (phw-edit-area-creators-init)))
              (raise-frame phw-frame)
              (select-frame phw-frame)

              (phw-enable-own-temp-buffer-show-function t)

              ;; now we can activate PHW

              ;; first we run all tree-buffer-creators
              (phw-tree-buffer-creators-run)

              ;; activate the eshell-integration - does not load eshell but
              ;; prepares PHW to run eshell right - if loaded and activated
              (phw-eshell-activate-integration)

              ;; we need some hooks
              (add-hook (phw--semantic-after-partial-cache-change-hook)
                        'phw-update-after-partial-reparse t)
              (add-hook (phw--semantic-after-toplevel-cache-change-hook)
                        'phw-rebuild-methods-buffer-with-tagcache t)
;;               (add-hook (phw--semantic--before-fetch-tags-hook)
;;                         'phw-prevent-from-parsing-if-exceeding-threshold)
              (phw-activate-phw-autocontrol-function phw-highlight-tag-with-point-delay
                                                     'phw-tag-sync)
              (phw-activate-phw-autocontrol-function phw-basic-buffer-sync-delay
                                                     'phw-basic-buffer-sync)
              (phw-activate-phw-autocontrol-function phw-compilation-update-idle-time
                                                     'phw-compilation-buffer-list-changed-p)
              (phw-activate-phw-autocontrol-function 'post
                                                     'phw-layout-post-command-hook)
              (phw-activate-phw-autocontrol-function 'pre
                                                     'phw-layout-pre-command-hook)
              (phw-activate-phw-autocontrol-function 0.5
                                                     'phw-repair-only-phw-window-layout)
              (phw-activate-phw-autocontrol-function 'post
                                                     'phw-handle-major-mode-visibilty)
              (add-hook 'after-save-hook 'phw-update-methods-after-saving)
              (add-hook 'kill-buffer-hook 'phw-kill-buffer-hook)

              (add-hook 'find-file-hooks 'phw-find-file-hook)

              ;; after adding all idle-timers and post- and pre-command-hooks we
              ;; activate the monitoring
              (phw-activate-phw-autocontrol-function 1 'phw-monitor-autocontrol-functions)

              ;; We activate the stealthy update mechanism
              (phw-stealthy-function-state-init)
              (phw-activate-phw-autocontrol-function phw-stealthy-tasks-delay
                                                      'phw-stealthy-updates)

              ;; running the compilation-buffer update first time
              (phw-compilation-buffer-list-init)

              ;; ediff-stuff; we operate here only with symbols to avoid bytecompiler
              ;; warnings
              (phw-activate-ediff-compatibility)

              ;; enabling the VC-support
              (phw-vc-enable-internals 1)

              (add-hook (if phw-running-xemacs
                            'activate-menubar-hook
                          'menu-bar-update-hook)
                        'phw-compilation-update-menu)
              )
          (error
           ;;          (backtrace)
           (phw-clean-up-after-activation-failure
            "Errors during the basic setup of PHW." err-obj)))

        (condition-case err-obj
            ;; run personal hooks before drawing the layout
            (run-hooks 'phw-activate-before-layout-draw-hook)
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the hooks of phw-activate-before-layout-draw-hook."
            err-obj)))

        (setq phw-minor-mode t)

        ;; now we draw the screen-layout of PHW.
        (condition-case err-obj
            ;; now we draw the layout chosen in `phw-layout'. This function
            ;; activates at its end also the adviced functions if necessary!
            ;; Here the directories- and history-buffer will be updated.
            (let ((phw-redraw-layout-quickly nil)
                  (use-last-win-conf (and phw-last-window-config-before-deactivation
                                          (equal phw-split-edit-window-after-start
                                                 'before-deactivation)
                                          (not (phw-window-configuration-invalidp
                                                phw-last-window-config-before-deactivation)))))
              (phw-enable-temp-buffer-shrink-to-fit phw-compile-window-height)
              (if use-last-win-conf
                  (setq phw-edit-area-creators
                        (nth 4 phw-last-window-config-before-deactivation)))

              (phw-redraw-layout-full 'no-buffer-sync
                                      nil
                                      (and use-last-win-conf
                                           (nth 6 phw-last-window-config-before-deactivation))
                                      (and use-last-win-conf
                                           (nth 5 phw-last-window-config-before-deactivation)))

              ;; if there was no compile-window before deactivation then we have
              ;; to hide the compile-window after activation
              (if (and use-last-win-conf
                       (null (nth 2 phw-last-window-config-before-deactivation)))
                  (phw-toggle-compile-window -1))

              (when (member phw-split-edit-window-after-start
                            '(vertical horizontal nil))
                (delete-other-windows)
                (case phw-split-edit-window-after-start
                  (horizontal (split-window-horizontally))
                  (vertical (split-window-vertically))))

              ;; now we synchronize all PHW-windows
              (phw-window-sync)

              ;; now update all the PHW-buffer-modelines
              (phw-mode-line-format)
              )
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the layout setup of PHW." err-obj))
          )

        (condition-case err-obj
            (let ((edit-window (car (phw-canonical-edit-windows-list))))
              (when (and phw-display-default-dir-after-start
                         (null (phw-buffer-file-name
                                (window-buffer edit-window))))
                (phw-set-selected-directory
                 (phw-fix-filename (with-current-buffer (window-buffer edit-window)
                                     default-directory)))))
          (error
           (phw-clean-up-after-activation-failure
            "Errors during setting the default directory." err-obj)))

        (condition-case err-obj
            ;; we run any personal hooks
            (run-hooks 'phw-activate-hook)
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the hooks of phw-activate-hook." err-obj)))

        (condition-case err-obj
            ;; enable mouse-tracking for the phw-tree-buffers; we do this after
            ;; running the personal hooks because if a user putﾴs activation of
            ;; follow-mouse.el (`turn-on-follow-mouse') in the
            ;; `phw-activate-hook' then our own PHW mouse-tracking must be
            ;; activated later. If `turn-on-follow-mouse' would be activated
            ;; after our own follow-mouse stuff, it would overwrite our
            ;; mechanism and the show-node-name stuff would not work!
            (if (phw-show-any-node-info-by-mouse-moving-p)
                (tree-buffer-activate-follow-mouse))
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the mouse-tracking activation." err-obj)))

        (setq phw-minor-mode t)
        (message "The PHW is now activated.")

        (condition-case err-obj
            ;; now we display all `phw-not-compatible-options' and
            ;; `phw-renamed-options'
            (if (and phw-auto-compatibility-check
                     (or (phw-not-compatible-or-renamed-options-detected)
                         (not (phw-options-version=phw-version-p))))
                ;; we must run this with an idle-times because otherwise these
                ;; options are never displayed when Emacs is started with a
                ;; file-argument and PHW is automatically activated. I this
                ;; case the buffer of the file-argument would be displayed
                ;; after the option-display and would so hide this buffer.
                (phw-run-with-idle-timer 0.25 nil 'phw-display-upgraded-options)
              (phw-display-news-for-upgrade))
          (error
           (phw-clean-up-after-activation-failure
            "Error during the compatibility-check of PHW." err-obj)))

        ;; if we activate PHW first time then we display the node "First steps" of
        ;; the online-manual
        (ignore-errors
          (when (null phw-source-path)
            (let ((phw-show-help-format 'info))
              (phw-show-help)
              (Info-goto-node "First steps"))))

        ;; display tip of the day if `phw-tip-of-the-day' is not nil
        (ignore-errors
          (phw-show-tip-of-the-day))

        (phw-enable-advices 'phw-layout-basic-adviced-functions)

        (condition-case err-obj
            ;;now take a snapshot of the current window configuration
            (setq phw-activated-window-configuration
                  (phw-current-window-configuration))
          (error
           (phw-clean-up-after-activation-failure
            "Errors during the snapshot of the windows-configuration." err-obj)))
        ))))


(defun phw-deactivate ()
  "Deactivates the PHW and kills all PHW buffers and windows."
  (interactive)
  (phw-minor-mode 0))

(defun phw-deactivate-internal (&optional run-no-hooks)
  "Deactivates the PHW and kills all PHW buffers and windows."
  (unless (not phw-minor-mode)

    (when (or run-no-hooks
              (run-hook-with-args-until-failure 'phw-before-deactivate-hook))

      (setq phw-last-window-config-before-deactivation
            (phw-current-window-configuration))

      ;; deactivating the adviced functions
      (dolist (adviced-set-elem phw-adviced-function-sets)
        ;; Note: as permanent defined advices-sets are not disabled here!
        (phw-disable-advices (car adviced-set-elem)))

      (phw-enable-own-temp-buffer-show-function nil)

      (phw-enable-temp-buffer-shrink-to-fit nil)

      ;; deactivate and reset the speedbar stuff
      (ignore-errors (phw-speedbar-deactivate))

      ;; deactivates the eshell-integration; this disables also the
      ;; eshell-advices!
      (phw-eshell-deactivate-integration)

      ;; For XEmacs
      (tree-buffer-activate-follow-mouse)
      (tree-buffer-deactivate-follow-mouse)

      ;; remove the hooks
      (remove-hook (phw--semantic-after-partial-cache-change-hook)
                   'phw-update-after-partial-reparse)
      (remove-hook (phw--semantic-after-toplevel-cache-change-hook)
                   'phw-rebuild-methods-buffer-with-tagcache)
;;       (remove-hook (phw--semantic--before-fetch-tags-hook)
;;                 'phw-prevent-from-parsing-if-exceeding-threshold)
      (phw-stop-all-autocontrol/sync-functions)
      (remove-hook 'after-save-hook 'phw-update-methods-after-saving)
      (remove-hook 'kill-buffer-hook 'phw-kill-buffer-hook)

      (remove-hook 'find-file-hooks 'phw-find-file-hook)

      ;; ediff-stuff
      (phw-deactivate-ediff-compatibility)

      ;; disabling the VC-support
      (phw-vc-enable-internals -1)

      (remove-hook (if phw-running-xemacs
                       'activate-menubar-hook
                     'menu-bar-update-hook)
                   'phw-compilation-update-menu)

      ;; run any personal hooks
      (unless run-no-hooks
        (run-hooks 'phw-deactivate-hook))

      ;; clear the phw-frame. Here we try to preserve the split-state after
      ;; deleting the PHW-screen-layout.
      (when (frame-live-p phw-frame)
        (raise-frame phw-frame)
        (select-frame phw-frame)
        (condition-case oops
            (let* ((config (phw-window-configuration-data))
                   (window-before-redraw (nth 0 config))
                   (pos-before-redraw (nth 1 config))
                   (edit-win-data-before-redraw (nth 2 config))
                   (edit-win-list-after-redraw nil))
              ;; first we make all windows of the PHW-frame not dedicated and
              ;; then we delete all PHW-windows
              (phw-select-edit-window)
              (phw-make-windows-not-dedicated phw-frame)

              ;; deletion of all windows. (All other advices are already
              ;; disabled!)
              (phw-with-original-permanent-layout-functions
               (delete-other-windows))

              ;; some paranoia....
              (set-window-dedicated-p (selected-window) nil)

              ;; now we restore the edit-windows as before the deactivation
              ;; (All other advices are already disabled!)
              (if (= (length edit-win-data-before-redraw)
                     (phw-edit-area-creators-number-of-edit-windows))
                  (phw-with-original-permanent-layout-functions
                   (phw-restore-edit-area))
                (phw-edit-area-creators-init))

              (setq edit-win-list-after-redraw (phw-canonical-edit-windows-list))

              ;; a safety-check if we have now at least as many windows as
              ;; edit-windows before deactivation. If yes we restore all
              ;; window-data as before deactivation.
              (when (= (length edit-win-list-after-redraw)
                       (length edit-win-data-before-redraw))
                (dotimes (i (length edit-win-data-before-redraw))
                  (let ((win (nth i edit-win-list-after-redraw))
                        (data (nth i edit-win-data-before-redraw)))
                    (set-window-buffer win (nth 0 data))
                    (set-window-start win (nth 1 data))
                    (set-window-point win (nth 2 data))
                    (if (> (length edit-win-list-after-redraw) 1)
                        (phw-set-window-size win (nth 3 data)))
                    )))

              ;; at the end we always stay in that window as before the
              ;; deactivation.
              (when (integerp window-before-redraw)
                (phw-select-edit-window window-before-redraw))
              ;; if we were in an edit-window before deactivation let us go to
              ;; the old place
              (when pos-before-redraw
                (goto-char pos-before-redraw)))
          (error
           ;; in case of an error we make all windows not dedicated and delete
           ;; at least all other windows
           (phw-warning "phw-deactivate-internal (error-type: %S, error-data: %S)"
                        (car oops) (cdr oops))
           (ignore-errors (phw-make-windows-not-dedicated phw-frame))
           (ignore-errors (delete-other-windows))))

        (if (get 'phw-frame 'phw-new-frame-created)
            (ignore-errors (delete-frame phw-frame t))))

      (phw-initialize-layout)

      ;; we do NOT disable the permanent-advices of
      ;; `phw-permanent-adviced-layout-functions' unless the user don't want
      ;; preserving the split-state after reactivating PHW.
      (when (not (equal phw-split-edit-window-after-start 'before-activation))
        (phw-disable-advices 'phw-permanent-adviced-layout-functions t)
        (phw-edit-area-creators-init))

      ;; we can safely do the kills because killing non existing buffers
      ;; doesnﾴt matter. We kill these buffers because some customize-options
      ;; takes only effect when deactivating/reactivating PHW, or to be more
      ;; precise when creating the tree-buffers again.
      (dolist (tb-elem (phw-phw-buffer-registry-name-list 'only-tree-buffers))
        (tree-buffer-destroy tb-elem))
      (phw-phw-buffer-registry-init)

      (setq phw-activated-window-configuration nil)

      (setq phw-minor-mode nil)

      ;; restoring the value of temporary modified vars
      (phw-modify-emacs-variable 'max-specpdl-size 'restore)
      (phw-modify-emacs-variable 'max-lisp-eval-depth 'restore)
      (when (and phw-running-xemacs
                 (boundp 'progress-feedback-use-echo-area))
        (phw-modify-emacs-variable 'progress-feedback-use-echo-area 'restore))))


  (if (null phw-minor-mode)
      (message "The PHW is now deactivated."))
  phw-minor-mode)

;;;###autoload
(defun phw-minor-mode (&optional arg)
  "Toggle PHW minor mode.
With prefix argument ARG, turn on if positive, otherwise off. Return non-nil
if the minor mode is enabled.

\\{phw-mode-map}"
  (interactive "P")
  (let ((new-state (if (null arg)
                       (not phw-minor-mode)
                     (> (prefix-numeric-value arg) 0))))
    (if new-state
        (phw-activate-internal)
      (phw-deactivate-internal)))

  (if phw-running-xemacs
      (phw-redraw-modeline t)
    (force-mode-line-update t))

  phw-minor-mode)


;; PHW byte-compilation

(defun phw-compile-file-if-necessary (file &optional force)
  "Compile the PHW-file FILE if necessary. This is done if FORCE is not nil or
FILE.el is newer than FILE.elc or if FILE.elc doesn't exist."
  (let ((elc-file (concat (phw-file-name-sans-extension file) ".elc")))
    (if (or force
	    (not (phw-file-exists-p elc-file))
	    (file-newer-than-file-p file elc-file))
        (byte-compile-file file))))

;;;###autoload
(defun phw-byte-compile (&optional force-all)
  "Byte-compiles the PHW package.
This is done for all lisp-files of PHW if FORCE-ALL is not nil or for each
lisp-file FILE.el which is either newer than FILE.elc or if FILE.elc doesn't
exist."
  (interactive "P")
  (phw-check-requirements)
  (let ((files (phw-directory-files (phw-file-name-directory (locate-library "phw"))
                                    t)))
    (save-excursion
      (dolist (file files)
	(if (save-match-data
              (and (string-match "\\(silentcomp\\|tree-buffer\\|phw.*\\)\\.el$" file)
                   (not (string-match "phw-autoloads" file))))
            (phw-compile-file-if-necessary file force-all))))))

(defun phw-auto-activate-hook()
  "If necessary, run `phw-activate' when Emacs is started."
  (when phw-auto-activate
    (phw-activate)))

(add-hook 'emacs-startup-hook 'phw-auto-activate-hook)

(silentcomp-defvar menu-bar-tools-menu)
(condition-case oops
    (progn
      (require 'easymenu)
      (easy-menu-add-item (if phw-running-xemacs nil menu-bar-tools-menu)
                          (if phw-running-xemacs '("tools") nil)
                          (phw-menu-item
                           [ "Start Code Browser (PHW)"
                             phw-activate
                             :active t
                             :help "Start the Emacs Code Browser."
                             ]))
      )
  (error
   (phw-warning "Not critical error during adding menu-entry to Tools-menu (error-type: %S, error-data: %S)"
                (car oops) (cdr oops))))


;; some goodies for editing the phw-elisp-code

;; parsing of our phw-macros

(eval-after-load (if (locate-library "semantic/bovine/el")
                     "el"
                   "semantic-el")
  (condition-case oops
      (when (fboundp 'semantic-elisp-setup-form-parser)
        ;; defphw-multicache
        (semantic-elisp-reuse-form-parser defvar defphw-multicache)
        ;; defphw-advice-set
        (semantic-elisp-reuse-form-parser defvar defphw-advice-set)
        ;; defphw-stealthy and tree-buffer-defpopup-command
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (symbol-name (nth 1 read-lobject)) nil nil
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 2 read-lobject))))
          defphw-stealthy
          tree-buffer-defpopup-command)
        ;; defphw-tree-buffer-creator
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (symbol-name (nth 1 read-lobject)) nil nil
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 3 read-lobject))))
          defphw-tree-buffer-creator)
        ;; defphw-window-dedicator-to-phw-buffer
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (symbol-name (nth 1 read-lobject)) nil nil
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 4 read-lobject))))
          defphw-window-dedicator-to-phw-buffer)
        ;; defphw-advice
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (symbol-name (nth 1 read-lobject)) nil
               (semantic-elisp-desymbolify
                (list '**phw-advice: (nth 2 read-lobject) (nth 3 read-lobject)))
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 4 read-lobject))))
          defphw-advice)
        ;; defphw-tree-buffer-callback
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (symbol-name (nth 1 read-lobject)) nil
               (semantic-elisp-desymbolify
                (append '(node phw-button edit-window-nr shift-mode meta-mode)
                        (nth 4 read-lobject)))
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 5 read-lobject))))
          defphw-tree-buffer-callback)
        ;; defphw-autocontrol/sync-function
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (symbol-name (nth 1 read-lobject)) nil
               (semantic-elisp-desymbolify
                (list '**autocontrol/sync_for_buffer: (nth 2 read-lobject)))
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 5 read-lobject))))
          defphw-autocontrol/sync-function)
        ;; phw-layout-define
        (semantic-elisp-setup-form-parser
            (lambda (read-lobject start end)
              (semantic-tag-new-function
               (nth 1 read-lobject) nil
               (semantic-elisp-desymbolify (list (nth 2 read-lobject)))
               :user-visible-flag nil
               :documentation (semantic-elisp-do-doc (nth 3 read-lobject))))
          phw-layout-define)
        ;; when-phw-running-... macros
        (semantic-elisp-reuse-form-parser eval-and-compile
                                          when-phw-running-xemacs
                                          when-phw-running-emacs-22
                                          when-phw-running-emacs-23
                                          when-phw-running-emacs)
        )
    (error
     (phw-warning "Not critical error during supporting parsing the phw-macros: (error-type: %S, error-data: %S)"
                  (car oops) (cdr oops)))))

;; highlighting of some phw-keywords
(condition-case oops
    (progn
      (defconst phw-font-lock-keywords
        (eval-when-compile
          (let* (
                 ;; Function declarations and exec-with-macros
                 (variable-defs '(
                                  "defphw-multicache"
                                  "defphw-advice-set"
                                  ))
                 (function-defs '(
                                  "defphw-stealthy"
                                  "defphw-tree-buffer-creator"
                                  "defphw-window-dedicator-to-phw-buffer"
                                  "defphw-advice"
                                  "defphw-autocontrol/sync-function"
                                  "defphw-tree-buffer-callback"
                                  ))
                 (plain-keywords '(
                                   "phw-exec-in-window"
                                   "phw-do-with-unfixed-phw-buffers"
                                   "phw-do-with-fixed-phw-buffers"
                                   "phw-with-original-adviced-function-set"
                                   "phw-with-original-permanent-layout-functions"
                                   "phw-with-dedicated-window"
                                   "phw-with-original-basic-functions"
                                   "phw-with-phw-advice"
                                   "phw-with-readonly-buffer"
                                   "phw-do-if-buffer-visible-in-phw-frame"
                                   "phw-when-point-in-edit-window-phw-windows-visible"
                                   "phw-layout-define"
                                   "when-phw-running-xemacs"
                                   "when-phw-running-emacs"
                                   "when-phw-running-emacs-22"
                                   "when-phw-running-emacs-23"
                                   "phw-exit-on-input"
                                   ))
                 (v-regexp (regexp-opt variable-defs t))
                 (f-regexp (regexp-opt function-defs t))
                 (k-regexp (regexp-opt plain-keywords t))
                 ;; Regexp depths
                 (v-depth (regexp-opt-depth v-regexp))
                 (f-depth (regexp-opt-depth f-regexp))
                 (k-depth (regexp-opt-depth k-regexp))
                 (full (concat
                        ;; Declarative things: the whole parenthesis expr has always
                        ;; number 1 ==> The paren-expression number for a keyword
                        ;; contained in (append variable-defs function-defs
                        ;; plain-keywords) is always 1
                        "(\\(" v-regexp "\\|" f-regexp "\\|" k-regexp "\\)"
                        ;; Whitespaces & name: The parenthesis expr for name has
                        ;; always the number
                        ;; (+ 1        -- the whole paren-expr for the declarative
                        ;;                things
                        ;;    v-depth  -- all paren-expressions of the variable-defs
                        ;;    f-depth  -- all paren-expressions of the function-defs
                        ;;    k-depth  -- all paren-expressions of the plain keywords
                        ;;    1        -- The \\(\\sw+\\)?: This is the name in case
                        ;;                of a variable- or function-def
                        ;;  )
                        ;; So variable, functions and keywords have the following
                        ;; numbers:
                        ;; - variable-match: Always 2 (The whole surrounding
                        ;;   paren-expr + the surrounding paren-expr defined with
                        ;;   regexp-opt for the variable-defs
                        ;; - function-match: 1 (for the whole surrounding
                        ;;   paren-expr) + v-depth (to jump over the paren-expr of
                        ;;   the variable-defs + 1 (the surrounding paren-expr
                        ;;   defined with regexp-opt for the function-defs
                        "\\>[ \t]*\\(\\sw+\\)?"
                        ))
                 )
            `((,full
               (1 font-lock-keyword-face)
               (,(+ 1 v-depth f-depth k-depth 1) ;; see explanation above
                (cond ((match-beginning 2) ;; see explanation above
                       font-lock-variable-name-face)
                      ((match-beginning ,(+ 1 v-depth 1)) ;; see explanation above
                       font-lock-function-name-face)
                      (t nil))
                nil t)))
            ))
        "Highlighted phw keywords.")

      (when (fboundp 'font-lock-add-keywords)
        (font-lock-add-keywords 'emacs-lisp-mode
                                phw-font-lock-keywords)
        ))
  (error
   (phw-warning "Not critical error during supporting fontifying the phw-macros: (error-type: %S, error-data: %S)"
                (car oops) (cdr oops))))


;; Klaus Berndl <klaus.berndl@sdm.de>: Cause of the magic autostart stuff of
;; the advice-package we must disable at load-time all these advices!!
;; Otherwise would just loading phw (not activating!) activate each advice
;; AFTER the FIRST usage of our advices!!

(dolist (adviced-set-elem phw-adviced-function-sets)
  (phw-disable-advices (car adviced-set-elem) t))

;; init the method- and file-browser at load-time
(phw-file-browser-initialize)
(phw-method-browser-initialize)

(silentcomp-provide 'phw)

;;; phw, 2015.el ends here
