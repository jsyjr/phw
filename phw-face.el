;;; phw-face.el --- all face-options of PHW

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

;; $Id: phw-face.el,v 1.26 2009/06/04 08:38:15 berndl Exp $

;;; Commentary:

;; This file contains all options with type 'face and all face-definitions of
;; PHW.

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(silentcomp-defun set-face-parent)
(silentcomp-defun make-face-bold)
(silentcomp-defun make-face)
(silentcomp-defun set-face-foreground)

(defgroup phw-face-options nil
  "Settings for all faces used in PHW."
  :group 'phw
  :prefix "phw-")

(defgroup phw-faces nil
  "Definitions of all PHW-faces"
  :group 'phw-face-options 
  :group 'faces
  :prefix "phw-")

(defmacro phw-face-default (&optional height bold-p italic-p
                                      inherit
                                      fg-light-col fg-dark-col
                                      bg-light-col bg-dark-col
                                      fg-rest bg-rest
                                      reverse-video-p)
  "Macro for setting default values for an PHW face.
The parameters are set for the following display-types:
- ((class color) (background light)): HEIGHT, BOLD-P, ITALIC-P, INHERIT
                                      FG-LIGHT-COL, BG-LIGHT-COL
- ((class color) (background dark)): HEIGHT, BOLD-P, ITALIC-P, INHERIT
                                     FG-DARK-COL, BG-DARK-COL
- t: HEIGHT, BOLD-P, ITALIC-P, INHERIT, FG-REST, BG-REST, REVERSE-VIDEO."
  `(list (list '((class color) (background light))
               (append (if (and ,height (not phw-running-xemacs)) (list :height ,height))
                       (if ,bold-p (if (not phw-running-xemacs)
                                       (list :weight 'bold)
                                     (list :bold t)))
                       (if ,italic-p (if (not phw-running-xemacs)
                                         (list :slant 'italic)
                                       (list :italic t)))
                       (if (and ,inherit (not phw-running-xemacs)) (list :inherit ,inherit))
                       (if ,fg-light-col (list :foreground ,fg-light-col))
                       (if ,bg-light-col (list :background ,bg-light-col))))
         (list '((class color) (background dark))
               (append (if (and ,height (not phw-running-xemacs)) (list :height ,height))
                       (if ,bold-p (if (not phw-running-xemacs)
                                       (list :weight 'bold)
                                     (list :bold t)))
                       (if ,italic-p (if (not phw-running-xemacs)
                                         (list :slant 'italic)
                                       (list :italic t)))
                       (if (and ,inherit (not phw-running-xemacs)) (list :inherit ,inherit))
                       (if ,fg-dark-col (list :foreground ,fg-dark-col))
                       (if ,bg-dark-col (list :background ,bg-dark-col))))
         (list 't (append (if (and ,height (not phw-running-xemacs)) (list :height ,height))
                          (if ,bold-p (if (not phw-running-xemacs)
                                          (list :weight 'bold)
                                        (list :bold t)))
                          (if ,italic-p (if (not phw-running-xemacs)
                                            (list :slant 'italic)
                                          (list :italic t)))
                          (if (and ,inherit (not phw-running-xemacs)) (list :inherit ,inherit))
                          (if ,fg-rest (list :foreground ,fg-rest))
                          (if ,bg-rest (list :foreground ,bg-rest))
                          (if ,reverse-video-p (list :reverse-video t))))))

(defface phw-default-general-face (phw-face-default 1.0)
  "*Basic face for all PHW tree-buffers.
It큦 recommended to define here the font-family, the font-size, the basic
color etc.

In GNU Emacs 21.X all faces \(even the face 'phw-default-highlight-face') used
in the PHW tree-buffers inherit from this face. Therefore the default
attributes like font etc. of a face used in a tree-buffer can be very easily
changed with face 'phw-default-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature but the options
`phw-directories-general-face', `phw-sources-general-face',
`phw-methods-general-face' and `phw-history-general-face' offer the choice to
use the face 'phw-default-general-face' so also with XEmacs and GNU Emacs 20.X
the basic face-settings can be easily changed just by customizing the face
'phw-default-general-face'!"
  :group 'phw-faces)

(defface phw-tree-guide-line-face (phw-face-default 1.0 nil nil
                                                    'phw-default-general-face
                                                    "grey" "gray")
  "*Face for the guide-lines in the tree-buffers."
  :group 'phw-faces)

(defcustom phw-tree-guide-line-face 'phw-tree-guide-line-face
  "*Face for the guide-lines in the tree-buffers."
  :group 'phw-face-options)

(defface phw-directories-general-face (phw-face-default 1.0 nil nil
                                                        'phw-default-general-face)
  "*Basic face for the PHW directories buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'phw-faces)

(defcustom phw-directories-general-face 'phw-default-general-face
  "*Basic face for the PHW directories buffer.
This defines the basic face the whole directory buffer should displayed with.
If the face 'phw-default-general-face' is used then the display of all
PHW-tree-buffers can be changed by modifying only the face
'phw-default-general-face'.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-directories
  :type '(radio (const :tag "Use phw-default-general-face"
                       :value phw-default-general-face)
                (face :tag "Special face"
                      :value phw-directories-general-face)))

(defface phw-sources-general-face (phw-face-default 1.0 nil nil
                                                    'phw-default-general-face)
  "*Basic face for the PHW sources buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'phw-faces)

(defcustom phw-sources-general-face 'phw-default-general-face
  "*Basic face for the PHW sources buffer.
This defines the basic face the whole sources buffer should displayed with. If
the face 'phw-default-general-face' is used then the display of all
PHW-tree-buffers can be changed by modifying only the face
'phw-default-general-face'.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-sources
  :type 'face
  :type '(radio (const :tag "Use phw-default-general-face"
                       :value phw-default-general-face)
                (face :tag "Special face"
                      :value phw-sources-general-face)))

(defface phw-methods-general-face (phw-face-default 1.0 nil nil
                                                    'phw-default-general-face)
  "*Basic face for the PHW methods buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'phw-faces)

(defcustom phw-methods-general-face 'phw-default-general-face
  "*Basic face for the PHW methods buffer.
This defines the basic face the whole methods buffer should displayed with. If
the face 'phw-default-general-face' is used then the display of all
PHW-tree-buffers can be changed by modifying only the face
'phw-default-general-face'.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-methods
  :type '(radio (const :tag "Use phw-default-general-face"
                       :value phw-default-general-face)
                (face :tag "Special face"
                      :value phw-methods-general-face)))

(defface phw-history-general-face (phw-face-default 1.0 nil nil
                                                    'phw-default-general-face)
  "*Basic face for the PHW history buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'phw-faces)

(defcustom phw-history-general-face 'phw-default-general-face
  "*Basic face for the PHW history buffer.
This defines the basic face the whole history buffer should displayed with. If
the face 'phw-default-general-face' is used then the display of all
PHW-tree-buffers can be changed by modifying only the face
'phw-default-general-face'.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-history
  :type '(radio (const :tag "Use phw-default-general-face"
                       :value phw-default-general-face)
                (face :tag "Special face"
                      :value phw-history-general-face)))

(defface phw-history-bucket-node-face (phw-face-default nil nil nil
                                                        'phw-bucket-node-face)
  "*Face used for displaying a bucket-node in the PHW-history-buffer.

In GNU Emacs 21.X this face inherits from the face 'phw-bucket-node-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the PHW-history-buffer should be displayed with the same basic
attributes set by 'phw-bucket-node-face' this set of basic attributes have
to be set in 'phw-history-bucket-node-face' too!"
  :group 'phw-faces)

(defcustom phw-history-bucket-node-face 'phw-history-bucket-node-face
  "*Basic face for displaying a bucket-node in the PHW-history-buffer.
This defines the basic face for the bucket-nodes used to bucketize the
history-entries as defined with the option `phw-history-make-buckets'.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-history
  :type '(radio (const :tag "Use phw-bucket-node-face"
                       :value phw-bucket-node-face)
                (face :tag "Special face"
                      :value phw-history-bucket-node-face)))

(defface phw-history-bucket-node-dir-soure-path-face (phw-face-default nil nil nil
                                                                       'phw-history-bucket-node-face
                                                                       ;; DarkCyan would be good too
                                                                       "DarkMagenta"
                                                                       "DarkMagenta")
  "*Face used for displaying the source-path part in a directory-bucket.

In GNU Emacs 21.X this face inherits from the face 'phw-history-bucket-node-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the PHW-history-buffer should be displayed with the same basic
attributes set by 'phw-history-bucket-node-face' this set of basic attributes have
to be set in 'phw-history-bucket-node-dir-soure-path-face' too!"
  :group 'phw-faces)

(defcustom phw-history-bucket-node-dir-soure-path-face 'phw-history-bucket-node-dir-soure-path-face
  "*Basic face for displaying the source-path part in a directory bucket.
This defines the face for the source-path part in a bucket-node of the
history-buffer if `phw-history-make-buckets' is set to
'directory-with-source-path. For all other values of
`phw-history-make-buckets' this option takes no effect.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-history
  :type '(radio (const :tag "Use phw-history-bucket-node-face"
                       :value phw-history-bucket-node-face)
                (face :tag "Special face"
                      :value phw-history-bucket-node-dir-soure-path-face)))

(defface phw-history-indirect-buffer-face (phw-face-default nil nil t
                                                            'phw-history-general-face)
  "*Define a face for displaying indirect buffers in the history buffer.

In GNU Emacs 21.X this face inherits from the face 'phw-history-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the PHW-history-buffer should be displayed with the same basic
attributes set by 'phw-history-general-face' this set of basic attributes have
to be set in 'phw-history-indirect-buffer-face' too!"
  :group 'phw-faces)
 
(defcustom phw-history-indirect-buffer-face 'phw-history-indirect-buffer-face
  "*Face for indirect buffers in the history buffer."
  :group 'phw-history
  :group 'phw-face-options
  :type '(radio (const :tag "Use phw-history-general-face"
                       :value phw-history-general-face)
                (face :tag "Special face"
                      :value phw-history-indirect-buffer-face)))

(defface phw-history-dead-buffer-face (phw-face-default nil nil nil
                                                        'phw-history-general-face
                                                        "gray60"
                                                        "gray60"
                                                        nil nil
                                                        nil "gray60")
  "*Define a face for history entries pointing to dead buffers.

In GNU Emacs 21.X this face inherits from the face 'phw-history-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the PHW-history-buffer should be displayed with the same basic
attributes set by 'phw-history-general-face' this set of basic attributes have
to be set in 'phw-history-dead-buffer-face' too!"
  :group 'phw-faces)
 
(defcustom phw-history-dead-buffer-face 'phw-history-dead-buffer-face
  "*Face for history entries pointing to dead buffers."
  :group 'phw-history
  :group 'phw-face-options
  :type '(radio (const :tag "Use phw-history-general-face"
                       :value phw-history-general-face)
                (face :tag "Special face"
                      :value phw-history-dead-buffer-face)))


(defface phw-default-highlight-face (phw-face-default nil nil nil
                                                      nil ;'phw-default-general-face
                                                      "yellow" nil
                                                      "cornflower blue" "magenta"
                                                      nil nil t)
  "*Define basic face for highlighting the selected node in a tree-buffer.
In GNU Emacs 21.X all highlighting faces in the PHW tree-buffers inherit from
this face. Therefore the default attributes like font etc. of a face used in a
tree-buffer for highlighting the current tag can be very easily changed with
face 'phw-default-highlight-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature but the options
`phw-directory-face', `phw-source-face', `phw-method-face' and
`phw-history-face' offer the choice to use the face
'phw-default-highlight-face' so also with XEmacs and GNU Emacs 20.X the basic
face-settings can be easily changed just by customizing the face
'phw-default-highlight-face'!"
  :group 'phw-faces)

(defface phw-directory-face (phw-face-default nil nil nil
                                              'phw-default-highlight-face
                                              "yellow" nil
                                              "cornflower blue" "magenta"
                                              nil nil t)
  "*Define face used for highlighting current dir in directories buffer."
  :group 'phw-faces)

(defcustom phw-directory-face 'phw-default-highlight-face
  "*Face used for highlighting current directory in the directories-buffer.
If the face 'phw-default-highlight-face' is used then the display of all
PHW-tree-buffers can be changed by modifying only the face
'phw-default-highlight-face'.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-directories
  :type '(radio (const :tag "Use phw-default-highlight-face"
                       :value phw-default-highlight-face)
                (face :tag "Special face"
                      :value phw-directory-face)))

(defface phw-source-face (phw-face-default nil nil nil
                                           'phw-default-highlight-face
                                           "yellow" nil
                                           "cornflower blue" "magenta"
                                           nil nil t)
  "*Define face used for highlighting current source in the sources buffer."
  :group 'phw-faces)

(defcustom phw-source-face 'phw-default-highlight-face
  "*Face used for highlighting current source in the sources buffer.
If the face 'phw-default-highlight-face' is used then the display of all
PHW-tree-buffers can be changed by modifying only the face
'phw-default-highlight-face'.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-sources
  :type '(radio (const :tag "Use phw-default-highlight-face"
                       :value phw-default-highlight-face)
                (face :tag "Special face"
                      :value phw-sources-face)))

(defface phw-method-face (phw-face-default nil nil nil
                                           'phw-default-highlight-face
                                           "yellow" nil
                                           "cornflower blue" "magenta"
                                           nil nil t)
  "*Define face used for highlighting current tag in the methods buffer."
  :group 'phw-faces)

(defcustom phw-method-face 'phw-default-highlight-face
  "*Face used for highlighting current tag in the methods buffer.
If the face 'phw-default-highlight-face' is used then the display of all
PHW-tree-buffers can be changed by modifying only the face
'phw-default-highlight-face'.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-methods
  :type '(radio (const :tag "Use phw-default-highlight-face"
                       :value phw-default-highlight-face)
                (face :tag "Special face"
                      :value phw-method-face)))

(defface phw-method-non-semantic-face (phw-face-default nil nil nil
                                                        'phw-methods-general-face
                                                        "brown" "brown")
  "*Define face used for displaying tags of non-semantic-sources."
  :group 'phw-faces)

(defcustom phw-method-non-semantic-face 'speedbar-tag-face
  "*Face used for for displaying tags of non-semantic-sources.
Default is the face used by speedbar for tags.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-methods
  :type '(radio (const :tag "Use phw-default-highlight-face"
                       :value phw-default-highlight-face)
                (face :tag "Special face"
                      :value phw-method-face)))

(defface phw-history-face (phw-face-default nil nil nil
                                            'phw-default-highlight-face
                                            "yellow" nil
                                            "cornflower blue" "magenta"
                                            nil nil t)
  "*Define face used for highlighting current entry in the history buffer."
  :group 'phw-faces)

(defcustom phw-history-face 'phw-default-highlight-face
  "*Face used for highlighting current entry in the history buffer.
If the face 'phw-default-highlight-face' is used then the display of all
PHW-tree-buffers can be changed by modifying only the face
'phw-default-highlight-face'.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-history
  :type '(radio (const :tag "Use phw-default-highlight-face"
                       :value phw-default-highlight-face)
                (face :tag "Special face"
                      :value phw-history-face)))

(defface phw-analyse-face (phw-face-default nil nil nil
                                            'phw-default-highlight-face
                                            "yellow" nil
                                            "cornflower blue" "magenta"
                                            nil nil t)
  "*Define face used for highlighting current entry in the analyse buffer."
  :group 'phw-faces)

(defcustom phw-analyse-face 'phw-default-highlight-face
  "*Face used for highlighting current entry in the analyse buffer.
If the face 'phw-default-highlight-face' is used then the display of all
PHW-tree-buffers can be changed by modifying only the face
'phw-default-highlight-face'.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-analyse
  :type '(radio (const :tag "Use phw-default-highlight-face"
                       :value phw-default-highlight-face)
                (face :tag "Special face"
                      :value phw-analyse-face)))

(defface phw-analyse-general-face (phw-face-default 1.0 nil nil
                                                    'phw-default-general-face)
  "*Basic face for the PHW analyse buffer.
It큦 recommended to define here the font-family, the font-size, the basic
color etc."
  :group 'phw-faces)

(defcustom phw-analyse-general-face 'phw-default-general-face
  "*Basic face for the PHW analyse buffer.
This defines the basic face the whole history buffer should displayed with. If
the face 'phw-default-general-face' is used then the display of all
PHW-tree-buffers can be changed by modifying only the face
'phw-default-general-face'.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-analyse
  :type '(radio (const :tag "Use phw-default-general-face"
                       :value phw-default-general-face)
                (face :tag "Special face"
                      :value phw-analyse-general-face)))

(defface phw-analyse-bucket-element-face (phw-face-default nil nil nil
                                                           'phw-analyse-general-face
                                                           "brown")
  "*Face used for displaying elements of buckets in the PHW-analyse-buffer.

In GNU Emacs 21.X this face inherits from the face 'phw-default-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the PHW-buffers should be displayed with the same basic
attributes set by 'phw-default-general-face' this set of basic attributes have
to be set in 'phw-analyse-bucket-node-face' too!"
  :group 'phw-faces)

(defcustom phw-analyse-bucket-element-face 'phw-analyse-bucket-element-face
  "*Basic face for displaying elements of buckets in the PHW-analyse-buffer.
This defines the basic face for the elements of category-buckets like Context,
Prefix, Completions etc. in the PHW-analyse-buffer.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-analyse
  :type '(radio (const :tag "Use phw-default-general-face"
                       :value phw-default-general-face)
                (face :tag "Special face"
                      :value phw-analyse-bucket-element-face)))

(defface phw-analyse-bucket-node-face (phw-face-default nil t nil
                                                        'phw-bucket-node-face)
  "*Face used for displaying a bucket-node in the PHW-analyse-buffer.

In GNU Emacs >= 21.X this face inherits from the face 'phw-bucket-node-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the PHW-buffers should be displayed with the same basic
attributes set by 'phw-bucket-node-face' this set of basic attributes have
to be set in 'phw-analyse-bucket-node-face' too!"
  :group 'phw-faces)

(defcustom phw-analyse-bucket-node-face 'phw-analyse-bucket-node-face
  "*Basic face for displaying a bucket-node in the PHW-analyse-buffer.
This defines the basic face for the bucket-nodes like Context, Prefix,
Completions etc. in the PHW-analyse-buffer.

Changes take first effect after finishing and reactivating PHW!"
  :group 'phw-face-options
  :group 'phw-analyse
  :type '(radio (const :tag "Use phw-bucket-node-face"
                       :value phw-bucket-node-face)
                (face :tag "Special face"
                      :value phw-analyse-bucket-node-face)))

(defface phw-symboldef-symbol-face (phw-face-default nil t nil
                                                     'phw-default-general-face)
  "*Define face used for the symbol itself in the symboldef-buffer.

In GNU Emacs >= 21.X this face inherits from the face 'phw-default-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the PHW-buffers should be displayed with the same basic
attributes set by 'phw-default-general-face' this set of basic attributes have
to be set in 'phw-analyse-bucket-node-face' too!"
  :group 'phw-faces)

(defcustom phw-symboldef-symbol-face 'use-font-lock-face
  "*Which face should be used for the symbol itself in the symboldef-buffer.

There are two choices: Either a face or the special symbol 'use-font-lock-face
whereas the latter one means that PHW uses that face font-lock uses for
fontifying the symbol \(e.g. font-lock-function-name-face for a function
symbol or font-lock-variable-name-face for a variable symbol)."
  :group 'phw-face-options
  :group 'phw-symboldef
  :type '(radio (const :tag "No special face" :value nil)
                (const :tag "Use font-lock face"
                       :value use-font-lock-face)
                (face :tag "Use face"
                      :value phw-symboldef-symbol-face)))
  
(defface phw-symboldef-prototype-face (phw-face-default nil t nil
                                                        'phw-default-general-face)
  "*Define face used for the prototype of symbol in the symboldef-buffer.

In GNU Emacs >= 21.X this face inherits from the face 'phw-default-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the PHW-buffers should be displayed with the same basic
attributes set by 'phw-default-general-face' this set of basic attributes have
to be set in 'phw-analyse-bucket-node-face' too!"
  :group 'phw-faces)

(defcustom phw-symboldef-prototype-face nil
  "*Which face should be used for the symbol prototype in the symboldef-buffer.

The prototype can be the function prototype in case of a function or method or
- in case of a variable - the type of the variable \(in case of a typed
language)."
  :group 'phw-face-options
  :group 'phw-symboldef
  :type '(radio (const :tag "No special face" :value nil)
                (const :tag "Use phw-symboldef-prototype-face"
                       :value phw-symboldef-prototype-face)
                (face :tag "Special face")))

(defface phw-tag-header-face (phw-face-default nil nil nil nil nil nil
                                               "SeaGreen1" "SeaGreen1"
                                               nil nil t)
  "*Define face used for highlighting the tag header.
The tag header is the first line of the tag which is highlighted after
jumping to it by clicking onto a node in the methods buffer."
  :group 'phw-faces)
  
(defcustom phw-tag-header-face 'phw-tag-header-face
  "*Face used for highlighting the tag header.
The tag header is the first line of the tag which is highlighted after
jumping to it by clicking onto a node in the methods buffer."
  :group 'phw-face-options
  :group 'phw-methods
  :type 'face)

(defface phw-source-in-directories-buffer-face (phw-face-default nil nil nil
                                                                 'phw-directories-general-face
                                                                 "medium blue"
                                                                 "LightBlue1"
                                                                 nil nil
                                                                 nil "gray")
  "*Define a face for displaying sources in the directories buffer."
  :group 'phw-faces)
 
(defcustom phw-source-in-directories-buffer-face 'phw-source-in-directories-buffer-face
  "*Face for source files in the directories buffer."
  :group 'phw-directories
  :group 'phw-face-options
  :type 'face)

(defface phw-source-read-only-face (phw-face-default nil nil t
                                                     'phw-default-general-face)
  "*Define a face for displaying read-only sources."
  :group 'phw-faces)
 
(defcustom phw-source-read-only-face 'phw-source-read-only-face
  "*Face for read-only sources."
  :group 'phw-sources
  :group 'phw-directories
  :group 'phw-face-options
  :type 'face)

(defface phw-directory-not-accessible-face (phw-face-default nil nil nil
                                                             'phw-directories-general-face
                                                             "gray60"
                                                             "gray60"
                                                             nil nil
                                                             nil "gray60")
  "*Define a face for displaying not accessible dirs in the directories buffer."
  :group 'phw-faces)
 
(defcustom phw-directory-not-accessible-face 'phw-directory-not-accessible-face
  "*Face for not accessible dirs in the directories buffer."
  :group 'phw-directories
  :group 'phw-face-options
  :type 'face)

(defface phw-type-tag-class-face (phw-face-default nil t)
  "*Define face used with option `phw-type-tag-display'."
  :group 'phw-faces)

(defface phw-type-tag-interface-face (phw-face-default nil t)
  "*Define face used with option `phw-type-tag-display'."
  :group 'phw-faces)

(defface phw-type-tag-struct-face (phw-face-default nil t)
  "*Define face used with option `phw-type-tag-display'."
  :group 'phw-faces)

(defface phw-type-tag-typedef-face (phw-face-default nil t)
  "*Define face used with option `phw-type-tag-display'."
  :group 'phw-faces)

(defface phw-type-tag-union-face (phw-face-default nil t)
  "*Define face used with option `phw-type-tag-display'."
  :group 'phw-faces)

(defface phw-type-tag-enum-face (phw-face-default nil t)
  "*Define face used with option `phw-type-tag-display'."
  :group 'phw-faces)

(defface phw-type-tag-group-face (phw-face-default nil t nil nil
                                                   (if phw-running-xemacs
                                                       "dimgray"
                                                     "dim gray")
                                                   (if phw-running-xemacs
                                                       "dimgray"
                                                     "dim gray"))
  "*Define face used with option `phw-type-tag-display'."
  :group 'phw-faces)

(defface phw-bucket-node-face (phw-face-default nil t nil
                                                'phw-default-general-face)
  "*Face used for displaying bucket-nodes in the PHW-buffers.
See also `phw-bucket-node-display'.

In GNU Emacs 21.X this face inherits from the face 'phw-default-general-face'.

With XEmacs and GNU Emacs 20.X there is no inheritance-feature so if the
buckets in the PHW-buffers should be displayed with the same basic
attributes set by 'phw-default-general-face' this set of basic attributes have
to be set in 'phw-bucket-node-face' too!"
  :group 'phw-faces)

;; - mode-line faces-------------------------------------------

;; For XEmacs a face in the modeline should really inhertit from the face
;; 'modeline!
;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Currently with XEmacs 21.4.X
;; set-face-parent MUST be before defface - therefore we have to use make-face
;; first and then adding the values to this face we would have also added
;; by`defface. The defface is here only used to make this face customizable!
;; Maybe later XEmacs-versions support the parent-keyword with defface then we
;; can change back this ugly hack.
(when phw-running-xemacs
  (make-face 'phw-mode-line-win-nr-face)
  (set-face-parent 'phw-mode-line-win-nr-face 'modeline nil '(default))
  (make-face-bold 'phw-mode-line-win-nr-face))
(defface phw-mode-line-win-nr-face (phw-face-default nil t)
  "*Define face for the window-number in the mode-line.
See `phw-mode-line-display-window-number'."
  :group 'phw-faces)

(defcustom phw-mode-line-win-nr-face 'phw-mode-line-win-nr-face
  "*Face used for the window-number in the mode-line.
See `phw-mode-line-display-window-number'. For XEmacs the face should inherit
from the face 'modeline \(see `set-face-parent')!"
  :group 'phw-mode-line
  :group 'phw-face-options
  :type 'face)

(when phw-running-xemacs
  (make-face 'phw-mode-line-prefix-face)
  (set-face-parent 'phw-mode-line-prefix-face 'modeline nil '(default))
  (set-face-foreground 'phw-mode-line-prefix-face "forestgreen"))
;;                        nil '(default color win)))
(defface phw-mode-line-prefix-face (phw-face-default nil nil nil nil
                                                     "forestgreen"
                                                     "forestgreen")
  "*Define face for the prefix in the mode-line.
See `phw-mode-line-prefixes'."
  :group 'phw-faces)

(defcustom phw-mode-line-prefix-face 'phw-mode-line-prefix-face
  "*Face used for the prefix in the mode-line.
See `phw-mode-line-prefixes'. For XEmacs the face should inherit from the face
'modeline \(see `set-face-parent')!"
  :group 'phw-mode-line
  :group 'phw-face-options
  :type 'face)

(when phw-running-xemacs
  (make-face 'phw-mode-line-data-face)
  (set-face-parent 'phw-mode-line-data-face 'modeline nil '(default)))
(defface phw-mode-line-data-face (phw-face-default)
  "*Define face for the data in the mode-line.
See `phw-mode-line-data'."
  :group 'phw-faces)

(defcustom phw-mode-line-data-face 'phw-mode-line-data-face
  "*Face used for the data in the mode-line.
See `phw-mode-line-data'. For XEmacs the face should inherit from the face
'modeline \(see `set-face-parent')!"
  :group 'phw-mode-line
  :group 'phw-face-options
  :type 'face)

(silentcomp-provide 'phw-face)

;;; phw, 2015, 2015-face.el ends here
