;;; phw-layout-defs.el --- layout definitions for PHW

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2002

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
;; Contains all layout definitions for PHW
;;
;; This file is part of the PHW package which can be found at:
;; http://phw.sourceforge.net

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the PHW-package see the file NEWS.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'phw-util)
(require 'phw-layout)

;; ========= Current available layouts ===============================

;; Here come all the index layout-functions:

;; Layout left1 -----------------------------------------------------

(phw-layout-define "left1" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |      |       |                                      |
   | Sour | Hist  |                 Edit                 |
   |      |       |                                      |
   |      |       |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-directories-buffer)
  (phw-split-ver 0.3)
  (phw-set-sources-buffer)
  (phw-split-ver 0.5)
  (phw-set-methods-buffer)
  (select-window (previous-window))
  (phw-split-hor 0.5)
  (phw-set-history-buffer)
  (select-window (next-window (next-window))))

;; Layout left2 -----------------------------------------------------

(phw-layout-define "left2" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Sources     |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-directories-buffer)
  (phw-split-ver 0.5)
  (phw-set-sources-buffer)
  (select-window (next-window)))

;; Layout left3 -----------------------------------------------------

(phw-layout-define "left3" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-directories-buffer)
  (phw-split-ver 0.3)
  (phw-set-sources-buffer)
  (phw-split-ver 0.5)
  (phw-set-methods-buffer)
  (select-window (next-window)))

;; Layout left4 -----------------------------------------------------

(phw-layout-define "left4" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |      |       |                                      |
   |      |       |                                      |
   |      |       |                                      |
   | Sour | Hist  |                                      |
   |      |       |                                      |
   |      |       |                                      |
   |      |       |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-directories-buffer)
  (phw-split-ver 0.5)
  (phw-set-sources-buffer)
  (phw-split-hor 0.5)
  (phw-set-history-buffer)
  (select-window (next-window)))

;; Layout left5 -----------------------------------------------------

(phw-layout-define "left5" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  History     |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-directories-buffer)
  (phw-split-ver 0.3)
  (phw-set-sources-buffer)
  (phw-split-ver 0.5)
  (phw-set-history-buffer)
  (select-window (next-window)))

;; Layout right1 -----------------------------------------------------

(phw-layout-define "right1" right
  "This function creates the following layout:

   -------------------------------------------------------
   |                                      |              |
   |                                      |  Directories |
   |                                      |              |
   |                                      |              |
   |                                      |--------------|
   |                                      |              |
   |                                      |              |
   |             Edit                     |  Sources     |
   |                                      |              |
   |                                      |              |
   |                                      |--------------|
   |                                      |              |
   |                                      |  Methods     |
   |                                      |              |
   |                                      |              |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (let ((edit-win (previous-window (selected-window) 0)))
    (phw-set-directories-buffer)
    (phw-split-ver 0.3)
    (phw-set-sources-buffer)
    (phw-split-ver 0.5)
    (phw-set-methods-buffer)
    (select-window edit-win)))

;; Layout right2 -----------------------------------------------------

(phw-layout-define "right2" right
  "This function creates the following layout:

   -------------------------------------------------------
   |                                      |              |
   |                                      |              |
   |                                      |              |
   |                                      |  Directories |
   |                                      |              |
   |                                      |              |
   |                                      |              |
   |             Edit                     |--------------|
   |                                      |              |
   |                                      |              |
   |                                      |              |
   |                                      |  Methods     |
   |                                      |              |
   |                                      |              |
   |                                      |              |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (let ((edit-win (previous-window (selected-window) 0)))
    (phw-set-directories-buffer)
    (phw-split-ver 0.5)
    (phw-set-methods-buffer)
    (select-window edit-win)))

;; Layout left6 -----------------------------------------------------

(phw-layout-define "left6" left
  "This function creates the following layout:

   -------------------------------------------------------
   |  Sources     |                                      | 
   |--------------|                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                 Edit                 |
   |              |                                      | 
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |  History     |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-sources-buffer)
  (phw-split-ver 0.2)
  (phw-set-methods-buffer)
  (phw-split-ver 0.75)
  (phw-set-history-buffer)
  (select-window (next-window)))

;; Layout top1 -----------------------------------------------------

(phw-layout-define "top1" top
  "This function creates the following layout:

   -------------------------------------------------------
   |                        |             |              |
   |                        |             |              |
   |      Directories       |  Sources    |  Methods     |
   |                        |             |              |
   |                        |             |              |
   |-----------------------------------------------------|
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   |                    Edit                             |
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-directories-buffer)
  (phw-split-hor 0.5)
  (phw-set-sources-buffer)
  (phw-split-hor 0.5)
  (phw-set-methods-buffer)
  (select-window (next-window)))

;; Layout left7 -----------------------------------------------------

(phw-layout-define "left7" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |  History     |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. This layout works best if it is contained in
`phw-show-sources-in-directories-buffer'!"
  (phw-set-directories-buffer)
  (phw-split-ver 0.6)
  (phw-set-history-buffer)
  (phw-split-ver 0.4)
  (phw-set-methods-buffer)
  (select-window (next-window)))

;; Layout left8 -----------------------------------------------------

(phw-layout-define "left8" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |  History     |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-directories-buffer)
  (phw-split-ver 0.3)
  (phw-set-sources-buffer)
  (phw-split-ver 0.35)
  (phw-set-methods-buffer)
  (phw-split-ver 0.65)
  (phw-set-history-buffer)
  (select-window (next-window)))

;; Layout top2 -----------------------------------------------------

(phw-layout-define "top2" top
  "This function creates the following layout:

   -------------------------------------------------------
   |                                                     |
   |                                                     |
   |                    Methods                          |
   |                                                     |
   |                                                     |
   |-----------------------------------------------------|
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   |                    Edit                             |
   |                                                     |
   |                                                     |
   |                                                     |
   |                                                     |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-methods-buffer)
  (select-window (next-window)))

;; Layout left9 -----------------------------------------------------

(phw-layout-define "left9" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |   Methods    |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-methods-buffer)
  (select-window (next-window)))

;; Layout left10 -----------------------------------------------------

(phw-layout-define "left10" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |  Sou | Hist  |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then the
layout contains no persistent compilation window and the other windows get a little
more place."
  (phw-set-methods-buffer)
  (phw-split-ver 0.75)
  (phw-set-sources-buffer)
  (phw-split-hor 0.5)
  (phw-set-history-buffer)
  (select-window (next-window)))

;; Layout left11 -----------------------------------------------------

(phw-layout-define "left11" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |    Hist      |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then the
layout contains no persistent compilation window and the other windows get a little
more place."
  (phw-set-methods-buffer)
  (phw-split-ver 0.75)
  (phw-set-history-buffer)
  (select-window (next-window)))

;; Layout left12 -----------------------------------------------------

(phw-layout-define "left12" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |   History    |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-history-buffer)
  (select-window (next-window)))

;; Layout left13 -----------------------------------------------------

(phw-layout-define "left13" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   | Directories  |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. This layout works best if it is contained in
`phw-show-sources-in-directories-buffer'!"
  (phw-set-directories-buffer)
  (select-window (next-window)))

;; Layout left14 -----------------------------------------------------

(phw-layout-define "left14" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |              |                                      |
   |              |                                      |
   | Directories  |                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                                      |
   |    Hist      |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. This layout works best if it is contained in
`phw-show-sources-in-directories-buffer'!"
  (phw-set-directories-buffer)
  (phw-split-ver 0.75)
  (phw-set-history-buffer)
  (select-window (next-window)))

;; Layout left15 -----------------------------------------------------

(phw-layout-define "left15" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. This layout works best if it is contained in
`phw-show-sources-in-directories-buffer'!"
  (phw-set-directories-buffer)
  (phw-split-ver 0.5)
  (phw-set-methods-buffer)
  (select-window (next-window)))

;; Layout leftright1 -----------------------------------------------------

(phw-layout-define "leftright1" left-right
 "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Directories |                               |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |             |
   |              |                               |             |
   |  Sources     |                               |             |
   |              |                               |             |
   |--------------|                               |             |
   |              |                               |             |
   |  History     |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
 (phw-set-directories-buffer)
 (phw-split-ver 0.4)
 (phw-set-sources-buffer)
 (phw-split-ver 0.5)
 (phw-set-history-buffer)
 (select-window (next-window (next-window)))
 (phw-set-methods-buffer)
 (select-window (previous-window (selected-window) 0)))

;; Layout leftright2 -----------------------------------------------------

(phw-layout-define "leftright2" left-right
 "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Directories |                               |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |-------------|
   |              |                               |             |
   |  Sources     |                               |  History    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
 (phw-set-directories-buffer)
 (phw-split-ver 0.66)
 (phw-set-sources-buffer)
 (select-window (next-window (next-window)))
 (phw-set-methods-buffer)
 (phw-split-ver 0.66)
 (phw-set-history-buffer)
 (select-window (previous-window (previous-window (selected-window) 0) 0)))

;; Layout leftright3 -----------------------------------------------------

(phw-layout-define "leftright3" left-right
  "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Directories |                               |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |             Edit              |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-directories-buffer)
  (select-window (next-window (next-window)))
  (phw-set-methods-buffer)
  (select-window (previous-window (selected-window) 0)))


(phw-layout-define "left-dir-plus-speedbar" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   |  Speedbar    |                                      |
   |              |                                      |
   |              |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place. This layout works best if it is contained in
`phw-show-sources-in-directories-buffer'!"
  (phw-set-directories-buffer)
  (phw-split-ver 0.5)
  (phw-set-speedbar-buffer)
  (select-window (next-window)))

(phw-layout-define "left-analyse" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Analyse     |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-directories-buffer)
  (phw-split-ver 0.3)
  (phw-set-sources-buffer)
  (phw-split-ver 0.35)
  (phw-set-methods-buffer)
  (phw-split-ver 0.5)
  (phw-set-analyse-buffer)
  (select-window (next-window)))

(phw-layout-define "leftright-analyse" left-right
  "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Directories |                               |  Methods    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |-------------|
   |              |                               |             |
   |  Sources     |                               |             |
   |              |                               |             |
   |--------------|                               |  Analyse    |
   |              |                               |             |
   |  History     |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-directories-buffer)
  (phw-split-ver 0.4)
  (phw-set-sources-buffer)
  (phw-split-ver 0.5)
  (phw-set-history-buffer)
  (select-window (next-window (next-window)))
  (phw-set-methods-buffer)
  (phw-split-ver 0.5)
  (phw-set-analyse-buffer)
  (select-window (previous-window (previous-window (selected-window) 0) 0)))

(phw-layout-define "left-symboldef" left
  "This function creates the following layout:

   -------------------------------------------------------
   |              |                                      |
   |  Directories |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Sources     |                                      |
   |              |                                      |
   |--------------|                 Edit                 |
   |              |                                      |
   |  Methods     |                                      |
   |              |                                      |
   |--------------|                                      |
   |              |                                      |
   |  Symbol-defs |                                      |
   |              |                                      |
   -------------------------------------------------------
   |                                                     |
   |                    Compilation                      |
   |                                                     |
   -------------------------------------------------------

If you have not set a compilation-window in `phw-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
  (phw-set-directories-buffer)
  (phw-split-ver 0.3)
  (phw-set-sources-buffer)
  (phw-split-ver 0.35)
  (phw-set-methods-buffer)
  (phw-split-ver 0.5)
  (phw-set-symboldef-buffer)
  (select-window (next-window)))

(defconst phw-buildin-layouts (phw-copy-list phw-available-layouts)
  "All layouts defined until now.")

(silentcomp-provide 'phw-layout-defs)

;;; phw, 2015, 2015-layout-defs.el ends here
