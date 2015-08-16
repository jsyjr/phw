# This file defines some settings used for the PHW-Makefiles

# Copyright (C) 2000 - 2005, 2015 Klaus Berndl,
#                           Free Software Foundation, Inc.

# Author: Klaus Berndl <klaus.berndl@sdm.de>
# Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
# Keywords: browser, code, programming, tools
# Created: 2004

# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# GNU Emacs; see the file COPYING.  If not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# $Id$

RM=rm -f
CP=cp
MV=mv -f
MKDIR=mkdir -p

EBATCH=$(EMACS) -batch -no-site-file

# phw_LISP_EL=tree-buffer.el phw-util.el phw-mode-line.el phw-help.el \
#             phw-layout.el phw-layout-defs.el phw-navigate.el phw.el \
#             phw-eshell.el phw-cycle.el phw-face.el phw-compilation.el \
#             phw-upgrade.el phw-create-layout.el silentcomp.el \
#             phw-speedbar.el phw-examples.el phw-tod.el phw-autogen.el \
# 	    phw-jde.el phw-file-browser.el phw-method-browser.el \
# 	    phw-winman-support.el phw-cedet-wrapper.el \
# 	    phw-compatibility.el phw-common-browser.el phw-analyse.el \
# 	    phw-symboldef.el phw-semantic.el phw-advice-test.el \
# 	    phw-buffertab.el phw-multiframe.el phw-semantic-wrapper.el

phw_LISP_EL=phw.el \
	    phw-autogen.el \
	    phw-compilation.el \
	    phw-util.el \
	    silentcomp.el \
            phw-layout-defs.el \
            phw-layout.el \

phw_LISP_ELC=$(phw_LISP_EL:.el=.elc)

phw_AUTOLOADS=phw-autoloads.el

phw_ETC=NEWS README RELEASE_NOTES phw-makedef.mk Makefile make.bat

phw_TEXI=phw.texi

phw_INFO=$(phw_TEXI:.texi=.info)
phw_HTML=$(phw_TEXI:.texi=.html)
phw_HTML_DIR=html-help
phw_INFO_DIR=info-help

phw_DVI=$(phw_TEXI:.texi=.dvi)
phw_PS=$(phw_TEXI:.texi=.ps)
phw_PDF=$(phw_TEXI:.texi=.pdf)

phw_IMAGE_DIR=phw-images

phw_DISTRIB_FILES=$(phw_LISP_EL) $(phw_AUTOLOADS) $(phw_TEXI) $(phw_ETC)

