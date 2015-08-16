# This Makefile byte-compiles the PHW lisp files and generates online-help.

# Copyright (C) 2000 - 2015 Jesper Nordenberg,
#                           Klaus Berndl,
#                           Ryan Ware,
#                           Free Software Foundation, Inc.

# Author: Jesper Nordenberg <mayhem@home.se>
#         Klaus Berndl <klaus.berndl@sdm.de>
#         Ryan Ware <ryan.r.ware@intel.com>
# Maintainer: Ryan Ware <ryan.r.ware@intel.com>
# Keywords: browser, code, programming, tools
# Created: 2001

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

# New Makefile layout created by Alex Ott.

SHELL = /bin/bash

PLATFORM=$(shell uname -s)

.PHONY: phw autoloads online-help

#
# Override vars in Makefile.conf if needed
#
-include Makefile.conf

ifeq ($(wildcard Makefile.conf),)
$(warning Makefile.conf not found. Using defaults for $(PLATFORM)!)
$(warning Create Makefile.conf from Makefile.conf.template to override the defaults.)
endif

# When run inside Emacs, the variable contains 't', so correct it
ifeq ($(origin EMACS),environment)
	EMACS = emacs
endif

EMACS ?= emacs

# If it's one of Linux, Cygwin, or Darwin, use defaults.
ifneq ($(filter Linux CYGWIN% Darwin, $(PLATFORM)),)
	EMACS ?= emacs
	CEDET ?=
	LOADPATH ?=
	MAKEINFO ?= makeinfo
	TEXI2PDF ?= texi2pdf
	TEXI2DVI ?= texi2dvi
	DVIPDFM ?= dvipdf
	DVIPS ?= dvips
	PS2PDF ?= ps2pdf
	EMACSINFOPATH ?=
	INSTALLINFO ?= install-info
endif

# For the PHW-maintainers: Change the version-number here and not
# elsewhere!
phw_VERSION=2.50

include phw-makedef.mk

all: phw autoloads online-help

phw: $(phw_LISP_EL)
	@echo "Byte-compiling PHW with LOADPATH=${LOADPATH} ..."
	@$(RM) $(phw_LISP_ELC) phw-compile-script
	@echo "(add-to-list 'load-path nil)" > phw-compile-script
	@if test ! -z "${CEDET}"; then\
		if test -f $(CEDET)/cedet-devel-load.el ; then \
		   echo "(load-file \"$(CEDET)/cedet-devel-load.el\")" >> phw-compile-script; \
		else \
		   echo "(load-file \"$(CEDET)/common/cedet.el\")" >> phw-compile-script; \
		fi \
	else \
	   echo "(semantic-mode 1)" >> phw-compile-script; \
	   echo "(require 'semantic/bovine/el)" >> phw-compile-script; \
	fi
	@if test ! -z "${LOADPATH}"; then\
	   for loadpath in ${LOADPATH}; do \
	      echo "(add-to-list 'load-path \"$$loadpath\")" >> phw-compile-script; \
	   done; \
	fi
	@echo "(require 'phw)" >> phw-compile-script
	@echo "(setq debug-on-error t)" >> phw-compile-script
	$(EBATCH) -l phw-compile-script --eval '(phw-byte-compile t)'
	@$(RM) phw-compile-script

online-help: $(phw_TEXI)
	@if command -v "$(MAKEINFO)" >/dev/null 2>&1 ; then\
	   $(RM) -R $(phw_INFO_DIR) $(phw_HTML_DIR); \
	   $(MKDIR) $(phw_INFO_DIR) $(phw_HTML_DIR); \
	   echo Generating info-format...; \
	   $(MAKEINFO) --fill-column=78 $<; \
	   $(MV) *.info* $(phw_INFO_DIR); \
	   echo Generating html-format...; \
	   $(MAKEINFO) --html --output=$(phw_HTML_DIR) $<; \
	   for file in $(phw_HTML_DIR)/*.html; do\
	      $(MV) $$file tmpfile; \
	      sed "s/index\\.html/$(phw_HTML)/g" tmpfile > $$file; \
	      $(RM) tmpfile; \
	   done; \
	   $(MV) $(phw_HTML_DIR)/index.html $(phw_HTML_DIR)/$(phw_HTML); \
	else \
	   echo No info- and html-format generating because the tool; \
	   echo - makeinfo in $(MAKEINFO); \
	   echo is not available!; \
	fi

pdf: $(phw_TEXI)
	@if command -v "$(TEXI2PDF)" >/dev/null 2>&1; then\
	   $(RM) $(phw_PDF); \
	   echo Generating pdf-format with texi2pdf ...; \
	   $(TEXI2PDF) --clean $<; \
	elif command -v "$(TEXI2DVI)" >/dev/null 2>&1 -a command -v "$(DVIPDFM)" >/dev/null 2>&1; then\
	   $(RM) $(phw_DVI) $(phw_PDF); \
	   echo Generating pdf-format with dvipdfm ...; \
	   $(TEXI2DVI) --clean $<; \
	   $(DVIPDFM) $(phw_DVI); \
	   $(RM) $(phw_DVI); \
	elif command -v "$(TEXI2DVI)" >/dev/null 2>&1 -a command -v "$(DVIPS)" >/dev/null 2>&1 -a command -v "$(PS2PDF)" >/dev/null 2>&1; then\
	   $(RM) $(phw_DVI) $(phw_PS) $(phw_PDF); \
	   echo Generating pdf-format with dvips and ps2pdf ...; \
	   $(TEXI2DVI) --quiet --clean $<; \
	   $(DVIPS) -Pcmz -q $(phw_DVI) -o $(phw_PS); \
	   $(PS2PDF) $(phw_PS); \
	   $(RM) $(phw_DVI) $(phw_PS); \
	else \
	   echo No pdf-format generating because at least one of the tools; \
	   echo - texi2pdf in $(TEXI2PDF); \
	   echo - texi2dvi in $(TEXI2DVI); \
	   echo - dvips in $(DVIPS); \
	   echo - ps2pdf in $(PS2PDF); \
	   echo is not available!; \
	fi


install-help: $(phw_INFO_DIR)/$(phw_INFO)
	@if command -v "$(INSTALLINFO)" >/dev/null 2>&1 -a -f "$(EMACSINFOPATH)/dir"; then\
	   echo Installing the Online-help in $(EMACSINFOPATH)...; \
	   $(CP) $(phw_INFO_DIR)/*info* $(EMACSINFOPATH); \
	   $(INSTALLINFO) $< $(EMACSINFOPATH)/dir; \
	else \
	   echo Can not install the online-help because either; \
	   echo - the tool $(INSTALLINFO) or; \
	   echo - the file $(EMACSINFOPATH)/dir; \
	   echo is not available!; \
	fi


clean:
	@$(RM) $(phw_LISP_ELC) phw-compile-script phw-autoloads.el*

# The targets below are only for maintaining the PHW-package.

$(phw_INFO_DIR)/$(phw_INFO): online-help

# updates RELEASE_NOTES, README, NEWS, phw.texi and phw.el to the
# version-number of $(phw_VERSION).
prepversion:
	@$(MV) RELEASE_NOTES RELEASE_NOTES.tmp
	@sed "1s/version.*/version $(phw_VERSION)/" RELEASE_NOTES.tmp > RELEASE_NOTES
	@$(RM) RELEASE_NOTES.tmp
	@$(MV) README README.tmp
	@sed "1s/version.*/version $(phw_VERSION)/" README.tmp > README
	@$(RM) README.tmp
	@$(MV) NEWS NEWS.tmp
	@sed "1s/version.*/version $(phw_VERSION)/" NEWS.tmp > NEWS
	@$(RM) NEWS.tmp
	@$(MV) phw-upgrade.el phw-upgrade.el.tmp
	@sed "s/^(defconst phw-version.*/(defconst phw-version \"$(phw_VERSION)\"/" phw-upgrade.el.tmp > phw-upgrade.el
	@$(RM) phw-upgrade.el.tmp
	@(echo "/@macro phwver";		\
	  echo "+";				\
	  echo "c";				\
	  echo "$(phw_VERSION)";		\
	  echo ".";				\
	  echo "w";				\
	  echo "q") | ed -s $(phw_TEXI) 1> /dev/null

autoloads: phw
	@$(RM) $(phw_AUTOLOADS) $(phw_AUTOLOADS)c
	$(EBATCH) --eval "(add-to-list 'load-path nil)" -l phw-autogen -f phw-update-autoloads

# builds the distribution file $(phw_VERSION).tar.gz
distrib: $(phw_INFO_DIR)/$(phw_INFO) prepversion autoloads phw
	@$(RM) phw-$(phw_VERSION).tar.gz
	@$(RM) -R phw-$(phw_VERSION)
	@$(MKDIR) phw-$(phw_VERSION)
	@$(CP) $(phw_DISTRIB_FILES) phw-$(phw_VERSION)
	@$(CP) -r $(phw_INFO_DIR) phw-$(phw_VERSION)
	@$(CP) -r $(phw_HTML_DIR) phw-$(phw_VERSION)
	@$(CP) -r $(phw_IMAGE_DIR) phw-$(phw_VERSION)
	@find phw-$(phw_VERSION)/$(phw_IMAGE_DIR) -name CVS -print | xargs rm -Rf
	@find phw-$(phw_VERSION)/$(phw_IMAGE_DIR) -name *~ -print | xargs $(RM)
	@find phw-$(phw_VERSION)/$(phw_IMAGE_DIR) -name *.png -print | xargs $(RM)
	@tar -cvzf phw-$(phw_VERSION).tar.gz phw-$(phw_VERSION)
	@$(RM) -R phw-$(phw_VERSION)

printconf:
	@echo Platform: $(PLATFORM)
	@echo PHW version: $(phw_VERSION)
	@echo Emacs: $(EMACS)
	@if test -n "${CEDET}"; then echo "CEDET: ${CEDET}"; else echo "CEDET: Emacs-builtin"; fi
	@echo Load path: $(LOADPATH)
	@echo Emacs info path: $(EMACSINFOPATH)
	@echo command -v "${INSTALLINFO}" >/dev/null 2>&1 && echo "install-info: ${INSTALLINFO}" || echo "install-info: cannot execute ${INSTALLINFO}"
	@echo command -v "${MAKEINFO}" >/dev/null 2>&1 && echo "makeinfo: ${MAKEINFO}" || echo "makeinfo: cannot execute ${MAKEINFO}"
	@echo command -v "${TEXI2PDF}" >/dev/null 2>&1 && echo "texi2pdf: ${TEXI2PDF}" || echo "texi2pdf: cannot execute ${TEXI2PDF}"
	@echo command -v "${TEXI2DVI}" >/dev/null 2>&1 && echo "texi2dvi: ${TEXI2DVI}" || echo "texi2dvi: cannot execute ${TEXI2DVI}"
	@echo command -v "${DVIPDFM}" >/dev/null 2>&1 && echo "dvipdfm: ${DVIPDFM}" || echo "dvipdfm: cannot execute ${DVIPDFM}"
	@echo command -v "${DVIPS}" >/dev/null 2>&1 && echo "dvips: ${DVIPS}" || echo "dvips: cannot execute ${DVIPS}"
	@echo command -v "${PS2PDF}" >/dev/null 2>&1 && echo "ps2pdf: ${PS2PDF}" || echo "ps2pdf: cannot execute ${PS2PDF}"

# End of Makefile
