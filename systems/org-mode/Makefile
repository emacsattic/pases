URL = http://orgmode.org/org-6.33f.tar.gz
VERSION = 6.33f
NAME = org
PASESFILE = $(NAME)-$(VERSION).pases
TARFILE = org-6.33f.tar.gz
PASDEF = org.pasdef
DESCRIPTION = Package for org-mode $(VERSION)
ELFILES = \
build/contrib/lisp/*.el \
build/contrib/babel/lisp/*.el \
build/contrib/babel/lisp/langs/*.el \
build/lisp/*.el \
build/xemacs/*.el

DOCFILES = \
build/Change* \
build/README \
build/contrib/babel/library-of-babel.org \
build/contrib/ChangeLog \
build/contrib/README \
build/contrib/scripts/org2hpda \
build/contrib/scripts/dir2org.zsh \
build/contrib/scripts/ditaa.jar \
build/doc/orgcard.pdf \
build/doc/org.texi \
build/doc/dir \
build/doc/org.pdf \
build/doc/org \
build/doc/orgcard.tex \
build/doc/orgcard_letter.pdf \
build/request-assign-future.txt \
build/xemacs/README

FILES = $(ELFILES) $(DOCFILES)

include ../generic-targz.mk
include ../launchpad.mk
