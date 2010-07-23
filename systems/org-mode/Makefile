URL = http://orgmode.org/org-7.01g.tar.gz
VERSION = 7.01g
NAME = org
PASESFILE = $(NAME)-$(VERSION).pases
TARFILE = org-7.01g.tar.gz
PASDEF = org.pasdef
DESCRIPTION = Package for org-mode $(VERSION)
ELFILES = \
build/contrib/lisp/*.el \
build/contrib/babel/langs/*.el \
build/lisp/*.el \
build/xemacs/*.el

DOCFILES = \
build/Changes.org \
build/Makefile \
build/README \
build/contrib/README \
build/contrib/babel/library-of-babel.org \
build/contrib/scripts/StartOzServer.oz \
build/contrib/scripts/dir2org.zsh \
build/contrib/scripts/ditaa.jar \
build/contrib/scripts/org2hpda \
build/doc/dir \
build/doc/org \
build/doc/org.pdf \
build/doc/org.texi \
build/doc/orgcard.pdf \
build/doc/orgcard.tex \
build/doc/orgcard_letter.pdf \
build/doc/orgguide.pdf \
build/doc/orgguide.texi \
build/doc/pdflayout.sty \
build/request-assign-future.txt \
build/xemacs/README

FILES = $(ELFILES) $(DOCFILES)

include ../generic-targz.mk
include ../launchpad.mk
