URL = http://orgmode.org/org-6.28e.tar.gz
VERSION = 6.28e
NAME = org
PASESFILE = $(NAME)-$(VERSION).pases
TARFILE = org-6.28e.tar.gz
PASDEF = org.pasdef
ELFILES = \
build/contrib/lisp/*.el \
build/contrib/packages/org-export-freemind-0.1.0/*.el \
build/lisp/*.el \
build/xemacs/*.el

DOCFILES = \
build/Change* \
build/README \
build/contrib/ChangeLog \
build/contrib/README \
build/contrib/packages/org-export-freemind-0.1.0/ChangeLog \
build/contrib/packages/org-export-freemind-0.1.0/README \
build/contrib/scripts/* \
build/doc/* \
build/request-assign-future.txt \
build/xemacs/README

FILES = $(ELFILES) $(DOCFILES) $(PASDEF)

include ../generic-targz.mk
