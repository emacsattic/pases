URL = http://confluence-el.googlecode.com/files/confluence-el-1.2.tar.gz
VERSION = 1.2
NAME = confluence
PASESFILE = $(NAME)-$(VERSION).pases
TARFILE = confluence-el-1.2.tar.gz
PASDEF = confluence.pasdef
DESCRIPTION = Package for confluence $(VERSION)
ELFILES = \
build/confluence.el \
build/xml-rpc.el

DOCFILES = \
build/README

FILES = $(ELFILES) $(DOCFILES) $(PASDEF)

include ../generic-targz.mk
include ../launchpad.mk
