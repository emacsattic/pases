GITURL		= http://github.com/wanderlust/wanderlust.git 
NAME		= wl
VERSION	        = 2.15.9
TAG             = c62f5baee88ad707e737eb584bfe97be10890027
PASDEF		= wl.pasdef
FILES		= \
BUGS \
BUGS.* \
COPYING \
ChangeLog \
NEWS \
NEWS.* \
README \
README.* \
doc/* \
elmo/*.el \
elmo/ChangeLog \
etc/* \
etc/icons/* \
samples/* \
tests/* \
utils/*.el \
utils/ChangeLog \
wl/*.el \
wl/ChangeLog

DESCRIPTION	= Package for $NAME ($VERSION).

include ../generic-git.mk
include ../launchpad.mk

$(PASESFILE): $(FILES)
	# need to call make to make wl-news.el, maybe there is a better way
	cd build && patch -p0 <../hack.patch
	cd build && make
	cd build && zip ../$(PASESFILE) $(FILES)
	zip $(PASESFILE) $(PASDEF)

