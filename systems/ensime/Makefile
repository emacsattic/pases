NAME		= ensime
VERSION		= 0.0.20
URL		= http://github.com/downloads/aemoncannon/ensime/ensime-0.0.20.zip
ZIPFILE		= ensime-0.0.20.zip
PASDEF		= ensime.pasdef
DESCRIPTION	= Package for $NAME ($VERSION).
PASESFILE	= $(NAME)-$(VERSION).pases
FILES		= \
elisp/ensime*.el \
LICENSE \
README.md \
bin/server.bat \
bin/server.sh \
lib/*.jar

include ../generic-zip.mk

.unpack: $(ZIPFILE) build
	rm -rf build ; mkdir build ; cd build ; unzip ../$(ZIPFILE) ; mv -f dist/* . ; mv -f dist/.ensime.example . ; rmdir dist
