NAME		= ensime
VERSION		= 0.0.20
URL		= http://github.com/downloads/aemoncannon/ensime/ensime-0.0.20.zip
ZIPFILE		= ensime-0.0.20.zip
PASDEF		= ensime.pasdef
DESCRIPTION	= Package for $NAME ($VERSION).
PASESFILE	= $(NAME)-$(VERSION).pases
FILES		= \
build/elisp/ensime*.el \
build/LICENSE \
build/README.md \
build/bin/server.bat \
build/bin/server.sh \
build/lib/*.jar

$(PASESFILE): $(FILES)
	tar c -p --owner=nobody --group=users --transform s/^build\\///g -zf $(PASESFILE) $(FILES) $(PASDEF)

build: 
	mkdir build

clean:
	rm -rf build
	rm $(PASESFILE)
	rm $(ZIPFILE)

$(ZIPFILE):
	wget "$(URL)" -q

.unpack: $(ZIPFILE) build
	rm -rf build ; mkdir build ; cd build ; unzip ../$(ZIPFILE) ; mv -f dist/* . ; mv -f dist/.ensime.example . ; rmdir dist

$(FILES): .unpack

include ../launchpad.mk
