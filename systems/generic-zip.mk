PASESFILE = $(NAME)-$(VERSION).pases

$(PASESFILE): $(FILES)
	cd build && zip ../$(PASESFILE) $(FILES)
	zip $(PASESFILE) $(PASDEF)

build: 
	mkdir build

clean:
	rm -rf build
	rm $(PASESFILE)
	rm $(ZIPFILE)

$(ZIPFILE):
	wget "$(URL)" -q

.unpack: $(ZIPFILE) build
	cd build && unzip ../$(ZIPFILE)

$(FILES): .unpack

