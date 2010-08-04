PASESFILE = $(NAME)-$(VERSION).pases

$(PASESFILE): $(FILES)
	cd build && zip ../$(PASESFILE) $(FILES)
	zip $(PASESFILE) $(PASDEF)

build: 
	mkdir build

clean:
	rm -rf build
	rm $(PASESFILE)
	rm $(TARFILE)

$(TARFILE):
	wget "$(URL)" -q

.unpack: $(TARFILE) build
	tar x --strip-components 1 --format=v7 -C build -jf $(TARFILE)

$(FILES): .unpack

