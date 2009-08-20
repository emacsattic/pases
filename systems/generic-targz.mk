PASESFILE = $(NAME)-$(VERSION).pases

$(PASESFILE): $(FILES)
	tar c --owner=nobody --group=users --transform s/^build\\///g -zf $(PASESFILE) $(FILES) $(PASDEF)

build: 
	mkdir build

clean:
	rm -rf build
	rm $(PASESFILE)
	rm $(TARFILE)

$(TARFILE):
	wget "$(URL)" -q

.unpack: $(TARFILE) build
	tar x --strip-components 1 --format=v7 -C build -zf $(TARFILE)

$(FILES): .unpack

