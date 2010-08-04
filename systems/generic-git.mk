PASESFILE = $(NAME)-$(VERSION).pases

$(PASESFILE): $(FILES)
	cd build && zip ../$(PASESFILE) $(FILES)
	zip $(PASESFILE) $(PASDEF)

build:
	git clone $(GITURL) build

clean:
	rm -rf build
	rm $(PASESFILE)

.update: build
ifeq ($(TAG),)
	cd build ; git pull
else
	cd build ; git pull ; git checkout $(TAG)
endif

$(FILES): .update
