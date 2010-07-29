PASESFILE = $(NAME)-$(VERSION).pases

$(PASESFILE): $(FILES)
	tar c --owner=nobody --group=users --transform s/^build\\///g -zf $(PASESFILE) $(FILES) $(PASDEF)

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
