PASESFILE = $(NAME)-$(VERSION).pases

$(PASESFILE): $(FILES)
	tar c --owner=nobody --group=users --transform s/^build\\///g -zf $(PASESFILE) $(FILES) $(PASDEF)

build:
	svn co $(SVNURL) build

clean:
	rm -rf build
	rm $(PASESFILE)

.update: build
	cd build ; svn up

$(FILES): .update
