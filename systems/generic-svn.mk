VERSION = $(shell cd builddir ; svn info |grep "Last Changed Rev" | sed -e "s/[^0-9]//g")
PASESFILE = $(NAME)-$(VERSION).pases

.PHONY: update-builddir

$(PASESFILE): $(FILES)
	tar c --owner=nobody --group=users --transform s/^builddir\\///g -zf $(PASESFILE) $(FILES)

builddir:
	svn co $(SVNURL) builddir

update-builddir: builddir
	cd builddir ; svn up

clean:
	rm -rf builddir
	rm $(PASESFILE)
	rm $(TARFILE)

$(FILES): update-builddir
