NAME=pases
VERSION=0.2
PASESFILE = $(NAME)-$(VERSION).pases
FILES=pases.el \
pases-luna.el \
pases-package.el \
pases-load.el \
pases.pasdef \
README

$(PASESFILE): $(FILES)
	tar c --owner=nobody --group=users --transform s/^build\\///g -zf $(PASESFILE) $(FILES)

clean:
	rm $(PASESFILE)

pases-bootstrap.el:
	cat pases-luna.el pases.el pases-package.el > pases-bootstrap.el
