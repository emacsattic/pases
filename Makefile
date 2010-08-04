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
	zip $(PASESFILE) $(FILES)
clean:
	rm $(PASESFILE)

pases-bootstrap.el: pases-luna.el pases.el pases-package.el
	cat pases-luna.el pases.el pases-package.el > pases-bootstrap.el
