push:
	gpg --armor --sign --detach-sig -u ehetzner@gmail.com $(PASESFILE)
	rsync -ab $(PASESFILE) $(PASESFILE).asc e6h.org:/srv/www/e6h.org/pases/
