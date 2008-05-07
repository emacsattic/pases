PASESFILE = $(NAME)-$(VERSION).pases

$(PASESFILE): $(FILE)
	tar c --owner=nobody --group=users -zf $(PASESFILE) $(FILE) $(PASDEF)

clean:
	rm $(FILE)
	rm $(PASESFILE)

$(FILE):
	wget "$(URL)" -q
	mv $(FETCHFILE) $(FILE)
