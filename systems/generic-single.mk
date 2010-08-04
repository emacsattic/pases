PASESFILE = $(NAME)-$(VERSION).pases

$(PASESFILE): $(FILE)
	zip $(PASESFILE) $(PASDEF) $(FILE)

clean:
	rm $(FILE)
	rm $(PASESFILE)

$(FILE): $(FETCHFILE)
	mv $(FETCHFILE) $(FILE)

$(FETCHFILE):
	wget "$(URL)" -q
