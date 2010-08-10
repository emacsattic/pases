PASESFILE = $(NAME)-$(VERSION).pases

$(PASESFILE): $(FILES)
	cd build && zip ../$(PASESFILE) $(FILES)
	zip $(PASESFILE) $(PASDEF)

clean:
	rm -rf build
	rm -f $(PASESFILE)
