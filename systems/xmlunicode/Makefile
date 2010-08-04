VERSION		= 1.10
NAME		= xmlunicode
PASDEF		= xmlunicode.pasdef
PASESFILE	= $(NAME)-$(VERSION).pases
FILES		= xmlunicode.el unichars.el

$(PASESFILE): $(FILES)
	zip $(PASESFILE) $(FILES) $(PASDEF)

clean:
	rm $(FILES)
	rm $(PASESFILE)

unichars.el:
	wget http://nwalsh.com/emacs/xmlchars/unichars.el -q

xmlunicode.el:
	wget http://nwalsh.com/emacs/xmlchars/xmlunicode.el -q
