include ../generic.mk

build:
	svn co $(SVNURL) build

clean:
	rm -rf build
	rm $(PASESFILE)

.update: build
	cd build ; svn up

$(FILES): .update

