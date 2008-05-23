SVNURL = http://emacspeak.googlecode.com/svn/trunk/lisp/g-client
NAME = g-client
PASESFILE = $(NAME)-$(VERSION).pases
PASDEF = g-client.pasdef
ELFILES = \
builddir/g-app.el \
builddir/g-auth.el \
builddir/g-cus-load.el \
builddir/g-loaddefs.el \
builddir/g-load-path.el \
builddir/g-utils.el \
builddir/g.el \
builddir/gblogger.el \
builddir/gcal.el \
builddir/gnotebook.el \
builddir/gphoto.el \
builddir/greader.el \
builddir/gsheet.el \
builddir/gskeleton.el \
builddir/gtube.el \
builddir/indent-files.el \
builddir/json.el

#builddir/g-load-path.el \

DOCFILES = \
builddir/g-client.html \
builddir/g-client.org \
builddir/COPYING 

MISCFILES = \
builddir/atom-view.xsl \
builddir/gcal-view.xsl \
builddir/blogger-edit-post.xsl \
builddir/gevent-accept.xsl \
builddir/gevent-view.xsl \
builddir/gevent.xsl \
builddir/ut.xsl

FILES = $(ELFILES) $(DOCFILES) $(MISCFILES) $(PASDEF)

include ../generic-svn.mk

$(FILES): builddir .mk-builddir

.mk-builddir:
	cd builddir ; make
