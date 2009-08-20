#/bin/sh
URL = http://rubyforge.org/frs/download.php/20976/emacs-rails-0.5.99.5.tar.gz
VERSION = 0.5.99.5
NAME = emacs-rails
PASESFILE = $(NAME)-$(VERSION).pases
TARFILE = emacs-rails-0.5.99.5.tar.gz
PASDEF = emacs-rails.pasdef
ELFILES = \
build/rails-navigation.el \
build/rails-compat.el \
build/rails-mailer-minor-mode.el \
build/rails-fixture-minor-mode.el \
build/rails-functional-test-minor-mode.el \
build/rails-model-layout.el \
build/rails-cmd-proxy.el \
build/untabify-file.el \
build/rails-project.el \
build/rails-scripts.el \
build/rails-rake.el \
build/rails-core.el \
build/rails.el \
build/rails-view-minor-mode.el \
build/predictive-prog-mode.el \
build/rails-ui.el \
build/rails-bytecompile.el \
build/rails-ws.el \
build/rails-speedbar-feature.el \
build/rails-model-minor-mode.el \
build/rails-ruby.el \
build/rails-layout-minor-mode.el \
build/rails-plugin-minor-mode.el \
build/rails-controller-layout.el \
build/rails-helper-minor-mode.el \
build/rails-migration-minor-mode.el \
build/rails-controller-minor-mode.el \
build/rails-test.el \
build/rails-find.el \
build/rails-lib.el \
build/rails-log.el \
build/inflections.el \
build/rails-snippets-feature.el \
build/rails-features.el \
build/rails-unit-test-minor-mode.el

DOCFILES = \
build/History \
build/README \
build/ChangeLog

FILES = $(ELFILES) $(DOCFILES)

include ../generic-targz.mk
