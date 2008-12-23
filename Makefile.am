SUBDIRS = src tests

dist-hook:
	mkdir $(distdir)/ebin

libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck
