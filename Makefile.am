SUBDIRS = src tests

clean-hook:
	rm -rf ebin/*
	rm -rf priv/lib/*

dist-hook:
	mkdir $(distdir)/ebin

libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck
