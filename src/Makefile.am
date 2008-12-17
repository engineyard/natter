app = natter
appdir = $(ERLANG_INSTALL_LIB_DIR)/$(app)-$(VERSION)

erl_files = $(wildcard *.erl)
beam_files = $(erl_files:%.erl=../ebin/%.beam)
ebindir = $(appdir)/ebin
ebin_DATA = $(beam_files)

driverdir = $(appdir)/priv/lib
driver_LTLIBRARIES = natter_expat.la
erl_root_dir = @ERLANG_ROOT_DIR@
erl_ei_dir = @ERLANG_LIB_DIR_erl_interface@
natter_expat_la_SOURCES = natter_expat.c
natter_expat_la_CFLAGS = -I$(erl_ei_dir)/include -I$(erl_root_dir)/usr/include
natter_expat_la_LDFLAGS = -fPIC -shared -module -avoid-version \
	-L$(erl_ei_dir)/lib -lerl_interface -lei -lexpat

CLEANFILES = $(ebin_DATA) $(driver_LTLIBRARIES)

ERLCFLAGS = @ERLCFLAGS@ +warn_obsolete_guard +warn_unused_import \
	+warn_shadow_vars +warn_export_vars +debug_info

../ebin/%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) -b beam -o ../ebin $<
