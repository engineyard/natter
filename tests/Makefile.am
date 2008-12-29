erl_files = $(wildcard *.erl)
hrl_files = $(wildcard *.hrl)
beam_files = $(erl_files:%.erl=../ebin/%.beam)
check_DATA = $(beam_files)

CLEANFILES = $(check_DATA)
EXTRA_DIST = $(erl_files) $(hrl_files)

ERLCFLAGS = @ERLCFLAGS@ +debug_info

check-local:
	$(ERL) -pa ../ebin -eval "test_suite:test()" -s init stop -noshell

../ebin/%.beam: %.erl
	$(ERLC) $(ERLCFLAGS) -b beam -I ../src -o ../ebin $<
