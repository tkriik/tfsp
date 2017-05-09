OBJ=            ebin/tfsp_fs_entry.beam \
		ebin/tfsp_fs_entry_tests.beam \
		ebin/tfsp_fs_scanner.beam \
		ebin/tfsp_fs_scanner_tests.beam \
		ebin/tfsp_fs_table.beam \
                ebin/tfsp_fs_table_tests.beam

ERL=		erl
ERLC=		erlc
DFLAGS=		-D EXPORT_TEST
EFLAGS=         -I include/ -o ebin/ $(DFLAGS)

all: $(OBJ)

test: $(OBJ)
	$(ERL) -noinput \
	       -pa ebin/ \
	       -eval 'case eunit:test([{dir, "ebin/"}], [verbose]) of error -> init:stop(1); Result -> Result end.' \
	       -s init stop

ebin/tfsp_fs_entry.beam:	include/fs_entry.hrl \
				src/tfsp_fs_entry.erl
	$(ERLC) $(EFLAGS) src/tfsp_fs_entry.erl

ebin/tfsp_fs_scanner.beam:	include/fs_entry.hrl \
				src/tfsp_fs_entry.erl \
				src/tfsp_fs_table.erl \
				src/tfsp_fs_scanner.erl
	$(ERLC) $(EFLAGS) src/tfsp_fs_scanner.erl

ebin/tfsp_fs_table.beam:	include/fs_entry.hrl \
				src/tfsp_fs_table.erl
	$(ERLC) $(EFLAGS) src/tfsp_fs_table.erl

ebin/tfsp_fs_entry_tests.beam: test/tfsp_fs_entry_tests.erl
	$(ERLC) $(EFLAGS) test/tfsp_fs_entry_tests.erl

ebin/tfsp_fs_scanner_tests.beam: test/tfsp_fs_scanner_tests.erl
	$(ERLC) $(EFLAGS) test/tfsp_fs_scanner_tests.erl

ebin/tfsp_fs_table_tests.beam: test/tfsp_fs_table_tests.erl
	$(ERLC) $(EFLAGS) test/tfsp_fs_table_tests.erl

.PHONY: clean

clean:
	rm -f $(OBJ)
