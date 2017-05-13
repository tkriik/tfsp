OBJ=            ebin/bitset.beam \
		ebin/bitset_tests.beam \
		ebin/fs_ent.beam \
		ebin/fs_ent_tests.beam \
		ebin/fs_ent_tab.beam \
                ebin/fs_ent_tab_tests.beam \
		ebin/tfsp_scanner.beam \
		ebin/tfsp_scanner_tests.beam \
		ebin/tfsp_ssh_server.beam \
		ebin/tfsp_ssh_server_tests.beam

ERL=		erl
ERLC=		erlc
DFLAGS=		-D TFSP_TEST
EFLAGS=         -I include/ -o ebin/ $(DFLAGS)

all: $(OBJ)

test: $(OBJ)
	$(ERL) -noinput \
	       -pa ebin/ \
	       -eval 'case eunit:test([{dir, "ebin/"}], [verbose]) of error -> init:stop(1); Result -> Result end.' \
	       -s init stop

ebin/bitset.beam: src/bitset.erl
	$(ERLC) $(EFLAGS) src/bitset.erl

ebin/bitset_tests.beam:	test/bitset_tests.erl \
			ebin/bitset.beam
	$(ERLC) $(EFLAGS) test/bitset_tests.erl

ebin/fs_ent.beam: include/fs_ent.hrl \
		  src/fs_ent.erl
	$(ERLC) $(EFLAGS) src/fs_ent.erl

ebin/fs_ent_tests.beam: test/fs_ent_tests.erl \
			ebin/fs_ent.beam
	$(ERLC) $(EFLAGS) test/fs_ent_tests.erl

ebin/fs_ent_tab.beam: include/fs_ent.hrl \
		      src/fs_ent_tab.erl
	$(ERLC) $(EFLAGS) src/fs_ent_tab.erl

ebin/fs_ent_tab_tests.beam: test/fs_ent_tab_tests.erl \
			    ebin/fs_ent_tab.beam
	$(ERLC) $(EFLAGS) test/fs_ent_tab_tests.erl

ebin/tfsp_scanner.beam:	include/fs_ent.hrl \
			src/fs_ent.erl \
			src/fs_ent_tab.erl \
			src/tfsp_scanner.erl
	$(ERLC) $(EFLAGS) src/tfsp_scanner.erl

ebin/tfsp_scanner_tests.beam: test/tfsp_scanner_tests.erl \
			      ebin/tfsp_scanner.beam
	$(ERLC) $(EFLAGS) test/tfsp_scanner_tests.erl

ebin/tfsp_ssh_server.beam: src/tfsp_ssh_server.erl
	$(ERLC) $(EFLAGS) src/tfsp_ssh_server.erl

ebin/tfsp_ssh_server_tests.beam: test/tfsp_ssh_server_tests.erl \
				 ebin/tfsp_ssh_server.beam
	$(ERLC) $(EFLAGS) test/tfsp_ssh_server_tests.erl

.PHONY: clean

clean:
	rm -f $(OBJ)
