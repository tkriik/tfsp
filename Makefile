OBJ=            ebin/bitset.beam \
		ebin/bitset_tests.beam \
		ebin/fs_tick.beam \
		ebin/fs_tick_tests.beam \
		ebin/event_queue.beam \
		ebin/event_queue_tests.beam \
		ebin/fs_ent.beam \
		ebin/fs_ent_tests.beam \
		ebin/fs_ent_tab.beam \
                ebin/fs_ent_tab_tests.beam \
                ebin/path.beam \
                ebin/wire.beam \
                ebin/wire_tests.beam \
                ebin/tfsp_client.beam \
                ebin/tfsp_event.beam \
                ebin/tfsp_proto.beam \
                ebin/tfsp_proto_tests.beam \
		ebin/tfsp_scanner.beam \
		ebin/tfsp_scanner_tests.beam \
		ebin/tfsp_server.beam \
		ebin/tfsp_ssh_client.beam \
		ebin/tfsp_ssh_client_tests.beam \
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

shell: $(OBJ)
	$(ERL) -pa ebin/ \
	       -eval '[code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F)))) || F <- filelib:wildcard("ebin/*.beam")].'

ebin/bitset.beam: src/bitset.erl
	$(ERLC) $(EFLAGS) src/bitset.erl

ebin/bitset_tests.beam:	test/bitset_tests.erl \
			ebin/bitset.beam
	$(ERLC) $(EFLAGS) test/bitset_tests.erl

ebin/fs_ent.beam: include/fs.hrl \
		  src/fs_ent.erl
	$(ERLC) $(EFLAGS) src/fs_ent.erl

ebin/fs_ent_tests.beam: test/fs_ent_tests.erl \
			ebin/fs_ent.beam
	$(ERLC) $(EFLAGS) test/fs_ent_tests.erl

ebin/fs_ent_tab.beam: include/fs.hrl \
		      src/fs_ent_tab.erl \
		      ebin/fs_ent.beam
	$(ERLC) $(EFLAGS) src/fs_ent_tab.erl

ebin/fs_ent_tab_tests.beam: test/fs_ent_tab_tests.erl \
			    ebin/fs_ent_tab.beam
	$(ERLC) $(EFLAGS) test/fs_ent_tab_tests.erl

ebin/fs_tick.beam: src/fs_tick.erl
	$(ERLC) $(EFLAGS) src/fs_tick.erl

ebin/fs_tick_tests.beam: test/fs_tick_tests.erl \
			 ebin/fs_tick.beam \
			 ebin/tfsp_event.beam
	$(ERLC) $(EFLAGS) test/fs_tick_tests.erl

ebin/event_queue.beam: test/event_queue.erl \
		       ebin/tfsp_event.beam
	$(ERLC) $(EFLAGS) test/event_queue.erl

ebin/event_queue_tests.beam: test/event_queue_tests.erl \
			     ebin/event_queue.beam
	$(ERLC) $(EFLAGS) test/event_queue_tests.erl

ebin/path.beam: src/path.erl
	$(ERLC) $(EFLAGS) src/path.erl

ebin/wire.beam: src/wire.erl \
		include/fs.hrl
	$(ERLC) $(EFLAGS) src/wire.erl

ebin/wire_tests.beam: test/wire_tests.erl \
		      ebin/wire.beam
	$(ERLC) $(EFLAGS) test/wire_tests.erl

ebin/tfsp_client.beam: src/tfsp_client.erl \
		       ebin/tfsp_ssh_client.beam \
		       include/fs.hrl
	$(ERLC) $(EFLAGS) src/tfsp_client.erl

ebin/tfsp_event.beam: src/tfsp_event.erl \
		      include/fs.hrl
	$(ERLC) $(EFLAGS) src/tfsp_event.erl

ebin/tfsp_proto.beam: src/tfsp_proto.erl
	$(ERLC) $(EFLAGS) src/tfsp_proto.erl

ebin/tfsp_proto_tests.beam: test/tfsp_proto_tests.erl \
			    include/fs.hrl \
			    ebin/tfsp_proto.beam
	$(ERLC) $(EFLAGS) test/tfsp_proto_tests.erl

ebin/tfsp_scanner.beam:	include/fs.hrl \
			src/tfsp_scanner.erl \
			ebin/fs_ent.beam \
			ebin/fs_ent_tab.beam \
			ebin/tfsp_event.beam
	$(ERLC) $(EFLAGS) src/tfsp_scanner.erl

ebin/tfsp_scanner_tests.beam: test/tfsp_scanner_tests.erl \
			      ebin/tfsp_scanner.beam
	$(ERLC) $(EFLAGS) test/tfsp_scanner_tests.erl

ebin/tfsp_server.beam: src/tfsp_server.erl \
		       ebin/tfsp_ssh_server.beam
	$(ERLC) $(EFLAGS) src/tfsp_server.erl

ebin/tfsp_ssh_client.beam: src/tfsp_ssh_client.erl \
			   include/conn.hrl \
			   include/fs.hrl \
			   ebin/tfsp_event.beam
	$(ERLC) $(EFLAGS) src/tfsp_ssh_client.erl

ebin/tfsp_ssh_client_tests.beam: test/tfsp_ssh_client_tests.erl \
				 test/ssh_defs.hrl \
				 ebin/tfsp_client.beam
	$(ERLC) $(EFLAGS) test/tfsp_ssh_client_tests.erl

ebin/tfsp_ssh_server.beam: src/tfsp_ssh_server.erl \
			   include/conn.hrl \
			   include/fs.hrl
	$(ERLC) $(EFLAGS) src/tfsp_ssh_server.erl

ebin/tfsp_ssh_server_tests.beam: test/tfsp_ssh_server_tests.erl \
				 test/ssh_defs.hrl \
				 ebin/tfsp_server.beam
	$(ERLC) $(EFLAGS) test/tfsp_ssh_server_tests.erl

.PHONY: clean

clean:
	rm ebin/*
