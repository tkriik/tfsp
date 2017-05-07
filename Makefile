SRC=            src/tfsp_fs_table.erl \
                test/tfsp_fs_table_tests.erl

OBJ=            ebin/tfsp_fs_table.beam \
                ebin/tfsp_fs_table_tests.beam

ERLC=			erlc
CFLAGS=         -I include/ -o ebin/

all: $(OBJ)

test: $(OBJ)
		erl -noinput \
		    -pa ebin/ \
		    -eval 'case eunit:test([{dir, "ebin/"}], [verbose]) of error -> init:stop(1); Result -> Result end.' \
		    -s init stop

ebin/tfsp_fs_table.beam: src/tfsp_fs_table.erl include/fs_entry.hrl
		$(ERLC) $(CFLAGS) src/tfsp_fs_table.erl

ebin/tfsp_fs_table_tests.beam: test/tfsp_fs_table_tests.erl
		$(ERLC) $(CFLAGS) test/tfsp_fs_table_tests.erl

.PHONY: clean

clean:
		rm -f $(OBJ)
