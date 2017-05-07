SRC=            src/tfsp_fs_table.erl

TEST_SRC=       test/tfsp_fs_table_tests.erl

all: $(SRC) $(TEST_SRC)
	mkdir -p ebin/
	erlc -o ebin/ $(SRC) $(TEST_SRC)
