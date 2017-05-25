%%% Constants for SSH client and server tests

-define(SSH_HOST,               "localhost").
-define(SSH_PORT,               1234).
-define(SSH_TIMEOUT,            5000).

-define(SSH_CLIENT_ROOT,        <<"test/data/ssh_client/sync_root">>).
-define(SSH_CLIENT_HOST,        "localhost").
-define(SSH_CLIENT_KNOWN_HOSTS, "test/data/ssh_client/home/ssh_client/ssh/known_hosts").
-define(SSH_CLIENT_OPTS,        [{user_dir, "test/data/ssh_client/home/ssh_client/ssh/"},
                                 {user_interaction, false},
                                 {silently_accept_hosts, true},
                                 {user, "ssh_client"}]).

-define(SSH_SERVER_ROOT,        <<"test/data/ssh_server/sync_root">>).
-define(SSH_SERVER_SYSTEM_DIR,  "test/data/ssh_server/etc/ssh/").
-define(SSH_SERVER_USER_DIR,    "test/data/ssh_server/home/ssh_server/ssh").
