%%% tfsp transport specs, currently only SSH transport is defined.

-ifndef(TFSP_TRANSPORT_HRL).
-define(TFSP_TRANSPORT_HRL, 1).

-type ssh_host() :: string().
-type ssh_port() :: non_neg_integer().
-type ssh_options() :: [term()].

-record(ssh_connect_transport, { host       :: ssh_host(),
                                 port       :: ssh_port(),
                                 options    :: ssh_options() }).

-record(ssh_serve_transport, { port         :: ssh_port(),
                               system_dir   :: string(),
                               user_dir     :: string() }).

-type ssh_connect_transport() :: #ssh_connect_transport{}.
-type ssh_serve_transport() :: #ssh_serve_transport{}.

-type connect_transport() :: ssh_connect_transport().
-type serve_transport() :: ssh_serve_transport().

-type sync_transport() :: connect_transport() | serve_transport().

-endif.
