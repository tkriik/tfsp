%%% File system -related records and types.

%% Ensure we always operate on binary paths.
-type fs_path() :: binary().

%% File system entity metadata.
-record(fs_ent, { % both regular file and directory entities shared these 
                  type      :: regular | directory,
                  path      :: fs_path(),
                  mtime     :: non_neg_integer(),
                  access    :: read | write | read_write | none | undefined,
                  deleted   :: boolean(),

                  % only regular files have size and sha256 set
                  size      :: non_neg_integer() | undefined,
                  sha256    :: binary() | undefined }).

-type fs_ent() :: #fs_ent{}.


%% Entity table handle
-type fs_ent_tab() :: {fs_ent_tab, ets:tid()}.


%% File system context for each sync handler.
-record(fs_ctx, { ev_mgr_ref    :: pid(), % event manager pid
                  root          :: fs_path(),
                  ent_tab       :: fs_ent_tab() }).

-type fs_ctx() :: #fs_ctx{}.
