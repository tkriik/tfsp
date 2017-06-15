%% Sync connection handler state.
-record(conn_st, { fs_ctx,
                   timeout  :: non_neg_integer(),
                   buffer }). % TODO: proto handler pid
