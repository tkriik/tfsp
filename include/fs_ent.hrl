% File system entity metadata.
-record(fs_ent, { path      :: file:path(),
                  sha256    :: binary() | undefined,
                  size      :: non_neg_integer() | undefined,
                  type      :: regular | directory,
                  access    :: read | write | read_write | none | undefined,
                  mtime     :: non_neg_integer(),
                  deleted   :: boolean()
                }).

-type fs_ent() :: #fs_ent{}.
