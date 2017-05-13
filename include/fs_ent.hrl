% File system entity metadata.
-record(fs_ent, { path,
                  hash,
                  size,
                  type,
                  access,
                  mtime,
                  deleted }).

-type fs_ent() :: #fs_ent{}.
