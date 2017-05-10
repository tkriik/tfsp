% File system metadata entry.
-record(fs_entry, { path,
                    hash,
                    size,
                    type,
                    access,
                    mtime,
                    deleted }).

-type fs_entry() :: #fs_entry{}.
