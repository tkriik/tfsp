% File system metadata entry.
-record(fs_entry, { path,
                    hash,
                    size,
                    type,
                    access,
                    mtime }).

-type fs_entry() :: #fs_entry{}.
