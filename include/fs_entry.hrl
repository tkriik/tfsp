% File system metadata entry.
-record(fs_entry, { path,
                    hash,
                    file_info }).

-type fs_entry() :: #fs_entry{}.
