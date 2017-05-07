% File system metadata entry.
-record(fs_entry, { path,
                    hash,
                    crc,
                    file_info }).

-type fs_entry() :: #fs_entry{}.
