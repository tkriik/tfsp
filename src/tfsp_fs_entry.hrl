% File system metadata entry.
-record(tfsp_fs_entry, { path,
                         hash,
                         crc,
                         size,
                         type,
                         access,
                         mtime }).

-type tfsp_fs_entry() :: #tfsp_fs_entry{}.
