%%% Packing and unpacking tfsp protocol frames.
-module(wire).
-export([pack_fs_ent_info/1,
         unpack_fs_ent_info/1]).

-include("fs.hrl").


%%% Protocol wire constants

%% event types

-define(EVENT_BITS,             8).
-define(EVENT_FS_ENT_INFO,      1).


%% fs entity property bits

-define(ENT_PATH_SIZE_BITS,     32).

-define(ENT_TYPE_BITS,          8).
-define(ENT_TYPE_REG,           0).
-define(ENT_TYPE_DIR,           1).

-define(ENT_MTIME_BITS,         64).

-define(ENT_ACCESS_BITS,        8).
-define(ENT_ACCESS_READ,        0).
-define(ENT_ACCESS_WRITE,       1).
-define(ENT_ACCESS_READ_WRITE,  2).
-define(ENT_ACCESS_NONE,        3).
-define(ENT_ACCESS_UNDEF,       4).

-define(ENT_DELETED_BITS,       1).
-define(ENT_DELETED_FALSE,      0).
-define(ENT_DELETED_TRUE,       1).

-define(ENT_SIZE_BITS,          64).

-define(ENT_SHA256_BYTES,       32).


%% API

% Constructs an fs entity info frame.
% TODO: endianness
pack_fs_ent_info(#fs_ent{ type      = Type,
                          path      = Path,
                          mtime     = Mtime,
                          access    = Access,
                          deleted   = Deleted,
                          size      = Size,
                          sha256    = Sha256 }) ->
    PathSize = erlang:byte_size(Path),
    AccessBits = case Access of 
                     read       -> ?ENT_ACCESS_READ;
                     write      -> ?ENT_ACCESS_WRITE;
                     read_write -> ?ENT_ACCESS_READ_WRITE;
                     none       -> ?ENT_ACCESS_NONE;
                     undefined  -> ?ENT_ACCESS_UNDEF
                 end,
    DeletedBits = case Deleted of
                      false -> ?ENT_DELETED_FALSE;
                      true  -> ?ENT_DELETED_TRUE
                  end,
    case Type of
        regular ->
            <<?EVENT_FS_ENT_INFO:?EVENT_BITS,
              ?ENT_TYPE_REG:?ENT_TYPE_BITS,
              PathSize:?ENT_PATH_SIZE_BITS,
              Path/binary,
              Mtime:?ENT_MTIME_BITS,
              AccessBits:?ENT_ACCESS_BITS,
              DeletedBits:?ENT_DELETED_BITS,
              Size:?ENT_SIZE_BITS,
              Sha256:?ENT_SHA256_BYTES/binary>>;
        directory ->
            <<?EVENT_FS_ENT_INFO:?EVENT_BITS,
              ?ENT_TYPE_DIR:?ENT_TYPE_BITS,
              PathSize:?ENT_PATH_SIZE_BITS,
              Path/binary,
              Mtime:?ENT_MTIME_BITS,
              AccessBits:?ENT_ACCESS_BITS,
              DeletedBits:?ENT_DELETED_BITS>>
    end.

% Unpacks an fs entity info frame.
% TODO: endianness
unpack_fs_ent_info(<<?EVENT_FS_ENT_INFO:?EVENT_BITS,
                     TypeBits:?ENT_TYPE_BITS,
                     PathSize:?ENT_PATH_SIZE_BITS,
                     Path:PathSize/binary,
                     Mtime:?ENT_MTIME_BITS,
                     AccessBits:?ENT_ACCESS_BITS,
                     DeletedBits:?ENT_DELETED_BITS,
                     Rest/binary>>) ->
    Access = case AccessBits of
                 ?ENT_ACCESS_READ       -> read;
                 ?ENT_ACCESS_WRITE      -> write;
                 ?ENT_ACCESS_READ_WRITE -> read_write;
                 ?ENT_ACCESS_NONE       -> none;
                 ?ENT_ACCESS_UNDEF      -> undefined
             end,
    Deleted = case DeletedBits of
                  ?ENT_DELETED_FALSE    -> false;
                  ?ENT_DELETED_TRUE     -> true
              end,
    case TypeBits of
        ?ENT_TYPE_REG ->
            <<Size:?ENT_SIZE_BITS, Sha256:?ENT_SHA256_BYTES/binary>> = Rest,
            #fs_ent{ type       = regular,
                     path       = Path,
                     mtime      = Mtime,
                     access     = Access,
                     deleted    = Deleted,
                     size       = Size,
                     sha256     = Sha256 };
        ?ENT_TYPE_DIR ->
            #fs_ent{ type       = directory,
                     path       = Path,
                     mtime      = Mtime,
                     access     = Access,
                     deleted    = Deleted }
    end.
