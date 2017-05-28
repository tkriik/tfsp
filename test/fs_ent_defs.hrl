%%% Raw fs entity definitions for files in the test/data/scan directory

-include("fs.hrl").

-define(FS_ENT_LICENSE, #fs_ent{ path       = <<"test/data/scan/LICENSE.txt">>,
                                 sha256     = <<131,41,250,6,40,66,164,44,119,157,178,254,125,150,165,27,169,229,
                                                25,100,150,77,81,190,146,114,124,253,109,144,120,60>>,
                                 size       = 1064,
                                 type       = regular,
                                 access     = read_write,
                                 mtime      = 1494221667,
                                 deleted    = false }).

-define(FS_ENT_DAT, #fs_ent{ path       = <<"test/data/scan/empty.dat">>,
                             sha256     = <<227,176,196,66,152,252,28,20,154,251,244,200,153,111,185,36,39,
                                            174,65,228,100,155,147,76,164,149,153,27,120,82,184,85>>,
                             size       = 0,
                             type       = regular,
                             access     = read_write,
                             mtime      = 1494650079,
                             deleted    = false }).

-define(FS_ENT_NESTED_DIR, #fs_ent{ path    = <<"test/data/scan/nested">>,
                                    sha256  = undefined,
                                    size    = undefined,
                                    type    = directory,
                                    access  = read_write,
                                    mtime   = 1494221667,
                                    deleted = false }).

-define(FS_ENT_PELICAN, #fs_ent{ path       = <<"test/data/scan/pelican.jpg">>,
                                 sha256     = <<216,164,148,172,100,134,39,211,7,162,233,96,64,24,48,209,1,235,
                                                236,194,166,85,60,251,252,88,141,117,59,153,209,14>>,
                                 size       = 286889,
                                 type       = regular,
                                 access     = read_write,
                                 mtime      = 1494221667,
                                 deleted    = false }).

-define(FS_ENT_SCAN_DIR, #fs_ent{ path      = <<"test/data/scan">>,
                                  sha256    = undefined,
                                  size      = undefined,
                                  type      = directory,
                                  access    = read_write,
                                  mtime     = 1495949813,
                                  deleted   = false }).
