-module(tfsp_ssh_client).
-behaviour(ssh_channel).

-export([start_link/6]).

-export([init/1,
         terminate/2,

         handle_call/3,
         handle_cast/2,
         handle_msg/2,
         handle_ssh_msg/2,

         code_change/3]).

-include("conn.hrl").
-include("fs.hrl").


%% Specs

-spec start_link(fs_path(),
                 fs_ent_tab(),
                 string(),
                 integer(),
                 integer() | infinity,
                 fs_path()) -> {ok, pid()} | {error, term()}.


%% API

start_link(Root, FsTab, Host, Port, Timeout, UserDir) ->
    Opts = [{user_dir, UserDir}],
    case ssh:connect(Host, Port, Opts, Timeout) of
        {ok, ConnRef} ->
            case ssh_connection:session_channel(ConnRef, Timeout) of
                {ok, ChanId} ->
                    ssh_channel:start_link(ConnRef, ChanId, tfsp_ssh_client, [Root, FsTab]);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% SSH channel callbacks

init([_Root, _FsTab]) ->
    St = #conn_st{ buffer = <<>> },
    {ok, St}.

terminate(_Reason, _St) ->
    ok.

handle_call(_Msg, _From, St) ->
    {noreply, St}.

handle_cast(_Msg, St) ->
    {noreply, St}.

handle_msg(_Msg, St) ->
    {ok, St}.

% Got data from channel, append to connection state buffer.
handle_ssh_msg({ssh_cm, _ConnRef, {data, _ChanId, 0, Data}},
               #conn_st { buffer = Buffer } = St) ->
    _Buffer = <<Buffer/binary, Data/binary>>,
    _St = St#conn_st{ buffer = _Buffer },
    {ok, _St};
%% Got EOF from channel, flush connection state buffer.
handle_ssh_msg({ssh_cm, _ConnRef, {eof, _ChanId}}, St) ->
    _St = St#conn_st{ buffer = <<>> },
    {ok, _St};
handle_ssh_msg(_Msg, St) ->
    {ok, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
