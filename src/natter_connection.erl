-module(natter_connection).

-author("ksmith@engineyard.com").

-behaviour(supervisor).

-include("typespecs.hrl").

%% API
-export([start_link/1, close/1, register_default_exchange/2, unregister_default_exchange/1]).
-export([register_exchange/3, unregister_exchange/2, raw_send/2, send_iq/5, send_wait_iq/5]).

%% Supervisor callbacks
-export([init/1]).

-spec(start_link/1 :: (Config :: config()) -> {ok, pid()} | {exit, string()}).
start_link(Config) ->
  {ok, Pid} = supervisor:start_link(?MODULE, [Config]),
  try
    natter_auth:start_link(Pid, self(), Config),
    receive
      {auth, ok} ->
        {ok, Pid};
      {auth, err} ->
        {error, auth_error}
    after 60000 ->
        {error, auth_error}
    end
  catch
    Error ->
      {error, Error}
  end.

-spec(close/1 :: (ConnectionPid :: pid()) -> ok).
close(ConnectionPid) ->
  exit(ConnectionPid, shutdown),
  ok.

-spec(register_default_exchange/2 :: (ConnectionPid :: pid(), SinkPid :: pid()) -> ok | natter_error()).
register_default_exchange(ConnectionPid, SinkPid) ->
  DispatcherPid = find_child(ConnectionPid, natter_dispatcher),
  natter_dispatcher:register_exchange(DispatcherPid, default, SinkPid).

-spec(unregister_default_exchange/1 :: (ConnectionPid :: pid()) -> ok).
unregister_default_exchange(ConnectionPid) ->
  DispatcherPid = find_child(ConnectionPid, natter_dispatcher),
  natter_dispatcher:unregister_exchange(DispatcherPid, default).

-spec(register_exchange/3 :: (ConnectionPid :: pid(), TargetJid :: string(), SinkPid :: pid()) -> ok | natter_error()).
register_exchange(ConnectionPid, TargetJid, SinkPid) when is_list(TargetJid) ->
  DispatcherPid = find_child(ConnectionPid, natter_dispatcher),
  natter_dispatcher:register_exchange(DispatcherPid, TargetJid, SinkPid).

-spec(unregister_exchange/2 :: (ConnectionPid :: pid(), TargetJid :: string()) -> 'ok').
unregister_exchange(ConnectionPid, TargetJid) when is_list(TargetJid) ->
  DispatcherPid = find_child(ConnectionPid, natter_dispatcher),
  natter_dispatcher:unregister_exchange(DispatcherPid, TargetJid).

-spec(raw_send/2 :: (ConnectionPid :: pid(), Packet :: string()) -> 'ok').
raw_send(ConnectionPid, Packet) ->
  Packetizer = find_child(ConnectionPid, natter_packetizer),
  natter_packetizer:send(Packetizer, Packet).

-spec(send_iq/5 :: (ConnectionPid :: pid(), Type :: string(), PacketId :: string(), To :: string(), Packet :: string()) -> 'ok').
send_iq(ConnectionPid, Type, PacketId, To, Packet) ->
  Packetizer = find_child(ConnectionPid, natter_packetizer),
  Iq = build_iq_packet(Type, PacketId, To, Packet),
  natter_packetizer:send(Packetizer, Iq).

-spec(send_wait_iq/5 :: (ConnectionPid :: pid(), Type :: string(), PacketId :: string(),
                         To :: string(), Packet :: string()) -> {'ok', parsed_xml()} | natter_error()).

send_wait_iq(ConnectionPid, Type, PacketId, To, Packet) when Type =:= "set";
                                                             Type =:= "get" ->
  Dispatcher = find_child(ConnectionPid, natter_dispatcher),
  Iq = build_iq_packet(Type, PacketId, To, Packet),
  Result = natter_dispatcher:send_and_wait(Dispatcher, Iq),
  case Result of
    {ok, {xmlelement, _, Attrs, _}} ->
      case proplists:get_value("type", Attrs) of
        "result" ->
          Result;
        _ ->
          throw({illegal_xmpp_reply, Result})
      end;
    Err ->
      Err
  end.

init([Config]) ->
  {ok, {{one_for_all, 5, 60}, build_child_specs(Config)}}.

%% Internal functions
-spec(find_child/2 :: (ConnectionPid :: pid(), 'natter_dispatcher' | 'natter_packetizer') -> pid() | atom()).
find_child(ConnectionPid, natter_dispatcher) ->
  lists:foldl(fun({Id, ChildPid, _, _}, Acc) ->
                  if
                    Id =:= natter_dispatcher ->
                      ChildPid;
                    true ->
                      Acc
                  end end, not_found, supervisor:which_children(ConnectionPid));
find_child(ConnectionPid, natter_packetizer) ->
  DispatcherPid = find_child(ConnectionPid, natter_dispatcher),
  natter_dispatcher:get_packetizer(DispatcherPid).

build_iq_packet(Type, PacketId, To, Packet) ->
  T = "<iq xml:lang='en' type='~s'",
  {T1, D1} = case PacketId of
                 "" ->
                   {T, [Type]};
                 _ ->
                   {T ++ " id='~s'", [PacketId, Type]}
               end,
  {T2, D2} = case To of
               "" ->
                 {T1, D1};
               _ ->
                 {T1 ++ " to='~s'", [To|D1]}
             end,
  {T3, D3} = {T2 ++ ">~s</iq>", [Packet|D2]},
  io_lib:format(T3, lists:reverse(D3)).

build_child_specs(Config) ->
  CS1 = [{natter_dispatcher,
          {natter_dispatcher, start_link, [Config]},
          transient,
          5,
          worker,
    [natter_dispatcher]}],
  case proplists:get_value(log_file, Config) of
    undefined ->
      CS1;
    LogFile ->
      lists:reverse([{natter_logger,
                      {natter_logger, start_link, [LogFile]},
                      transient,
                      5,
                      worker,
                      [natter_logger]}|CS1])
  end.
