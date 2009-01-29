% Copyright 2008, Engine Yard, Inc.
%
% This file is part of Natter.
%
% Natter is free software: you can redistribute it and/or modify it under the
% terms of the GNU Lesser General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.
%
% Natter is distributed in the hope that it will be useful, but WITHOUT ANY
% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
% A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more
% details.
%
% You should have received a copy of the GNU Lesser General Public License
% along with Natter.  If not, see <http://www.gnu.org/licenses/>.

-module(natter_dispatcher).

-behaviour(gen_server).

-include("typespecs.hrl").

-author("ksmith@engineyard.com").

-define(SERVER, ?MODULE).

-define(DEFAULT_TIMEOUT, 0).

-record(state,
        {routes=dict:new(),
         temp_routes=dict:new(),
         config,
         packetizer,
         inspector_mod,
         inspector,
         state=normal,
         auth_pid}).

%% API
-export([start_link/0, start_link/3, register_temporary_exchange/4, register_temporary_exchange/5, register_exchange/4]).
-export([unregister_exchange/3, dispatch/2, get_packetizer/1, raw_send/2, send_and_wait/2, send_and_wait/3, clear/1]).

-export([simulate_auth/1, ping/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec(start_link/3 :: (Config :: config(), InspectorMod :: atom(), InspectorPid :: pid()) -> {'ok', pid()}).
start_link(Config, InspectorMod, InspectorPid) ->
  gen_server:start_link(?MODULE, [Config, InspectorMod, InspectorPid], []).

%% For unit testing only
simulate_auth(ServerPid) ->
  gen_server:call(ServerPid, simulate_auth).

ping(ServerPid) ->
  gen_server:call(ServerPid, ping).

-spec(clear/1 :: (ServerPid :: pid()) -> 'ok').
clear(ServerPid) ->
  case gen_server:call(ServerPid, clear) of
    {wait, reconnecting} ->
      reconnect_wait(),
      clear(ServerPid);
    V ->
      V
  end.

-spec(register_exchange/4 :: (ServerPid :: pid(), PacketType :: string(), TargetJid :: string() | 'default', ConsumerPid :: pid()) -> 'ok' | {'error', string()}).
register_exchange(ServerPid, PacketType, TargetJid, ConsumerPid) ->
  case gen_server:call(ServerPid, {register_exchange, PacketType, TargetJid, ConsumerPid}) of
    {wait, reconnecting} ->
      reconnect_wait(),
      register_exchange(ServerPid, PacketType, TargetJid, ConsumerPid);
    V ->
      V
  end.

-spec(register_temporary_exchange/4 :: (ServerPid :: pid(), PacketType :: string(),
                                        TargetJid :: string() | 'default', ConsumerPid :: pid()) -> 'ok' | {'error', string()}).
register_temporary_exchange(ServerPid, PacketType, TargetJid, ConsumerPid) ->
  register_temporary_exchange(ServerPid, PacketType, TargetJid, ConsumerPid, ?DEFAULT_TIMEOUT).

-spec(register_temporary_exchange/5 :: (ServerPid :: pid(), PacketType :: string(),
                                        TargetJid :: string() | 'default', ConsumerPid :: pid(), ExchangeTimeout :: integer()) -> 'ok' | {'error', string()}).
register_temporary_exchange(ServerPid, PacketType, TargetJid, ConsumerPid, ExchangeTimeout) ->
  case gen_server:call(ServerPid, {register_temp_exchange, PacketType, TargetJid, ConsumerPid, ExchangeTimeout}) of
    {wait, reconnecting} ->
      reconnect_wait(),
      register_temporary_exchange(ServerPid, PacketType, TargetJid, ConsumerPid, ExchangeTimeout);
    V ->
      V
  end.

-spec(unregister_exchange/3 :: (ServerPid :: pid(), PacketType :: string(), TargetJid :: string() | 'default') -> 'ok').
unregister_exchange(ServerPid, PacketType, TargetJid) ->
  case gen_server:call(ServerPid, {unregister_exchange, PacketType, TargetJid, self()}) of
    {wait, reconnecting} ->
      reconnect_wait(),
      unregister_exchange(ServerPid, PacketType, TargetJid);
    V ->
      V
  end.

-spec(get_packetizer/1 :: (ServerPid :: pid()) -> {ok, pid()}).
get_packetizer(ServerPid) ->
  case gen_server:call(ServerPid, get_packetizer) of
    {wait, reconnecting} ->
      reconnect_wait(),
      get_packetizer(ServerPid);
    V ->
      V
  end.

-spec(dispatch/2 :: (ServerPid :: pid(), Stanza :: string()) -> any()).
dispatch(ServerPid, Stanza) ->
  case gen_server:cast(ServerPid, {dispatch, natter_parser:parse(Stanza)}) of
    {wait, reconnecting} ->
      reconnect_wait(),
      dispatch(ServerPid, Stanza);
    V ->
      V
  end.

-spec(raw_send/2 :: (ServerPid :: pid(), Data :: string()) -> any()).
raw_send(ServerPid, Data) ->
  case gen_server:call(ServerPid, {raw_send, Data}) of
    {wait, reconnecting} ->
      reconnect_wait(),
      raw_send(ServerPid, Data);
    V ->
      V
  end.

-spec(send_and_wait/2 :: (ServerPid :: pid(), Stanza :: tuple() | string() ) -> {'ok', parsed_xml()} | {'error', 'timeout'}).
send_and_wait(ServerPid, Stanza) ->
  send_and_wait(ServerPid, Stanza, ?DEFAULT_TIMEOUT).

-spec(send_and_wait/3 :: (ServerPid :: pid(), Stanza :: tuple() | string(), Timeout :: integer() ) -> {'ok', parsed_xml()} | {'error', 'timeout'}).
send_and_wait(ServerPid, Stanza, Timeout) when is_list(Stanza) ->
  send_and_wait(ServerPid, natter_parser:parse(Stanza), Timeout);
send_and_wait(ServerPid, {xmlelement, PacketType, Attrs, _} = Stanza, Timeout) ->
  To = extract_routable_jid("to", Attrs),
  case register_temporary_exchange(ServerPid, PacketType, To, self(), Timeout) of
    {error, already_registered} ->
      timer:sleep(100),
      send_and_wait(ServerPid, Stanza, Timeout);
    {wait, reconnecting} ->
      reconnect_wait(),
      send_and_wait(ServerPid, Stanza, Timeout);
    ok ->
      send_with_timeout(ServerPid, Stanza, Timeout)
  end.
%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}};

init([Config, InspectorMod, InspectorPid]) ->
  process_flag(trap_exit, true),
  {ok, P} = natter_packetizer:start_link(Config, self()),
  if
    InspectorPid =:= undefined ->
      ok;
    true ->
      link(InspectorPid)
  end,
  authenticate(Config),
  {ok, #state{packetizer=P, state=auth, config=Config, inspector_mod=InspectorMod, inspector=InspectorPid}}.

handle_call(_, {SenderPid, _}, State) when State#state.state =:= auth,
                                           (State#state.auth_pid =:= SenderPid) == false ->
  {reply, {wait, reconnecting}, State};

handle_call(ping, _From, State) ->
  {reply, pong, State};

handle_call(simulate_auth, _From, State) ->
  {reply, ok, State#state{state=auth}};

handle_call(clear, _From, _State) ->
  {reply, ok, #state{}};

handle_call({send, Stanza}, _From, State) ->
  FinalStanza = natter_parser:element_to_string(prepare_stanza(Stanza, State#state.config)),
  Result = case State#state.packetizer of
             %% For unit testing only
             undefined ->
               timer:apply_after(250, natter_dispatcher, dispatch, [self(), FinalStanza]);
             _ ->
               natter_packetizer:send(State#state.packetizer, FinalStanza)
           end,
  {reply, Result, State};

handle_call({raw_send, Data}, _From, State) ->
  Result = case State#state.packetizer of
             undefined ->
               ok;
             _ ->
               natter_packetizer:send(State#state.packetizer, Data)
           end,
  {reply, Result, State};

handle_call(get_packetizer, _From, State) ->
  {reply, State#state.packetizer, State};

handle_call({register_temp_exchange, PacketType, TargetJid, ConsumerPid, Timeout}, _From, State) ->
  Key = make_exchange_key(PacketType, TargetJid),
  [Reply, NewState] = case dict:find(Key, State#state.temp_routes) of
                        {ok, Pids} ->
                          [ok, State#state{temp_routes=dict:store(Key, [ConsumerPid|Pids], State#state.temp_routes)}];
                        error ->
                          [ok, State#state{temp_routes=dict:store(Key, [ConsumerPid], State#state.temp_routes)}]
                      end,
  if
    Timeout > 0 ->
      timer:send_after(Timeout, {temp_exchange_timeout, Key, ConsumerPid});
    true ->
      ok
  end,
  {reply, Reply, NewState};

handle_call({register_exchange, PacketType, TargetJid, ConsumerPid}, _From, State) ->
  Key = make_exchange_key(PacketType, TargetJid),
  [Reply, NewState] = case dict:find(Key, State#state.routes) of
                     {ok, Pids} ->
                          [ok, State#state{routes=dict:store(Key, [ConsumerPid|Pids], State#state.routes)}];
                     error ->
                       [ok, State#state{routes=dict:store(Key, [ConsumerPid], State#state.routes)}]
                   end,
  {reply, Reply, NewState};

handle_call({unregister_exchange, PacketType, TargetJid, Consumer}, _From, State) ->
  Key = make_exchange_key(PacketType, TargetJid),
  FinalState = case dict:find(Key, State#state.routes) of
                {ok, Pids} ->
                   State#state{routes=dict:store(Key, lists:delete(Consumer, Pids), State#state.routes)};
                error ->
                   State
               end,
  {reply, ok, FinalState};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({dispatch, Stanza}, State) ->
  ServerPid = self(),
  case evaluate_inbound_stanza(Stanza, State#state.inspector_mod, State#state.inspector) of
    {replace, NewStanza} ->
      {noreply, route_message(NewStanza, State)};
    route ->
      {noreply, route_message(Stanza, State)};
    drop ->
      {noreply, State};
    {Action, DelaySeconds} when Action =:= delay ->
      spawn(fun() -> send_with_delay(ServerPid, Stanza, DelaySeconds) end),
      {noreply, State}
  end;

handle_cast({redispatch, Stanza}, State) ->
  {noreply, route_message(Stanza, State)};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(reconnected, State) ->
  NewState = if
               State#state.state =:= auth ->
                 State;
               true ->
                 authenticate(State#state.config),
                 State#state{state=auth}
             end,
  {noreply, NewState};

handle_info({auth_pid, AuthPid}, State) ->
  {noreply, State#state{auth_pid=AuthPid, state=auth}};

handle_info({temp_exchange_timeout, Key, ConsumerPid}, State) ->
  NewState = case dict:find(Key, State#state.temp_routes) of
               error ->
                 State;
               {ok, ConsumerPids} ->
                 FilteredPids = [P || P <- ConsumerPids, (P =:= ConsumerPid) =:= false],
                 State#state{temp_routes=dict:store(Key, FilteredPids, State#state.temp_routes)}
             end,
  {noreply, NewState};

handle_info({auth, ok}, State) ->
  {noreply, State#state{state=normal, auth_pid=undefined}};

handle_info({auth, err}, State) ->
  {stop, {error, {auth, err}}, State};

handle_info({auth, fatal, Reason}, State) ->
  {stop, {error, {auth, fatal, Reason}}, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  case State#state.packetizer =:= undefined of
    true ->
      ok;
    false ->
      exit(State#state.packetizer, Reason)
  end,
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
send_with_timeout(ServerPid, Stanza, Timeout) ->
  gen_server:call(ServerPid, {send, Stanza}),
  if
    Timeout > 0 ->
      receive
        {wait, reconnecting} ->
          reconnect_wait(),
          send_with_timeout(ServerPid, Stanza, Timeout);
        {packet, Reply} ->
          {ok, Reply}
      after Timeout ->
          {error, timeout}
      end;
    true ->
      receive
        {wait, reconnecting} ->
          reconnect_wait(),
          send_with_timeout(ServerPid, Stanza, Timeout);
        {packet, Reply} ->
          {ok, Reply}
      end
  end.


reconnect_wait() ->
  timer:sleep(random:uniform(200)).

authenticate(Config) ->
  Server = self(),
  timer:apply_after(100, natter_auth, start_link, [Server, Server, Config]).

send_with_delay(DispatcherPid, Stanza, DelaySeconds) ->
  timer:sleep(DelaySeconds * 1000),
  gen_server:cast(DispatcherPid, {redispatch, Stanza}).

route_message({xmlelement, PacketType, Attrs, _}=Stanza, State) ->
  RouteId = case extract_routable_jid("from", Attrs) of
              default ->
                case is_error(Attrs) of
                  true ->
                    "error";
                  false ->
                    default
                end;
              Value ->
                Value
            end,
  case find_target(PacketType, RouteId, State) of
    {{ok, Routes}, S} ->
      lists:foreach(fun(R) -> R ! {packet, Stanza} end, Routes),
      S;
    {error, S} ->
      natter_logger:log(?FILE, ?LINE, ["Ignoring unroutable packet: ",
                                       natter_parser:element_to_string(Stanza)]),
      S
  end.

evaluate_inbound_stanza(_Stanza, undefined, undefined) ->
  route;
evaluate_inbound_stanza(Stanza, InspectorMod, Inspector) when is_atom(InspectorMod),
                                                              is_pid(Inspector) ->
  InspectorMod:inspect_inbound_stanza(Inspector, Stanza).

is_error(Attrs) ->
  case proplists:get_value("type", Attrs) of
    "error" ->
      true;
    _ ->
      false
  end.

extract_routable_jid(FieldName, Attrs) ->
  case proplists:get_value(FieldName, Attrs) of
    undefined ->
      default;
    Value ->
      Value
  end.

find_target(PacketType, Jid, State) ->
  RouteNames = [make_exchange_key(PacketType, Jid)],
  case find_registered_route(temp_routes, RouteNames, State) of
    {error, State} ->
      RouteList = case Jid =:= "error" of
                    true ->
                      lists:append([RouteNames, [make_exchange_key("all", "error")], [make_exchange_key("all", default)]]);
                    false ->
                      lists:reverse([make_exchange_key("all", default)|RouteNames])
                  end,
      find_registered_route(routes, RouteList, State);
    Routes ->
      Routes
  end.

find_registered_route(temp_routes, [H|T], State) ->
  case dict:find(H, State#state.temp_routes) of
    error ->
      find_registered_route(temp_routes, T, State);
    Route ->
      {Route, State#state{temp_routes=dict:erase(H, State#state.temp_routes)}}
  end;
find_registered_route(temp_routes, [], State) ->
  {error, State};
find_registered_route(routes, [H|T], State) ->
  case dict:find(H, State#state.routes) of
    error ->
      find_registered_route(routes, T, State);
    Routes ->
      {Routes, State}
  end;
find_registered_route(routes, [], State) ->
  {error, State}.

prepare_stanza({xmlelement, Name, Attrs, SubEls}, Config) ->
  FinalAttrs = case proplists:get_value("from", Attrs) of
                 undefined ->
                   User = proplists:get_value(user, Config),
                   Host = proplists:get_value(host, Config),
                   Resource = proplists:get_value(resource, Config, User),
                   [{"from", lists:flatten(io_lib:format("~s@~s/~s", [User, Host, Resource]))}|Attrs];
                 _ ->
                   Attrs
               end,
  {xmlelement, Name, FinalAttrs, SubEls}.

make_exchange_key(PacketType, TargetJid) when is_atom(TargetJid) ->
  make_exchange_key(PacketType, atom_to_list(TargetJid));

make_exchange_key(PacketType, TargetJid) when is_list(TargetJid) ->
  lists:flatten([TargetJid, "|", PacketType]).
