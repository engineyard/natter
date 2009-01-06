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

-record(state,
        {routes=dict:new(),
         temp_routes=dict:new(),
         config,
         packetizer,
         inspector}).

%% API
-export([start_link/0, start_link/1, register_temporary_exchange/4, register_exchange/4]).
-export([unregister_exchange/3, dispatch/2, get_packetizer/1, send_and_wait/2, clear/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-spec(start_link/0 :: () -> {'ok', pid()}).
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec(start_link/1 :: (Config :: config()) -> {'ok', pid()}).
start_link(Config) ->
  gen_server:start_link(?MODULE, [Config], []).

-spec(clear/1 :: (ServerPid :: pid()) -> 'ok').
clear(ServerPid) ->
  gen_server:call(ServerPid, clear).

-spec(register_exchange/4 :: (ServerPid :: pid(), PacketType :: string(), TargetJid :: string() | 'default', ConsumerPid :: pid()) -> 'ok' | {'error', string()}).
register_exchange(ServerPid, PacketType, TargetJid, ConsumerPid) ->
  gen_server:call(ServerPid, {register_exchange, PacketType, TargetJid, ConsumerPid}).

-spec(register_temporary_exchange/4 :: (ServerPid :: pid(), PacketType :: string(), TargetJid :: string() | 'default', ConsumerPid :: pid()) -> 'ok' | {'error', string()}).
register_temporary_exchange(ServerPid, PacketType, TargetJid, ConsumerPid) ->
  gen_server:call(ServerPid, {register_temp_exchange, PacketType, TargetJid, ConsumerPid}).

-spec(unregister_exchange/3 :: (ServerPid :: pid(), PacketType :: string(), TargetJid :: string() | 'default') -> 'ok').
unregister_exchange(ServerPid, PacketType, TargetJid) ->
  gen_server:call(ServerPid, {unregister_exchange, PacketType, TargetJid, self()}).

-spec(get_packetizer/1 :: (ServerPid :: pid()) -> {ok, pid()}).
get_packetizer(ServerPid) ->
  gen_server:call(ServerPid, get_packetizer).

-spec(dispatch/2 :: (ServerPid :: pid(), Stanza :: string()) -> any()).
dispatch(ServerPid, Stanza) ->
  gen_server:cast(ServerPid, {dispatch, natter_parser:parse(Stanza)}).

-spec(send_and_wait/2 :: (ServerPid :: pid(), Stanza :: tuple() | string() ) -> {'ok', parsed_xml()} | {'error', 'timeout'}).
send_and_wait(ServerPid, Stanza) when is_list(Stanza) ->
  send_and_wait(ServerPid, natter_parser:parse(Stanza));
send_and_wait(ServerPid, {xmlelement, PacketType, Attrs, _} = Stanza) ->
  To = extract_routable_jid("to", Attrs),
  case register_temporary_exchange(ServerPid, PacketType, To, self()) of
    {error, already_registered} ->
      timer:sleep(100),
      send_and_wait(ServerPid, Stanza);
    ok ->
      gen_server:call(ServerPid, {send, Stanza}),
      receive
        {packet, Reply} ->
          {ok, Reply}
      after 60000 ->
          {error, timeout}
      end
  end.
%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}};

init([Config, InspectorPid]) ->
  process_flag(trap_exit, true),
  {ok, P} = natter_packetizer:start_link(Config, self()),
  {ok, #state{packetizer=P, config=Config, inspector=InspectorPid}}.

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

handle_call(get_packetizer, _From, State) ->
  {reply, State#state.packetizer, State};

handle_call({register_temp_exchange, PacketType, TargetJid, ConsumerPid}, _From, State) ->
  Key = make_exchange_key(PacketType, TargetJid),
  [Reply, NewState] = case dict:find(Key, State#state.temp_routes) of
                        {ok, Pids} ->
                          [ok, State#state{temp_routes=dict:store(Key, [ConsumerPid|Pids], State#state.temp_routes)}];
                        error ->
                          [ok, State#state{temp_routes=dict:store(Key, [ConsumerPid], State#state.temp_routes)}]
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
  case evaluate_stanza(Stanza, State#state.inspector) of
    route ->
      {noreply, route_message(Stanza, State)};
    drop ->
      {noreply, State};
    {Action, DelaySeconds} when Action =:= delay;
                                Action =:= duplicate ->
      spawn(fun() -> send_with_delay(self(), Stanza, DelaySeconds) end),
      {noreply, State}
  end;

handle_cast({redispatch, Stanza}, State) ->
  {noreply, route_message(Stanza, State)};

handle_cast(_Msg, State) ->
  {noreply, State}.


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
send_with_delay(DispatcherPid, Stanza, DelaySeconds) ->
  receive
    _ ->
      ok
  after DelaySeconds ->
      gen_server:cast(DispatcherPid, {redispatch, Stanza})
  end.

route_message({xmlelement, PacketType, Attrs, _}=Stanza, State) ->
  Jid = extract_routable_jid("from", Attrs),
  case find_target(PacketType, Jid, State) of
    {{ok, Routes}, S} ->
      lists:foreach(fun(R) -> R ! {packet, Stanza} end, Routes),
      S;
    {error, S} ->
      natter_logger:log(?FILE, ?LINE, ["Ignoring unroutable packet: ",
                                       natter_parser:element_to_string(Stanza)]),
      S
  end.


evaluate_stanza(Stanza, undefined) ->
  route.

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
      find_registered_route(routes, lists:reverse([make_exchange_key("all", default)|RouteNames]), State);
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
