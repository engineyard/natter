-module(natter_dispatcher).

-behaviour(gen_server).

-include("typespecs.hrl").

-author("ksmith@engineyard.com").

-define(SERVER, ?MODULE).

-record(state,
        {routes=dict:new(),
         temp_routes=dict:new(),
         config,
         packetizer}).

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
  gen_server:call(ServerPid, {unregister_exchange, PacketType, TargetJid}).

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

init([Config]) ->
  process_flag(trap_exit, true),
  {ok, P} = natter_packetizer:start_link(Config, self()),
  {ok, #state{packetizer=P, config=Config}}.

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
                        {ok, _} ->
                          [{error, already_registered}, State];
                        error ->
                          [ok, State#state{temp_routes=dict:store(Key, ConsumerPid, State#state.temp_routes)}]
                      end,
  {reply, Reply, NewState};

handle_call({register_exchange, PacketType, TargetJid, ConsumerPid}, _From, State) ->
  Key = make_exchange_key(PacketType, TargetJid),
  [Reply, NewState] = case dict:find(Key, State#state.routes) of
                     {ok, CP} ->
                       [{error, already_registered, TargetJid, CP}, State];
                     error ->
                       [ok, State#state{routes=dict:store(Key, ConsumerPid, State#state.routes)}]
                   end,
  {reply, Reply, NewState};

handle_call({unregister_exchange, PacketType, TargetJid}, _From, State) ->
  Key = make_exchange_key(PacketType, TargetJid),
  {reply, ok, State#state{routes=dict:erase(Key, State#state.routes)}};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast({dispatch, {xmlelement, PacketType, Attrs, _}=Stanza}, State) ->
  Jid = extract_routable_jid("from", Attrs),
  NewState = case find_target(PacketType, Jid, State) of
               {{ok, ConsumerPid}, S} ->
                 ConsumerPid ! {packet, Stanza},
                 S;
               {error, S} ->
                 natter_logger:log(?FILE, ?LINE, ["Ignoring unroutable packet: ",
                                                  natter_parser:element_to_string(Stanza)]),
                 S
             end,
  {noreply, NewState};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) ->
  case State#state.packetizer =:= undefined of
    true ->
      ok;
    false ->
      exit(State#state.packetizer, Reason)
  end,
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
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
      find_registered_route(routes, lists:reverse([default|RouteNames]), State);
    Route ->
      Route
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
    Route ->
      {Route, State}
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
