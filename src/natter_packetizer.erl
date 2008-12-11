-module(natter_packetizer).

-behaviour(gen_server).

-author("ksmith@engineyard.com").


%% API
-export([start_link/0, start_link/2, current_buffer/1, reset/1, send/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {buffer=[],
         config,
         socket,
         dispatcher}).

send(ServerPid, Packet) ->
  gen_server:cast(ServerPid, {send_packet, Packet}).

current_buffer(ServerPid) ->
  Buffer = gen_server:call(ServerPid, current_buffer),
  lists:flatten(Buffer).

reset(ServerPid) ->
  gen_server:cast(ServerPid, reset).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

start_link(Config, Dispatcher) ->
  gen_server:start_link(?MODULE, [{Config, Dispatcher}], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}};

init([{Config, Dispatcher}]) ->
  process_flag(trap_exit, true),
  {ok, Socket} = open_connection(Config),
  {ok, #state{socket=Socket, dispatcher=Dispatcher, config=Config}}.

handle_call(current_buffer, _From, State) ->
  {reply, State#state.buffer, State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(reset, State) ->
  {noreply, State#state{buffer=[]}};

handle_cast({send_packet, Packet}, State) ->
  ok = gen_tcp:send(State#state.socket, Packet),
  natter_logger:log(?FILE, ?LINE, ["Sent: ", Packet]),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
  natter_logger:log(?FILE, ?LINE, ["Received: ", Data]),
  reset_socket(Socket),
  S1 = buffer_data(Data, State),
  NewState = case string:str(S1#state.buffer, "<stream:stream") of
               0 ->
                 case natter_packet_engine:analyze(S1#state.buffer) of
                   {[], _} ->
                     S1;
                   {Stanzas, NewBuffer} ->
                     lists:foreach(fun(Stanza) -> dispatch(Stanza, S1) end, Stanzas),
                     S1#state{buffer=NewBuffer}
                 end;
               _ ->
                 dispatch(strip_xml_decl(S1#state.buffer), S1),
                 S1#state{buffer=[]}
             end,
  {noreply, NewState};


handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
  case State#state.socket of
    undefined ->
      ok;
    [] ->
      ok;
    Socket ->
      gen_tcp:close(Socket)
  end.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
dispatch(Stanza, State) ->
  if
    State#state.dispatcher =:= undefined ->
      ok;
    true ->
      FinalStanza = case erlang:hd(Stanza) of
                      62 ->
                        erlang:tl(Stanza);
                      60 ->
                        Stanza
                    end,
      natter_dispatcher:dispatch(State#state.dispatcher, FinalStanza)
  end.

buffer_data(Data, State) ->
  State#state{buffer=lists:append(State#state.buffer, Data)}.

reset_socket([]) ->
  ok;
reset_socket(Socket) ->
  inet:setopts(Socket, [{active, once}]).


open_connection(Config) ->
  Host = proplists:get_value(host, Config, "localhost"),
  Port = proplists:get_value(port, Config, 5222),
  User = proplists:get_value(user, Config),
  Password = proplists:get_value(password, Config),
  case User =:= undefined orelse Password =:= undefined of
    true ->
      throw({missing_config_value, "user or password"});
    false ->
      ok
  end,
  case proplists:get_value(ssl, Config) of
    undefined ->
      tcp_connect(Host, Port);
    false ->
      tcp_connect(Host, Port);
    true ->
      ssl_connect(Host, Port);
    Oops ->
      throw({badarg, Oops})
  end.

ssl_connect(_Host, _Port) ->
  exit(self(), unsupported_connect_type).

tcp_connect(Host, Port) ->
  {ok, Sock} = gen_tcp:connect(Host, Port, [list, {keepalive, true},
                                            {nodelay, true},
                                            {active, once},
                                            {packet, 0},
                                            {reuseaddr, true}]),
  gen_tcp:controlling_process(Sock, self()),
  {ok, Sock}.

strip_xml_decl(Buffer) ->
  case string:str(Buffer, "?>") of
    0 ->
      Buffer;
    Start ->
      string:substr(Buffer, Start + 2)
  end.
