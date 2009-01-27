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
         dispatcher,
         socket_type=tcp}).

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
  case open_connection(Config) of
    {ok, SocketType, Socket} ->
      {ok, #state{socket_type=SocketType, socket=Socket, dispatcher=Dispatcher, config=Config}};
    Error ->
      {stop, Error}
  end.

handle_call(current_buffer, _From, State) ->
  {reply, State#state.buffer, State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(reset, State) ->
  {noreply, State#state{buffer=[]}};

handle_cast({send_packet, Packet}, State) ->
  Result = case State#state.socket_type of
             tcp ->
               gen_tcp:send(State#state.socket, Packet);
             ssl ->
               ssl:send(State#state.socket, Packet)
           end,
  case Result of
    ok ->
      natter_logger:log(?FILE, ?LINE, ["Sent: ", Packet]),
      {noreply, State, 15000};
    {error, Reason} ->
      natter_logger:log(?FILE, ?LINE, ["Fatal Error", Reason]),
      {stop, Reason, State}
  end;

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({SockType, Socket, Data}, State) when SockType =:= tcp;
                                                  SockType =:= ssl ->
  natter_logger:log(?FILE, ?LINE, ["Received: ", Data]),
  reset_socket(State#state.socket_type, Socket),
  S1 = buffer_data(Data, State),
  case classify(S1#state.buffer) of
    start_stream ->
      dispatch(strip_xml_decl(S1#state.buffer), S1),
      {noreply, S1#state{buffer=[]}};
    end_stream ->
      %% Exit when the end of the XMPP stream is detected
      {stop, stream_end, S1#state{buffer=[]}};
    data ->
      case natter_packet_engine:analyze(S1#state.buffer) of
        {[], _} ->
          {noreply, S1};
        {Stanzas, NewBuffer} ->
          lists:foreach(fun(Stanza) -> dispatch(Stanza, S1) end, Stanzas),
          {noreply, S1#state{buffer=NewBuffer}}
      end
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  case State#state.socket of
    undefined ->
      ok;
    [] ->
      ok;
    Socket ->
      case State#state.socket_type of
        tcp ->
          gen_tcp:close(Socket);
        ssl ->
          ssl:close(Socket)
      end
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
classify(Buffer) ->
  case string:str(Buffer, "<stream:stream") of
    0 ->
      case string:str(Buffer, "</stream:stream") of
        0 ->
          data;
        _ ->
          end_stream
      end;
    _ ->
      start_stream
  end.

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

reset_socket(_, []) ->
  ok;
reset_socket(tcp, Socket) ->
  inet:setopts(Socket, [{active, once}]);
reset_socket(ssl, Socket) ->
  ssl:setopts(Socket, [{active, once}]).


open_connection(Config) ->
  Hosts = prepare_connect_values(Config),
  open_socket(Hosts).

%% TODO: Remove this function when SSL socket upgrades
%% work. Hopefully in R12B-6.
open_socket(Hosts) ->
  tcp_connect(Hosts).


%% FIXME: Test this code again when R12B-6 has been released
%% See if the socket upgrade code in ssl_new is fixed
%% http://www.nabble.com/Upgrading-gen_tcp-socket-to-ssl-td20322574.html
%% open_socket(Hosts) ->
%%   ssl:start(),
%%   case ssl_connect(Hosts) of
%%     {ok, ssl, Socket} ->
%%       natter_logger:log(?FILE, ?LINE, "Connected via SSL"),
%%       {ok, ssl, Socket};
%%     _Error ->
%%       natter_logger:log(?FILE, ?LINE, "Connected via plain TCP"),
%%       tcp_connect(Hosts)
%%   end.

prepare_connect_values(Config) ->
  Hosts = case proplists:get_value(service, Config) of
            undefined ->
              H = proplists:get_value(host, Config, "localhost"),
              P = proplists:get_value(port, Config, 5222),
              [{H, P}];
            {Service, Domain} ->
              case natter_srv:resolve_service(Service, Domain) of
                {ok, Entries} ->
                  Entries;
                Error ->
                  throw({config_error, Error})
              end
          end,
  User = proplists:get_value(user, Config),
  Password = proplists:get_value(password, Config),
  case User =:= undefined orelse Password =:= undefined of
    true ->
      throw({missing_config_value, "user or password"});
    false ->
      ok
  end,
  Hosts.

tcp_connect([{Host, Port}|T]) ->
  case gen_tcp:connect(Host, Port, [list, {keepalive, true},
                                    {nodelay, true},
                                    {packet, 0},
                                    {reuseaddr, true}]) of
    {ok, Sock} ->
      gen_tcp:controlling_process(Sock, self()),
      {ok, tcp, Sock};
    _ ->
      tcp_connect(T)
  end;
tcp_connect([]) ->
  {error, no_hosts_available}.

ssl_connect([{Host, Port}|T]) ->
  case ssl:connect(Host, Port, [list, {keepalive, true},
                                {nodelay, true},
                                {packet, 0},
                                {reuseaddr, true},
                                {verify, 0},
                                {depth, 0}]) of
    {ok, Socket} ->
      {ok, ssl, Socket};
    _Error ->
      ssl_connect(T)
  end;
ssl_connect([]) ->
  {error, no_hosts_available}.

strip_xml_decl(Buffer) ->
  case string:str(Buffer, "?>") of
    0 ->
      Buffer;
    Start ->
      string:substr(Buffer, Start + 2)
  end.
