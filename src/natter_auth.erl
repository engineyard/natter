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

-module(natter_auth).

-author("ksmith@engineyard.com").

-behaviour(gen_server).

-include("typespecs.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {user,
         password,
         resource,
         owner,
         dispatcher}).

-spec(start_link/3 :: (Dispatcher :: pid(), Owner :: pid(), Config :: config()) -> {'ok', pid()} | {'error', string() | atom()}).
start_link(Dispatcher, Owner, Config) ->
  gen_server:start_link(?MODULE, [Dispatcher, Owner, Config], []).

init([Dispatcher, Owner, Config]) ->
  Owner ! {auth_pid, self()},
  ok = natter_dispatcher:register_exchange(Dispatcher, "all", default, self()),
  Host = proplists:get_value(host, Config, "localhost"),
  User = proplists:get_value(user, Config),
  Password = proplists:get_value(password, Config),
  Resource = proplists:get_value(resource, Config, User),
  ok = natter_dispatcher:raw_send(Dispatcher, build_streams_header(Host)),
  {ok, #state{user=User, password=Password, resource=Resource,
              owner=Owner, dispatcher=Dispatcher}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({packet, {xmlelement, "stream:stream", _, _}}, State) ->
  AuthStanza = natter_util:build_iq_stanza("set", "1", "", build_auth_packet(State#state.user,
                                                                             State#state.password,
                                                                             State#state.resource)),
  case natter_dispatcher:send_and_wait(State#state.dispatcher, AuthStanza) of
    {ok, {xmlelement, "iq", Attrs, _}} ->
      case proplists:get_value("type", Attrs) of
        "result" ->
          State#state.owner ! {auth, ok};
        "error" ->
          State#state.owner ! {auth, err}
      end;
    Error ->
      io:format("Send and wait returned: ~p~n", [Error]),
      State#state.owner ! {auth, fatal, {Error}}
  end,
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  natter_dispatcher:unregister_exchange(State#state.dispatcher, "all", default),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
-spec(build_streams_header/1 :: (Host :: string()) -> string()).
build_streams_header(Host) ->
  "<?xml version=\"1.0\" ?>\n<stream:stream to=\"" ++ Host ++
    "\" xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\">".

-spec(build_auth_packet/3 :: (User :: string(), Password :: string(), Resource :: string()) -> string()).
build_auth_packet(User, Password, Resource) ->
  Packet = "<query xmlns=\"jabber:iq:auth\">" ++
           "<username>~s</username><password>~s</password>" ++
           "<resource>~s</resource></query>",
  lists:flatten(io_lib:format(Packet, [User, Password, Resource])).
