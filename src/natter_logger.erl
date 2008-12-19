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

-module(natter_logger).

-author("ksmith@engineyard.com").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/1, log/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {logfile}).

start_link(LogFile) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [LogFile], []).

log(Module, LineNo, Messages) ->
  case whereis(?SERVER) of
    undefined ->
      ok;
    _ ->
      gen_server:cast(?SERVER, {log, Module, LineNo, {messages, Messages}}),
      ok
  end.

init([LogFile]) ->
  {ok, #state{logfile=LogFile}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({log, Module, LineNo, {messages, Messages}}, State)  ->
  {_, _, Ms} = Now = erlang:now(),
  {{Yr, Mo, Day}, {Hr, Min, Sec}} = calendar:now_to_local_time(Now),
  LogEntryTemplate = "~p/~p/~p ~p:~p:~p:~p -- (~p:~p) ",
  {ok, Dev} = file:open(State#state.logfile, [append, raw]),
  file:write(Dev, io_lib:format(LogEntryTemplate, [Mo, Day, Yr,
                                                   Hr, Min, Sec, Ms / 1000,
                                                   Module, LineNo])),
  lists:foreach(fun(M) ->
                    file:write(Dev, M) end, Messages),
  file:write(Dev, "\n"),
  file:close(Dev),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
