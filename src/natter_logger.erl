-module(natter_logger).

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
