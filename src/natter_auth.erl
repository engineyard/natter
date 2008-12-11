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
         connection}).

-spec(start_link/3 :: (Connection :: pid(), Owner :: pid(), Config :: config()) -> {'ok', pid()} | {'error', string() | atom()}).
start_link(Connection, Owner, Config) ->
  gen_server:start_link(?MODULE, [Connection, Owner, Config], []).

init([Connection, Owner, Config]) ->
  natter_connection:register_default_exchange(Connection, self()),
  Host = proplists:get_value(host, Config, "localhost"),
  User = proplists:get_value(user, Config),
  Password = proplists:get_value(password, Config),
  Resource = proplists:get_value(resource, Config, User),
  natter_connection:raw_send(Connection, build_streams_header(Host)),
  {ok, #state{user=User, password=Password, resource=Resource,
              owner=Owner, connection=Connection}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({packet, {xmlelement, "stream:stream", _, _}}, State) ->
  case natter_connection:send_wait_iq(State#state.connection, "set", "1", "", build_auth_packet(State#state.user,
                                                                                                State#state.password,
                                                                                                State#state.resource)) of
    {ok, {xmlelement, "iq", Attrs, _}} ->
      case proplists:get_value("type", Attrs) of
        "result" ->
          State#state.owner ! {auth, ok};
        "error" ->
          State#state.owner ! {auth, err}
      end;
    Error ->
      State#state.owner ! Error
  end,
  {stop, normal, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  natter_connection:unregister_default_exchange(State#state.connection),
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
