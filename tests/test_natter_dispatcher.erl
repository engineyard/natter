-module(test_natter_dispatcher).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").

-define(TEST_STANZA1, {xmlelement, "iq", [{"from", "bar@localhost"},
                                          {"to", "foo@localhost"},
                                          {"id", "100"},
                                          {"type", "set"}], [{xmlelement, "op", [{"token", "abc:def"},
                                                                               {"type", "/security/verify"}], []}]}).

-define(TEST_STANZA2, {xmlelement, "iq", [{"from", "foo@localhost"},
                                          {"to", "bar@localhost"},
                                          {"id", "100"},
                                          {"type", "result"}], [{xmlelement, "ack", [{"token", "def:ghi"}], []}]}).

registration_test_() ->
  [{setup, fun() ->
               {ok, Pid} = natter_dispatcher:start_link(),
               register(natterd, Pid),
               Pid end,
    fun(P) ->
        exit(P, shutdown) end,
    [?_assertMatch(ok, natter_dispatcher:register_exchange(natterd, "foo@localhost", self())),
     ?_assertMatch(ok, natter_dispatcher:unregister_exchange(natterd, "foo@localhost")),
     fun() ->
         natter_dispatcher:clear(natterd),
         ?assertMatch(ok, natter_dispatcher:register_exchange(natterd, "foo@localhost", self())),
         ?assertMatch({error, already_registered, _To, _CP},
                      natter_dispatcher:register_exchange(natterd, "foo@localhost", self())) end]}].

simple_routing_test_() ->
  [{setup, fun() ->
               {ok, Pid} = natter_dispatcher:start_link(),
               register(natterd, Pid),
               Pid end,
    fun(P) ->
        exit(P, shutdown) end,
    [fun() ->
        natter_dispatcher:clear(natterd),
        setup_receiving_worker(self(), ?TEST_STANZA1),
        ?assertMatch(ok, natter_dispatcher:dispatch(natterd, natter_parser:element_to_string(?TEST_STANZA1))),
        receive
          {result, DoesMatch} ->
            ?assertMatch(true, DoesMatch);
          {error, timeout} ->
            throw({error, worker_timeout})
        after 2000 ->
            throw({error, timeout})
        end,
        natter_dispatcher:unregister_exchange(natterd, "bar@localhost") end]}].
async_routing_test_() ->
  [{setup, fun() ->
               {ok, Pid} = natter_dispatcher:start_link(),
               register(natterd, Pid),
               Pid end,
    fun(P) ->
        exit(P, shutdown) end,
    [fun() ->
        natter_dispatcher:clear(natterd),
        setup_async_pair(self(), ?TEST_STANZA1, ?TEST_STANZA2),
        receive
          {result, DoesMatch} ->
            ?assertMatch(true, DoesMatch)
        after 2000 ->
            throw({error, timeout})
        end,
        natter_dispatcher:unregister_exchange(natterd, "foo@localhost"),
        natter_dispatcher:unregister_exchange(natterd, "bar@localhost") end]}].

blocking_routing_test_() ->
  [{setup, fun() ->
               {ok, Pid} = natter_dispatcher:start_link(),
               register(natterd, Pid),
               Pid end,
    fun(P) ->
        exit(P, shutdown) end,
    [fun() ->
         natter_dispatcher:clear(natterd),
         setup_blocking_pair(self(), ?TEST_STANZA1, ?TEST_STANZA2),
         receive
           {result, DoesMatch} ->
             ?assertMatch(true, DoesMatch);
           {worker_error, Error} ->
             throw({error, Error})
         after 2000 ->
             throw({error, timeout})
         end end]}].

setup_blocking_pair(Owner, ReqStanza, ReplyStanza) ->
  spawn(fun() ->
            timer:sleep(1000),
            case natter_dispatcher:send_and_wait(natterd, ReqStanza) of
              {ok, Result} ->
                Owner ! {result, Result =:= ReplyStanza};
              Error ->
                Owner ! {worker_error, Error}
            end end),
  RecvWorker = spawn(fun() ->
                         receive
                           {packet, _Stanza} ->
                             natter_dispatcher:dispatch(natterd, natter_parser:element_to_string(ReplyStanza))
                         after 2000 ->
                             Owner ! {error, timeout}
                         end end),
  natter_dispatcher:register_exchange(natterd, "bar@localhost", RecvWorker).

setup_async_pair(Owner, ReqStanza, ReplyStanza) ->
  SendWorker = spawn(fun() ->
                         timer:sleep(1000),
                         natter_dispatcher:dispatch(natterd, natter_parser:element_to_string(ReqStanza)),
                         receive
                           {packet, S} ->
                             Owner ! {result, S =:= ReplyStanza}
                         after 2000 ->
                             Owner ! {error, timeout}
                         end end),
  natter_dispatcher:register_exchange(natterd, "foo@localhost", SendWorker),
  ReplyWorker = spawn(fun() ->
                          receive
                            {packet, S} ->
                              case S =:= ReqStanza of
                                false ->
                                  exit(Owner, {error, badmatch, {ReqStanza, S}});
                                true ->
                                  natter_dispatcher:dispatch(natterd, natter_parser:element_to_string(ReplyStanza))
                              end
                          after 2000 ->
                              Owner ! {error, timeout}
                          end end),
  natter_dispatcher:register_exchange(natterd, "bar@localhost", ReplyWorker).

setup_receiving_worker(Owner, Stanza) ->
  Worker = worker(Owner, Stanza),
  ?assertMatch(ok, natter_dispatcher:register_exchange(natterd, "bar@localhost", Worker)),
  Worker.

worker(Owner, Stanza) ->
  spawn(fun() ->
            receive
              {packet, S} ->
                Owner ! {result, S =:= Stanza}
            after 2000 ->
                Owner ! {error, timeout}
            end end).
