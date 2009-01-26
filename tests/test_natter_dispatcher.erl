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

-define(ERROR_STANZA, {xmlelement, "iq", [{"type", "error"},
                                          {"to", "wtf@locahost"}], [{xmlelement, "error", [{"type", "cancel"},
                                                                                           {"code", "500"}], []}]}).

registration_test_() ->
  [{setup, fun start_dispatcher/0,
    fun stop_dispatcher/1,
    [?_assertMatch(ok, natter_dispatcher:register_exchange(natterd, "iq", "foo@localhost", self())),
     ?_assertMatch(ok, natter_dispatcher:unregister_exchange(natterd, "iq", "foo@localhost")),
     fun() ->
         natter_dispatcher:clear(natterd),
         ?assertMatch(ok, natter_dispatcher:register_exchange(natterd, "iq", "foo@localhost", self())),
         ?assertMatch(ok, natter_dispatcher:register_exchange(natterd, "iq", "foo@localhost", self())) end]}].

simple_routing_test_() ->
  [{setup, fun start_dispatcher/0,
    fun stop_dispatcher/1,
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
        natter_dispatcher:unregister_exchange(natterd, "iq", "bar@localhost") end]}].

async_routing_test_() ->
  [{setup, fun start_dispatcher/0,
    fun stop_dispatcher/1,
    [fun() ->
        natter_dispatcher:clear(natterd),
        setup_async_pair(self(), ?TEST_STANZA1, ?TEST_STANZA2),
        receive
          {result, DoesMatch} ->
            ?assertMatch(true, DoesMatch)
        after 2000 ->
            throw({error, timeout})
        end,
        natter_dispatcher:unregister_exchange(natterd, "iq", "foo@localhost"),
        natter_dispatcher:unregister_exchange(natterd, "iq", "bar@localhost") end]}].

error_routing_test_() ->
  [{setup, fun start_dispatcher/0,
    fun stop_dispatcher/1,
    [fun() ->
         natter_dispatcher:clear(natterd),
         send_and_verify(fun() -> setup_error_handler(self(), "iq", ?ERROR_STANZA) end,
                         ?ERROR_STANZA),
         natter_dispatcher:unregister_exchange(natterd, default, "error") end,
    fun() ->
         natter_dispatcher:clear(natterd),
         send_and_verify(fun() -> setup_error_handler(self(), "all", ?ERROR_STANZA) end,
                         ?ERROR_STANZA),
         natter_dispatcher:unregister_exchange(natterd, default, "error") end]}].

blocking_routing_test_() ->
  [{setup, fun start_dispatcher/0,
    fun stop_dispatcher/1,
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

timeout_routing_test_() ->
  [{setup, fun start_dispatcher/0,
    fun stop_dispatcher/1,
    [fun() ->
         natter_dispatcher:clear(natterd),
         setup_timeout_pair(self(), 1000, ?TEST_STANZA1, ?TEST_STANZA2),
         receive
           {error, timeout} ->
             ok;
           Result ->
             throw({error, unexpected_result, Result})
         after 10000 ->
             throw({error, timeout})
         end end]}].

start_dispatcher() ->
  {ok, Pid} = natter_dispatcher:start_link(),
  register(natterd, Pid),
  Pid.

stop_dispatcher(Pid) ->
  exit(Pid, shutdown).

send_and_verify(SetupFun, Stanza) ->
  natter_dispatcher:clear(natterd),
  SetupFun(),
  receive
    handler_ready ->
      natter_dispatcher:dispatch(natterd, natter_parser:element_to_string(Stanza))
  after 2000 ->
      throw({error, setup_timeout})
  end,
  receive
    {result, DoesMatch} ->
      ?assertMatch(true, DoesMatch);
    {worker_error, Error} ->
      throw({error, Error})
  after 2000 ->
      throw({error, timeout})
  end.


setup_error_handler(Owner, PacketType, ErrorStanza) ->
  spawn(fun() ->
            natter_dispatcher:register_exchange(natterd, PacketType, "error", self()),
            Owner ! handler_ready,
            receive
              {packet, Stanza} ->
                Owner ! {result, Stanza =:= ErrorStanza}
            after 500 ->
                Owner ! {worker_error, no_packet_routed}
            end,
            natter_dispatcher:unregister_exchange(natterd, default, "error") end).


setup_blocking_pair(Owner, ReqStanza, ReplyStanza) ->
  spawn(fun() ->
            timer:sleep(1000),
            case natter_dispatcher:send_and_wait(natterd, ReqStanza) of
              {ok, Result} ->
                Owner ! {result, Result =:= ReplyStanza};
              Error ->
                Owner ! Error
            end end),
  RecvWorker = spawn(fun() ->
                         receive
                           {packet, _Stanza} ->
                             natter_dispatcher:dispatch(natterd, natter_parser:element_to_string(ReplyStanza))
                         after 2000 ->
                             Owner ! {error, timeout}
                         end end),
  natter_dispatcher:register_exchange(natterd, "iq", "bar@localhost", RecvWorker).

setup_timeout_pair(Owner, Timeout, ReqStanza, ReplyStanza) when Timeout > 0 ->
  spawn(fun() ->
            timer:sleep(1000),
            case natter_dispatcher:send_and_wait(natterd, ReqStanza, Timeout) of
              {ok, Result} ->
                Owner ! {result, Result =:= ReplyStanza};
              Error ->
                Owner ! Error
            end end),
  RecvWorker = spawn(fun() ->
                         receive
                           {packet, _Stanza} ->
                             timer:sleep(Timeout + 500),
                             natter_dispatcher:dispatch(natterd, natter_parser:element_to_string(ReplyStanza))
                         end end),
  natter_dispatcher:register_exchange(natterd, "iq", "bar@localhost", RecvWorker).

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
  natter_dispatcher:register_exchange(natterd, "iq", "foo@localhost", SendWorker),
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
  natter_dispatcher:register_exchange(natterd, "iq", "bar@localhost", ReplyWorker).

setup_receiving_worker(Owner, Stanza) ->
  Worker = worker(Owner, Stanza),
  ?assertMatch(ok, natter_dispatcher:register_exchange(natterd, "iq", "bar@localhost", Worker)),
  Worker.

worker(Owner, Stanza) ->
  spawn(fun() ->
            receive
              {packet, S} ->
                Owner ! {result, S =:= Stanza}
            after 2000 ->
                Owner ! {error, timeout}
            end end).
