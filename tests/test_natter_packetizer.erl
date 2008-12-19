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

-module(test_natter_packetizer).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  [{setup, fun() ->
               {ok, Pid} = natter_packetizer:start_link(),
               register(natterp, Pid),
               Pid end,
    fun(P) ->
        exit(P, shutdown) end,
    [fun() ->
         smsg({tcp, [], "<iq><foo/>"}),
         ?assertMatch("<iq><foo/>", natter_packetizer:current_buffer(natterp)),
         smsg({tcp, [], "</iq>"}),
         ?assertMatch("", natter_packetizer:current_buffer(natterp)),
         reset() end,
     fun() ->
         smsg({tcp, [], "<iq><foo></foo>"}),
         smsg({tcp, [], "<wibble></wibble>"}),
         ?assertMatch("<iq><foo></foo><wibble></wibble>", natter_packetizer:current_buffer(natterp)),
         reset() end,
     fun() ->
         smsg({tcp, [], "<iq><foo></foo>"}),
         smsg({tcp, [], "</iq><iq><foo>"}),
         ?assertMatch("<iq><foo>", natter_packetizer:current_buffer(natterp)),
         reset() end,
     fun() ->
         smsg({tcp, [], "<iq><foo /></iq>"}),
         ?assertMatch("", natter_packetizer:current_buffer(natterp)),
         reset() end,
     fun() ->
         smsg({tcp, [], "<iq><foo /></iq>"}),
         ?assertMatch("", natter_packetizer:current_buffer(natterp)),
         reset() end,
     fun() ->
         smsg({tcp, [], "<iq id=\"1\" />"}),
         ?assertMatch("", natter_packetizer:current_buffer(natterp)),
         reset() end,
     fun() ->
         smsg({tcp, [], "<foo>"}),
         smsg({tcp, [], "<iq id=\"abc\"></iq>"}),
         ?assertMatch("<foo><iq id=\"abc\"></iq>", natter_packetizer:current_buffer(natterp)),
         reset() end,
     fun() ->
         smsg({tcp, [], "<iq i"}),
         smsg({tcp, [], "d"}),
         smsg({tcp, [], "=\"1"}),
         smsg({tcp, [], "\"/"}),
         smsg({tcp, [], ">"}),
         ?assertMatch("", natter_packetizer:current_buffer(natterp)),
         reset() end]}].

%% Helper functions
reset() ->
  natter_packetizer:reset(natterp).

smsg(Message) ->
  natterp ! Message,
  timer:sleep(25).
