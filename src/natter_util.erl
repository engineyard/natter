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

-module(natter_util).

-author("ksmith@engineyard.com").

-export([load_xml_driver/0, build_iq_stanza/4, test_out/1, test_out/2]).

load_xml_driver() ->
  try
    erl_ddll:info(natter_expat, driver_options)
  catch
    error:badarg ->
      load_xml_driver(natter_expat)
  end,
  ok.

build_iq_stanza(Type, PacketId, To, Packet) ->
  T = "<iq xml:lang='en' type='~s'",
  {T1, D1} = case PacketId of
                 "" ->
                   {T, [Type]};
                 _ ->
                   {T ++ " id='~s'", [PacketId, Type]}
               end,
  {T2, D2} = case To of
               "" ->
                 {T1, D1};
               _ ->
                 {T1 ++ " to='~s'", [To|D1]}
             end,
  {T3, D3} = {T2 ++ ">~s</iq>", [Packet|D2]},
  io_lib:format(T3, lists:reverse(D3)).

test_out(Format, Data) ->
  test_out(io_lib:format(Format, Data)).
test_out(Message) ->
  file:write_file("/tmp/test_out.txt", io_lib:format("~s~n", [Message]), [append]).

%% Private functions
load_xml_driver(Name) ->
  erl_ddll:load_driver(get_so_path(), Name).

get_so_path() ->
  case code:priv_dir(natter) of
    {error,_} ->
      ".";
    Path ->
      filename:join([Path, "lib"])
  end.
