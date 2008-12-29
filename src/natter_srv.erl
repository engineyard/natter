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

-module(natter_srv).

-author("ksmith@engineyard.com").

-include_lib("kernel/include/inet.hrl").

-export([resolve_service/2, resolve_service/3]).

resolve_service(Service, Domain) ->
  resolve_service(Service, "tcp", Domain).

resolve_service(Service, Protocol, Domain) ->
  ServiceName = lists:flatten(["_", Service, "._", Protocol, ".", Domain]),
  case inet_res:getbyname(ServiceName, srv) of
    {ok, R} ->
      #hostent{h_addr_list=Addrs} = R,
      E = lists:sort(Addrs),
      {ok, lists:foldr(fun({_, _, Port, Host}, Acc) -> [{Host, Port}|Acc] end, [], E)};
    Error ->
      Error
  end.
