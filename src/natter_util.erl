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

-export([load_xml_driver/0]).

load_xml_driver() ->
  try
    erl_ddll:info(natter_expat, driver_options)
  catch
    error:badarg ->
      load_xml_driver(natter_expat)
  end,
  ok.

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
