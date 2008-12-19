
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

-module(natter_parser_util).

-export([sanitize/1, escape/1, clean_whitespace/1]).

-define(ESCAPED_CHARS, [{$<, "&lt;"}, {$>, "&gt;"}, {$&, "&amp;"},
                        {$", "&quot;"}, {$', "&apos;"}]).

clean_whitespace(XML) when is_list(XML) ->
  %% Strip whitespace inserted between elements
  %% Expat driver wants to turn these into extra CDATA
  %% elements which is bad, bad, bad
  re:replace(XML, ">(\\n|\\t| )+<", "><", [{return, list}, global]).

sanitize(XML) ->
  X = lists:flatten(XML),
  X1 = case string:str(X, "?>") of
         0 ->
           X;
         V ->
           string:substr(X, V + 2)
       end,
  clean_whitespace(X1).

escape(S) when is_list(S) ->
  [proplists:get_value(C, ?ESCAPED_CHARS, C) || C <- S];
escape(S) when is_binary(S) ->
    escape(binary_to_list(S)).
