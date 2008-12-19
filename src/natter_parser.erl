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

-module(natter_parser).

-author("ksmith@engineyard.com").

-export([parse/1, element_to_string/1]).

element_to_string(El) ->
  natter_parser_util:clean_whitespace(to_string(El)).

parse(XML) when is_list(XML) ->
  CXML = natter_parser_util:sanitize(XML),
  natter_util:load_xml_driver(),
  DriverPort = open_port({spawn, natter_expat}, [binary]),
  try parse(DriverPort, [], CXML)
  catch Error ->
      {error, Error}
  after
    close(DriverPort)
  end.

parse(DriverPort, Stack, Str) ->
  case binary_to_term(port_control(DriverPort, 0, Str)) of
    [{error, Error}] ->
      {error, Error};
    Result ->
      [Root] = lists:foldl(
                 fun(Data, St) ->
                     NewSt = handle_event(Data, St),
                     if
                       length(NewSt) == 0 ->
                         St;
                       true ->
                         NewSt
                     end  end, Stack, Result),
      Root
  end.

%% Internal functions
close(DriverPort) ->
  port_close(DriverPort).

to_string(El) ->
  lists:flatten(lists:reverse(to_string(El, []))).

to_string({xmlelement, Name, Attrs, []}, Stack) ->
  [build_element(Name, Attrs, true)|Stack];

to_string({xmlelement, Name, Attrs, SubEls}, Stack) ->
  S1 = [build_element(Name, Attrs, false)|Stack],
  S2 = lists:foldl(fun(E, Acc) ->
                       to_string(E, Acc) end, S1, SubEls),
  [io_lib:format("</~s>", [Name])|S2];
to_string({xmlcdata, CData}, Stack) when is_binary(CData) ->
  [binary_to_list(CData)|Stack];

to_string({xmlcdata, CData}, Stack) ->
  [CData|Stack].

build_element(Name, [], Terminate) ->
  if
    Terminate ->
      io_lib:format("<~s />", [Name]);
    true ->
      io_lib:format("<~s>", [Name])
  end;
build_element(Name, Attrs, Terminate) ->
  if
    Terminate ->
      io_lib:format("<~s~s />", [Name, attrs_to_list(Attrs)]);
    true ->
      io_lib:format("<~s~s>", [Name, attrs_to_list(Attrs)])
  end.


add_to_parent({xmlelement, ChildName, ChildAttrs, ChildSubEls},
              {xmlelement, ParentName, ParentAttrs, ParentSubEls}) ->
  {xmlelement, ParentName, ParentAttrs,
   [{xmlelement, ChildName, ChildAttrs, lists:reverse(ChildSubEls)}|ParentSubEls]}.

handle_event({element_start, {Name, Attrs}}, Stack) ->
  [{xmlelement, Name, Attrs, []} | Stack];

handle_event({element_end, _EndName}, Stack) ->
  case length(Stack) of
    0 ->
      Stack;
    1 ->
      [{xmlelement, Name, Attrs, SubEls}] = Stack,
      [{xmlelement, Name, Attrs, lists:reverse(SubEls)}];
    _ ->
      [Current|T] = Stack,
      [Parent|T1] = T,
      [add_to_parent(Current, Parent)|T1]
  end;

handle_event({cdata, CData}, Stack) ->
  case Stack of
    %% Append contiguous CDATA blocks together
    [{xmlelement, Name, Attrs, [{xmlcdata, PrevCData}|SubEls]}|T] ->
      [{xmlelement, Name, Attrs, [{xmlcdata, erlang:concat_binary([PrevCData, CData])}|SubEls]}|T];
    %% Create a new CDATA block
    [{xmlelement, Name, Attrs, SubEls}|T] ->
      [{xmlelement, Name, Attrs, [{xmlcdata, CData}|SubEls]}|T];
    [] ->
      []
  end;

handle_event({error, Error}, _Stack) ->
  [{error, Error}].

attrs_to_list(Attrs) ->
    [attr_to_list(A) || A <- Attrs].

attr_to_list({Name, Value}) ->
    [$\s, natter_parser_util:escape(Name), $=, $', natter_parser_util:escape(Value), $'].
