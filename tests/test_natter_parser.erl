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

-module(test_natter_parser).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
  [?_assertMatch({xmlelement, "foo", [], []}, natter_parser:parse("<foo />")),
   ?_assertMatch({xmlelement, "foo:bar", [], []}, natter_parser:parse("<foo:bar />")),
   ?_assertMatch({xmlelement, "foo", [], [{xmlcdata, <<"testing">>}]}, natter_parser:parse("<foo>testing</foo>")),
   ?_assertMatch({xmlelement, "foo", [],
                  [{xmlelement, "bar", [], [{xmlcdata, <<"testing">>}]}]}, natter_parser:parse("<foo><bar>testing</bar></foo>")),
   ?_assertMatch({xmlelement, "foo", [{"id", "5"}], []}, natter_parser:parse("<foo id=\"5\" />")),
   ?_assertMatch({xmlelement, "foo", [{"id", "5"}],
                  [{xmlelement, "bar", [{"name", "test"}], []}]}, natter_parser:parse("<foo id=\"5\"><bar name=\"test\" /></foo>")),
   ?_assertMatch({xmlelement, "foo", [],
                  [{xmlelement, "bar", [], []},
                   {xmlelement, "baz", [], []}]}, natter_parser:parse("<foo><bar/><baz /></foo>")),
   ?_assertMatch({xmlelement, "foo", [],
                  [{xmlelement, "bar", [],
                    [{xmlelement, "baz", [], []}]}]}, natter_parser:parse("<foo><bar><baz /></bar></foo>")),
   ?_assertMatch({error, _}, natter_parser:parse("<foo><bar><baz id=123 /></bar></foo>"))].

unparse_test_() ->
  [?_assertMatch("<foo>bar</foo>", natter_parser:element_to_string(natter_parser:parse("<foo>bar</foo>"))),
   ?_assertMatch("<foo id='1'>bar</foo>", natter_parser:element_to_string(natter_parser:parse("<foo id='1'>bar</foo>"))),
   ?_assertMatch("<foo id='1' name='wibble' />", natter_parser:element_to_string(natter_parser:parse("<foo id='1' name='wibble' />")))].

util_test_() ->
  [?_assertMatch(["&gt;"], natter_parser_util:escape(">")),
   ?_assertMatch(["&lt;"], natter_parser_util:escape("<")),
   ?_assertMatch(["&amp;"], natter_parser_util:escape("&")),
   ?_assertMatch(["&apos;"], natter_parser_util:escape("'")),
   ?_assertMatch("<foo />", natter_parser_util:sanitize("<?xml version='1.0'?><foo />")),
   ?_assertMatch("<foo><bar /></foo>", natter_parser_util:sanitize("<?xml version='1.0'?><foo>\n <bar /> </foo>"))].
