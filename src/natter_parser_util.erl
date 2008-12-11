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
