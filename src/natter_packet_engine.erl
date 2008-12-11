-module(natter_packet_engine).

-export([analyze/1]).

analyze(Text) ->
  analyze(Text, []).

analyze(Text, Accum) ->
  {TagName, S1, T1} = read_tag_name(Text),
  case read_tag_end(TagName, S1, T1) of
    not_found ->
      {lists:reverse(Accum), Text};
    {Stanza, S} ->
      analyze(S, [Stanza|Accum])
  end.

read_tag_end(TagName, S, T) ->
  case terminating_tag_pos(TagName, T) of
    0 ->
      find_end(T, S);
    V ->
      S1 = lists:flatten([lists:reverse(string:sub_string(T, 1, V))|S]),
      T1 = string:sub_string(T, V, length(T)),
      {lists:reverse(S1), T1}
  end.

find_end(Text, S) ->
  find_end(Text, false, S).

find_end([H|T], false, Accum) ->
  case H of
    $< ->
      not_found;
    $/ ->
      find_end(T, true, [H|Accum]);
    _ ->
      find_end(T, false, [H|Accum])
  end;
find_end([H|T], true, Accum) ->
  case H of
    $\s ->
      find_end(T, true, [H|Accum]);
    $> ->
      {lists:reverse([H|Accum]), T};
    _ ->
      find_end(T, false, [H|Accum])
  end;
find_end([], _, _) ->
  not_found.

terminating_tag_pos(TagName, T) ->
  TermTag = "</" ++ TagName ++ ">",
  case string:str(T, TermTag) of
    0 ->
      0;
    P ->
      P + length(TermTag)
  end.

read_tag_name(Text) ->
  read_tag_name(Text, [], []).

read_tag_name([H|T], Accum, Stanza) ->
  case H of
    $< ->
      read_tag_name(T, Accum, [H|Stanza]);
    $\s ->
      {lists:reverse(Accum), [H|Stanza], T};
    $> ->
      {lists:reverse(Accum), [H|Stanza], T};
    _ ->
      read_tag_name(T, [H|Accum], [H|Stanza])
  end;
read_tag_name([], Accum, Stanza) ->
  {lists:reverse(Accum), Stanza, []}.
