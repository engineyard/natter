-module(natter_stanza_inspector).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{inspect_stanza, 1}].
