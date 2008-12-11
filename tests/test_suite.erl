-module(test_suite).

-author("ksmith@engineyard.com").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [{module, test_natter_parser},
   {module, test_natter_packetizer},
   {module, test_natter_dispatcher}].
