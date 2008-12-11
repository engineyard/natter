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
