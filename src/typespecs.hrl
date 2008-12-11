%% Types used by function specs
-type(xml_attr() :: {string(), string()}).

-type(xml_attrs() :: [xml_attr()] | []).

%% Sub element list has to be a generic tuple since dialyzer can't
%% process recursive type specs :(
-type(parsed_xml() :: {'xmlelement', string(), xml_attrs(), [tuple()]}).

-type(natter_error() :: {atom(), string()}).

-type(config() :: list(tuple(atom(), string()))).
