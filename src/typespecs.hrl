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

%% Types used by function specs
-type(xml_attr() :: {string(), string()}).

-type(xml_attrs() :: [xml_attr()] | []).

%% Sub element list has to be a generic tuple since dialyzer can't
%% process recursive type specs :(
-type(parsed_xml() :: {'xmlelement', string(), xml_attrs(), [tuple()]}).

-type(natter_error() :: {atom(), string()}).

-type(config() :: list(tuple(atom(), string()))).
