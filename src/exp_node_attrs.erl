-module (exp_node_attrs).
-export ([
		attrs/1,
		attr/2,
		set_attrs/2,
		set_attr/3
	]).

-include("xml.hrl").

attrs( #xe{ a = Attrs } ) -> Attrs.

attr( Name, #xe{ a = Attrs } ) ->
	ok = exp_check:check_xml_ncname( Name ),
	proplists:get_value( Name, Attrs, undefined ).

-spec set_attrs( [ xml_attribute() ], #xe{} ) -> #xe{}.
set_attrs( Attrs, Xe0 = #xe{} ) ->
	lists:foldl(
		fun( {K, V}, Xe ) ->
			set_attr( K, V, Xe )
		end, Xe0, Attrs ).


-spec set_attr( xml_attribute_name(), xml_attribute_value(), #xe{} ) -> #xe{}.
set_attr( K, V, Xe = #xe{ a = Attrs0 } ) ->
	Attrs1 = lists:keystore( K, 1, Attrs0, {K, V} ),
	Xe #xe{ a = Attrs1 }.
