-module (exp_node_attrs).
-export ([
		attrs/1,
		attr/2,
		set_attrs/2,
		set_attr/3,
		rm_attr/2
	]).

-include("xml.hrl").

attrs( #xe{ a = Attrs } ) -> Attrs.

attr( Name, #xe{ a = Attrs } ) ->
	ok = exp_check:check_xml_ncname( Name ),
	proplists:get_value( Name, Attrs, undefined ).

-spec set_attrs( [ xml_attribute() | {xml_attribute_name(), undefined} ], #xe{} ) -> #xe{}.
set_attrs( Attrs, Xe0 = #xe{} ) ->
	lists:foldl(
		fun
			( {K, undefined}, Xe ) ->
				rm_attr( K, Xe );
			( {K, V}, Xe ) ->
				set_attr( K, V, Xe )
		end, Xe0, Attrs ).


-spec set_attr( xml_attribute_name(), xml_attribute_value() | undefined, #xe{} ) -> #xe{}.
set_attr( K, undefined, Xe ) ->
	rm_attr( K, Xe );
set_attr( K, V, Xe = #xe{ a = Attrs0 } ) ->
	Attrs1 = lists:keystore( K, 1, Attrs0, {K, V} ),
	Xe #xe{ a = Attrs1 }.


-spec rm_attr( xml_attribute_name(), #xe{} ) -> #xe{}.
rm_attr( K, Xe = #xe{ a = Attrs0 } ) ->
	Attrs1 = lists:keydelete( K, 1, Attrs0 ),
	Xe #xe{ a = Attrs1 }.
