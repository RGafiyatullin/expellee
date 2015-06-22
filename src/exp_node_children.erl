-module (exp_node_children).
-export ([
		get/1,
		set/2,
		append/2,
		prepend/2
	]).
-include("xml.hrl").

-spec get( #xe{} ) -> [ xml_element() ].
get( #xe{ c = Chs } ) -> Chs.

-spec set( [ xml_element() ], #xe{} ) -> #xe{}.
set( Chs, Xml = #xe{} ) -> Xml #xe{ c = Chs }.

-spec append( [xml_element()], #xe{} ) -> #xe{}.
append( Appendees, Xml ) when is_list( Appendees ) ->
	Children = ?MODULE:get( Xml ),
	?MODULE:set( Children ++ Appendees, Xml ).

-spec prepend( [xml_element()], #xe{} ) -> #xe{}.
prepend( Prependees, Xml ) when is_list( Prependees ) ->
	Children = ?MODULE:get( Xml ),
	?MODULE:set( Prependees ++ Children, Xml ).
