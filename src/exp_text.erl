-module (exp_text).
-export ([
		text_iolist/1,
		text_flat/1
	]).

-include ("xml.hrl").

text_iolist( #cd{ data = D } ) -> D;
text_iolist( #chars{ data = D } ) -> D;
text_iolist( #xe{ c = C } ) ->
	lists:map( fun text_iolist/1, C ).

text_flat( Xml ) -> iolist_to_binary([ text_iolist( Xml ) ]).
