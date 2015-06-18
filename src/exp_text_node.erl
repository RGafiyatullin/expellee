-module (exp_text_node).
-export ([
		chars_new/1,
		chars_value/1,
		cdata_new/1,
		cdata_value/1
	]).
-include("xml.hrl").

chars_new( Value ) ->
	ok = exp_check:check_is_binary( chars_value, Value ),
	#chars{ data = Value }.

chars_value( #chars{ data = Value } ) ->
	Value.

cdata_new( Value ) ->
	ok = exp_check:check_is_binary( cdata_value, Value ),
	#cd{ data = Value }.

cdata_value( #cd{ data = Value } ) ->
	Value.




