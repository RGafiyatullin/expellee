-module (exp_lib).

-export ([
		utfl_to_bin/1,
		bin_to_utfl/1
	]).
-export ([
		xml_entities_encode/1
	]).

-spec utfl_to_bin( L :: [integer()] ) -> binary().
-spec bin_to_utfl( B :: binary() ) -> [integer()].




utfl_to_bin(L) ->
	unicode:characters_to_binary(L, utf8, utf8).

bin_to_utfl(B) ->
	unicode:characters_to_list(B, utf8).

-spec xml_entities_encode( binary() ) -> binary().
xml_entities_encode( Bin ) ->
	<< <<
		case Ch of
			$< -> <<"&lt;">>;
			$> -> <<"&gt;">>;
			$' -> <<"&apos;">>;
			$" -> <<"&quot;">>;
			$& -> <<"&amp;">>;
			_ -> <<Ch/utf8>>
		end /binary>> || <<Ch/utf8>> <= Bin >>.


