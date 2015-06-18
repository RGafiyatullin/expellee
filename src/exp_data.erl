-module (exp_data).

-export ([
		data_store/3,
		data_find/2,
		data_erase/2
	]).

-include("xml.hrl").

data_store( K, V, X = #chars{} ) -> X #chars{ d = orddict:store( K, V, X #chars.d ) };
data_store( K, V, X = #cd{} ) -> X #cd{ d = orddict:store( K, V, X #cd.d ) };
data_store( K, V, X = #xe{} ) -> X #xe{ d = orddict:store( K, V, X #xe.d ) }.

data_find( K, #chars{ d = D } ) -> orddict:find( K, D );
data_find( K, #cd{ d = D } ) -> orddict:find( K, D );
data_find( K, #xe{ d = D } ) -> orddict:find( K, D ).

data_erase( K, Xml = #chars{ d = D } ) -> Xml #chars{ d = orddict:erase( K, D ) };
data_erase( K, Xml = #cd{ d = D } ) -> Xml #cd{ d = orddict:erase( K, D ) };
data_erase( K, Xml = #xe{ d = D } ) -> Xml #xe{ d = orddict:erase( K, D ) }.
