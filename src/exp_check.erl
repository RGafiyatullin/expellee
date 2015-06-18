-module (exp_check).

-export ([
		check_xml/1
	]).
-export ([
		check_attr_collection/1,
		check_attr_name/1,
		check_attr_value/1,
		check_xml_ns/1,
		check_xml_ncname/1
	]).
-export ([
		check_is_ncname/2,
		check_is_binary/2,
		check_is_list/2
	]).

-include("xml.hrl").

check_xml( #cd{ data = B } ) ->
	ok = check_is_binary( cd_data, B );
check_xml( #chars{ data = B } ) ->
	ok = check_is_binary( chars_data, B );
check_xml( #xe{ ns = NS, ncn = NCN, a = Attrs, c = Chs } ) ->
	ok = lists:foreach( fun check_xml/1, Chs ),
	ok = check_xml_ns( NS ),
	ok = check_is_ncname( xml_ncn, NCN ),
	ok = check_attr_collection( Attrs ).

check_xml_ns( ?xml_ns_inherit ) -> ok;
check_xml_ns( NS ) -> check_is_binary( xml_ns, NS ).

check_xml_ncname( NCN ) -> check_is_ncname( xml_ncn, NCN ).

check_attr_name( Name ) -> check_is_ncname( attr_name, Name ).
check_attr_value( Value ) -> check_is_binary( attr_value, Value ).

check_attr_collection( Attrs ) ->
	ok = check_is_list( attr_collection, Attrs ),
	lists:foreach( fun check_attr_pair/1, Attrs ).

check_attr_pair( { Name, Value } ) ->
	ok = check_attr_name( Name ),
	ok = check_attr_value( Value );
check_attr_pair( NotATupleOfTwo ) ->
	error({badarg, attr_pair, NotATupleOfTwo}).

check_is_binary( _, B ) when is_binary( B ) -> ok;
check_is_binary( What, NotABinary ) -> error({badarg, What, NotABinary}).

check_is_ncname( What, V ) -> check_is_binary( What, V ).

check_is_list( _, L ) when is_list( L ) -> ok;
check_is_list( What, NotAList ) -> error({badarg, What, NotAList}).


