
-module(exp_node).
-export([
		new/1, new/2, new/3,

		ns/1, ncn/1,
		fqn/1
	]).

-include("xml.hrl").

new( FQN = {_, _} ) -> new( FQN, [] ).
new( FQN, Attrs ) -> new( FQN, Attrs, [] ).

new( {NS, NCN}, Attrs, Chs ) ->
	ok = exp_check:check_xml_ncname( NCN ),
	ok = exp_check:check_xml_ns( NS ),
	ok = exp_check:check_attr_collection( Attrs ),
	ok = lists:foreach( fun exp_check:check_xml/1, Chs ),
	#xe{ ns = NS, ncn = NCN, a = Attrs, c = Chs }.

ns( #xe{ ns = NS } ) -> NS.
ncn( #xe{ ncn = NCN } ) -> NCN.
fqn( #xe{ ns = NS, ncn = NCN } ) -> {NS, NCN}.


