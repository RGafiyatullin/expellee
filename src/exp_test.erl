-module (exp_test).
-include_lib("eunit/include/eunit.hrl").

-include("xml.hrl").

t1_test() ->
	NS = <<"https://github.com/rgafiyatullin/expellee.git#ns1">>,
	NCN = <<"e1">>,

	Node = exp_node:new( {NS, NCN} ),
	?assert( {NS, NCN} == exp_node:fqn( Node ) ),
	?assert( NS == exp_node:ns( Node ) ),
	?assert( NCN == exp_node:ncn( Node ) ),
	?assert( exp_node_attrs:attrs( Node ) == [] ),
	?assert( exp_node_attrs:attr( <<"an-attribute">>, Node ) == undefined ),
	Node.


t2_test() ->
	NS = <<"https://github.com/rgafiyatullin/expellee.git#ns2">>,
	NCN = <<"e1">>,
	{K1, V1} = {<<"first-attribute">>, <<"first-attr-value">>},
	{K2, V2} = {<<"second-attribute">>, <<"second-attr-value">>},
	{K3, V3} = {<<"third-attribute">>, <<"third-attr-value">>},

	Node = exp_node:new( {NS, NCN}, [ {K1, V1}, {K2, V2}, {K3, V3} ] ),
	?assert( {NS, NCN} == exp_node:fqn( Node ) ),
	?assert( NS == exp_node:ns( Node ) ),
	?assert( NCN == exp_node:ncn( Node ) ),
	?assert( exp_node_attrs:attr( <<"an-attribute">>, Node ) == undefined ),
	?assert( exp_node_attrs:attr( K1, Node ) == V1 ),
	?assert( exp_node_attrs:attr( K2, Node ) == V2 ),
	?assert( exp_node_attrs:attr( K3, Node ) == V3 ),
	Node.

t3_test() ->
	NS = <<"https://github.com/rgafiyatullin/expellee.git#ns3">>,
	NCN = <<"root">>,
	NCN_1 = <<"ch1">>,
	NCN_2 = <<"ch2">>,
	NCN_3 = <<"ch3">>,
	{K1, V1} = {<<"first-attribute">>, <<"first-attr-value">>},
	{K2, V2} = {<<"second-attribute">>, <<"second-attr-value">>},
	{K3, V3} = {<<"third-attribute">>, <<"third-attr-value">>},

	Ch1 = exp_node:new( {NS, NCN_1} ),
	Ch2 = exp_node:new( {NS, NCN_2} ),
	Ch3 = exp_node:new( {NS, NCN_3} ),

	Node = exp_node:new( {NS, NCN}, [ {K1, V1}, {K2, V2}, {K3, V3} ], [ Ch1, Ch2, Ch3 ] ),
	?assert( {NS, NCN} == exp_node:fqn( Node ) ),
	?assert( NS == exp_node:ns( Node ) ),
	?assert( NCN == exp_node:ncn( Node ) ),
	?assert( exp_node_attrs:attr( <<"an-attribute">>, Node ) == undefined ),
	?assert( exp_node_attrs:attr( K1, Node ) == V1 ),
	?assert( exp_node_attrs:attr( K2, Node ) == V2 ),
	?assert( exp_node_attrs:attr( K3, Node ) == V3 ),
	?assert( exp_node_children:get( Node ) == [ Ch1, Ch2, Ch3 ] ),
	Node.

t4_render_test() ->
	X1 = t1_test(),
	X2 = t2_test(),
	X3 = t3_test(),
	EscapeMeText = <<"\"It's seven o'clock, sir\" - he said <nice & tidy>">>,
	NodeWithCD = exp_node:new({<<"test">>, <<"cdata-inside">>}, [], [ exp_text_node:cdata_new( EscapeMeText ) ]),
	NodeWithText = exp_node:new({<<"test">>, <<"chars-inside">>}, [], [ exp_text_node:chars_new( EscapeMeText ) ]),
	X4 = exp_node:new( {<<"test">>, <<"text">>}, [ {<<"test-attr">>, EscapeMeText} ], [ NodeWithCD, NodeWithText ] ),

	ok = io:format("~s~n", [ exp_render:render( X1 ) ]),
	ok = io:format("~s~n", [ exp_render:render( X2 ) ]),
	ok = io:format("~s~n", [ exp_render:render( X3 ) ]),
	ok = io:format("~s~n", [ exp_render:render( X4 ) ]).

t5_render_iq_test() ->
	% <iq type='set' xmlns='jabber:iq:roster' id='rosterpush1234567890'>
	% 	<query xmlns='jabber:iq:roster'>
	% 		<item jid='alice@nowhere.particular.in' subscription='to'>
	% 			<note xmlns='https://github.com/rgafiyatullin/expellee.git#roster-item-ext-v1' id='123'>That is Alice</node>
	% 			<note xmlns='https://github.com/rgafiyatullin/expellee.git#roster-item-ext-v1' id='234'>I don&apos;t know her personally. Bob does</node>
	% 			<notes xmlns='https://github.com/rgafiyatullin/expellee.git#roster-item-ext-v2'>
	% 				<note id='123'>This is Alice</note>
	% 				<note id='234'>I don&apos;t know her personally. Bob does</note>
	% 			</notes>
	% 		</item>
	% 		<item jid='bob@nowhere.particular.in' subscription='both'>
	% 			<note xmlns='https://github.com/rgafiyatullin/expellee.git#roster-item-ext-v1' id='345'>Bob. Well, Bob</note>
	% 			<note xmlns='https://github.com/rgafiyatullin/expellee.git#roster-item-ext-v1' id='456'>We know each other for quite a long period of time</note>
	% 			<notes xmlns='https://github.com/rgafiyatullin/expellee.git#roster-item-ext-v2'>
	% 				<note id='345'>Bob. Well, Bob</note>
	% 				<note id='456'>We know each other for quite a long period of time</note>
	% 			</notes>
	% 		</item>
	% 		<item jid='eve@nowhere.particular.in' subscription='from' />
	% 	</query>
	% </iq>
	NS_JabberClient = <<"jabber:client">>,
	NS_JabberIQRoster = <<"jabber:iq:roster">>,
	NS_ExtV1 = <<"https://github.com/rgafiyatullin/expellee.git#roster-item-ext-v1">>,
	NS_ExtV2 = <<"https://github.com/rgafiyatullin/expellee.git#roster-item-ext-v2">>,

	Xml_IQ_Query_Item_Alice_Note_v1_123 = exp_node:new( {NS_ExtV1, <<"note">>}, [ {<<"id">>, <<"123">>} ], [ exp_text_node:chars_new(<<"That is Alice">>) ] ),
	Xml_IQ_Query_Item_Alice_Note_v1_234 = exp_node:new( {NS_ExtV1, <<"note">>}, [ {<<"id">>, <<"234">>} ], [ exp_text_node:chars_new(<<"I don't know her personally. Bob does">>) ] ),
	Xml_IQ_Query_Item_Alice_Notes_v2 = exp_node:new( {NS_ExtV2, <<"notes">>}, [], [
			exp_node:new( {NS_ExtV2, <<"note">>}, [ {<<"id">>, <<"123">>} ], [ exp_text_node:chars_new(<<"That is Alice">>) ] ),
			exp_node:new( {NS_ExtV2, <<"note">>}, [ {<<"id">>, <<"234">>} ], [ exp_text_node:chars_new(<<"I don't know her personally. Bob does">>) ] )
		] ),
	Xml_IQ_Query_Item_Alice = exp_node:new(
			{NS_JabberIQRoster, <<"item">>},
			[ {<<"jid">>, <<"alice@nowhere.particular.in">>}, {<<"subscription">>, <<"to">>} ],
			[
				Xml_IQ_Query_Item_Alice_Note_v1_123,
				Xml_IQ_Query_Item_Alice_Note_v1_234,
				Xml_IQ_Query_Item_Alice_Notes_v2
			] ),

	Xml_IQ_Query_Item_Bob_Note_v1_345 = exp_node:new( {NS_ExtV1, <<"note">>}, [ {<<"id">>, <<"345">>} ], [ exp_text_node:chars_new(<<"Bob. Well, Bob">>) ] ),
	Xml_IQ_Query_Item_Bob_Note_v1_456 = exp_node:new( {NS_ExtV1, <<"note">>}, [ {<<"id">>, <<"456">>} ], [ exp_text_node:chars_new(<<"We know each other for quite a long period of time">>) ] ),
	Xml_IQ_Query_Item_Bob_Notes_v2 = exp_node:new( {NS_ExtV2, <<"notes">>}, [], [
			exp_node:new( {NS_ExtV2, <<"note">>}, [ {<<"id">>, <<"345">>} ], [ exp_text_node:chars_new(<<"Bob. Well, Bob">>) ] ),
			exp_node:new( {NS_ExtV2, <<"note">>}, [ {<<"id">>, <<"456">>} ], [ exp_text_node:chars_new(<<"We know each other for quite a long period of time">>) ] )
		] ),
	Xml_IQ_Query_Item_Bob = exp_node:new( {NS_JabberIQRoster, <<"item">>}, [ {<<"jid">>, <<"bob@nowhere.particular.in">>}, {<<"subscription">>, <<"both">>} ], [
			Xml_IQ_Query_Item_Bob_Note_v1_345,
			Xml_IQ_Query_Item_Bob_Note_v1_456,
			Xml_IQ_Query_Item_Bob_Notes_v2
		] ),

	Xml_IQ_Query_Item_Eve = exp_node:new( {NS_JabberIQRoster, <<"item">>}, [ {<<"jid">>, <<"eve@nowhere.particular.in">>}, {<<"subscription">>, <<"from">>} ], [] ),

	Xml_IQ_Query = exp_node:new( {NS_JabberIQRoster, <<"query">>}, [], [ Xml_IQ_Query_Item_Alice, Xml_IQ_Query_Item_Bob, Xml_IQ_Query_Item_Eve ] ),
	Xml_IQ = exp_node:new( {NS_JabberClient, <<"iq">>}, [{<<"id">>, <<"rosterpush1234567890">>}, {<<"type">>, <<"set">>}], [ Xml_IQ_Query ] ),

	exp_render:render( Xml_IQ ).


t6_parse_sax_one_piece_test() ->
	FileName = "priv/sample-roster-push.xml",
	{ok, Content} = file:read_file( FileName ),
	{ok, Sax0} = exp_parse_sax:new(),
	{ok, SaxEvents, Sax1} = exp_parse_sax:parse( Content, Sax0 ),
	ok = exp_parse_sax:free(Sax1),
	{ok, SaxEvents}.

t6_parse_sax_feed_slowly_test() ->
	FileName = "priv/sample-roster-push.xml",
	{ok, Content} = file:read_file( FileName ),
	{ok, Sax0} = exp_parse_sax:new(),
	{ok, SaxEvents, Sax1} = t6_parse_sax_feed_slowly_test_loop( {queue:new(), Sax0}, Content ),
	ok = exp_parse_sax:free( Sax1 ),
	{ok, SaxEvents}.

t6_parse_sax_feed_slowly_test_loop( {EventsQueue, SaxFinal}, <<>> ) ->
	{ok, queue:to_list( EventsQueue ), SaxFinal};
t6_parse_sax_feed_slowly_test_loop( {EventsQueue, Sax}, Source ) ->
	{ Piece, SourceNext } = t6_parse_sax_feed_slowly_test_take_piece( Source ),
	{ ok, Events, SaxNext } = exp_parse_sax:parse( Piece, Sax ),
	EventsQueueNext = lists:foldl( fun queue:in/2, EventsQueue, Events ),
	t6_parse_sax_feed_slowly_test_loop( {EventsQueueNext, SaxNext}, SourceNext ).

t6_parse_sax_feed_slowly_test_take_piece( << Piece:32/binary, Rest/binary >> ) -> { Piece, Rest };
t6_parse_sax_feed_slowly_test_take_piece( Rest ) -> { Rest, <<>> }.


t6_parse_sax_own_rendered_test() ->
	RenderedIOData = t5_render_iq_test(),
	Rendered = iolist_to_binary([RenderedIOData]),
	{ok, Sax0} = exp_parse_sax:new(),
	{ok, SaxEvents, Sax1} = t6_parse_sax_feed_slowly_test_loop( {queue:new(), Sax0}, Rendered ),
	ok = exp_parse_sax:free( Sax1 ),
	{ok, SaxEvents}.



t7_sax_to_dom_test() ->
	{ok, Events} = t6_parse_sax_feed_slowly_test(),
	{final, _} = lists:foldl(
					fun( Evt, {ok, S2D} ) ->
						exp_sax2dom:sax_event_in( Evt, S2D )
					end,
					exp_sax2dom:new(), Events).
