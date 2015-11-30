-module (exp_parse_sax).
-compile ({parse_transform, ct_expand}).
-export ([new/0, free/1]).
-export ([parse/2]).
-export_type ([sax/0]).
-include("xml.hrl").
-include("sax.hrl").

-type exml() :: exml_event:c_parser().

-record(ns, {
	'#module' = ?MODULE :: ?MODULE,
	ns :: xml_ns(),
	prefix :: xml_ns_prefix() | none
}).

-type ns_map() :: [ #ns{} ].

-record(sax, {
		'#module' = ?MODULE :: ?MODULE,
		exml :: exml(),
		ns_stack = [] :: [ ns_map() ]
	}).
-opaque sax() :: #sax{}.

-spec new() -> {ok, sax()}.
new() ->
	{ok, Exml} = exml_event:new_parser(),
	{ok, #sax{ exml = Exml }}.

-spec free( sax() ) -> ok | {error, already_freed}.
free( #sax{ exml = Exml } ) ->
	case catch exml_event:reset_parser( Exml ) of
		ok -> ok = exml_event:free_parser( Exml );
		_ -> {error, already_freed}
	end.

-spec parse( binary(), sax() ) -> { ok, [ sax_event() ], sax() } | {error, term(), sax()}.
parse( Bin, S0 = #sax{ exml = Exml } ) when is_binary( Bin ) ->
	case exml_event:parse( Exml, Bin ) of
		{error, XmlError} ->
			{error, XmlError, S0};
		{ok, ExmlEvents} when is_list(ExmlEvents) ->
			{SaxEventsRev, S1} = lists:foldl(
				fun parse_exml_event/2,
				{[], S0}, ExmlEvents ),
			{ok, sax_events_reverse_and_finalize(SaxEventsRev), S1}
	end.

-spec parse_exml_event(
		  {xml_element_start, binary(), [ {none | binary(), binary()} ], [ {binary(), binary()} ]}
		| {xml_element_end, binary()}
		| {xml_cdata, binary()},

		{[ #xml_element_start{} | #xml_element_end{} | #xml_cdata_tmp{} ], sax()}
	) -> {[ #xml_element_start{} | #xml_element_end{} | #xml_cdata_tmp{} ], sax()}.
parse_exml_event( {xml_element_start, NS_NCN_Bin, NS_Imports, Attrs}, { SaxEventsAcc, S0 = #sax{ ns_stack = NSStack0 } } )
	when is_binary( NS_NCN_Bin )
	andalso is_list( NS_Imports )
	andalso is_list( Attrs )
->
	NSMap = lists:map(fun( {ImpNS, ImpPrefix} ) ->
			#ns{ ns = ImpNS, prefix = ImpPrefix }
		end, NS_Imports ),
	NSStack1 = [ NSMap | NSStack0 ],
	{Prefix, NCName} =  xml_element_name_split( NS_NCN_Bin ),
	NS = resolve_prefix( Prefix, NSStack1 ),
	SaxEvent = #xml_element_start{ ns = NS, ncn = NCName, attrs = Attrs },
	{ [ SaxEvent | SaxEventsAcc ], S0 #sax{ ns_stack = NSStack1 } };

parse_exml_event( {xml_element_end, NS_NCN_Bin}, {SaxEventsAcc, S0 = #sax{ ns_stack = NSStack0 }} ) when is_binary(NS_NCN_Bin) ->
	[ _ | NSStack1 ] = NSStack0,
	{Prefix, NCName} = xml_element_name_split( NS_NCN_Bin ),
	NS = resolve_prefix( Prefix, NSStack0 ),
	SaxEvent = #xml_element_end{ ns = NS, ncn = NCName },
	{ [ SaxEvent | SaxEventsAcc ], S0 #sax{ ns_stack = NSStack1 } };

parse_exml_event( {xml_cdata, Value}, { [ #xml_cdata_tmp{ value = PrevValue } | SaxEventsAcc ], S0 } ) when is_binary( Value ) ->
	SaxEvent = #xml_cdata_tmp{ value = [PrevValue, Value] },
	{ [ SaxEvent | SaxEventsAcc ], S0 };
parse_exml_event( {xml_cdata, Value}, { SaxEventsAcc, S0 } ) when is_binary( Value ) ->
	SaxEvent = #xml_cdata_tmp{ value = [Value] },
	{ [ SaxEvent | SaxEventsAcc ], S0 }.

-spec xml_element_name_split( binary() ) -> { none | binary(), binary() }.
xml_element_name_split( NS_NCN_Bin ) when is_binary(NS_NCN_Bin) ->
	case binary:split( NS_NCN_Bin, <<":">> ) of
		[ P, N ] -> {P, N};
		[JustNCN] -> {none, JustNCN}
	end.

-spec resolve_prefix( xml_ns_prefix(), [ ns_map() ] ) -> xml_ns().
resolve_prefix( _, [] ) -> <<>>;
resolve_prefix( Prefix, [ Map | MapsLeft ] ) ->
	case lists:keyfind( Prefix, #ns.prefix, Map ) of
		false -> resolve_prefix( Prefix, MapsLeft );
		#ns{ ns = NS } -> NS
	end.


-spec sax_events_reverse_and_finalize( [ #xml_element_start{} | #xml_element_end{} | #xml_cdata_tmp{} ] ) -> [sax_event()].
sax_events_reverse_and_finalize( SaxEventsRev ) ->
	lists:foldl(
		fun
			( #xml_cdata_tmp{ value = IOL }, SaxEventsFw ) ->
				Bin = iolist_to_binary( IOL ),
				case strip_gaps( Bin ) of
					<<>> -> SaxEventsFw;
					_ -> [#xml_cdata{ value = Bin } | SaxEventsFw ]
				end;
			( Evt, SaxEventsFw ) -> [ Evt | SaxEventsFw ]
		end,
		[], SaxEventsRev).


strip_gaps( Bin ) ->
	{ok, Re} = ct_expand:term( re:compile("^\\s*") ),
	iolist_to_binary([ re:replace( Bin, Re, <<>> ) ]).


