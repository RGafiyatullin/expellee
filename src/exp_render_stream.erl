-module (exp_render_stream).
-export ([
		ctx_new/1
	]).
-export ([
		node_self_closing_tag/4,
		node_open_tag/4,
		node_close_tag/3
	]).
-export ([
		cdata_node/2,
		text_node/2
	]).
-export ([
		attribute_value_quote/1
	]).
-export_type ([
		ctx/0
	]).

-include("xml.hrl").

-record( ctx, {
		'#module' = ?MODULE :: ?MODULE,
		imports = [] :: [ xml_ns_import() ],
		use_single_quotes = true :: boolean(),
		parent_ctx = undefined :: undefined | #ctx{},
		original_ctx = undefined :: undefined | #ctx{},
		current_ns = <<>> :: xml_ns()
	} ).
-type ctx() :: #ctx{}.

-type rendered() :: iodata().
-type text() :: iolist() | binary().
-type maybe_ctx() :: undefined | ctx().

-spec ctx_new( [ tuple() ] ) -> ctx().
-spec node_self_closing_tag( xml_ns(), xml_ncname(), [ xml_attribute() ], ctx() ) -> {rendered(), maybe_ctx()}.
-spec node_open_tag( xml_ns(), xml_ncname(), [ xml_attribute() ], ctx() ) -> {rendered(), ctx()}.
-spec node_close_tag( xml_ns(), xml_ncname(), ctx() ) -> {rendered(), maybe_ctx()}.
-spec attribute_value_quote( ctx() ) -> char().
-spec cdata_node( text(), ctx() ) -> {iolist(), ctx()}.
-spec text_node( text(), ctx() ) -> {binary(), ctx()}.

ctx_new( Args ) ->
	lists:foldl(
		fun ctx_new_handle_arg/2,
		#ctx{}, Args ).

node_self_closing_tag( NS, NCN, Attrs, Ctx0 = #ctx{} ) ->
	NSResolved = resolve_maybe_inherited_ns( NS, Ctx0 ),
	{Prefix, Ctx1} = get_prefix_for_ns( NSResolved, Ctx0 ),
	MaybePrefixAndColon =
		case Prefix of
			<<>> -> <<>>;
			NonEmptyPrefix -> [ NonEmptyPrefix, $: ]
		end,

	{NSImportsAndCornerBracket, _SameCtx} = lists:foldl(
		fun node_open_tag_render_xmlns_import_folder/2,
		{[ $/, $> ], Ctx1}, Ctx1 #ctx.imports ),

	{AttrsAndEndCornerBracket, _SameCtx} = lists:foldl(
		fun node_open_tag_render_attrs_folder/2,
		{NSImportsAndCornerBracket, Ctx1}, Attrs ),

	ResultingIOL = [ $<, MaybePrefixAndColon, NCN | AttrsAndEndCornerBracket ],

	{ResultingIOL, Ctx0}.

node_open_tag( NS, NCN, Attrs, Ctx0 ) ->
	NSResolved = resolve_maybe_inherited_ns( NS, Ctx0 ),
	{Prefix, Ctx1} = get_prefix_for_ns( NSResolved, Ctx0 ),
	MaybePrefixAndColon =
		case Prefix of
			<<>> -> <<>>;
			NonEmptyPrefix -> [ NonEmptyPrefix, $: ]
		end,

	{NSImportsAndCornerBracket, _SameCtx} = lists:foldl(
		fun node_open_tag_render_xmlns_import_folder/2,
		{[ $> ], Ctx1}, Ctx1 #ctx.imports ),

	{AttrsAndEndCornerBracket, _SameCtx} = lists:foldl(
		fun node_open_tag_render_attrs_folder/2,
		{NSImportsAndCornerBracket, Ctx1}, Attrs ),

	ResultingIOL = [ $<, MaybePrefixAndColon, NCN | AttrsAndEndCornerBracket ],

	Ctx2 = Ctx1 #ctx{ original_ctx = Ctx0 },
	ChildCtx = Ctx2 #ctx{ current_ns = NS, imports = [], parent_ctx = Ctx2 },

	{ResultingIOL, ChildCtx}.

node_close_tag( NS, NCN, #ctx{ parent_ctx = Ctx0 = #ctx{ original_ctx = ParentOriginalCtx } } ) ->
	NSResolved = resolve_maybe_inherited_ns( NS, Ctx0 ),
	{Prefix, _Ctx1} = get_prefix_for_ns( NSResolved, Ctx0 ),
	MaybePrefixAndColon =
		case Prefix of
			<<>> -> <<>>;
			NonEmptyPrefix -> [ NonEmptyPrefix, $: ]
		end,
	ResultingIOL = [ $<, $/, MaybePrefixAndColon, NCN, $> ],

	{ResultingIOL, ParentOriginalCtx}.

attribute_value_quote( #ctx{ use_single_quotes = false } ) -> $";
attribute_value_quote( #ctx{ use_single_quotes = true } ) -> $'.

cdata_node( CData, Ctx ) when is_list( CData ) orelse is_binary( CData ) ->
	ResultingIOL = [
		<<"<![CDATA[">>,
		CData,
		<<"]]>">> ],
	{ResultingIOL, Ctx}.

text_node( Text, Ctx ) when is_list( Text ) orelse is_binary( Text ) ->
	ResultingBinary = exp_lib:xml_entities_encode(iolist_to_binary([Text])),
	{ResultingBinary, Ctx}.



%%% Internal %%%

node_open_tag_render_xmlns_import_folder( {NS, Prefix}, {Acc0, Ctx} ) ->
	Quot = attribute_value_quote( Ctx ),
	XmlnsAndMaybeColon =
		case Prefix of
			<<>> -> <<" xmlns">>;
			_NonEmptyPrefix -> <<" xmlns:">>
		end,
	Acc1 = [ XmlnsAndMaybeColon, Prefix, $=, Quot, NS, Quot | Acc0 ],
	{Acc1, Ctx}.

node_open_tag_render_attrs_folder( { Name, Value }, {Acc0, Ctx} ) ->
	Quot = attribute_value_quote( Ctx ),
	ValueEscaped = exp_lib:xml_entities_encode( Value ),
	Acc1 = [ $\ , Name, $=, Quot, ValueEscaped, Quot | Acc0 ],
	{Acc1, Ctx}.


get_prefix_for_ns( NS, Ctx0 ) ->
	case get_prefix_for_ns_loop( NS, Ctx0 ) of
		undefined ->
			FilteredImports =
				lists:filter(
					fun( {_, Prefix} ) ->
						Prefix /= <<>>
					end,
					Ctx0 #ctx.imports ),
			{<<>>, Ctx0 #ctx{ imports = [ {NS, <<>>} | FilteredImports ] }};

		Prefix when is_binary( Prefix ) ->
			{Prefix, Ctx0}
	end.

get_prefix_for_ns_loop( _, undefined ) -> undefined;
get_prefix_for_ns_loop( NS, #ctx{ imports = Imports, parent_ctx = ParentCtx } ) ->
	case lists:keyfind( NS, 1, Imports ) of
		{NS, Prefix} ->
			Prefix;

		false ->
			get_prefix_for_ns_loop( NS, ParentCtx )
	end.


resolve_maybe_inherited_ns( ?xml_ns_inherit, #ctx{ current_ns = ?xml_ns_inherit, parent_ctx = Ctx } ) -> resolve_maybe_inherited_ns( ?xml_ns_inherit, Ctx );
resolve_maybe_inherited_ns( ?xml_ns_inherit, #ctx{ current_ns = NS } ) when is_binary( NS ) -> NS;
resolve_maybe_inherited_ns( NS, _ ) when is_binary( NS ) -> NS.


ctx_new_handle_arg( Arg, Ctx ) ->
	case Arg of
		{ns_import, NS, Prefix} ->
			ok = exp_check:check_is_binary( xml_ns, NS ),
			ok = exp_check:check_is_binary( xml_ns_prefix, Prefix ),
			Ctx #ctx{ imports = [ {NS, Prefix} | Ctx #ctx.imports ] };

		{use_single_quotes, B} when is_boolean( B ) ->
			Ctx #ctx{ use_single_quotes = B };

		Unexpected ->
			error({badarg, ?MODULE, ctx_new, Unexpected})
	end.


