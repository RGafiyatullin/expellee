-module (exp_render).
-export([
		render/1,
		render/2
	]).

-export ([
		ctx_new/1
	]).

-include("xml.hrl").

-spec ctx_new( [ tuple() ] ) -> exp_render_stream:ctx().

-type rendered() :: iodata().
-type maybe_ctx() :: undefined | exp_render_stream:ctx().
-spec render( xml_element() ) -> rendered().
-spec render( xml_element(), exp_render_stream:ctx() ) -> {rendered(), maybe_ctx()}.


ctx_new( Args ) -> exp_render_stream:ctx_new( Args ).

render( Xml ) ->
	{Rendered, _CtxOut} = render( Xml, ctx_new([]) ),
	Rendered.

render( Xml, Ctx ) ->
	render_impl( Xml, Ctx ).



%%% Internal %%%

render_impl( #cd{ data = Data }, CtxIn ) ->
	{_CDataIOL, _CtxOut} = exp_render_stream:cdata_node( Data, CtxIn );

render_impl( #chars{ data = Data }, CtxIn ) ->
	{_CDataIOL, _CtxOut} = exp_render_stream:text_node( Data, CtxIn );

render_impl( #xe{ ns = NS, ncn = NCN, a = Attrs, c = [] }, Ctx0 ) ->
	{_SelfClosingTagIOL, _Ctx1} = exp_render_stream:node_self_closing_tag( NS, NCN, Attrs, Ctx0 );

render_impl( #xe{ ns = NS, ncn = NCN, a = Attrs, c = Children }, Ctx0 ) ->
	{OpenTagIOL, Ctx1} = exp_render_stream:node_open_tag( NS, NCN, Attrs, Ctx0 ),
	{ChildrenIOL, Ctx2} = lists:foldl(
		fun ( Ch, { ChIOLAcc, CtxIn } ) ->
			{ ChIOL, CtxOut } = render_impl( Ch, CtxIn ),
			{ [ ChIOL | ChIOLAcc ], CtxOut }
		end,
		{[], Ctx1}, lists:reverse( Children )),
	{CloseTagIOL, Ctx3} = exp_render_stream:node_close_tag( NS, NCN, Ctx2 ),
	{[ OpenTagIOL, ChildrenIOL, CloseTagIOL ], Ctx3}.
