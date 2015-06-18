-module (exp_sax2dom).
-export ([
		new/1, new/0,
		sax_event_in/2
	]).
-include("xml.hrl").
-include("sax.hrl").

-record(cd_frame, {
		'#module' = ?MODULE :: ?MODULE,
		chunks = queue:new() :: queue:queue( iodata() ),
		parent_frame :: xe_frame()
	}).
-record(xe_frame, {
		'#module' = ?MODULE :: ?MODULE,
		ns :: xml_ns(),
		ncn :: xml_ncname(),
		attrs :: [ xml_attribute() ],
		chs_queue = queue:new() :: queue:queue( xml_element() ),
		parent_frame :: maybe_xe_frame()
	}).
-type maybe_xe_frame() :: undefined | #xe_frame{}.
-type xe_frame() :: #xe_frame{}.
-type frame() :: #xe_frame{} | #cd_frame{}.

-record(ctx, {
		'#module' = ?MODULE :: ?MODULE,
		current_frame :: undefined | frame()
	}).

-type args() :: [ tuple() ].
-type ctx() :: #ctx{}.

-define( ret_incomplete( Ctx ), {ok, Ctx} ).
-define( ret_complete( Xml ), {final, Xml} ).

-spec new( args() ) -> {ok, ctx()}.
-spec sax_event_in( sax_event(), ctx() ) -> ?ret_incomplete( ctx() ) | ?ret_complete(xml_element()) | {error, term()}.

new() -> new([]).
new( _Args ) -> {ok, #ctx{}}.

%%%
%%% --- cursor ---
%%% <root-element>
sax_event_in(
	#xml_element_start{ ns = NS, ncn = NCN, attrs = Attrs },
	Ctx0 = #ctx{ current_frame = undefined }
) ->
	Frame = #xe_frame{ ns = NS, ncn = NCN, attrs = Attrs },
	Ctx1 = Ctx0 #ctx{ current_frame = Frame },
	?ret_incomplete(Ctx1);

%%% <element-to-be-grand-parent>
%%%   ...
%%%   <element-to-be-parent>
%%% --- cursor ---
%%%     <element-to-be-current-frame>
sax_event_in(
	#xml_element_start{ ns = NS, ncn = NCN, attrs = Attrs },
	Ctx0 = #ctx{ current_frame = FrameToBeParent }
) when is_record( FrameToBeParent, xe_frame ) ->
	Frame = #xe_frame{ ns = NS, ncn = NCN, attrs = Attrs, parent_frame = FrameToBeParent },
	Ctx1 = Ctx0 #ctx{ current_frame = Frame },
	?ret_incomplete(Ctx1);

%%% <maybe-parent-element><!-- in case this element does not exist - we have an XmlElement complete -->
%%%   ...
%%%   <element-that-is-current-frame>
%%%     ...
%%% --- cursor ---
%%%   </element-that-is-current-frame>
sax_event_in(
	#xml_element_end{ ns = NS, ncn = NCN },
	Ctx0 = #ctx{ current_frame = Frame }
) when is_record( Frame, xe_frame ) andalso { Frame #xe_frame.ns, Frame #xe_frame.ncn } == { NS, NCN } ->
	{ok, FinalizedXeFrame, ParentFrame0} = finalize_frame( Frame ),
	case ParentFrame0 of
		undefined -> ?ret_complete( FinalizedXeFrame );
		#xe_frame{} ->
			{ok, ParentFrame1} = xe_frame_ch_enqueue( FinalizedXeFrame, ParentFrame0 ),
			Ctx1 = Ctx0 #ctx{ current_frame = ParentFrame1 },
			?ret_incomplete( Ctx1 )
	end;

%%% <element-to-be-parent>
%%% --- cursor ---
%%%   <![CDATA[this is a CData to form a new current frame]]>
sax_event_in(
	#xml_cdata{ value = CData },
	Ctx0 = #ctx{ current_frame = FrameToBeParent }
) when is_record( FrameToBeParent, xe_frame ) ->
	Chunks = queue:from_list([ CData ]),
	CDFrame = #cd_frame{ chunks = Chunks, parent_frame = FrameToBeParent },
	Ctx1 = Ctx0 #ctx{ current_frame = CDFrame },
	?ret_incomplete( Ctx1 );

%%% <element-to-be-grand-parent>
%%%   ...
%%%   <element-to-stay-parent>
%%%     <![CDATA[this is a CData to be immediately previous sibling]]>
%%% --- cursor ---
%%%     <element-to-be-current-frame>
sax_event_in(
	#xml_element_start{ ns = NS, ncn = NCN, attrs = Attrs },
	Ctx0 = #ctx{ current_frame = CDFrameToBePreviousSibling }
) when is_record( CDFrameToBePreviousSibling, cd_frame ) ->
	{ok, FinalizedCD, ParentFrame0} = finalize_frame( CDFrameToBePreviousSibling ),
	{ok, ParentFrame1} = xe_frame_ch_enqueue( FinalizedCD, ParentFrame0 ),
	NewFrame = #xe_frame{ ns = NS, ncn = NCN, attrs = Attrs, parent_frame = ParentFrame1 },
	Ctx1 = Ctx0 #ctx{ current_frame = NewFrame },
	?ret_incomplete(Ctx1);

%%% <maybe-parent-element>
%%%   ...
%%%   <![CDATA[this is current frame]]>
%%% --- cursor ---
%%%   <![CDATA[this is to be appended to the current-frame]]>
sax_event_in(
	#xml_cdata{ value = CData },
	Ctx0 = #ctx{ current_frame = Frame0 }
) when is_record( Frame0, cd_frame ) ->
	{ok, Frame1} = cd_frame_append( CData, Frame0 ),
	Ctx1 = Ctx0 #ctx{ current_frame = Frame1 },
	?ret_incomplete( Ctx1 );

%%%
%%% ...
%%% <parent-element>
%%%   ...
%%%   <![CDATA[this is current frame]]>
%%% --- cursor ---
%%% </parent-element>
sax_event_in(
	#xml_element_end{ ns = NS, ncn = NCN },
	Ctx0 = #ctx{ current_frame = CDFrame }
) when is_record( CDFrame, cd_frame ) andalso
	{NS, NCN} == {CDFrame #cd_frame.parent_frame #xe_frame.ns, CDFrame #cd_frame.parent_frame #xe_frame.ncn}
->
	{ok, FinalizedCD, ImmediateParentFrame0} = finalize_frame( CDFrame ),
	{ok, ImmediateParentFrame1} = xe_frame_ch_enqueue( FinalizedCD, ImmediateParentFrame0 ),
	{ok, FinalizedImmediateParent, FrameToBeCurrent0} = finalize_frame( ImmediateParentFrame1 ),
	{ok, FrameToBeCurrent1} = xe_frame_ch_enqueue( FinalizedImmediateParent, FrameToBeCurrent0 ),
	Ctx1 = Ctx0 #ctx{ current_frame = FrameToBeCurrent1 },
	?ret_incomplete( Ctx1 );

sax_event_in(
	#xml_cdata{},
	#ctx{ current_frame = undefined }
) ->
	{error, cdata_on_top_level};

sax_event_in(
	#xml_element_end{ ns = NS, ncn = NCN },
	#ctx{ current_frame = #cd_frame{ parent_frame = #xe_frame{ ns = NSExpected, ncn = NCNExpected } } }
) when { NSExpected, NCNExpected } /= { NS, NCN } ->
	{error, closing_tag_mismatch};

sax_event_in(
	#xml_element_end{ ns = NS, ncn = NCN },
	#ctx{ current_frame = #xe_frame{ ns = NSExpected, ncn = NCNExpected } }
) when { NSExpected, NCNExpected } /= { NS, NCN }->
	{error, closing_tag_mismatch};

sax_event_in(
	#xml_element_end{},
	#ctx{ current_frame = undefined }
) ->
	{error, closing_tag_on_top_level};

sax_event_in(
	UnexpectedEvent,
	#ctx{ current_frame = CurrentFrame }
) ->
	{error, {unexpected_event, UnexpectedEvent, CurrentFrame}}.



finalize_frame( #xe_frame{ ns = NS, ncn = NCN, attrs = Attrs, chs_queue = ChsQ, parent_frame = ParentFrame } ) ->
	Xml = exp_node:new( {NS, NCN}, Attrs, queue:to_list( ChsQ ) ),
	{ok, Xml, ParentFrame};

finalize_frame( #cd_frame{ chunks = ChunksQ, parent_frame = ParentFrame } ) ->
	Xml = exp_text_node:cdata_new( iolist_to_binary( queue:to_list( ChunksQ ) ) ),
	{ok, Xml, ParentFrame}.

xe_frame_ch_enqueue( Ch, Frame0 = #xe_frame{ chs_queue = ChsQ0 } ) ->
	ChsQ1 = queue:in( Ch, ChsQ0 ),
	Frame1 = Frame0 #xe_frame{ chs_queue = ChsQ1 },
	{ok, Frame1}.

cd_frame_append( Bin, Frame0 = #cd_frame{ chunks = ChsQ0 } ) ->
	ChsQ1 = queue:in( Bin, ChsQ0 ),
	Frame1 = Frame0 #cd_frame{ chunks = ChsQ1 },
	{ok, Frame1}.


