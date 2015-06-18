-ifndef(expellee_include_xml_hrl).
-define(expellee_include_xml_hrl, true).

-define(xml_ns_inherit, inherit).

-define(is_xml_ns(S), (is_binary(S) orelse S == ?xml_ns_inherit) ).
-define(is_xml_ncname(S), is_binary(S)).

-type xml_ncname() :: binary().
-type xml_ns() :: binary() | ?xml_ns_inherit.
-type xml_name() ::
		{ xml_ns(), xml_ncname() } |
		xml_ncname().

-type xml_attribute_name() :: xml_ncname().
-type xml_attribute_value() :: binary().
-type xml_attribute() :: { xml_attribute_name(), xml_attribute_value() }.

-type xml_ns_prefix() :: binary().
-type xml_ns_import() :: { xml_ns(), xml_ns_prefix() }.

-record(xe, {
		ncn :: xml_ncname(),
		ns = ?xml_ns_inherit :: ?xml_ns_inherit | xml_ns(),

		a = []:: [ xml_attribute() ],
		c = [] :: [ xml_element() ],
		d = [] :: [ term() ] %% Data: not rendered => does not survive serialization-deserialization.
	}).
-record(chars, {
		data = <<>> :: binary(),
		d = [] :: [ term() ] %% Data: not rendered => does not survive serialization-deserialization.
	}).
-record(cd, {
		data = <<>> :: binary(),
		d = [] :: [ term() ] %% Data: not rendered => does not survive serialization-deserialization.
	}).
-type xml_element() :: #xe{} | #cd{} | #chars{}.

-define( is_xml_element( XE ),
			(is_record(XE, xe) orelse
			 is_record(XE, chars) orelse
			 is_record(XE, cd))
		).
-define( is_xml_xe( XE ), is_record( XE, xe ) ).
-define( is_xml_chars( XE ), is_record( XE, chars ) ).
-define( is_xml_cd( XE ), is_record( XE, cd ) ).


-endif. % expellee_include_xml_hrl
