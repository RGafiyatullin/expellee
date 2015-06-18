-ifndef (expellee_include_sax_hrl).
-define (expellee_include_sax_hrl, true).

-include_lib("expellee/include/xml.hrl").

-record(xml_element_start, { ns :: xml_ns(), ncn :: xml_ncname(), attrs :: [ xml_attribute() ] }).
-record(xml_element_end, { ns :: xml_ns(), ncn :: xml_ncname() }).
-record(xml_cdata, { value :: binary() }).
-record(xml_cdata_tmp, { value :: iolist() }).

-type sax_event() :: #xml_element_start{} | #xml_element_end{} | #xml_cdata{}.

-endif. % epellee_include_sax_hrl
