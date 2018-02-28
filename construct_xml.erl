-module(construct_xml).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").


%% <Tracking>..</Tracking>
generate_vast_tracking_event_complete(Nast) ->
  case xmerl_xpath:string("//Tracker[@type='completion']/text()", Nast) of
    [ #xmlText{value=Text} ] ->
      #xmlElement{
        name='Tracking',
        attributes=[#xmlAttribute{name='event', value="complete"}],
        content=[
          #xmlText{
            value=generate_cdata(Text)
          }
        ]
      };
    _otherwise ->
      {error, "completion should just 1"}
  end.

%% <MediaFile>..</MediaFile>
generate_vast_media_file(Nast) ->
  [#xmlAttribute{value=With}] = xmerl_xpath:string("//Video/@width", Nast),
  [#xmlAttribute{value=Height}] = xmerl_xpath:string("//Video/@height", Nast),
  case xmerl_xpath:string("//Video[@type='main']/text()", Nast) of
    [ #xmlText{value=Text} ] ->
      #xmlElement{
        name='MediaFile',
        attributes=[
          #xmlAttribute{name='delivery', value="progressive"},
          #xmlAttribute{name='type', value="video/mp4"}, %% TODO これないとVAST_TEST通らない mp4以外にどんなの来るか先方確認中
          #xmlAttribute{name='width', value=With},
          #xmlAttribute{name='height', value=Height}
        ],
        content=[
          #xmlText{
            value=generate_cdata(Text)
          }
        ]
      };
    _otherwise ->
      {error, "video should just 1"}
  end.

%% <ClickThrough>..</ClickThrough>
genarate_vast_click_through(Nast) ->
  case xmerl_xpath:string("//Action[@type='click']/text()", Nast) of
    [ #xmlText{value=Text} ] ->
      #xmlElement{
        name='ClickThrough',
        content=[
          #xmlText{
            value=generate_cdata(Text)
          }
        ]
      };
    _otherwise ->
      {error, "click_through should just 1"}
  end.

%% <Vast>
%%   <Ad>..</Ad>
%% </Vast>
generate_vast(Nast) ->
  [ #xmlElement{
    name='VAST',
    attributes=[
      generate_version("4.0"),
      #xmlAttribute{name='xmlns:xs', value='http://www.w3.org/2001/XMLSchema'},
      #xmlAttribute{name=xmlns, value='http://www.iab.com/VAST'}
    ],
    content= generate_vast_ad(Nast)
  } ].

%% <Ad>
%%   <InLine>..</InLine>
%% </Ad>
generate_vast_ad(Nast) ->
  [
    #xmlElement{
      name='Ad',
      attributes=[
        #xmlAttribute{name='id', value="20001"}, %% an ad server-defined identifier string for the ad
        #xmlAttribute{name='sequence', value="1"}, %% FIXED
        #xmlAttribute{name='conditionalAd', value="false"} %% FIXED
      ],
      content=generate_vast_inline(Nast)
    }
  ].

%% <InLine>
%%  <AdSystem>..</AdSystem>
%%  <AdTitle>..</AdTitle>
%%  <Impression>..</Impression>
%%  <Creatives>..</Creatives>
%% <InLine>
generate_vast_inline(Nast) ->
  case generate_vast_ad_title(Nast) of
    {error, Reason} -> {error, Reason};
    AdTitle ->
      [
        #xmlElement{
          name='InLine',
          content=[
            generate_vast_ad_system(), %% AdSystem
            AdTitle, %%AdTitle
            generate_vast_impression(Nast), %% Impression
            generate_vast_creatives(Nast) %% Creatives
          ]
        }
      ]
  end.

%% <AdTitle>..</AdTitle>
-spec( generate_vast_ad_title(#xmlElement{}) -> iolist() | {error, iolist()} ).
generate_vast_ad_title(Nast) ->
  case xmerl_xpath:string("//Text[@type='headline']/text()", Nast) of
    [ #xmlText{value=Text} ] ->
      #xmlElement{
        name='AdTitle',
        content=[
          #xmlText{
            value=unicode:characters_to_binary(Text)
          }
        ]
      };
    _otherwise ->
      {error, "headline should just 1"}
  end.

%% <AdSystem>..</AdSystem>
generate_vast_ad_system() ->
  #xmlElement{
    name='AdSystem',
    attributes=[ generate_version("0.1") ], %% A string that provides the version number of the ad system that returned the ad
    content=[
      #xmlText{value="Hoge"} %% A string that provides the name of the ad server that returned the ad
    ]
  }.

%% <Impression>..</Impression>
generate_vast_impression(Nast) ->
  case xmerl_xpath:string("//Tracker[@type='impression']/text()", Nast) of
    [ #xmlText{value=Text} ] ->
      #xmlElement{
        name='Impression',
        content=[
          #xmlText{
            value=generate_cdata(Text)
          }
        ]
      };
    _otherwise ->
      {error, "impression should just 1"}
  end.

%% <Creatives>..</Creatives>
generate_vast_creatives(Nast) ->
  #xmlElement{
    name='Creatives',
    content=generate_vast_creative(Nast)
  }.

%% <Creative>
%%   <UniversalAdId>..</UniversalAdId>
%%   <Linear>..</Linear>
%% </Creative>
generate_vast_creative(Nast) ->
  [
    #xmlElement{
      name='Creative',
      content=[
        generate_vast_universal_ad_id(), %% UniversalAdId
        generate_vast_linear(Nast) %% Linear
      ]
    }
  ].

%% <UniversalAdId>..</UniversalAdId>
generate_vast_universal_ad_id() ->
  #xmlElement{
    name='UniversalAdId',
    attributes=[
      #xmlAttribute{name='idRegistry', value="unknown"}, %% A string used to identify the URL for the registry website where the unique creative ID is cataloged. Default value is “unknown.”
      #xmlAttribute{name='idValue', value="unknown"} %% A string for the unique creative ID. Default value is “unknown.”
    ],
    content=[
      #xmlText{value="unknown"} %% A string identifying the unique creative identifier.
    ]
  }.

%% <Linear>
%%   <TrackingEvents>..</TrackingEvents>
%%   <MediaFile>..</MediaFile>
%%   <VideoClicks>..</VideoClicks>
%% </Linear>
generate_vast_linear(Nast) ->
  #xmlElement{
    name='Linear',
    content=[
      generate_vast_tracking_events(Nast),
      generate_vast_media_files(Nast),
      generate_vast_video_clicks(Nast)
    ]
  }.

%% <TrackingEvents>..</TrackingEvents>
generate_vast_tracking_events(Nast) ->
  #xmlElement{
    name='TrackingEvents',
    content=[
      generate_vast_tracking_event_complete(Nast)
    ]
  }.

%% <MediaFile>..</MediaFile>
generate_vast_media_files(Nast) ->
  #xmlElement{
    name='MediaFiles',
    content=[
      generate_vast_media_file(Nast)
    ]
  }.

%% <VideoClicks>..</VideoClicks>
generate_vast_video_clicks(Nast) ->
  #xmlElement{
    name='VideoClicks',
    content=[
      genarate_vast_click_through(Nast)
    ]
  }.

generate_cdata(Text) ->
  string:join(["<![CDATA[", Text, "]]>"], "").

generate_version(Version) ->
  #xmlAttribute{
    name=version,
    value=Version
  }.

serialize(Data) ->
  Xml = lists:flatten(xmerl:export_simple(Data, xmerl_xml)),
  io:format("~s~n", [Xml]).

test() ->
%%  serialize(generate_vast(Nast)).
  serialize(generate_vast(parse_al())).

%% XMLをパースしてVASTを組み立てる
parse_al() ->
  Body = try_hackney_request_al(),
  {Xml, _} = xmerl_scan:string(binary_to_list(Body), [{space, normalize}]),
  Xml.

try_hackney_request_al() ->
  application:ensure_all_started(hackney),
  Method = get,
  URL = <<"">>, %% WriteME
  ReqHeaders = [],
  ReqBody = <<>>,
  Options = [{follow_redirect, true}],
  {ok, _, _, ClientRef} = hackney:request(Method, URL, ReqHeaders, ReqBody, Options),
  {ok, Body} = hackney:body(ClientRef),
  Body.
