-module(adapt_xml).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

get_ad_title(Nast) ->
  case xmerl_xpath:string("//Text[@type='headline']/text()", Nast) of
    [ #xmlText{value=Text} ] ->
      unicode:characters_to_binary(Text);
    _otherwise ->
      {error, "headline should just 1"}
  end.

get_impression(Nast) ->
  case xmerl_xpath:string("//Tracker[@type='impression']/text()", Nast) of
    [ #xmlText{value=Text} ] ->
      generate_cdata(Text);
    _otherwise ->
      {error, "impression should just 1"}
  end.


get_tracking_event_complete(Nast) ->
  case xmerl_xpath:string("//Tracker[@type='completion']/text()", Nast) of
    [ #xmlText{value=Text} ] ->
      generate_cdata(Text);
    _otherwise ->
      {error, "completion should just 1"}
  end.

get_video_click_through(Nast) ->
  case xmerl_xpath:string("//Action[@type='click']/text()", Nast) of
    [ #xmlText{value=Text} ] ->
      generate_cdata(Text);
    _otherwise ->
      {error, "click_through should just 1"}
  end.

get_media_mp4_url(Nast) ->
  case xmerl_xpath:string("//Video/text()", Nast) of
    [ #xmlText{value=Text} ] ->
      generate_cdata(Text);
    _otherwise ->
      {error, "mp4 file should just 1"}
  end.

get_media_mp4_width(Nast) ->
  case xmerl_xpath:string("//Video/@width", Nast) of
    [ #xmlAttribute{value=Text} ] ->
      Text;
    _otherwise ->
      {error, "mp4 file width should just 1"}
  end.

get_media_mp4_height(Nast) ->
  case xmerl_xpath:string("//Video/@height", Nast) of
    [ #xmlAttribute{value=Text} ] ->
      Text;
    _otherwise ->
      {error, "mp4 file height should just 1"}
  end.

generate_cdata(Text) ->
  string:join(["<![CDATA[", Text, "]]>"], "").

request_al() ->
  application:ensure_all_started(hackney),
  Method = get,
  URL = <<"URL">>, %% WriteMe
  ReqHeaders = [],
  ReqBody = <<>>,
  Options = [{follow_redirect, true}],
  {ok, _, _, ClientRef} = hackney:request(Method, URL, ReqHeaders, ReqBody, Options),
  {ok, Body} = hackney:body(ClientRef),
  Body.

parse_al() ->
  Body = request_al(),
  {Nast, _} = xmerl_scan:string(binary_to_list(Body), [{space, normalize}]),
  Nast.

start() ->
  Nast = parse_al(),
  erlydtl:compile("vast_linear_template.xml", vast_linear_template),
  {ok, Xml} = vast_linear_template:render([
    {adSystem, "hoge"},
    {adTitle, get_ad_title(Nast)},
    {impression, get_impression(Nast)},
    {trackingComplete, get_tracking_event_complete(Nast)},
    {clickThrough, get_video_click_through(Nast)},
    {mediaMp4Height, get_media_mp4_height(Nast)},
    {mediaMp4Width, get_media_mp4_width(Nast)},
    {mediaMp4Url, get_media_mp4_url(Nast)}
    ]),
  io:format("~s~n",[Xml]).
