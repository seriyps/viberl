%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2017, Sergey Prokhorov
%%% @doc
%%% Main API module.
%%% @end
%%% Created :  8 Jun 2017 by Sergey Prokhorov <me@seriyps.ru>

-module(viberl).

-export([api_call/2, api_call/3]).

-export([set_webhook/3, remove_webhook/1, handle_webhook_event/4,
         get_event_user_id/1]).
-export([send_message/2, get_account_info/1, get_user_details/2, get_online/2, post/2]).

-type bot() :: any().
-type method() :: binary().
-type api_response() :: {ok, json_object()} | {error, Type :: atom(), term()}.

-type json_literal() :: null
                      | true
                      | false
                      | json_string()
                      | json_number().
-type json_value() :: json_literal()
                    | json_object()
                    | json_array().

-type json_array()  :: [json_value()].
-type json_string() :: atom() | binary().
-type json_number() :: integer() | float().

-type json_object() :: #{json_string() => json_value()}.


-define(HACKNEY_POOL, ?MODULE).
-define(SIGNATURE_HEADER_NAME, "x-viber-content-signature").
-define(TOKEN_HEADER_NAME, "x-viber-auth-token").

-spec send_message(bot(), map()) -> api_response().
send_message(Bot, #{receiver := _, type := _, sender := #{name := _}} = Req) ->
    api_call(Bot, <<"send_message">>, Req).

-spec get_account_info(bot()) -> api_response().
get_account_info(Bot) ->
    api_call(Bot, <<"get_account_info">>).

-spec get_user_details(bot(), map()) -> api_response().
get_user_details(Bot, #{id := _} = Req) ->
    api_call(Bot, <<"get_user_details">>, Req).

-spec get_online(bot(), map()) -> api_response().
get_online(Bot, #{ids := _} = Req) ->
    api_call(Bot, <<"get_online">>, Req).

-spec post(bot(), map()) -> api_response().
post(Bot, #{from := _, type := _} = Req) ->
    api_call(Bot, <<"post">>, Req).


%% delivered, seen, failed, subscribed, unsubscribed, conversation_started
-spec set_webhook(bot(), binary(), Events :: all | [binary()]) -> api_response().
set_webhook(Bot, Url, all) ->
    api_call(Bot, <<"set_webhook">>, #{url => Url});
set_webhook(Bot, Url, Events) ->
    api_call(Bot, <<"set_webhook">>, #{url => Url, event_types => Events}).

remove_webhook(Bot) ->
    api_call(Bot, <<"set_webhook">>, #{url => <<>>}).

-spec handle_webhook_event(bot(),
                           #{binary() => binary()},
                           #{binary() => binary()},
                           binary()) ->
                                  {ok, json_object()} | {error, Type :: atom(), term()}.
handle_webhook_event(Bot, HttpHeaders, QueryStringParams, Body) ->
    case get_signature(HttpHeaders, QueryStringParams) of
        {error, _, _} = Err ->
            Err;
        Signature ->
            Token = get_token(Bot),
            ComputedSignature = hmac(sha256, Token, Body),
            BinSignature = unhex(Signature),
            case ComputedSignature == BinSignature of
                true ->
                    BodyStruct = jiffy:decode(Body, [return_maps]),
                    parse_webhook_event(Bot, BodyStruct);
                false ->
                    {error, invalid_signature, {BinSignature, ComputedSignature}}
            end
    end.

-if(?OTP_RELEASE >= 23).
hmac(Alg, Key, Data) ->
    crypto:mac(hmac, Alg, Key, Data).
-else.
hmac(Alg, Key, Data) ->
    crypto:hmac(Alg, Key, Data).
-endif.

parse_webhook_event(_Bot, #{<<"event">> := _,
                            <<"timestamp">> := _,
                            <<"message_token">> := _} = Body) ->
    {ok, Body}.

get_signature(#{<<?SIGNATURE_HEADER_NAME>> := Sig}, _) -> Sig;
get_signature(_, #{<<"sig">> := Sig}) -> Sig;
get_signature(HttpHeaders, QS) ->
    {error, missing_signature, {HttpHeaders, QS}}.

get_event_user_id(#{<<"user">> := #{<<"id">> := Id}}) -> Id;
get_event_user_id(#{<<"user_id">> := Id}) -> Id;
get_event_user_id(#{<<"sender">> := #{<<"id">> := Id}}) -> Id;
get_event_user_id(#{<<"event">> := <<"webhook">>}) -> undefined.

-spec api_call(bot(), method()) -> api_response().
api_call(Bot, Method) ->
    api_call(Bot, Method, undefined).

-spec api_call(bot(), method(), json_object() | undefined) ->
                      {ok, json_object()} | {error, Type :: atom(), term()}.
api_call(Bot, Method, Body) ->
    Endpoint = application:get_env(viberl, api_server_endpoint, <<"https://chatapi.viber.com">>),
    Url = <<Endpoint/binary, "/pa/", Method/binary>>,
    Token = get_token(Bot),
    case do_api_call(Url, Token, Body) of
        {ok, Code, Hdrs, BodyRef} ->
            ContentType = hackney_headers:parse(<<"content-type">>, Hdrs),
            case {hackney:body(BodyRef), ContentType, Code} of
                {{ok, <<>>}, _, 200} -> ok;
                {{ok, RespBody}, {<<"application">>, <<"json">>, _}, 200} ->
                    {ok, jiffy:decode(RespBody, [return_maps])};
                {{error, ErrBody}, _, _} ->
                    {error, hackney_body, ErrBody}
            end;
        {error, ErrReason} -> {error, hackney, ErrReason}
    end.

do_api_call(Url, Token, undefined) ->
    do_api_call(Url, Token, #{});
do_api_call(Url, Token, Payload) when is_map(Payload) ->
    Json = jiffy:encode(Payload),
    Headers = [{<<"Content-Type">>, <<"application/json">>},
               {<<"Accept">>, <<"application/json">>},
               {<<?TOKEN_HEADER_NAME>>, Token}],
    hackney:request(<<"POST">>, Url, Headers, Json, [{pool, ?HACKNEY_POOL}]).

get_token(Bot) ->
    Bot.


%% hex(Bin) ->
%%     << (erlang:integer_to_binary(N, 16)) || <<N:4>> <= Bin >>.

unhex(Hex) ->
    << <<(erlang:binary_to_integer(<<C>>, 16)):4/integer>> || <<C:8>> <= Hex >>.
