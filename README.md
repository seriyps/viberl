viberl - Viber bot API wrapper
==============================

Thin wrapper for [Viber's](https://viber.com) bot [HTTP API](http://developers.viber.com/docs/api/rest-bot-api/).

Build
-----

    $ rebar3 compile

Cowboy example
--------------

``` erlang
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).
-define(NAME, <<"Echo bot">>).

start() ->
    {ok, Token} = application:get_env(viber_bot_token),
    {ok, Port} = application:get_env(viber_bot_port),
    {ok, Host} = application:get_env(viber_bot_webhook_host),
    Path = <<"/viber_webhook">>,
    Routes = [
		{'_', [
               {Path, ?MODULE, [Token]}
              ]
        }
    ],
	{ok, _} = cowboy:start_http(http, 100, [{port, Port}],
                                [{env, [{dispatch, cowboy_router:compile(Routes)}]}]),
    ok = hackney_pool:start_pool(viberl, [{timeout, 60000}, {max_connections, 30}]),
    viberl:set_webhook(Token, <<"https://", Host/binary, Path/binary>>, all).
    
init(_Transport, Req, [BotToken]) ->
    {ok, Req, BotToken}.

handle(Req, Bot) ->
    {Headers, Req1} = cowboy_req:headers(Req),
    {QSVals, Req2} = cowboy_req:qs_vals(Req1),
    {ok, Body, Req3} = cowboy_req:body(Req2),
    {ok, Event} = viberl:handle_webhook_event(
        Bot, maps:from_list(Headers), maps:from_list(QSVals), Body),
    case handle_event(Bot, Event) of
        ok ->
            {ok, Req4} = cowboy_req:reply(200, Req3),
            {ok, Req4, Bot};
        {reply, RespBody} ->
            RespHeaders = [{<<"content-type">>, <<"application/json">>}],
            {ok, Req4} = cowboy_req:reply(200, RespHeaders, RespBody, Req3),
            {ok, Req4, Bot}
    end.
        

terminate(_Reason, _Req, _S)->
    ok.

handle_event(Bot, #{<<"event">> := <<"subscribed">>,
                    <<"user">> :=
                        #{<<"id">> := Id, <<"name">> := Name}}) ->
    {ok, _} = viberl:send_message(
                   Bot,
                   #{receiver => Id,
                     type => text,
                     sender => #{name => ?NAME},
                     text => <<"Hello, ", Name/binary, "! Thanks for subscribing.">>}),
    ok;
handle_event(_Bot, #{<<"event">> := <<"conversation_started">>,
                    <<"user">> :=
                         #{<<"id">> := _Id, <<"name">> := _Name}}) ->
    Response = #{type => text,
                 sender => #{name => ?NAME},
                 text => <<"Welcome to ", ?NAME/binary, "! Please, subscribe to continue!">>},
    {reply, jiffy:encode(Response)};
handle_event(Bot, #{<<"event">> := <<"message">>,
                    <<"sender">> := #{<<"id">> := Id},
                    <<"message">> := #{<<"type">> := Type} = Message}) ->
    Message1 = maps:without([<<"type">>], Message),
    Message2 = Message1#{receiver => Id,
                         type => Type,
                         sender => #{name => ?NAME}},
    {ok, _} = viberl:send_message(Bot, Message2),
    ok;
handle_event(_Bot, #{<<"event">> := Event}) ->
    lager:info("Unhandled event ~s", [Event]),
    ok.

```
