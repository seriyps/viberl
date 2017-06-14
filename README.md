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
    viberl:set_webhook(Token, <<"https://", Host/binary, Path/binary>>, all).
    

init(_Transport, Req, [BotToken]) ->
    {ok, Req, BotToken}.

handle(Req, Bot) ->
    {AuthToken, Req1} = cowboy_req:header(<<"x-viber-auth-token">>, Req),
    {Body, Req2} = cowboy_req:body(Req1),
    {ok, Event} = viberl:handle_webhook_event(
        Bot, #{<<"x-viber-auth-token">> => AuthToken}, Body),
    case handle_event(Bot, Event) of
        ok ->
            {ok, Req2, Bot};
        {reply, Body} ->
            Headers = [{<<"content-type">>, <<"application/json">>}],
            {ok, Req3} = cowboy_req:reply(200, Headers, Body, Req2),
            {ok, Req3, Bot}
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
handle_event(Bot, #{<<"event">> := <<"conversation_started">>,
                    <<"user">> :=
                        #{<<"id">> := _Id, <<"name">> := Name}}) ->
    Response = #{type => text,
                 sender => #{name => ?NAME},
                 text => <<"Welcome to ", ?NAME/binary, "! Please, subscribe to continue!">>},
    {reply, jiffy:encode(Response)};
handle_event(Bot, #{<<"event">> := <<"message">>,
                    <<"sender">> := #{<<"id">> := Id},
                    <<"message">> := #{<<"type">> := Type} = Message}) ->
    Message1 = message:without([<<"type">>], Message),
    Message2 = Message1#{receiver => Id,
                         type => Type,
                         sender => #{name => ?NAME}},
    {ok, _} = viberl:send_message(Bot, Message2),
    ok;
handle_event(Bot, #{<<"event">> := Event}) ->
    lager:info("Unhandled event ~s", [Event]),
    ok.
```
