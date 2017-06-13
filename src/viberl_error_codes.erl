%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2017, Sergey Prokhorov
%%% @doc
%%% Viber error codes descriptions.
%%% See http://developers.viber.com/docs/api/rest-bot-api/#errorCodes
%%% @end
%%% Created : 13 Jun 2017 by Sergey Prokhorov <me@seriyps.ru>

-module(viberl_error_codes).

-export([code_to_error/1, code_to_tag/1, code_to_description/1]).

-type code() :: non_neg_integer().
-type tag() :: atom().
-type description() :: binary().

-spec code_to_tag(code()) -> tag().
code_to_tag(Code) ->
    element(1, code_to_error(Code)).

-spec code_to_description(code()) -> description().
code_to_description(Code) ->
    element(2, code_to_error(Code)).

-spec code_to_error(code()) -> {tag(), description()}.
code_to_error(0) -> {ok, success};
code_to_error(1) -> {invalid_url, <<"The webhook URL is not valid">>};
code_to_error(2) -> {invalid_auth_token, <<"The authentication token is not valid">>};
code_to_error(3) -> {bad_data, <<"There is an error in the request itself (missing comma, brackets, etc.)">>};
code_to_error(4) -> {missing_data, <<"Some mandatory data is missing">>};
code_to_error(5) -> {receiver_not_registered, <<"The receiver is not registered to Viber">>};
code_to_error(6) -> {receiver_not_subscribed, <<"The receiver is not subscribed to the PA">>};
code_to_error(7) -> {public_account_blocked, <<"The public account is blocked">>};
code_to_error(8) -> {public_account_not_found, <<"The account associated with the token is not a public account.">>};
code_to_error(9) -> {public_account_suspended, <<"The public account is suspended">>};
code_to_error(10) -> {webhook_not_set, <<"No webhook was set for the public account">>};
code_to_error(11) -> {receiver_no_suitable_device, <<"The receiver is using a device or a Viber version that don’t support public accounts">>};
code_to_error(12) -> {too_many_requests, <<"Rate control breach">>};
code_to_error(13) -> {api_version_not_supported, <<"Maximum supported PA version by all user’s devices is less than the minApiVersion in the message">>};
code_to_error(14) -> {incompatible_with_version, <<"minApiVersion is not compatible to the message fields">>};
code_to_error(_) -> {general_error, <<"General error">>}.
