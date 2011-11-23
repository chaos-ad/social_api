-module(social_api).

-export([start/0, stop/0, validate_auth/1, send_message/2, invoke_method/2, set_payment_callback/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    application:load(?MODULE),
    ensure_deps_started(),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_auth(AuthData) ->
    social_api_sup:validate_auth(AuthData).

send_message(Message, Users) ->
    social_api_sup:send_message(Message, Users).

invoke_method(Method, Args) ->
    social_api_sup:invoke_method(Method, Args).

set_payment_callback(Callback) ->
    social_api_sup:set_payment_callback(Callback).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_started(App) ->
    case application:start(App) of
        ok                              -> ok;
        {error, {already_started, App}} -> ok
    end.

ensure_deps_started() ->
    {ok, DepsList} = application:get_key(?MODULE, applications),
    lists:foreach( fun ensure_started/1, DepsList ).

