-module(social_api_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([validate_auth/1, send_message/2, invoke_method/2, set_payment_callback/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_auth(AuthData) ->
    social_api_client:validate_auth(AuthData).

send_message(Message, Users) ->
    social_api_client:send_message(Message, Users).

invoke_method(Method, Args) ->
    social_api_client:invoke_method(Method, Args).

set_payment_callback(Callback) ->
    social_api_settings:set_payment_callback(Callback).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    Spec = [{client, social_api_client}, {server, social_api_server}],
    Modules = [ Module || {Name, Module} <- Spec, has_env(Name) ],
    {ok, { {one_for_one, 5, 10},
        lists:map(fun(Module) -> ?CHILD(Module) end, Modules)
    } }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_env(Env) ->
    case application:get_env(Env) of
        {ok, _} -> true;
        _       -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
