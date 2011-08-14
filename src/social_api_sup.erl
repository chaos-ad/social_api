-module(social_api_sup).
-behaviour(supervisor).

-include("logger.hrl").

-export([start_link/0, init/1, validate_auth/1, invoke_method/2, set_payment_callback/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_auth(AuthData) ->
    social_api_client:validate_auth(social_api_client, AuthData).

invoke_method(Method, Args) ->
    social_api_client:invoke_method(social_api_client, Method, Args).

set_payment_callback(Callback) ->
    social_api_server:set_payment_callback(social_api_server, Callback).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok, Network}   = application:get_env(network),
    {ok, AppID}     = application:get_env(app_id),
    {ok, SecretKey} = application:get_env(secret_key),

    BaseArgs  = [{network, Network}, {app_id, AppID}, {secret_key, SecretKey}],
    ChildList = [{social_api_client, client_options}, {social_api_server, server_options}],
    {ok, { {one_for_one, 5, 10}, make_child_spec(BaseArgs, ChildList)} }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_child_spec(BaseArgs, List) -> make_child_spec(BaseArgs, List, []).

make_child_spec(_, [], Result) -> lists:reverse(Result);

make_child_spec(BaseArgs, [{Name, EnvName}|Tail], Result) ->
    case application:get_env(EnvName) of
        {ok, Args} ->
            Spec = ?CHILD(Name, [{local, Name}, lists:append(BaseArgs, Args)]),
            make_child_spec(BaseArgs, Tail, [Spec|Result]);
        _ ->
            make_child_spec(BaseArgs, Tail, Result)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
