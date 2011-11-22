-module(social_api_settings).

-export
([
    app_id/0,
    network/0,
    secret_key/0,
    network_mod/0,

    server_host/0,
    server_port/0,
    server_mode/0,
    server_mode/1,

    payment_callback/0,
    set_payment_callback/1,

    client_host/0,
    client_host/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(APPLICATION, social_api).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

app_id() -> get_env_strict(app_id).
network() -> get_env_strict(network).
secret_key() -> get_env_strict(secret_key).
network_mod() -> list_to_atom( "social_api_" ++ atom_to_list(network()) ).

server_host() -> get_env_strict(server, host).
server_port() -> get_env_strict(server, port).
server_mode() -> get_env_strict(server, mode).
server_mode(Def) -> get_env_default(server, mode, Def).

payment_callback() -> get_env_strict(server, callback).
set_payment_callback(Callback) -> set_env(server, callback, Callback).

client_host() -> get_env_strict(client, host).
client_host(Def) -> get_env_default(client, host, Def).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_env(Env) ->
    case application:get_env(?APPLICATION, Env) of
        {ok, Value} -> Value;
        _           -> undefined
    end.

get_env_strict(Env) ->
    case get_env(Env) of
        undefined -> exit(invalid_settings);
        Value     -> Value
    end.

get_env_strict(Env, Name) ->
    case proplists:get_value(Name, get_env_strict(Env)) of
        undefined -> exit(invalid_settings);
        Value     -> Value
    end.

get_env_default(Env, Name, Default) ->
    case proplists:get_value(Name, get_env_strict(Env)) of
        undefined -> Default;
        Value     -> Value
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_env(Env, Value) ->
    application:set_env(?APPLICATION, Env, Value).

set_env(Env, Name, Value) ->
    set_env(Env, lists:keystore(Name, 1, get_env_strict(Env), {Name, Value})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
