-module(social_api_vkontakte).

-export
([
    parse_client_options/1,
    parse_server_options/1,
    validate_auth/2,
    invoke_method/3,
    process_payment/2
]).

-record(client_options, {app_id, secret_key, viewer_id, host}).
-record(server_options, {app_id, secret_key}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_client_options(Options) ->
    {ok, #client_options{app_id     = proplists:get_value(app_id,     Options),
                         secret_key = proplists:get_value(secret_key, Options),
                         viewer_id  = proplists:get_value(viewer_id,  Options),
                         host       = proplists:get_value(host,       Options)}}.

parse_server_options(Options) ->
    {ok, #server_options{app_id     = proplists:get_value(app_id,     Options),
                         secret_key = proplists:get_value(secret_key, Options)}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_auth({UserID, _, Signature}, #client_options{app_id=AppID, secret_key=SecretKey}) ->
    Data = social_api_utils:concat([AppID, UserID, SecretKey], $_),
    case social_api_utils:md5_hex(Data) of
        Signature -> ok;
        _         -> {error, invalid_signature}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_method({secure, Function}, Args, #client_options{app_id=AppID, secret_key=SecretKey, host=Host}) ->
    Method   = social_api_utils:concat([{atom_to_list(secure), atom_to_list(Function)}], $., []),
    Required = [{api_id, AppID}, {format, json}, {method, Method}, {v, "3.0"},
                {random, random:uniform(10000000)}, {timestamp, social_api_utils:timestamp()}],

    Arguments     = social_api_utils:merge(Args, Required),
    UnsignedQuery = social_api_utils:concat(Arguments, $=, []) ++ SecretKey,
    SignedQuery   = social_api_utils:concat(social_api_utils:merge(Arguments, [{sig, social_api_utils:md5_hex(UnsignedQuery)}]), $=, $&),

    Request = "http://" ++ Host ++ "/api.php?" ++ SignedQuery,

    case catch(social_api_utils:http_request(Request)) of
        {ok, {{_HttpVer, 200, _Msg}, _Headers, Body}} ->
            mochijson2:decode(Body);
        {error, Reason} ->
            {error, Reason};
        Unexpected ->
            {error, unexpected_response, Unexpected}
    end;

invoke_method({Group, Function}, Args, #client_options{app_id=AppID, secret_key=SecretKey, viewer_id=ViewerID, host=Host}) ->
    Method   = social_api_utils:concat([{atom_to_list(Group), atom_to_list(Function)}], $., []),
    Required = [{api_id, AppID}, {format, json}, {method, Method}, {v, "3.0"}],

    Arguments     = social_api_utils:merge(Args, Required),
    UnsignedQuery = ViewerID ++ social_api_utils:concat(Arguments, $=, []) ++ SecretKey,
    SignedQuery   = social_api_utils:concat(social_api_utils:merge(Arguments, [{sig, social_api_utils:md5_hex(UnsignedQuery)}]), $=, $&),

    Request = "http://" ++ Host ++ "/api.php?" ++ SignedQuery,

    case catch(social_api_utils:http_request(Request)) of
        {ok, {{_HttpVer, 200, _Msg}, _Headers, Body}} ->
            mochijson2:decode(Body);
        {error, Reason} ->
            {error, Reason};
        Unexpected ->
            {error, unexpected_response, Unexpected}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_payment(_Request, _ServerOptions) ->
    {error, not_implemented}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
