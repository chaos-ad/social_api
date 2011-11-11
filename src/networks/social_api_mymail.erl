-module(social_api_mymail).

-export
([
    parse_client_options/1,
    parse_server_options/1,
    validate_auth/2,
    invoke_method/3,
    process_payment/3
]).

-record(client_options, {app_id, secret_key}).
-record(server_options, {app_id, secret_key, mode}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_client_options(Options) ->
    {ok, #client_options{app_id     = proplists:get_value(app_id,     Options),
                         secret_key = proplists:get_value(secret_key, Options)}}.

parse_server_options(Options) ->
    {ok, #server_options{app_id     = proplists:get_value(app_id,     Options),
                         secret_key = proplists:get_value(secret_key, Options),
                         mode       = proplists:get_value(mode,       Options, parsed)}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_auth({_, UserData, Signature}, #client_options{secret_key=SecretKey}) ->
    Data = social_api_utils:concat([UserData, SecretKey]),
    case social_api_utils:md5_hex(Data, bin) of
        Signature -> ok;
        _         -> {error, invalid_signature}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_method({Group, Function}, Args, #client_options{app_id=AppID, secret_key=SecretKey}) ->
    Method        = social_api_utils:concat([{atom_to_list(Group), atom_to_list(Function)}], $., []),
    Required      = [{format, "json"}, {secure, 1}, {method, Method}, {app_id, AppID}],
    Arguments     = social_api_utils:merge(Args, Required),
    UnsignedQuery = social_api_utils:concat(Arguments, $=, []) ++ SecretKey,
    SignedQuery   = mochiweb_util:urlencode(social_api_utils:merge(Arguments, [{sig, social_api_utils:md5_hex(UnsignedQuery)}])),

    Request = "http://www.appsmail.ru/platform/api" ++ "?" ++ SignedQuery,

    case catch(httpc:request(Request)) of
        {ok, {_, _, Body}} ->
            mochijson2:decode(Body);
        {error, Reason} ->
            {error, Reason};
        Unexpected ->
            {error, unexpected_response, Unexpected}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_payment(Request, Callback, #server_options{app_id=AppID, secret_key=SecretKey, mode=Mode}) ->
    Args = Request:parse_qs(),
    case validate_keys(AppID, SecretKey, Args) of
        ok ->
            case invoke_callback(Mode, Callback, Args) of
                ok                              -> send_response(Request, ok);
                {error, Err} when is_atom(Err)  -> send_response(Request, {error, Err});
                _                               -> send_response(Request, {error, invalid_response})
            end;
        {error, Err} when is_atom(Err)          -> send_response(Request, {error, Err});
        _                                       -> send_response(Request, {error, invalid_response})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_keys(AppID, SecretKey, Args) ->
    case social_api_utils:find("app_id", Args) of
        AppID ->
            Args2 = social_api_utils:sort(Args),
            Args3 = social_api_utils:delete("sig", Args2),
            UnsignedQuery = social_api_utils:concat(Args3, $=, []) ++ SecretKey,
            Signature     = social_api_utils:md5_hex(UnsignedQuery, list),
            case social_api_utils:find("sig", Args2) of
                Signature -> ok;
                _         -> {error, invalid_signature}
            end;
        _ -> {error, invalid_app_id}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_callback(raw, Callback, Args) ->
    social_api_utils:call_functor(Callback, [Args]);

invoke_callback(parsed, Callback, Args) ->

    TransactionID   = social_api_utils:find("transaction_id", Args),
    UID             = social_api_utils:find("uid",            Args),
    ProductCode     = social_api_utils:find("service_id",     Args),
    ProductOption   = nil,
    Amount          = case social_api_utils:find("sms_price", Args, integer) of
                          nil      -> social_api_utils:find("other_price", Args, integer);
                          SmsPrice -> SmsPrice
                      end,
    Profit          = social_api_utils:find("profit", Args, integer),
    invoke_callback(raw, Callback, {{TransactionID, UID}, {ProductCode, ProductOption}, {Amount, Profit}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_response(Request, {Status, 0}) when is_integer(Status) ->
    Success = "{\r\n  \"status\" : \"~p\"\r\n}\r\n",
    Response = lists:flatten( io_lib:format(Success, [Status]) ),
    Request:ok({"application/json", Response});

send_response(Request, {Status, Code}) when is_integer(Status), is_integer(Code) ->
    Error = "{\r\n  \"status\" : \"~p\",\r\n  \"error_code\" : \"~p\"\r\n}\r\n",
    Response = lists:flatten( io_lib:format(Error,   [Status, Code]) ),
    Request:ok({"application/json", Response});

send_response(Request, ok) ->
    send_response(Request, {1, 0});

send_response(Request, {error, invalid_app_id}) ->
    send_response(Request, {2, 700});

send_response(Request, {error, invalid_signature}) ->
    send_response(Request, {2, 700});

send_response(Request, {error, retry}) ->
    send_response(Request, {0, 701});

send_response(Request, {error, _}) ->
    send_response(Request, {2, 700}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
