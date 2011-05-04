-module(api_mymail).

-export
([
    parse_client_options/1,
    parse_server_options/1,
    process_payment/2,
    invoke_method/3
]).

-record(client_options, {app_id, secret_key, host}).
-record(server_options, {app_id, secret_key, callback, mode}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_client_options(Options) ->
    {[AppID,  SecretKey,  Host], _} = utils:parse_options(
     [app_id, secret_key, host], Options),
    {ok, #client_options{app_id=AppID, secret_key=SecretKey, host=Host}}.

parse_server_options(Options) ->
    {[AppID,  SecretKey,  Callback, Mode], _} = utils:parse_options(
     [app_id, secret_key, callback, mode], Options),
    {ok, #server_options{app_id=AppID, secret_key=SecretKey, callback=Callback, mode=Mode}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_payment(Request, #server_options{app_id=AppID, secret_key=SecretKey, callback=Callback, mode=Mode}) ->
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
    case social_utils:find("app_id", Args) of
        AppID ->
            Args2 = social_utils:sort(Args),
            Args3 = social_utils:delete("sig", Args2),
            UnsignedQuery = social_utils:concat(Args3, $=, []) ++ SecretKey,
            Signature     = binary_to_list(utils:md5_hex(UnsignedQuery)),
            case social_utils:find("sig", Args2) of
                Signature -> ok;
                _         -> {error, invalid_signature}
            end;
        _ -> {error, invalid_app_id}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_callback(raw, Callback, Args) ->
    utils:call_functor(Callback, [Args]);

invoke_callback(parsed, Callback, Args) ->
    UID = social_utils:find("uid", Args),
    ProductCode = social_utils:find("service_id", Args),
    ProductOption = nil,
    Amount = case social_utils:find("sms_price", Args) of
                nil -> social_utils:find("other_price", Args);
                SmsPrice -> SmsPrice
             end,
    Profit = social_utils:find("profit", Args),
    invoke_callback(raw, Callback, {UID, {ProductCode, ProductOption}, {Amount, Profit}}).

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

invoke_method({_Group, _Method}, _Args, #client_options{app_id=_AppID, secret_key=_SecretKey, host=_Host}) ->
    {error, not_implemented}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
