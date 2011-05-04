-module(api_odnoklassniki).

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
    case social_utils:find("application_key", Args) of
        AppID ->
            Args2 = social_utils:sort(Args),
            Args3 = social_utils:delete("sig", Args2),
            UnsignedQuery = social_utils:concat(Args3, $=, []) ++ SecretKey,
            Signature     = binary_to_list(utils:md5_hex(UnsignedQuery)),
            case social_utils:find("sig", Args) of
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
    ProductCode = social_utils:find("product_code", Args),
    ProductOption = social_utils:find("product_option", Args),
    Amount = social_utils:find("amount", Args),
    Profit = nil,
    invoke_callback(raw, Callback, {UID, {ProductCode, ProductOption}, {Amount, Profit}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_response(Request, ok) ->
    Response = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n"
               "<callbacks_payment_response xmlns=\"http://api.forticom.com/1.0/\">\r\n"
               "true\r\n"
               "</callbacks_payment_response>",
    Request:ok({"application/xml", Response});

send_response(Request, {Code, Msg}) when is_integer(Code), is_list(Msg) ->
    CodeString = integer_to_list(Code),
    Response = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n"
               "<ns2:error_response xmlns:ns2='http://api.forticom.com/1.0/'>\r\n"
               "    <error_code>" ++ CodeString ++ "</error_code>\r\n"
               "    <error_msg>" ++ Msg ++ "</error_msg>\r\n"
               "</ns2:error_response>",
    Request:ok({"application/xml", [{"invocation-error", CodeString}], Response});

send_response(Request, {error, invalid_app_id}) ->
    send_response(Request, {1001, "Payment is invalid and can not be processed"});

send_response(Request, {error, invalid_signature}) ->
    send_response(Request, {104, "Invalid signature"});

send_response(Request, {error, _}) ->
    send_response(Request, {1, "Unknown error"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_method({Group, Function}, Args, #client_options{app_id=AppID, secret_key=SecretKey, host=Host}) ->
    Method        = social_utils:concat([{atom_to_list(Group), atom_to_list(Function)}], $/, []),
    Required      = [{format, "JSON"}, {application_key, AppID}],
    Arguments     = social_utils:merge(Args, Required),
    UnsignedQuery = social_utils:concat(Arguments, $=, []) ++ SecretKey,
    SignedQuery   = social_utils:concat(social_utils:merge(Arguments, [{sig, utils:md5_hex(UnsignedQuery)}]), $=, $&),

    Request = "http://" ++ Host ++ "/api/" ++ Method ++ "?" ++ SignedQuery,

    case catch(social_utils:http_request(Request)) of
        {ok, {{_HttpVer, 200, _Msg}, _Headers, Body}} ->
            mochijson2:decode(Body);
        {error, Reason} ->
            {error, Reason};
        Unexpected ->
            {error, unexpected_response, Unexpected}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
