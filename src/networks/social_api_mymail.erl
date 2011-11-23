-module(social_api_mymail).

-export
([
    init_client/0,
    init_server/0,
    validate_auth/1,
    invoke_method/3,
    send_message/3,
    process_payment/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_client() -> undefined.
init_server() -> undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate_auth({_, UserData, Signature}) ->
    SecretKey = social_api_settings:secret_key(),
    Data = social_api_utils:concat([UserData, SecretKey]),
    case social_api_utils:md5_hex(Data, bin) of
        Signature -> ok;
        _         -> {error, invalid_signature}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

invoke_method({Group, Function}, Args, State) ->

    AppID         = social_api_settings:app_id(),
    SecretKey     = social_api_settings:secret_key(),

    Method        = social_api_utils:concat([Group, Function], $.),
    Required      = [{format, "json"}, {secure, 1}, {method, Method}, {app_id, AppID}],
    Arguments     = social_api_utils:merge(Args, Required),
    UnsignedQuery = social_api_utils:concat(Arguments, $=, []) ++ SecretKey,
    SignedQuery   = mochiweb_util:urlencode(social_api_utils:merge(Arguments, [{sig, social_api_utils:md5_hex(UnsignedQuery)}])),

    Request = "http://www.appsmail.ru/platform/api" ++ "?" ++ SignedQuery,

    case catch(httpc:request(Request)) of
        {ok, {_, _, Body}} ->
            {mochijson2:decode(Body), State};
        {error, Reason} ->
            {{error, Reason}, State};
        Unexpected ->
            {{error, {unexpected_response, Unexpected}}, State}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_message(Message, Users, State) ->
    Fun =
    fun(UserList, {Acc, TmpState}) ->
        {Reply, NewState} = do_send(Message, UserList, TmpState),
        {[Reply|Acc], NewState}
    end,
    {Result, NewState} = lists:foldl(Fun, {[], State}, social_api_utils:split(200, Users)),
    {lists:concat(lists:reverse(Result)), NewState}.

do_send(Message, Users, State) ->
    Method = {notifications, send},
    Args   = [{uids, social_api_utils:concat(Users, $,)}, {text, Message}],
    {Result, NewState} = invoke_method(Method, Args, State),
    {parse_response(Users, Result), NewState}.

parse_response(Users, {struct, [{<<"error">>, {struct, ErrorInfo}}]}) ->
    Code = proplists:get_value(<<"error_code">>, ErrorInfo),
    Message = proplists:get_value(<<"error_msg">>, ErrorInfo),
    lists:zip(Users, lists:duplicate(length(Users), {error, {Code, Message}}));

parse_response(Users, Result) when is_list(Result) ->
    {Delivered, Undelivered} = social_api_utils:split_delivered(Users, Result),
    lists:zip(Delivered, lists:duplicate(length(Delivered), ok)) ++
    lists:zip(Undelivered, lists:duplicate(length(Undelivered), {error, undelivered})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_payment(Args, State) ->
    AppID       = social_api_settings:app_id(),
    SecretKey   = social_api_settings:secret_key(),
    Callback    = social_api_settings:payment_callback(),
    Mode        = social_api_settings:server_mode(parsed),
    Response =
    case validate_keys(AppID, SecretKey, Args) of
        ok ->
            case invoke_callback(Mode, Callback, Args) of
                ok                              -> response(ok);
                {error, Err} when is_atom(Err)  -> response({error, Err});
                _                               -> response({error, invalid_response})
            end;
        {error, Err} when is_atom(Err)          -> response({error, Err});
        _                                       -> response({error, invalid_response})
    end,
    {Response, State}.

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

response({Status, 0}) when is_integer(Status) ->
    Success = "{\r\n  \"status\" : \"~p\"\r\n}\r\n",
    Response = lists:flatten( io_lib:format(Success, [Status]) ),
    {"application/json", Response};

response({Status, Code}) when is_integer(Status), is_integer(Code) ->
    Error = "{\r\n  \"status\" : \"~p\",\r\n  \"error_code\" : \"~p\"\r\n}\r\n",
    Response = lists:flatten( io_lib:format(Error,   [Status, Code]) ),
    {"application/json", Response};

response(ok) ->
    response({1, 0});

response({error, invalid_app_id}) ->
    response({2, 700});

response({error, invalid_signature}) ->
    response({2, 700});

response({error, retry}) ->
    response({0, 701});

response({error, _}) ->
    response({2, 700}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
