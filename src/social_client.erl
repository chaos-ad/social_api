-module(social_client).

-include_lib("logger.hrl").

-export
([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-export
([
    start_link/1,
    start_link/2,
    stop/1,
    stop/2,
    call/3,
    test/0
]).

-record(state, {module, data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Options)        -> gen_server:start_link( ?MODULE, Options, [] ).
start_link(Name, Options)  -> gen_server:start_link( Name, ?MODULE, Options, [] ).
stop(Pid)                  -> stop(Pid, shutdown).
stop(Pid, Reason)          -> gen_server:call(Pid, {shutdown, Reason}, infinity).

call(Pid, Method, Args)    -> gen_server:call(Pid, {call, Method, Args}).

init(Options) ->
    {[Network], OtherOptions} = utils:parse_options([network], Options),
    Module = social_utils:get_network_module(Network),
    {ok, Data} = Module:parse_client_options(OtherOptions),
    {ok, #state{module=Module, data=Data}}.

handle_call({call, Method, Args}, From, State=#state{module=Module, data=Data}) ->
    spawn( fun() -> gen_server:reply(From, Module:invoke_method(Method, Args, Data)) end ),
    {noreply, State};

handle_call({shutdown, Reason}, _From, State) ->
    {stop, Reason, ok, State};

handle_call(Msg, _From, State) ->
    ?LOG_ERROR(": unexpected call received: ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?LOG_ERROR(": unexpected cast received: ~p", [Msg]),
    {noreply, State}.

handle_info( {'EXIT', _Pid, _Msg}, State ) ->
    ?LOG_INFO(": exit signal received from ~p: ~p", [_Pid, _Msg]),
    {noreply, State};

handle_info( Msg, State ) ->
    ?LOG_ERROR(": unexpected info received: ~p", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, _State) ->
    ?LOG_DEBUG(": terminated with reason ~p", [Reason]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

test() ->
    VkOptions = [
        {network,           vkontakte},
        {app_id,            "AAAAAAA"},
        {secret_key,        "BBBBBBBBBBBBBBBBBBBB"},
        {host,              "api.vkontakte.ru"} ],

    OkOptions = [
        {network,           odnoklassniki},
        {app_id,            "AAAAAAAAAAAAAAAAA"},
        {secret_key,        "BBBBBBBBBBBBBBBBBBBBBBBB"},
        {host,              "api-sandbox.odnoklassniki.ru:8088"} ],

    test(VkOptions),
    test(OkOptions).


test(Options) ->
    OldVal = process_flag(trap_exit, true),
    R = case ?MODULE:start_link(Options) of
        {ok, Pid} ->

            {network, Network} = proplists:lookup(network, Options),
            ok = test_operations(Network, Pid),

            ?assertEqual(ok,   ?MODULE:stop(Pid)),
            receive {'EXIT', Pid, shutdown} -> ok;
                    {'EXIT', Pid, _}        -> {error, invalid_shutdown_reason}
            after 5000 ->
                {error, shutdown_timeout}
            end;
        Err ->
            Err
    end,
    process_flag(trap_exit, OldVal),
    ?LOG_INFO(": testing ~p : ~p", [?MODULE, R]), R.

test_operation(Pid, Method, Args) ->
    ?LOG_INFO(": invoking ~p with args ~p...", [Method, Args]),
    Result = ?MODULE:call(Pid, Method, Args),
    ?LOG_INFO(": invoking ~p: result: ~p", [Method, Result]),
    ok.

test_operations(vkontakte, Pid) ->
    ok = test_operation(Pid, {secure, getBalance}, [{uid,1111111}]);

test_operations(odnoklassniki, Pid) ->
    Uids = [ get_uid(Pid, N) || N <- lists:seq(1, 19) ],
    ?LOG_TRACE(": Users: ~p", [Uids]).

get_uid(Pid, N) ->
    Login = "test_user_" ++ integer_to_list(N),
    Passwd = Login ++ "_pwd",
    case ?MODULE:call(Pid, {auth, login}, [{user_name, Login}, {password, Passwd}]) of
        {struct, [{<<"uid">>, UID}, _, _, _, _, _]} -> binary_to_list(UID);
        Another -> {error, response_parsing_failed, Another}
    end.

