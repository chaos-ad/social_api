-module(social_api_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    start_http_server(),
    Module = social_api_settings:network_mod(),
    {ok, Module:init_server()}.

handle_call({payment, Args}, _, State) ->
    Module = social_api_settings:network_mod(),
    {Response, NewState} = Module:process_payment(Args, State),
    {reply, Response, NewState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_http_server() ->
    Self = self(),
    mochiweb_http:start_link
    ([
        {ip,   social_api_settings:server_host()},
        {port, social_api_settings:server_port()},
        {loop, fun(Request) -> Request:ok( gen_server:call(Self, {payment, Request:parse_qs()}) ) end}
    ]).