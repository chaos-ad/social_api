-module(social_api_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([set_payment_callback/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_payment_callback(Callback) ->
    gen_server:call(?MODULE, {set_payment_callback, Callback}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    Self = self(),
    Loop =
    fun(Request) ->
        Request:ok( gen_server:call(Self, {payment, Request:parse_qs()}) )
    end,
    {ok, _} = mochiweb_http:start_link
    ([
        {ip,   social_api_settings:server_host()},
        {port, social_api_settings:server_port()},
        {loop, Loop}
    ]),
    Module = social_api_settings:network_mod(),
    {ok, Module:init_server()}.

handle_call({set_payment_callback, Callback}, _, State) ->
    social_api_settings:set_payment_callback(Callback),
    {reply, ok, State};

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
