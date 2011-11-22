-module(social_api_client).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([validate_auth/1, invoke_method/2, send_message/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

validate_auth(AuthData) ->
    Module = social_api_settings:network_mod(),
    Module:validate_auth(AuthData).

invoke_method(Method, Args) ->
    gen_server:call(?MODULE, {invoke_method, Method, Args}).

send_message(Message, Users) ->
    UnicodeMessage = unicode:characters_to_binary(Message),
    gen_server:call(?MODULE, {send_message, UnicodeMessage, Users}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
    Module = social_api_settings:network_mod(),
    {ok, Module:init_client()}.

handle_call({invoke_method, Method, Args}, _, State) ->
    Module = social_api_settings:network_mod(),
    {Reply, NewState} = Module:invoke_method(Method, Args, State),
    {reply, Reply, NewState};

handle_call({send_message, Message, Users}, _, State) ->
    Module = social_api_settings:network_mod(),
    {Reply, NewState} = Module:send_message(Message, Users, State),
    {reply, Reply, NewState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
