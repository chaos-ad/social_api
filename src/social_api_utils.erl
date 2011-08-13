-module(social_api_utils).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge(List1, List2) ->
    lists:keymerge(1, sort(List1), sort(List2)).

sort(List) ->
    lists:keysort(1, List).

delete(Key, List) ->
    lists:keydelete(Key, 1, List).

find(Key, List) ->
    proplists:get_value(Key, List, nil).

find(Key, List, integer) ->
    case find(Key, List) of
        nil -> nil;
        Res -> list_to_integer(Res)
    end.

concat(Args, Separator) ->
    lists:flatten(lists:reverse(concat_vals(Args, Separator, []))).

concat(Args, RowSeparator, ColSeparator) ->
    lists:flatten(lists:reverse(concat_pairs(Args, RowSeparator, ColSeparator, []))).

concat_vals([], _, Result) ->
    Result;
concat_vals([Value], _, Result) ->
    [to_list(Value)|Result];
concat_vals([Value|Tail], Separator, Result) ->
    S = [to_list(Value), Separator],
    concat_vals(Tail, Separator, [S|Result]).

concat_pairs([], _, _, Result) ->
    Result;
concat_pairs([{Key, Value}], RowSeparator, _, Result) ->
    S = [social_api_utils:to_list(Key), RowSeparator, social_api_utils:to_list(Value)],
    [S|Result];
concat_pairs([{Key, Value}|Tail], RowSeparator, ColSeparator, Result) ->
    S = [social_api_utils:to_list(Key), RowSeparator, social_api_utils:to_list(Value), ColSeparator],
    concat_pairs(Tail, RowSeparator, ColSeparator, [S|Result]).

get_network_module(Network) when is_atom(Network) ->
    list_to_atom( "social_api_" ++ atom_to_list(Network) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timestamp() ->
    now_to_seconds(erlang:now()).

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.

call_functor({M, F, A}, Args) ->
    erlang:apply(M, F, Args ++ A);
call_functor({M, F}, Args) ->
    erlang:apply(M, F, Args);
call_functor(Functor, Args) ->
    erlang:apply(Functor, Args).

to_binary(X) when is_binary(X)  -> X;
to_binary(X) -> list_to_binary(to_list(X)).

to_list(A) when is_list(A)      -> A;
to_list(A) when is_atom(A)      -> atom_to_list(A);
to_list(A) when is_integer(A)   -> integer_to_list(A);
to_list(A) when is_float(A)     -> float_to_list(A);
to_list(A) when is_binary(A)    -> binary_to_list(A).

md5_hex(Data) ->
    md5_hex(Data, bin).

md5_hex(Data, bin) ->
    bin_to_hex(erlang:md5(Data));

md5_hex(Data, list) ->
    list_to_hex(binary_to_list(erlang:md5(Data))).

bin_to_hex(B) when is_binary(B) ->
    list_to_binary(list_to_hex(binary_to_list(B))).

list_to_hex(L) when is_list(L) ->
    lists:flatten(lists:map(fun(X) -> int_to_hex(X) end, L)).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 -> $0+N;
hex(N) when N >= 10, N < 16 -> $a + (N-10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

test() ->
    ?assertEqual(social_api_utils:concat([], $-), []),
    ?assertEqual(social_api_utils:concat([a], $-), "a"),
    ?assertEqual(social_api_utils:concat([a, b], $-), "a-b"),
    ?assertEqual(social_api_utils:concat([a, b, c], $-), "a-b-c"),
    ?assertEqual(social_api_utils:concat([{a, x}, {b,y},{c,z}], $=, $;), "a=x;b=y;c=z"),
    ?assertEqual(md5_hex("Hex is not working?", list),   "a574ec8a309cc5b1512599ec738aaf0a"),
    ?assertEqual(md5_hex("Hello!"),                    <<"952d2c56d0485958336747bcdd98590d">>),
    ?assertEqual(md5_hex("This is a hash, baby!"),     <<"873c569a197a722942ed7d38361a6bdd">>),
    ok.