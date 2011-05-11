-module(social_utils).

-export
([
    merge/2,
    sort/1,
    delete/2,
    find/2,
    concat/2,
    concat/3,
    http_request/1,
    get_network_module/1,
    test/0
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge(List1, List2) ->
    lists:keymerge(1, sort(List1), sort(List2)).

sort(List) ->
    lists:keysort(1, List).

delete(Key, List) ->
    lists:keydelete(Key, 1, List).

find(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} -> Value;
        false        -> nil
    end.

concat(Args, Separator) ->
    lists:flatten(lists:reverse(concat_vals(Args, Separator, []))).

concat(Args, RowSeparator, ColSeparator) ->
    lists:flatten(lists:reverse(concat_pairs(Args, RowSeparator, ColSeparator, []))).

concat_vals([], _, Result) ->
    Result;
concat_vals([Value], _, Result) ->
    [utils:to_list(Value)|Result];
concat_vals([Value|Tail], Separator, Result) ->
    S = [utils:to_list(Value), Separator],
    concat_vals(Tail, Separator, [S|Result]).

concat_pairs([], _, _, Result) ->
    Result;
concat_pairs([{Key, Value}], RowSeparator, _, Result) ->
    S = [utils:to_list(Key), RowSeparator, utils:to_list(Value)],
    [S|Result];
concat_pairs([{Key, Value}|Tail], RowSeparator, ColSeparator, Result) ->
    S = [utils:to_list(Key), RowSeparator, utils:to_list(Value), ColSeparator],
    concat_pairs(Tail, RowSeparator, ColSeparator, [S|Result]).

http_request(Request) ->
    case code:ensure_loaded(httpc) of
        {error, nofile} -> http:request(Request);
        {module, httpc} -> httpc:request(Request)
    end.

get_network_module(Network) when is_atom(Network) ->
    list_to_atom( "api_" ++ atom_to_list(Network) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

test() ->
    ?assertEqual(social_utils:concat([], $-), []),
    ?assertEqual(social_utils:concat([a], $-), "a"),
    ?assertEqual(social_utils:concat([a, b], $-), "a-b"),
    ?assertEqual(social_utils:concat([a, b, c], $-), "a-b-c"),
    ?assertEqual(social_utils:concat([{a, x}, {b,y},{c,z}], $=, $;), "a=x;b=y;c=z"),
    ok.