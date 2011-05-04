-module(social_utils).

-export
([
    merge/2,
    sort/1,
    delete/2,
    find/2,
    concat/3,
    concat/4,
    http_request/1,
    get_network_module/1
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

concat(Args, RowSeparator, ColSeparator) ->
    lists:flatten(lists:reverse(concat(Args, RowSeparator, ColSeparator, []))).

concat([{Key, Value}], RowSeparator, _, Result) ->
    S = [utils:to_list(Key), RowSeparator, utils:to_list(Value)],
    [S|Result];

concat([{Key, Value}|Tail], RowSeparator, ColSeparator, Result) ->
    S = [utils:to_list(Key), RowSeparator, utils:to_list(Value), ColSeparator],
    concat(Tail, RowSeparator, ColSeparator, [S|Result]).

http_request(Request) ->
    case code:ensure_loaded(httpc) of
        {error, nofile} -> http:request(Request);
        {module, httpc} -> httpc:request(Request)
    end.

get_network_module(Network) when is_atom(Network) ->
    list_to_atom( "api_" ++ atom_to_list(Network) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%