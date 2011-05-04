-module(utils).

-export
([
    parse_options/2,
    call_functor/2,
    timestamp/0,
    md5_hex/1,
    to_list/1,
    to_binary/1,
    bin_to_hex/1,
    list_to_hex/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_options([Key | Tail], Options) ->
    {Value, Options1} = case proplists:lookup(Key, Options) of
                {Key, Value1} ->
                    {Value1, proplists:delete(Key, Options)};
                none ->
                    {none, Options}
            end,
    {Values, Options2} = parse_options(Tail, Options1),
    {[Value | Values], Options2};

parse_options([], Options) ->
    {[], Options}.

call_functor({M, F, A}, Args) ->
    erlang:apply(M, F, Args ++ A);
call_functor({M, F}, Args) ->
    erlang:apply(M, F, Args);
call_functor(Functor, Args) ->
    erlang:apply(Functor, Args).

timestamp() ->
    now_to_seconds(erlang:now()).

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.

to_binary(X) when is_binary(X)  -> X;
to_binary(X) when is_list(X)    -> list_to_binary(X);
to_binary(X) when is_atom(X)    -> list_to_binary(atom_to_list(X));
to_binary(X) when is_float(X)   -> list_to_binary(float_to_list(X));
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X)).

to_list(A) when is_list(A)      -> A;
to_list(A) when is_atom(A)      -> atom_to_list(A);
to_list(A) when is_integer(A)   -> integer_to_list(A);
to_list(A) when is_float(A)     -> integer_to_list(round(A));
to_list(A) when is_binary(A)    -> binary_to_list(A).

md5_hex(Data) ->
    utils:bin_to_hex(erlang:md5(Data)).

bin_to_hex(B) when is_binary(B) ->
    list_to_binary(list_to_hex(binary_to_list(B))).

list_to_hex(L) when is_list(L) ->
    lists:flatten(lists:map(fun(X) -> int_to_hex(X) end, L)).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 -> $0+N;
hex(N) when N >= 10, N < 16 -> $a + (N-10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
