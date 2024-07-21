%%----------------------------------------------------
%% $Id: util.erl 13256 2014-06-18 05:40:49Z rolong $
%% @doc 工具包
%% @author Rolong<rolong@vip.qq.com>
%% @end
%%----------------------------------------------------

-module(util).
-export([
        parse_qs/1
        ,for/3
        ,for/4
        ,floor/1
        ,ceil/1
        ,unixtime/0
        ,unixtime/1
        ,sleep/1
        ,print/1
        ,rand/1
        ,rand/2
        ,rand_float1/2
        ,rand_float2/2
        ,rand_element/1
        ,rand_element/2
        ,rand_element1/1
        ,rand_element1/2
        ,find_repeat_element/1
        ,find_repeat_key_element/2
        ,find_all_repeat_key_element/3
        ,store_element/2
        ,rate/1
        ,rate1000/1
        ,test/1
        ,test/0
        ,get_time_list/1
        ,get_day_list/1
        ,term_to_string/1
        ,bitstring_to_term/1
        ,string_to_term/1
        ,term_to_bitstring/1
        ,string_to_num/1
        ,info/1
        ,info/2
        ,info/4
        ,debug/1
        ,debug/2
        ,debug/4
        ,test_time_limit/2
        ,md5/1
        ,ip_to_binary/1
        ,test1/0
        ,test2/1
        ,print_self/0
        ,get_val/2
        ,get_val/3
        ,get_val1/2
        ,get_val1/3
        ,set_val/3
        ,in_time/2
        ,mktime/1
        ,get_bool_sign/2
        ,set_bool_sign/3
        ,print_bit/1
        ,fix_max/2
        ,get_range_data/1
        ,min0/1
        ,shuffle/1
        ,del_repeat_element/1
        ,del_repeat_key_element/2
        ,today_stamp/0
        ,day_stamp/1
        ,weight_extract/1
        ,weight_extract/3
        ,weight_extract2/1
        ,weight_extract2/3
        ,rate_extract/1
        ,list2string/1
        ,list2string/2
        ,list2tuple/1
        ,file2string/1
        ,file2string/2
        ,keyborband/3
        ,quality2string/1
        ,merge_list/1
        ,sort2list/1
        ,qsort/1
        ,random_string/1
        ,random_token/0
        ,rngchars/1
        ,f/1
        ,calc_m/1
    ]
).
-include("common.hrl").

get_range_data(M) ->
    Rand = rand(M:get(range)),
    Ids = M:get(ids),
    case [{S, E} || {S, E} <- Ids, Rand >= S, Rand =< E] of
        [Key] -> M:get(Key);
        _ -> undefined
    end.

fix_max(Num, Max) when Num =< Max -> Num;
fix_max(_Num, Max) -> Max.

min0(A) when A >= 0 -> A;
min0(_) -> 0.

%% 解析 QueryString
parse_qs(Bin) ->
    BL = binary:split(Bin, <<38>>, [global]),
    [list_to_tuple(binary:split(B1, <<61>>)) || B1 <- BL].

%% parse_qs(String) when is_bitstring(String) ->
%%     parse_qs(bitstring_to_list(String));
%%
%% parse_qs(String) ->
%%     parse_qs(String, "&", "=").
%%
%% parse_qs(String, Token1, Token2) when is_bitstring(String) ->
%%     parse_qs(bitstring_to_list(String), Token1, Token2);
%%
%% parse_qs(String, Token1, Token2) ->
%%     [ list_to_tuple(string:tokens(KV, Token2)) || KV <- string:tokens(String, Token1) ].

%%
%% quality2string(1) ->   d;
%% quality2string(2) ->   c;
%% quality2string(3) ->   b;
%% quality2string(4) ->   a;
%% quality2string(5) ->   s;
%% quality2string(6) ->  ss;
%% quality2string(7) -> sss;
%% quality2string(_) -> undefined.

quality2string(1) ->   <<"D">>;
quality2string(2) ->   <<"C">>;
quality2string(3) ->   <<"B">>;
quality2string(4) ->   <<"B+">>;
quality2string(5) ->   <<"A">>;
quality2string(6) ->   <<"A+">>;
quality2string(7) ->   <<"S">>;
quality2string(_) ->   <<>>.

%% @doc 快速排序
-spec qsort(List) -> NewList when
    List    :: list(),
    NewList :: list().
qsort([]) -> [];
qsort([Pivot|T]) ->
    qsort([X || X <- T,X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- T,X >= Pivot]).

%% @doc 快速排序
-spec sort2list(List) -> NewList when
    List    :: list(),
    NewList :: list().
sort2list([]) -> [];
sort2list([Pivot | Rest]) ->
    {Smaller, Bigger} = split2list(Pivot, Rest),
    lists:append(sort2list(Smaller), [Pivot | sort2list(Bigger)]).

split2list(Pivot, L) ->
    split2list(Pivot, L, [], []).
split2list(_Pivot, [], Smaller, Bigger) ->
    {Smaller,Bigger};
split2list(Pivot, [H|T], Smaller, Bigger) when H < Pivot ->
    split2list(Pivot, T, [H|Smaller], Bigger);
split2list(Pivot, [H|T], Smaller, Bigger) when H >= Pivot ->
    split2list(Pivot, T, Smaller, [H|Bigger]).

%% @doc 合并列表[[id,num],..](id相同num相加,返回[{id,num}...])
-spec merge_list(List) -> NewList when
    List :: list(),
    NewList :: list().
merge_list(List) ->
    merge_list(List, []).

merge_list([{Id, Num} | Lists], Rt) ->
    Rt1 = case lists:keyfind(Id, 1, Rt) of
        false -> [{Id, Num} | Rt];
        {_, Num2} ->
            lists:keyreplace(Id, 1, Rt, {Id, Num + Num2})
    end,
    merge_list(Lists, Rt1);
merge_list([[Id, Num] | Lists], Rt) ->
    Rt1 = case lists:keyfind(Id, 1, Rt) of
        false -> [{Id, Num} | Rt];
        {_, Num2} ->
            lists:keyreplace(Id, 1, Rt, {Id, Num + Num2})
    end,
    merge_list(Lists, Rt1);
merge_list([Id | Lists], Rt) ->
    merge_list([{Id, 1} | Lists], Rt);
merge_list([], Rt) ->
    Rt.

%% @doc list to string, "," join
%% list = [num1, num2, num3...]
-spec list2string(L) -> Rt when
    L :: list(),
    Rt :: atom().
list2string(L) ->
    [H | T] = L,
    list2string2(T, ",", integer_to_list(H)).

list2string2([H | T], S, Rt) ->
    Rt1 = Rt ++ S ++ integer_to_list(H),
    list2string2(T, S, Rt1);
list2string2([], _S, Rt) ->
    Rt.

%% @doc list to string, join
%% list = [num1, num2, num3...]
-spec list2string(L, S) -> Rt when
    L :: list(),
    S :: atom(),
    Rt:: atom().
list2string(L, S) ->
    L2 = [integer_to_list(X) || X <- L],
    string:join(L2, S).

%% @doc list to tuple
%% list = [[A..], [B..]..]
-spec list2tuple(List) -> NewList when
    List    :: list(),
    NewList :: list().
list2tuple(List) ->
    list2tuple(List, []).
list2tuple([H | T], Rt) ->
    case is_list(H) of
        true ->
            Rt1 = list_to_tuple(H),
            list2tuple(T, [Rt1 | Rt]);
        false ->
            list2tuple(T, [Rt])
    end;
list2tuple([], Rt) -> Rt.

%% @see file2string/2
file2string(Key) ->
    file2string("data_file_string.txt", Key).

%% @doc file to string 现在不用,改成erl文件了
-spec file2string(File, Key) -> String when
    File    :: string(),
    Key     :: integer(),
    String  :: string().
file2string(File, Key) ->
    case file:open("src/data/" ++ File, read) of
        {ok, IoDevice} ->
            Value = file2read(IoDevice, Key),
            file:close(IoDevice),
            Value;
        {error, Reason} ->
            ?INFO("open file ~w error:~w", [File, Reason]),
            []
    end.
file2read(File, Key) ->
    case io:get_line(File, '') of
        eof -> [];
        {error, Reason} ->
            ?INFO("read file ~w error:~w", [File, Reason]),
            [];
        LineData ->
            [LineKey, LineString] = string:tokens(LineData, "=/n"),
            case list_to_integer(LineKey) == Key of
                true -> LineString;
                false -> file2read(File, Key)
            end
    end.

%% @doc 按位或/与,用来保存多个值状态
-spec keyborband(S,St,Type) -> R when
    S :: 1 | 2,
    St :: integer(),
    Type :: 1 .. 29,
    R :: integer().
keyborband(S,St,Type) ->
    Num = case Type of
        1 -> 1;
        2 -> 2;
        %% 3 -> 4;
        %% 4 -> 8;
        %% 5 -> 16;
        %% 6 -> 32;
        %% 7 -> 64;
        %% 8 -> 128;
        N when N =/=0 andalso N < 30 -> erlang:trunc(math:pow(2, (Type - 1)));
        _ -> 0
    end,
    case S of
        1 ->
            St band Num;
        2 ->
            St bor Num;
        _ -> 0
    end.

%% @doc for循环
%% @equiv for(I, Max, F, State)
%% @see for/4
-spec for(Min, Max, F) -> error | F when
    Min :: integer(),
    Max :: integer(),
    F :: fun().
for(Min, Max, _F) when Min>Max ->
    error;
for(Max, Max, F) ->
    F(Max);
for(I, Max, F)   ->
    F(I),
    for(I+1, Max, F).

%% @doc 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State)   ->
    {ok, NewState} = F(I, State),
    for(I+1, Max, F, NewState).

%% @doc 取小于X的最大整数
-spec floor(X) -> Num when
    X :: integer() | float(),
    Num :: integer().
floor(X) ->
    T = erlang:trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

%% @doc 取大于X的最小整数
-spec ceil(X) -> Num when
    X :: integer() | float(),
    Num :: integer().
ceil(X) ->
    T = erlang:trunc(X),
    case (X > T) of
        true -> T + 1;
        _ -> T
    end.

%% @doc 取得当前的unix时间戳
-spec unixtime() -> Num when
    Num :: integer().
unixtime() ->
    {M, S, _} = erlang:now(),
    M * 1000000 + S.

%% @doc 取得当前unix时间戳，精确到毫秒3位
-spec unixtime(Day) -> Num when
    Day :: micro | today | tomorrow | yesterday | noon,
    Num :: integer() | float().
unixtime(micro) ->
    {M, S, Micro} = erlang:now(),
    M * 1000000 + S + Micro / 1000000;

%% @doc 获取当天0时0分0秒的时间戳（这里是相对于当前时区而言，后面的unixtime调用都基于这个函数
unixtime(today) ->
    {M, S, MS} = now(),
    {_, Time} = calendar:now_to_local_time({M, S, MS}),
    M * 1000000 + S - calendar:time_to_seconds(Time);

%% @doc 获取明天0时0分0秒的时间戳
unixtime(tomorrow) ->
    unixtime(today) + 86400;

%% @doc 获取昨天0时0分0秒的时间戳
unixtime(yesterday) ->
    unixtime(today) - 86400;

%% @doc 获取当天12时0分0秒的时间戳
unixtime(noon) ->
    unixtime(today) + 43200.

%% 暂停执行T毫秒
sleep(T) ->
    receive
    after T ->
            true
    end.

%% @doc 返回今天的日期/月份(20140107/201401)
-spec today_stamp() -> {DayDate, MonDate} when
    DayDate :: integer(),
    MonDate :: integer().
today_stamp() ->
    {{Y, M, D}, _} = mktime({to_date, unixtime(today)}),
    DayDate = Y * 10000 + M * 100 + D,
    MonDate = Y * 100 + M,
    {DayDate, MonDate}.

%% @doc 格式化返回指定日期的日期/月份(20140107/201401)
-spec day_stamp(Timestamp) -> {DayDate, MonDate} when
    Timestamp :: integer(),
    DayDate :: integer(),
    MonDate :: integer().
day_stamp(Timestamp) ->
    {{Y, M, D}, _} = mktime({to_date, Timestamp}),
    DayDate = Y * 10000 + M * 100 + D,
    MonDate = Y * 100 + M,
    {DayDate, MonDate}.

%% 测试用
print(Data) ->
    io:format("~ts~n", [xmerl_ucs:from_utf8(Data)]).

-define(SAFE_CHARS, {$a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m,
                     $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
                     $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M,
                     $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
                     $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $_}).
%% 生成随机字符串
-spec rngchars(Num) -> String when
    Num    :: integer(),
    String :: string().
rngchars(0) ->
    "";
rngchars(N) ->
    [rngchar() | rngchars(N - 1)].

rngchar() ->
    rngchar(crypto:rand_uniform(0, tuple_size(?SAFE_CHARS))).

rngchar(C) ->
    element(1 + C, ?SAFE_CHARS).

%% @doc 产生GUID
-spec random_token() -> String when
    String :: string().
random_token() ->
    Term = term_to_binary({node(), make_ref()}),
    Digest = erlang:md5(Term),
    binary_to_hax(Digest).

binary_to_hax(Bin) when is_binary(Bin) ->
    [oct_to_hex(N) || <<N:4>> <= Bin].

oct_to_hex(0) -> $0;
oct_to_hex(1) -> $1;
oct_to_hex(2) -> $2;
oct_to_hex(3) -> $3;
oct_to_hex(4) -> $4;
oct_to_hex(5) -> $5;
oct_to_hex(6) -> $6;
oct_to_hex(7) -> $7;
oct_to_hex(8) -> $8;
oct_to_hex(9) -> $9;
oct_to_hex(10) -> $a;
oct_to_hex(11) -> $b;
oct_to_hex(12) -> $c;
oct_to_hex(13) -> $d;
oct_to_hex(14) -> $e;
oct_to_hex(15) -> $f.

%% -define(STRING, "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").
-define(STRING, "0123456789abcdefghijklmnopqrstuvwxyz").
%% @doc 产生随机字符串
-spec random_string(Len) -> String when
    Len    :: integer(),
    String :: string().
random_string(Len) ->
    random_string2(Len, ?STRING).

random_string2(Len, AllowedChars) ->
    lists:foldl(
        fun(_, Acc) ->
                [lists:nth(random:uniform(length(AllowedChars)), AllowedChars)]
                ++ Acc
        end, [], lists:seq(1, Len)).

%% @doc 产生一个Min到Max之间的随机整数
rand({Min, Max}) when Min > Max ->
    rand(Max, Min);
rand({Min, Max}) -> rand(Min, Max);
rand([Min, Max]) when Min > Max ->
    rand(Max, Min);
rand([Min, Max]) -> rand(Min, Max).

%% @equiv rand({Min, Max})
%% @see rand/1
-spec rand(Min, Max) -> Num when
    Min :: integer(),
    Max :: integer(),
    Num :: integer().
rand(Same, Same) -> Same;
rand(Min, Max) ->
    %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    case get("rand_seed") of
        undefined ->
            RandSeed = myrandomseed:get_seed(),
            random:seed(RandSeed),
            put("rand_seed", RandSeed);
        _ -> skip
    end,
    M = Min - 1,
    random:uniform(Max - M) + M.

%% @equiv rand_float2(Min, Max)
%% @see rand_float2/2
-spec rand_float1(Min, Max) -> Num when
    Min :: integer() | float(),
    Max :: integer() | float(),
    Num :: integer() | float().
rand_float1(Min, Max) when is_float(Min); is_float(Max) ->
    rand(round(Min * 10), round(Max * 10)) / 10;
rand_float1(Min, Max) ->
    rand(Min, Max).

%% @doc 产生一个Min到Max之间的随机浮点数
rand_float2(Min, Max) when is_float(Min); is_float(Max) ->
    rand(round(Min * 100), round(Max * 100)) / 100;
rand_float2(Min, Max) ->
    rand(Min, Max).

%% @equiv rand_element1(List)
%% @see rand_element1/1
-spec rand_element(List) -> Any when
    List :: list(),
    Any :: any().
rand_element([]) -> undefined;
rand_element([I]) -> I;
rand_element(List) ->
    Len = length(List),
    Nth = rand(1, Len),
    lists:nth(Nth, List).

%% @doc 在List中随机Num个元素
-spec rand_element(Num, List) -> NewList when
    Num :: integer(),
    List :: list(),
    NewList :: list().
rand_element(Num, List) ->
    rand_element(Num, List, []).

rand_element(0, _List, Reply) -> Reply;
rand_element(Num, List, Reply) ->
    case rand_element(List) of
        undefined -> Reply;
        E ->
            Reply1 = [E | Reply],
            List1 = lists:delete(E, List),
            rand_element(Num - 1, List1, Reply1)
    end.

%% @doc 在List中随机返回一个元素
rand_element1([]) -> undefined;
rand_element1([I]) -> I;
rand_element1(List) ->
    Len = length(List),
    Nth = rand(1, Len),
    lists:nth(Nth, List).

%% @doc 在List中随机Num个不重复元素
-spec rand_element1(Num, List) -> NewList when
    Num :: integer(),
    List :: list(),
    NewList :: list().
rand_element1(Num, List) ->
    rand_element1(Num, List, []).

rand_element1(0, _List, Reply) -> Reply;
rand_element1(Num, List, Reply) ->
    case rand_element1(List) of
        undefined -> Reply;
        E ->
            case lists:member(E, Reply) of
                true -> rand_element1(Num, List, Reply);
                false ->
                    Reply1 = [E | Reply],
                    List1 = lists:delete(E, List),
                    rand_element1(Num - 1, List1, Reply1)
            end
    end.

%% @doc 找出列表中重复的元素
-spec find_repeat_element(List) -> NewList when
    List :: list(),
    NewList :: list().
find_repeat_element([]) -> [];
find_repeat_element(L) ->
    find_repeat_element(L, []).

find_repeat_element([H | T], Reply) ->
    case lists:member(H, T) of
        true -> find_repeat_element(T, [H | Reply]);
        false -> find_repeat_element(T, Reply)
    end;
find_repeat_element([], Reply) -> Reply.

%% @doc 找出字典列表List中第Num个Key相同的元素
-spec find_repeat_key_element(Num, List) -> NewList when
    Num :: integer(),
    List :: list(),
    NewList :: list().
find_repeat_key_element(_Nth, []) -> [];
find_repeat_key_element(Nth, L) ->
    find_repeat_key_element(Nth, L, []).

find_repeat_key_element(Nth, [H | T], Reply) ->
    H1 = if
        is_tuple(H) -> tuple_to_list(H);
        true -> H
    end,
    Key = lists:nth(Nth, H1),
    case lists:keymember(Key, Nth, T) of
        true -> find_repeat_key_element(Nth, T, [H | Reply]);
        false ->
            case lists:keymember(Key, Nth, Reply) of
                true -> find_repeat_key_element(Nth, T, [H | Reply]);
                false -> find_repeat_key_element(Nth, T, Reply)
            end
    end;
find_repeat_key_element(_Nth, [], Reply) -> Reply.

%% @doc 找出字典列表List中第Num个指定Key相同的所有元素
-spec find_all_repeat_key_element(Key, Num, List) -> NewList when
    Key :: term(),
    Num :: integer(),
    List :: list(),
    NewList :: list().
find_all_repeat_key_element(_Key, _Nth, []) -> [];
find_all_repeat_key_element(Key, Nth, L) ->
    find_all_repeat_key_element(Key, Nth, L, []).

find_all_repeat_key_element(Key, Nth, [H | T], Reply) ->
    case lists:keyfind(Key, Nth, [H]) of
        false -> find_all_repeat_key_element(Key, Nth, T, Reply);
        _ -> find_all_repeat_key_element(Key, Nth, T, [H | Reply])
    end;
find_all_repeat_key_element(_Key, _Nth, [], Reply) -> Reply.

%% @doc 去除重复元素
-spec del_repeat_element(List) -> NewList when
    List :: list(),
    NewList :: list().
del_repeat_element([]) -> [];
del_repeat_element(L) ->
    del_repeat_element(L, []).

del_repeat_element([H | T], Reply) ->
    case lists:member(H, Reply) of
        true -> del_repeat_element(T, Reply);
        false -> del_repeat_element(T, [H | Reply])
    end;
del_repeat_element([], Reply) -> Reply.

%% @doc 去除字典列表中重复key元素
del_repeat_key_element(Nth, List) ->
    E = find_repeat_key_element(Nth, List),
    List -- E.

%% @doc 概率
-spec rate(Rate) -> true | false when
    Rate :: float() | integer().
rate(Rate) ->
    R = Rate * 100,
    case rand(1, 10000) of
        N when N =< R -> true;
        _ -> false
    end.

%% @doc 千分率
-spec rate1000(Rate) -> true | false when
    Rate :: float() | integer().
rate1000(Rate) ->
    R = Rate * 100,
    case rand(1, 100000) of
        N when N =< R -> true;
        _ -> false
    end.

%% @doc 物品权重值[{Id, Num, Weight},{...}]
%% Id=物品类型/物品Tid
%% Num=数量/{}
%% Weight=权重
-spec weight_extract(L) -> [{Id, Num}] | [] when
    L :: list(),
    Id :: integer(),
    Num :: integer() | tuple().
weight_extract([]) -> [];
weight_extract(L = [{_I, _N, _W} | _T]) ->
    Top = lists:foldl(fun({_, _, Sum}, X) -> X + Sum end, 0, L),
    Rand = rand(1, Top),
    L2 = lists:reverse(L),
    weight_extract(L2, Top, Rand).
weight_extract([{Id, Num, Weight} | T], Top, Rand) ->
    case Rand >= (Top - Weight) of
        true -> [{Id, Num}];
        false -> weight_extract(T, Top - Weight, Rand)
    end.

%% @doc 权重返回[{...,Weight} | T]
-spec weight_extract2(L) -> List | [] when
    L :: list(),
    List :: list().
weight_extract2([]) -> [];
weight_extract2(L) ->
    Top = lists:foldl(fun(S, X) -> X + lists:last(tuple_to_list(S)) end, 0, L),
    Rand = rand(1, Top),
    L2 = lists:reverse(L),
    weight_extract2(L2, Top, Rand).
weight_extract2([H | T], Top, Rand) ->
    W = lists:last(tuple_to_list(H)),
    case Rand >= (Top - W) of
        true -> [H];
        false -> weight_extract2(T, Top - W, Rand)
    end.

%% @doc 概率返回[{...,Rate} | T]
-spec rate_extract(L) -> List | [] when
    L :: list(),
    List :: list().
rate_extract([]) -> [];
rate_extract([H | T]) ->
    Rate = lists:last(tuple_to_list(H)),
    case util:rate(Rate) of
        true -> [H];
        false ->
            rate_extract(T)
    end.

test() ->
    ?INFO("test ...:~w", [test]),
    node().

test(T) ->
    ?INFO("test ...:~w", [T]),
    node().

%% @equiv get_time_list(now)
%% @see get_time_list/1
-spec get_day_list(T) -> List when
    T :: now,
    List :: list().
get_day_list(now) ->
    {{Y,Mo,D}, _} = erlang:localtime(),
    [t(Y), t(Mo), t(D)].

%% @doc 列表形式返回当前日期or时间
get_time_list(now) ->
    {{Y,Mo,D}, {H,Mi,S}} = erlang:localtime(),
    [t(Y), t(Mo), t(D), t(H), t(Mi), t(S)].

%% @doc 使用通用参数，转换list，int list [171,167,...] 不会保存成 string
-spec term_to_string(Term) -> Bit when
    Term :: term(),
    Bit :: bitstring().
term_to_string(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
    string_to_term(binary_to_list(BitString)).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~p", [Term])).

%% @doc 字符转数字，整数或者浮点数
-spec string_to_num(String) -> Num when
    String :: atom(),
    Num :: integer() | float().
string_to_num(String) ->
    case lists:member($., String) of
        true  -> list_to_float(String);
        false -> list_to_integer(String)
    end.

t(X) when is_integer(X) ->
    t1(integer_to_list(X));
t(_) -> "".

t1([X]) -> [$0,X];
t1(X)   -> X.

%% 普通信息
info(Msg) ->
    info(Msg, []).
info(Format, Args) ->
    info(Format, Args, null, null).
%% info(_Format, _Args, _Mod, _Line) -> ok.
info(Format, Args, Mod, Line) ->
    Msg = format("info", Format, Args, Mod, Line),
    io:format("~ts", [Msg]).

%% 调试信息
debug(Msg) ->
    debug(Msg, []).
debug(Format, Args) ->
    debug(Format, Args, null, null).
%%debug(_Format, _Args, _Mod, _Line) -> null.
debug(Format, Args, Mod, Line) ->
    case env:get(debug) =:= on orelse get('$mydebug') =:= true of
        true ->
            Msg = format("debug", Format, Args, Mod, Line),
            io:format("~ts", [Msg]);
        false -> ok
    end.

%% 格式化打印信息
%% T = "error" | "info" | "debug" 类型
%% F = list() 格式
%% A = list() 参数
%% Mod = list() 模块名
%% Line = int() 所在行
format(T, F, A, Mod, Line) ->
    {{Y, M, D}, {H, I, S}} = erlang:localtime(),
    Date = lists:concat([Y, "/", M, "/", D, " ", H, ":", I, ":", S]),
    case Line of
        null -> erlang:iolist_to_binary(io_lib:format(lists:concat(["# ", T, " ~s ", F, "~n"]), [Date] ++ A));
        _ -> erlang:iolist_to_binary(io_lib:format(lists:concat(["# ", T, " ~s ~w[~w:~w] ", F, "~n"]), [Date, self(), Mod, Line] ++ A))
    end.

%% 最小调用时间测试
test_time_limit(Key, MinTime) ->
    Now = erlang:now(),
    case get(Key) of
        undefined -> put(Key, Now);
        T->
            Td = timer:now_diff(Now, T) / 1000,
            case Td < MinTime of
                true ->
                    ?INFO("Invoked too fast! [Key:~w, Time:~wms]", [Key, Td]);
                false -> ok
            end,
            put(Key, Now)
    end.

test1() ->
    put('$test_time', erlang:now()).

test2(Msg) ->
    case get('$test_time') of
        undefined -> ?INFO("undefined '$test_time'");
        T1 ->
            Td = timer:now_diff(erlang:now(), T1) / 1000,
            ?INFO("[Run Time]~s: ~w ms", [Msg, Td])
    end.

%% 转换成HEX格式的md5
md5(S) ->
    bin_to_hex(erlang:md5(S)).

bin_to_hex(B) ->
    << <<(hex(X bsr 4)), (hex(X band 15))>> || <<X>> <= B >>.

hex(N) when N < 10->
	48 + N;
hex(N) ->
	87 + N.

ip_to_binary(Ip) when is_binary(Ip) -> Ip;
ip_to_binary({Ip1, Ip2, Ip3, Ip4}) ->
    Ip0 = lists:concat([
            integer_to_list(Ip1), ".",
            integer_to_list(Ip2), ".",
            integer_to_list(Ip3), ".",
            integer_to_list(Ip4)
        ]),
    list_to_binary(Ip0).

print_self() ->
    P = self(),
    Data = {
        P
        ,erlang:process_info(P, memory)
        ,erlang:process_info(P, message_queue_len)
    },
    io:format("~n~w", [Data]).

get_val(Key, L) ->
    get_val(Key, L, undefined).

get_val(Key, L, Default) when is_list(L) ->
    case lists:keyfind(Key, 1, L) of
        false -> Default;
        {_, V} -> V
    end;
get_val(_Key, _L, Default) -> Default.

get_val1(Key, L) ->
    get_val1(Key, L, undefined).

get_val1(Key, L, Default) when is_list(L) ->
    case lists:keyfind(Key, 1, L) of
        false -> Default;
        {_, [V1, V2]} -> rand(V1, V2);
        {_, V} -> V
    end;
get_val1(_Key, _L, Default) -> Default.

set_val(Key, Val, L) ->
    lists:keystore(Key, 1, L, {Key, Val}).

%% @doc 指定格式返回日期：{{Y, M, D},{H, I, S}}
-spec mktime({to_date, UnixSec}) -> Time when
    UnixSec :: integer(),
    Time :: tuple().
mktime({to_date, UnixSec})->
    DT = calendar:gregorian_seconds_to_datetime(UnixSec + calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
     erlang:universaltime_to_localtime(DT);

%% @doc 生成一个指定日期的unix时间戳(无时区问题)
%% Date = date() = {Y, M, D}
%% Time = time() = {H, I, S}
%% 参数必须大于1970年1月1日
mktime({Date, Time}) ->
    DT = erlang:localtime_to_universaltime({Date, Time}),
    calendar:datetime_to_gregorian_seconds(DT) - calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).

%% @doc 判断现在是否在某年某月某日某时间段内
-spec in_time(Start, End) -> ture | false when
    Start :: tuple(),
    End   :: tuple().
in_time({SYea, SMon, SDay, SH, SM, SS}, {EYea, EMon, EDay, EH, EM, ES}) ->
    in_time(unixtime(), {SYea, SMon, SDay, SH, SM, SS}, {EYea, EMon, EDay, EH, EM, ES}).

%% @doc 判断某时间是否在某年某月某日某时间段内
-spec in_time(Timestamp, Start, End) -> ture | false when
    Timestamp :: integer(),
    Start :: tuple(),
    End   :: tuple().
in_time(Timestamp, {SYea, SMon, SDay, SH, SM, SS}, {EYea, EMon, EDay, EH, EM, ES}) ->
    Timestamp >= mktime({{SYea, SMon, SDay}, {SH, SM, SS}}) andalso
    Timestamp =< mktime({{EYea, EMon, EDay}, {EH, EM, ES}}).

print_bit(<<V1:1, V2:1, V3:1, V4:1, V5:1, V6:1, V7:1, V8:1, Rest/binary>>) ->
    ?INFO("~w", [{V1, V2, V3, V4, V5, V6, V7, V8}]),
    print_bit(Rest);
print_bit(<<>>) ->
    ok.

%% Pos:
%%     1=领取成长礼包
%% Sign band util:ceil(math:pow(32 - Pos)).
%% Sign bor util:ceil(math:pow(32 - Pos)).
get_bool_sign(Pos, Bin) ->
    BitLen = byte_size(Bin) * 8,
    Len1 = Pos - 1,
    Len2 = BitLen - Pos,
    <<_B1:Len1, B:1, _B2:Len2>> = Bin,
    B.

set_bool_sign(Pos, Val, Bin) when Val == 0; Val == 1 ->
    BitLen = byte_size(Bin) * 8,
    Len1 = Pos - 1,
    Len2 = BitLen - Pos,
    <<B1:Len1, _OldVal:1, B2:Len2>> = Bin,
    <<B1:Len1, Val:1, B2:Len2>>.

%% @doc 打乱List
-spec shuffle(L) -> List when
    L :: list(),
    List :: list().
shuffle(L) ->
    List1 = [{random:uniform(), X} || X <- L],
    List2 = lists:keysort(1, List1),
    [E || {_, E} <- List2].

store_element(E, L) ->
    case lists:member(E, L) of
        true -> L;
        false -> [E | L]
    end.

f(N) ->
    Bin = <<N:32/float>>,
    <<S:1, E:8, M:23>> = Bin,
    io:format("~n"),
    io:format("*---*----------*-------------------------*~n"),
    io:format("* s * eeeeeeee * mmmmmmmmmmmmmmmmmmmmmmm *~n"),
    io:format("*---*----------*-------------------------*~n"),
    io:format("* ~1.2.0B * ~8.2.0B * ~23.2.0B * ~n", [S, E, M]),
    io:format("*---*----------*-------------------------*~n"),
    io:format("~n"),
    E1 = E - 127,
    M1 = calc_m(lists:flatten(io_lib:format("~23.2.0B", [M]))),
    io:format("~n"),
    io:format("S = s = ~w~n", [S]),
    io:format("E = eeeeeeee - 127 = ~w - 127 = ~w~n", [E, E1]),
    io:format("M = ~w~n", [M1]),
    %% 计算公式：V=(-1)^S*2^E*M
    V = math:pow(-1, S) * math:pow(2, E1) * M1,
    io:format("V = (-1)^S*2^E*M = (-1)^~w*2^~w*~w = ~w~n", [S, E1, M1, V]),
    io:format("~n"),
    io:format("~ts ~w ~ts~w~n", [iolist_to_binary("浮点数"), N, iolist_to_binary("以[单精度]存储后根据公式（V=(-1)^S*2^E*M）运算出来的值为："), V]),
    <<N1:32/float>> = Bin,
    io:format("~ts ~w ~ts~w~n", [iolist_to_binary("浮点数"), N, iolist_to_binary("以[单精度]存储后                    Erlang解包出来的值为："), N1]),
    io:format("~n"),
    f2(N).

f2(N) ->
    io:format("~ts~n", [iolist_to_binary("双精度：")]),
    Bin = <<N:64/float>>,
    <<S:1, E:11, M:52>> = Bin,
    io:format("~ns,eeeeeeeeeee,mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm~n"),
    io:format("~1.2.0B,", [S]),
    io:format("~11.2.0B,", [E]),
    io:format("~52.2.0B,", [M]),
    io:format("~n"),
    <<N1:64/float>> = Bin,
    io:format("~ts ~w ~ts~w", [iolist_to_binary("浮点数"), N, iolist_to_binary("以[双精度]存储后再读取出来的值为："), N1]),
    io:format("~n"),
    ok.

calc_m(M) ->
    calc_m(M, -1, 0).

calc_m([], _E, Rt) ->
    Rt1 = Rt + 1,
    io:format("        M = ~w~n", [Rt]),
    io:format("    1 + M = ~w~n", [Rt1]),
    Rt1;
calc_m([M | T], E, Rt) ->
    M1 = case M of
        49 -> 1;
        48 -> 0
    end,
    V = M1 * math:pow(2, E),
    io:format("~w * 2^~3w = ~w~n", [M1, E, V]),
    calc_m(T, E - 1, Rt + V).

%%% vim: set foldmethod=marker foldmarker=%%',%%.:
