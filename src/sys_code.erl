%%----------------------------------------------------
%% 代码管理器
%% 
%% @author yeahoo2000@gmail.com
%% @end
%%----------------------------------------------------
-module(sys_code).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
        start_link/0
        ,beam_file/0
        ,beam_hash/0
        ,beam_hash/1
        ,up/0
        ,up/1
        ,up/2
        ,compile_and_update/0
        ,prod/0
    ]
).

-define(dir, "/Users/whn/Desktop/svn/server/Casino/Center").
-record(state, {}).

%% ----------------------------------------------------
%% 对外接口
%% ----------------------------------------------------

%% @doc 启动
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 获取beam文件列表
-spec beam_file() -> list().
beam_file() ->
    {ok, Dir} = file:get_cwd(),
    case file:list_dir(Dir ++ "/_build/default/lib/eos/ebin") of
        {ok, FList} ->
            do_beam_file(FList, []);
        {error, _Why} ->
            []
    end.

%% @doc 获取所有beam文件的hash值
-spec beam_hash() -> list().
beam_hash() ->
    do_beam_hash(beam_file(), []).

%% @doc 获取指定beam文件的hash值
-spec beam_hash(string()) -> list().
beam_hash(M) ->
    do_beam_hash([M], []).

%% @doc 重新检查并加载所有的模块
-spec up() -> list().
up() ->
    O = ets:tab2list(sys_code),
    N = beam_hash(),
    do_up(N, O, [], fun code:soft_purge/1).

%% @doc 重新检查并加载所有的模块，并强制清除运行中的旧代码
%% <ul>
%% <li>force 处理所有模块</li>
%% <li>[ModName] 处理所有指定的模块</li>
%% </ul>
-spec up(Option) -> list() when
    Option :: force | [ModName],
    ModName :: atom().
up(force) ->
    N = beam_hash(),
    O = ets:tab2list(sys_code),
    do_up(N, O, [], fun code:purge/1);
up(ModList) ->
    O = ets:tab2list(sys_code),
    N = do_beam_hash(ModList, []),
    do_up(N, O, [], fun code:soft_purge/1).

%% @doc 重新检查并加载指定的模块，并强制清除运行中的旧代码
-spec up([ModName], force) -> list() when
    ModName :: atom().
up(ModList, force) ->
    O = ets:tab2list(sys_code),
    N = do_beam_hash(ModList, []),
    do_up(N, O, [], fun code:purge/1).


compile_and_update() ->
    os:cmd("rebar3 compile"),
    sys_code:up().

prod() ->
    os:cmd("rebar3 as prod tar").

%% ----------------------------------------------------
%% 内部实现
%% ----------------------------------------------------

init([]) ->
    ets:new(sys_code, [named_table, public, set]),
    [ets:insert(sys_code, X) || X <- beam_hash()],
    State = #state{},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------
%% 私有函数
%% ----------------------------------------------------

%% 返回所有的beam文件
do_beam_file([], List) -> List;
do_beam_file([F | T], List) ->
    L = case filename:extension(F) =:= ".beam" of
        true ->
            M = filename:basename(filename:rootname(F)),
            [M | List];
        _ -> List
    end,
    do_beam_file(T, L).

%% 返回beam hash
do_beam_hash([], List) -> List;
do_beam_hash([N | T], List) ->
    {ok, Dir} = file:get_cwd(),
    L = case beam_lib:md5(Dir ++ "/_build/default/lib/eos/ebin/" ++ N) of
        {ok, {M, Md5}} ->
            [{M, cry_hash:md5(Md5)} | List];
        _Err ->
            List
    end,
    do_beam_hash(T, L).

%% 执行更新
do_up([], _O, Rtn, _Fun) -> Rtn;
do_up([{Mod, NewHash} | N], O, Rtn, Fun) ->
    NewRtn = case lists:keyfind(Mod, 1, O) of
        false ->
            [load_beam(Mod, NewHash, Fun) | Rtn];
        {_, OldHash} ->
            case OldHash =:= NewHash of
                true -> Rtn;
                false -> [load_beam(Mod, NewHash, Fun) | Rtn]
            end
    end,
    do_up(N, O, NewRtn, Fun).

%% 加载beam文件(热更新)
load_beam(Mod, Hash, PurgeFun) ->
    PurgeFun(Mod),
    case code:load_file(Mod) of
        {module, _} ->
            ets:insert(sys_code, {Mod, Hash}),
            {Mod, ok};
        {error, Why} ->
            {Mod, {error, Why}}
    end.
