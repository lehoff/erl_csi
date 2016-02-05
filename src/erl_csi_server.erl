-module(erl_csi_server).

-behaviour(gen_server).

-export([start_link/1,
         set_ignored_apps/1,
         ignored_apps/0,
         ignored_modules/0]).


-export([init/1,
         terminate/2,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-export([start_clean/0]).

-record(state,
        {ignored_apps = [],
         ignored_modules = [],
         start_up=[]}).

start_link(MFAs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, MFAs, []).


set_ignored_apps(Apps) ->
    gen_server:cast(?MODULE, {set_ignored_apps, Apps}).

ignored_apps() ->
    gen_server:call(?MODULE, ignored_apps).

ignored_modules() ->
    gen_server:call(?MODULE, ignored_modules).

init(MFAs) ->
    run_initialisation(MFAs),
    {ok, #state{start_up=MFAs}}.


run_initialisation(MFAs) ->
    start_clean(),
    lists:foreach(fun ({M,F,A}) -> erlang:apply(M, F, A) end, MFAs).
%    gen_server:cast(?MODULE, {init, MFAs}).

%% handle_cast({init, MFAs}, State) ->
%%     lists:foreach(fun ({M,F,A}) -> erlang:apply(M, F, A) end, MFAs),
%%     {noreply, State};
handle_cast({set_ignored_apps, Apps}, State) ->
%%    erl_csi:stop(),
%%    run_initialisation(State#state.start_up),
    IgnoredModules = calc_ignored_modules(Apps),
    erl_csi:remove_application(Apps),
    {noreply, State#state{ignored_apps=Apps,
                          ignored_modules=IgnoredModules}}.

handle_call(ignored_apps,_From, State) ->
    {reply, State#state.ignored_apps, State};
handle_call(ignored_modules, _From, State) ->
    {reply, State#state.ignored_modules, State}.

handle_info(_, State) ->
    {no_reply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

calc_ignored_modules(Apps) ->
    lists:flatten( [ erl_csi:app_modules(A)
                     ||  A <- Apps]).



%% @doc it is assumed that the root_dir points to a location where a
%% standard Erlang release directory structure is found, i.e., there
%% is a lib directory there.
%% start(ConfigFile) ->
%%     set_config(ConfigFile),
%%     Config = read_config(),
%%     {ok, _Pid} = xref:start(name()),
%%     RootDir = proplists:get_value(root_dir, Config),
%%     %%    xref:add_release(name(), RootDir, {name, name()}).
%%     Apps = proplists:get_value(applications, Config),
%%     AppDirs = create_app_dirs(RootDir, Apps),
%%     lists:foreach(fun (AppDir) ->
%%                           add_app(RootDir,AppDir)
%%                   end,
%%                   AppDirs),
%%     add_build_dir(proplists:get_value(build_dir,Config)).

start_clean() ->
    case whereis(erl_csi:name()) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            xref:stop(erl_csi:name())
    end,
    {ok, _Pid} = xref:start(erl_csi:name()).
