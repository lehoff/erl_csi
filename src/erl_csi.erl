%% @doc Run the analysis like this:
%%      1. erl -make above src dir
%%      2. erl -pz ebin
%%      3. otp_analysis:start("otp_analysis.cfg").
%%      4. otp_analysis:core_analysis().

-module(erl_csi).

-compile(export_all).

-define(NAME, csi).


start(MFAs) ->
    erl_csi_server:start_link(MFAs).

set_ignored_apps(Apps) ->
    erl_csi_server:set_ignored_apps(Apps).

add_release(RelDir) ->
    xref:add_release(csi, RelDir).

remove_application(IgnoreApps) ->
    xref:remove_application(csi, IgnoreApps).


%% @doc it is assumed that the root_dir points to a location where a
%% standard Erlang release directory structure is found, i.e., there
%% is a lib directory there.
%% start(ConfigFile) ->
%%     set_config(ConfigFile),
%%     Config = read_config(),
%%     {ok, _Pid} = xref:start(?NAME),
%%     RootDir = proplists:get_value(root_dir, Config),
%%     %%    xref:add_release(?NAME, RootDir, {name, ?NAME}).
%%     Apps = proplists:get_value(applications, Config),
%%     AppDirs = create_app_dirs(RootDir, Apps),
%%     lists:foreach(fun (AppDir) ->
%%                           add_app(RootDir,AppDir)
%%                   end,
%%                   AppDirs),
%%     add_build_dir(proplists:get_value(build_dir,Config)).

start_clean() ->
    {ok, _Pid} = xref:start(?NAME).


add_build_dir(undefined) ->
    ok;
add_build_dir(Dir) ->
    xref:add_release(?NAME, Dir).

add_app(RootDir, AppDir) ->
%%    AppStr = atom_to_list(App),
    App = strip_app_version(AppDir),
    Dir = string:join([RootDir,AppDir],"/"),
    case xref:add_application(?NAME, Dir) of
        {ok, App} ->
            ok;
        Error ->
            exit(Error)
    end.

%%% @doc removes the -x.y.z from the dir name of an application and return the app name as an atom.
strip_app_version(Dir) ->
    {match, ResList} = re:run(Dir, "([^-])*", [{capture,first, list}]),
    erlang:list_to_atom(hd(ResList)).

%%% @doc finds all app-x.y.z.w dirs in the RootDir and returns the ones that are in list Apps
create_app_dirs(RootDir, Apps) ->
    {ok, AllDirs} = file:list_dir(RootDir),
    [ Dir || Dir <- AllDirs,
             lists:member(strip_app_version(Dir), Apps)].


stop() ->
    xref:stop(?NAME).

read_config(File) ->
    {ok,Config} = file:consult(File),
    Config.

read_config() ->
    read_config("tmp_analysis.cfg").

set_config(File) ->
    {ok, _} = file:copy(File, "tmp_analysis.cfg").

read_apps() ->
    Config = read_config(),
    proplists:get_value(applications, Config).


%% @doc outgoing_calls(Apps) are all calls from within the Apps list
%% to applications outside Apps.
outgoing_calls(Apps) ->
    Calls = app2app_calls(Apps),
    Edges = lists:flatten([ [{A, To} || To <- ToList]
                            || {A,ToList} <- Calls %, ToList /= []
                          ]),
    [ E
      || {_,To} = E <- Edges,
         not lists:member(To, Apps)
    ].

core_outgoing_calls() ->
    outgoing_calls(read_apps()).

%% @doc used to see which modules that we do not consider to be part
%% of the core modules of Erlang for embedded purposes.
core_analysis() ->
    OutgoingCalls = core_outgoing_calls(),
    io:format("Missing applications per core application~n"),
    [ io:format("~p~n",[Pair])
      || Pair <- OutgoingCalls ].

mod_calls_within_app(App) ->
    Modules = app_modules(App),
    M2MCalls = [ module_to_module(Mod) || Mod <- Modules ],
    Mod2ModCalls = filter_to_modules(M2MCalls, Modules),
    {App, Mod2ModCalls}.

%% @doc only leave the ToMods that are in the Mods list.
filter_to_modules(M2MCalls, Mods) ->
    [{M, list_intersection(ToMods, Mods)} || {M, ToMods} <- M2MCalls].

list_intersection(A,B) ->
    S1 = sets:from_list(A),
    S2 = sets:from_list(B),
    S  = sets:intersection(S1, S2),
    sets:to_list(S).


info() ->
    xref:info(?NAME).

info(Cat) ->
    xref:info(?NAME,Cat).

info(Cat,Items) ->
    xref:info(?NAME,Cat,Items).

analyze(Analysis) ->
    {ok, Answer} =xref:analyze(?NAME, Analysis),
    Answer.

q(Query) ->
    xref:q(?NAME,Query).

q(Query,Opts) ->
    xref:q(?NAME,Query,Opts).

apps() ->
    Info = info(applications),
    [ App || {App, _ } <- Info ].

mod_functions(Mod) ->
    {ok, Funs} = q("F *" ++ atom_to_string(Mod)),
    Funs.

atom_to_string(A) ->
    "'" ++ atom_to_list(A) ++ "'".

%% @doc returns all the modules that Mod calls
module_to_module(Mod) ->
    Opts = read_config(),
    {ok,Pre} = q("XC | " ++ atom_to_string(Mod)),
    Ignore = proplists:get_value(ignore_modules,Opts,[]),
    ToCalls = remove_to(Pre,Ignore),
    ToMods = lists:usort([to_module_of_call_tuple(C) || C <- ToCalls]),
    {Mod,ToMods}.

%% @doc figures out who calls a MFA from outside the module M
who_calls_mfa({M,_F,_A}=MFA) ->
    {ok, Pre} = q("XC || " ++ atom_to_string(M)),
    [ From || {From, MFA2} <- Pre,
              MFA2==MFA].
%% @doc who is calling which functions of a module
who_calls_mod_mfa(Mod) ->
    {_, FAs} = used_funs(Mod, [Mod]),
    MFAs = [ {Mod, F, A} || {F,A} <- FAs ],
    [ {MFA, who_calls_mfa(MFA)} || MFA <- MFAs].

mods_that_call_mod(Mod) ->
    Opts = read_config(),
    {ok,Pre} = q("(Mod) XC || " ++ atom_to_string(Mod)),
    Ignore = proplists:get_value(ignore_modules,Opts,[]),
    [ From || {From, _} <- Pre,
              not lists:member(From, Ignore) ].

%% @doc used_funs gives the functions used in a module, but
%%      ignoring calls from modules in the Ignore list.
used_funs(Mod, Ignore) ->
    {ok, Pre} = q("E || " ++ atom_to_string(Mod)),
    Used1 = remove_from(Pre, Ignore),
    Used = [{Fun,Arity} || {_,{_,Fun,Arity}} <- Used1],
    {Mod, lists:usort(Used)}.

%%% @doc prints a list that can be used in a -export directive
print_used_funs_as_export({_Mod, Used}) ->
    UsedStrings = [ erlang:atom_to_list(F) ++ "/" ++ erlang:integer_to_list(A)
                    || {F,A} <- Used ],
    List = string:join(UsedStrings, ",\n" ++ lists:duplicate(9, " ")),
    io:format("-export([~s]).~n",[List]).

%% @doc remove all calls to modules we are ignoring calls to.
remove_to(Calls,Ignore) ->
    [C || C <- Calls, not lists:member(to_module_of_call_tuple(C),Ignore)].

%% @doc remove calls from modules we want to ignore
remove_from(Calls, Ignore) ->
    [C || C <- Calls,
          not lists:member(from_module_of_call_tuple(C), Ignore)].

to_module_of_call_tuple({_,{To,_,_}}) ->
                          To.

from_module_of_call_tuple({{From,_,_}, _}) ->
    From.

filter_calls(app2app, Calls) ->
    [ {From, [ ToApp || {ToApp,_}<- ToAppModList] }
      || {From, ToAppModList} <- Calls ].




app2app_calls(Apps) ->
    [ {A, analyze({application_call, A}) -- [A]  } || A <- Apps ].

apps_called_by_app(App) ->
    {ok, A2A} = q("AE | " ++ atom_to_string(App)),
    % only intersted in apps called - not internal calls
    lists:delete(App,extract_second(A2A)).


apps_that_call_app(App) ->
    {ok, A2A} = q("AE || " ++ atom_to_string(App)),
    % only intersted in the other apps calling this app
    lists:delete(App, extract_first(A2A)).


extract_first(TupleList) ->
    [ A || {A,_} <- TupleList ].

extract_second(TupleList) ->
    [ B || {_,B} <- TupleList ].

app_functions(App) ->
    {ok, Funs} = q("F * " ++ atom_to_string(App)),
    Funs.

app_modules(App) ->
    {ok, Modules} = q("(Mod) " ++ atom_to_string(App)),
    Modules.
