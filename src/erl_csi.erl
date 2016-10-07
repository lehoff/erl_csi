%% @doc Run the analysis like this:
%%      1. erl -make above src dir
%%      2. erl -pz ebin
%%      3. otp_analysis:start("otp_analysis.cfg").
%%      4. otp_analysis:core_analysis().

-module(erl_csi).

-compile(export_all).

-define(NAME, csi).

name() ->
    csi.

start() ->
    erl_csi_server:start_link([]).

start(MFAs) ->
    erl_csi_server:start_link(MFAs).

set_ignored_apps(Apps) ->
    erl_csi_server:set_ignored_apps(Apps).

add_release(RelDir) ->
    xref:add_release(csi, RelDir).

remove_application(IgnoreApps) ->
    Present = erl_csi:intersection(erl_csi:apps(), IgnoreApps),
    xref:remove_application(csi, Present).


add_build_dir(undefined) ->
    ok;
add_build_dir(Dir) ->
    xref:add_release(name(), Dir).

add_app(RootDir, AppDir) ->
%%    AppStr = atom_to_list(App),
    App = strip_app_version(AppDir),
    Dir = string:join([RootDir,AppDir],"/"),
    case xref:add_application(name(), Dir) of
        {ok, App} ->
            ok;
        Error ->
            exit(Error)
    end.

add_application(App, AppDir) ->
    case xref:add_application(name(), AppDir, {name, App}) of
        {ok, App} ->
            ok;
        Error ->
            exit(Error)
    end.


add_all_apps_in_dir(RootDir) ->
    Apps = apps_in_dir(RootDir),
    lists:foreach( fun({App,AppDir}) ->
                             add_application(App, AppDir)
                   end,
                   Apps).

%%% @doc removes the -x.y.z from the dir name of an application and return the app name as an atom.
strip_app_version(Dir) ->
    {match, ResList} = re:run(Dir, "([^-])*", [{capture,first, list}]),
    erlang:list_to_atom(hd(ResList)).

%%% @doc finds all app-x.y.z.w dirs in the RootDir and returns the ones that are in list Apps
create_app_dirs(RootDir, Apps) ->
    {ok, AllDirs} = file:list_dir(RootDir),
    [ Dir || Dir <- AllDirs,
             lists:member(strip_app_version(Dir), Apps)].

%%% @doc returns all apps in the RootDir (should be a lib dir of a release) as a list of tuples of
%%%      the form {atom(), list()}.
apps_in_dir(RootDir) ->
    {ok, AllFiles} = file:list_dir(RootDir),
    AllDirs =  [ Filename
                 || Filename <- AllFiles,
                   is_app_dir(create_file_path(RootDir, Filename))],
    [ {strip_app_version(Dir), create_file_path(RootDir,Dir)}
      || Dir <- AllDirs ].

create_file_path(RootDir, Filename) ->
    string:join([RootDir,Filename],"/").


is_app_dir(Dir) ->
    filelib:is_dir(Dir ++ "/ebin").


stop() ->
    xref:stop(name()).

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
    xref:info(name()).

info(Cat) ->
    xref:info(name(),Cat).

info(Cat,Items) ->
    xref:info(name(),Cat,Items).

analyze(Analysis) ->
    {ok, Answer} =xref:analyze(name(), Analysis),
    Answer.

q(Query) ->
    xref:q(name(),Query).

q(Query,Opts) ->
    xref:q(name(),Query,Opts).

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
    {ok,Pre} = q("XC | " ++ atom_to_string(Mod) ++ ":Mod"),
    ToMods = filter_ignored_modules_from_calls(Pre),
    {Mod,ToMods}.

filter_ignored_modules_from_calls(Pre) ->
    Ignore = erl_csi_server:ignored_modules(),
    ToCalls = remove_to(Pre, Ignore),
    ToMods = lists:usort([to_module_of_call_tuple(C) || C <- ToCalls]),
    ToMods.

transitive_module_calls_to_modules(Mod) ->
    {ok, Pre} = q("closure XC | " ++ atom_to_string(Mod) ++ ":Mod"),
    ToMods = filter_ignored_modules_from_calls(Pre),
    {Mod, ToMods}.

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
    case q("(Mod) " ++ atom_to_string(App)) of
        {ok, Modules} ->
            Modules;
        {error, xref_compiler, {unknown_constant, _AppStr}} ->
            []
    end.

fun_calls_from_module_to_module(From, To) ->
    {ok, Res} = xref:q(csi, lists:flatten(io_lib:format("(Fun) ~p -> ~p:Mod", [From, To]))),
    Res.

app_transitive_calls(App) ->
    {ok, Res} = erl_csi:q("closure AE | " ++ erl_csi:atom_to_string(App)),
    [ To || {_From, To} <- Res] -- [App].

module_calls_from_app_to_app(From, To) ->
    KvMods = app_modules(From),
    TargetMods = app_modules(To),
    KvCalls = [ module_to_module(M) || M <- KvMods],
    All = [ {M, intersection(Calls, TargetMods)}
            || {M,Calls} <- KvCalls ],
    [ Mcalls || {_M, Calls} = Mcalls <- All, Calls /= []].

fun_calls_to_app(FromApp, ToApp) ->
    Mcalls = module_calls_from_app_to_app(FromApp, ToApp),
    AllMcallPairs = lists:flatten([ flatten_mcall(Mcall) || Mcall <- Mcalls]),
    [ erl_csi:fun_calls_from_module_to_module(From, To)
      || {From, To} <- AllMcallPairs].

flatten_mcall({From,ToList}) ->
    [ {From, To} || To <- ToList].

%% set operations for lists.
complement(All, A) ->
    All -- A.

intersection(A, B) ->
    sets:to_list(sets:intersection(sets:from_list(A),sets:from_list(B))).

union(A, B) ->
    sets:to_list(sets:union(sets:from_list(A),sets:from_list(B))).
