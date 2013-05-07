%% @doc Run the analysis like this:
%%      1. erl -make above src dir
%%      2. erl -pz ebin
%%      3. otp_analysis:start("otp_analysis.cfg").
%%      4. otp_analysis:core_analysis().

-module(otp_analysis).

-compile(export_all).

-define(NAME, otp_analysis).


%% @doc it is assumed that the root_dir points to a location where a
%% standard Erlang release directory structure is found, i.e., there
%% is a lib directory there.
start(ConfigFile) ->
    Config = read_config(ConfigFile),
    {ok, _Pid} = xref:start(?NAME),
    RootDir = proplists:get_value(root_dir, Config),
    xref:add_release(?NAME, RootDir, {name, ?NAME}).


stop() ->
    xref:stop(?NAME).

read_config(File) ->
    {ok,Config} = file:consult(File),
    Config.

read_config() ->
    read_config("otp_analysis.cfg").


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

module_calls_within_app(App) ->
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

%% @doc remove all calls to modules we are ignoring calls to.
remove_to(Calls,Ignore) ->
    [C || C <- Calls, not lists:member(to_module_of_call_tuple(C),Ignore)].

to_module_of_call_tuple({_,{To,_,_}}) ->
                          To.

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






