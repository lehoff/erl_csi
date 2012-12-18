%% @doc Run the analysis like this:
%%      1. erl -make above src dir
%%      2. erl -pz ebin
%%      3. otp_analysis:start("otp_analysis.cfg").
%%      4. otp_analysis:core_analysis().

-module(otp_analysis).

-compile(export_all).

-define(NAME, otp_analysis).

start(ConfigFile) ->
    Config = read_config(ConfigFile),
    {ok, _Pid} = xref:start(?NAME),
    RootDir = proplists:get_value(root_dir, Config),
%%    [ xref:add_application(?NAME,RootDir ++ "/" ++ atom_to_list(App)) || App <- Apps ].
    xref:add_release(?NAME, RootDir).


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

core_analysis() ->
    OutgoingCalls = core_outgoing_calls(),
    io:format("Missing applications per core application~n"),
    [ io:format("~p~n",[Pair])
      || Pair <- OutgoingCalls ].

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
    ToMods = lists:usort([ call_get_to_module(C) || C <- ToCalls ]),
    {Mod,ToMods}.

remove_to(Calls,Ignore) ->
    [ C || C <- Calls, not lists:member(call_get_to_module(C),Ignore) ].

call_get_to_module({_,{To,_,_}}) ->
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






