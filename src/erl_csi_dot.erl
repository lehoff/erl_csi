%%% @doc create a dot file out of various erl_csi outputs.
%%%
%%% @end
-module(erl_csi_dot).

-compile(export_all).

-spec create_dot_for_from_tolist(term(), [{term(), [term()]}]) -> iolist().
create_dot_for_from_tolist(Title, Calls) ->
    CallEdgesStrings = [ call_edges(From, ToList) || {From, ToList} <- Calls ],
    Header = io_lib:format("digraph ~p {", [Title]),
    End    = "}",
    lists:flatten([Header, CallEdgesStrings, End]).


call_edges(From, ToList) ->
    ToString = string:join( [term_to_string(To) || To <- ToList], ","),
    io_lib:format("~p -> { ~s };~n", [term_to_string(From), ToString] ).


term_to_string(T) ->
    lists:flatten(io_lib:format("~p", [T])).

digraph_to_dot(G) ->
    Vertices = digraph:vertices(G),
    FromToList = [ {V, digraph:out_neighbours(G, V)} ||
                     V <- Vertices ],
    create_dot_for_from_tolist(somegraph, FromToList).