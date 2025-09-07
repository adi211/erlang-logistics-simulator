%%%-------------------------------------------------------------------
%%% @doc Backup Node - Automatic failover for any failed node
%%% Simple version - keeps original pattern matching
%%%-------------------------------------------------------------------
-module(backup_node).
-export([start/0]).
-include("network_const.hrl").

start() ->
    io:format("Backup Node: Starting failover monitor...~n"),
    
    %% רשימת כל הnodes לניטור
    Nodes = [
        ?CTRL_NODE,
        ?NORTH_NODE,
        ?CENTER_NODE,
        ?SOUTH_NODE,
        ?VIZ_NODE
    ],
    
    %% התחבר ונטר כל node
    lists:foreach(fun(N) -> 
        net_kernel:connect_node(N),
        monitor_node(N, true),
        io:format("Backup Node: Now monitoring ~p~n", [N])
    end, Nodes),
    
    %% Table לזכור מה אני מחליף
    ets:new(active_replacements, [named_table, set, public]),
    
    %% לולאת המתנה
    monitor_loop().

monitor_loop() ->
    receive
        {nodedown, FailedNode} ->
            io:format("ALERT: Node ~p failed! Taking over...~n", [FailedNode]),
            take_over(FailedNode),
            ets:insert(active_replacements, {FailedNode, true}),
            monitor_loop();
            
        {nodeup, RecoveredNode} ->
            case ets:lookup(active_replacements, RecoveredNode) of
                [{RecoveredNode, true}] ->
                    io:format("Node ~p recovered! Stepping down...~n", [RecoveredNode]),
                    step_down(RecoveredNode),
                    ets:delete(active_replacements, RecoveredNode);
                [] ->
                    ok
            end,
            monitor_loop();
            
        _ ->
            monitor_loop()
    end.

%% Takeover function - simple approach
take_over(FailedNode) ->
    %% Extract node name without IP
    NodeStr = atom_to_list(FailedNode),
    case string:split(NodeStr, "@") of
        ["control" ++ _, _] ->
            io:format("Starting Control Center replacement...~n"),
            control_center:start(?VIZ_NODE);
            
        ["zone_north" ++ _, _] ->
            io:format("Starting Zone North replacement...~n"),
            zone_manager:start(?CTRL_NODE);
            
        ["zone_center" ++ _, _] ->
            io:format("Starting Zone Center replacement...~n"),
            zone_manager:start(?CTRL_NODE);
            
        ["zone_south" ++ _, _] ->
            io:format("Starting Zone South replacement...~n"),
            zone_manager:start(?CTRL_NODE);
            
        ["visualization" ++ _, _] ->
            io:format("Starting Visualization replacement...~n"),
            case whereis(map_visual_server) of
                undefined ->
                    {ok, _} = map_visual:start(),
                    io:format("Map visual started as replacement~n");
                _ ->
                    io:format("Visual already running~n")
            end;
            
        _ ->
            io:format("Unknown node: ~p~n", [FailedNode])
    end.

%% Step down when original returns
step_down(RecoveredNode) ->
    NodeStr = atom_to_list(RecoveredNode),
    case string:split(NodeStr, "@") of
        ["control" ++ _, _] ->
            catch gen_server:stop(control_center);
            
        ["visualization" ++ _, _] ->
            catch map_visual_server ! stop;
            
        ["zone_" ++ _, _] ->
            catch gen_server:stop(zone_manager);
            
        _ ->
            io:format("Unknown node for step_down: ~p~n", [RecoveredNode])
    end.