%%%-------------------------------------------------------------------
%%% @doc Backup Node - Automatic failover for any failed node
%%%-------------------------------------------------------------------
-module(backup_node).
-export([start/0]).

start() ->
    io:format("Backup Node: Starting failover monitor...~n"),
    
    %% רשימת כל הnodes לניטור (כולל visualization!)
    Nodes = [
        'control@127.0.0.1',
        'zone_north@127.0.0.1', 
        'zone_center@127.0.0.1',
        'zone_south@127.0.0.1',
        'visualization@127.0.0.1'  
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

%% Takeover functions לכל node
take_over('control@127.0.0.1') ->
    io:format("Starting Control Center replacement...~n"),
    control_center:start('visualization@127.0.0.1');
    
take_over('zone_north@127.0.0.1') ->
    io:format("Starting Zone North replacement...~n"),
    zone_manager:start('control@127.0.0.1');
    
take_over('zone_center@127.0.0.1') ->
    io:format("Starting Zone Center replacement...~n"),
    zone_manager:start('control@127.0.0.1');
    
take_over('zone_south@127.0.0.1') ->
    io:format("Starting Zone South replacement...~n"),
    zone_manager:start('control@127.0.0.1');
    
take_over('visualization@127.0.0.1') ->
    io:format("Starting Visualization replacement...~n"),
    %% כן! גם visualization אפשר!
    case whereis(map_visual_server) of
        undefined ->
            {ok, _} = map_visual:start(),
            io:format("Map visual started as replacement~n");
        _ ->
            io:format("Visual already running~n")
    end.

%% Step down when original returns
step_down('control@127.0.0.1') ->
    catch gen_server:stop(control_center);
    
step_down('visualization@127.0.0.1') ->
    catch map_visual_server ! stop;
    
step_down(_Zone) ->
    catch gen_server:stop(zone_manager).