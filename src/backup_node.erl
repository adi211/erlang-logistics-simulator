%%%-------------------------------------------------------------------
%%% Enhanced Backup Node with automatic module loading
%%%-------------------------------------------------------------------
-module(backup_node).
-export([start/0, stop/0, status/0]).
-include("network_const.hrl").

start() ->
    io:format("Backup Node: Starting enhanced failover monitor...~n"),
    
    %% First, ensure all modules are loaded
    ensure_modules_loaded(),
    
    %% Initialize map server locally
    map_server:initialize_local_map(map_data_100),
    
    %% רשימת כל הnodes לניטור
    Nodes = [
        ?CTRL_NODE,
        ?NORTH_NODE,
        ?CENTER_NODE,
        ?SOUTH_NODE,
        ?VIZ_NODE
    ],
    
    %% Register the backup process
    register(backup_monitor, self()),
    
    %% התחבר ונטר כל node
    lists:foreach(fun(N) -> 
        net_kernel:connect_node(N),
        monitor_node(N, true),
        io:format("Backup Node: Now monitoring ~p~n", [N])
    end, Nodes),
    
    %% Table לזכור מה אני מחליף
    ets:new(active_replacements, [named_table, set, public]),
    ets:new(backup_pids, [named_table, set, public]),
    
    %% Start periodic health check
    timer:send_interval(5000, self(), health_check),
    
    %% לולאת המתנה
    monitor_loop().

%% function to ensure all modules are loaded
ensure_modules_loaded() ->
    Modules = [
        control_center, zone_manager, courier, household, package,
        location_tracker, map_server, visualization_server, 
        dashboard_server, logistics_state_collector,
        map_data_100, map_data_200, map_data_1043
    ],
    
    lists:foreach(fun(Mod) ->
        case code:ensure_loaded(Mod) of
            {module, Mod} ->
                io:format("Backup Node: Module ~p loaded~n", [Mod]);
            {error, Reason} ->
                io:format("Backup Node: Failed to load ~p: ~p~n", [Mod, Reason])
        end
    end, Modules).


%% Enhanced takeover with zone parameter
take_over(FailedNode) ->
    NodeStr = atom_to_list(FailedNode),
    
    %% Store PIDs of started processes
    Pids = case string:split(NodeStr, "@") of
        ["control" ++ _, _] ->
            io:format("Starting Control Center replacement...~n"),
            {ok, CtrlPid} = control_center:start(),
            [{control_center, CtrlPid}];
            
        ["zone_north" ++ _, _] ->
            io:format("Starting Zone North replacement...~n"),
            {ok, ZonePid} = zone_manager:start_for_zone(north),  
            [{zone_manager, ZonePid}];
            
        ["zone_center" ++ _, _] ->
            io:format("Starting Zone Center replacement...~n"),
            {ok, ZonePid} = zone_manager:start_for_zone(center), 
            [{zone_manager, ZonePid}];
            
        ["zone_south" ++ _, _] ->
            io:format("Starting Zone South replacement...~n"),
            {ok, ZonePid} = zone_manager:start_for_zone(south),  
            [{zone_manager, ZonePid}];
            
        ["visualization" ++ _, _] ->
            io:format("Starting Visualization replacement...~n"),
            case visualization_server:start() of
                {ok, Pid} ->
                    [{visualization_server, Pid}];
                _ ->
                    case dashboard_server:start() of
                        {ok, DashPid} -> [{dashboard_server, DashPid}];
                        _ -> []
                    end
            end;
            
        _ ->
            io:format("Unknown node: ~p~n", [FailedNode]),
            []
    end,
    
    %% Store PIDs for cleanup
    lists:foreach(fun({Name, Pid}) ->
        ets:insert(backup_pids, {FailedNode, Name, Pid})
    end, Pids).

%% Enhanced step down with proper cleanup
step_down(RecoveredNode) ->
    %% Get all processes we started for this node
    Processes = ets:match(backup_pids, {RecoveredNode, '$1', '$2'}),
    
    lists:foreach(fun([Name, Pid]) ->
        io:format("Backup Node: Stopping ~p for recovered node ~p~n", 
                  [Name, RecoveredNode]),
        try
            gen_server:stop(Pid, normal, 5000)
        catch
            _:_ -> 
                exit(Pid, shutdown)
        end
    end, Processes),
    
    %% Clean up ETS entries
    ets:match_delete(backup_pids, {RecoveredNode, '_', '_'}).

%% status function
status() ->
    case whereis(backup_monitor) of
        undefined ->
            {error, not_running};
        Pid ->
            ActiveReplacements = ets:tab2list(active_replacements),
            BackupPids = ets:tab2list(backup_pids),
            {ok, #{
                pid => Pid,
                active_replacements => ActiveReplacements,
                running_processes => BackupPids
            }}
    end.


%% stop function
stop() ->
    case whereis(backup_monitor) of
        undefined ->
            {error, not_running};
        MonitorPid ->  % שינוי השם למשהו ברור יותר
            %% Clean up all backup processes
            AllPids = ets:tab2list(backup_pids),
            lists:foreach(fun({_Node, _Name, ProcessPid}) ->  % שם שונה
                catch gen_server:stop(ProcessPid)
            end, AllPids),
            
            %% Clean up ETS tables
            catch ets:delete(active_replacements),
            catch ets:delete(backup_pids),
            
            %% Stop the monitor
            exit(MonitorPid, shutdown),
            ok
    end.
	
	

monitor_loop() ->
    %% שלב 1: בדוק מה המצב הנוכחי של הגיבוי ממילון התהליך
    State = case get(backup_state) of
        undefined -> 
            put(backup_state, idle), % אם זה הריצה הראשונה, קבע את המצב ל-idle
            idle;
        CurrentState -> 
            CurrentState
    end,

    receive
        {nodedown, FailedNode} ->
            case State of
                idle ->
                    %% אם אנחנו במצב idle, קח אחריות על הצומת שנפל
                    io:format("IDLE MODE: Node ~p failed! Taking over...~n", [FailedNode]),
                    take_over(FailedNode),
                    ets:insert(active_replacements, {FailedNode, true}),
                    %% שנה את המצב ל-takeover
                    put(backup_state, {takeover, FailedNode});

                {takeover, ActiveForNode} ->
                    %% אם אנחנו כבר במצב takeover, התעלם מהנפילה החדשה
                    io:format("TAKEOVER MODE: Ignoring failure of ~p while active for ~p.~n",
                              [FailedNode, ActiveForNode])
            end,
            monitor_loop();
            
        {nodeup, RecoveredNode} ->
            case State of
                idle ->
                    %% לא אמורים לקבל nodeup במצב idle, אז פשוט נמשיך
                    ok;

                {takeover, ActiveForNode} when RecoveredNode == ActiveForNode ->
                    %% הצומת שגיבינו חזר! הפסק את הגיבוי וחזור למצב idle
                    io:format("TAKEOVER MODE: Node ~p recovered! Stepping down...~n", [RecoveredNode]),
                    step_down(RecoveredNode),
                    ets:delete(active_replacements, RecoveredNode),
                    io:format("Backup Node: Resuming monitoring for ~p~n", [RecoveredNode]),
                    monitor_node(RecoveredNode, true),
                    %% שנה את המצב בחזרה ל-idle
                    put(backup_state, idle);

                {takeover, _ActiveForNode} ->
                    %% צומת אחר חזר, לא זה שאנחנו מחכים לו. התעלם.
                    ok
            end,
            monitor_loop();
            
        health_check ->
            %% הוספנו בדיקה אקטיבית למצב takeover
            case State of
                {takeover, ActiveForNode} ->
                    net_kernel:connect_node(ActiveForNode);
                _ ->
                    ok
            end,
            Size = ets:info(active_replacements, size),
            io:format("Backup Node (~p): Health check - ~p replacements active~n", [State, Size]),
            monitor_loop();
            
        stop ->
            io:format("Backup Node: Shutting down~n"),
            stop();
            
        _ ->
            monitor_loop()
    end.