%%%-------------------------------------------------------------------
%%% Control Center - Fixed with proper PAUSE/RESUME/STOP handling
%%% and improved zone communication
%%%-------------------------------------------------------------------
-module(control_center).
-behaviour(gen_server).

-include("header.hrl").

%% API
-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    visualization_node,
    zones = [],
    total_zones = 0,
    total_couriers = 0,
    active_couriers = 0,
    total_deliveries = 0,
    failed_deliveries = 0,
    simulation_state = stopped,  % stopped | running | paused
    simulation_mode = visual,
    current_map = map_data_100,
    zone_monitors = #{},
    map_server_pid = undefined,
    state_collector_pid = undefined,
    simulation_config = #{}
}).

%%--------------------------------------------------------------------
%% @doc Starts the control center
%%--------------------------------------------------------------------
start(VisualizationNode) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [VisualizationNode], []).

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
%%--------------------------------------------------------------------
init([VisualizationNode]) ->
    io:format("Control Center: Starting up...~n"),
    
    StateCollectorPid = case logistics_state_collector:start_link() of
        {ok, Pid} ->
            timer:sleep(100),
            io:format("Control Center: State collector started~n"),
            Pid;
        {error, {already_started, Pid}} ->
            io:format("Control Center: State collector already running~n"),
            Pid;
        ErrorResult ->
            io:format("Control Center: Failed to start state collector: ~p~n", [ErrorResult]),
            undefined
    end,
    
    %% Launch GUI components on visualization node
    try
        case VisualizationNode of
            undefined -> 
                io:format("Control Center: No visualization node specified~n");
            _ ->
                case rpc:call(VisualizationNode, dashboard_server, start, []) of
                    {badrpc, RPCReason} ->
                        io:format("Control Center: Failed to start dashboard on ~p: ~p~n", 
                                 [VisualizationNode, RPCReason]);
                    {wx_ref, _, _, _} ->
                        io:format("Control Center: Dashboard started on ~p~n", [VisualizationNode]);
                    Other ->
                        io:format("Control Center: Unexpected dashboard start result: ~p~n", [Other])
                end,
                
                case rpc:call(VisualizationNode, visualization_server, start, []) of
                    {badrpc, VizRPCReason} ->
                        io:format("Control Center: Failed to start visualization server on ~p: ~p~n", 
                                 [VisualizationNode, VizRPCReason]);
                    {ok, _VizPid} ->
                        io:format("Control Center: Visualization server started on ~p~n", [VisualizationNode]);
                    {error, {already_started, _}} ->
                        io:format("Control Center: Visualization server already running on ~p~n", [VisualizationNode]);
                    VizOther ->
                        io:format("Control Center: Unexpected visualization server start result: ~p~n", [VizOther])
                end
        end
    catch
        ErrType:ErrReason ->
            io:format("Control Center: Error starting GUI components: ~p:~p~n", [ErrType, ErrReason])
    end,
    
    %% Initialize ETS tables
    ets:new(simulation_state, [set, named_table, public]),
    ets:new(zone_stats, [set, named_table, public]),
    
    %% Monitor visualization node if provided
    case VisualizationNode of
        undefined -> ok;
        _ ->
            case net_adm:ping(VisualizationNode) of
                pong ->
                    monitor_node(VisualizationNode, true),
                    io:format("Control Center: Monitoring visualization node ~p~n", [VisualizationNode]);
                pang ->
                    io:format("Control Center: Cannot reach visualization node ~p~n", [VisualizationNode])
            end
    end,
    
    %% Add heartbeat timer - check zones every 3 seconds
    timer:send_interval(3000, self(), check_and_reconnect_zones),
    timer:send_interval(5000, self(), check_zones),
    
    {ok, #state{
        visualization_node = VisualizationNode,
        zones = [],
        simulation_state = stopped,
        simulation_mode = visual,
        current_map = map_data_100,
        zone_monitors = #{},
        state_collector_pid = StateCollectorPid
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages - Zone connection handler
%%--------------------------------------------------------------------
handle_call({connect, ZoneNode}, _From, State = #state{zones = CurrentZones}) ->
    io:format("Control Center: Zone ~p connecting...~n", [ZoneNode]),
    
    ZoneType = case atom_to_list(ZoneNode) of
        "zone_north" ++ _ -> north;
        "zone_center" ++ _ -> center;
        "zone_south" ++ _ -> south;
        _ -> unknown
    end,
    
    monitor_node(ZoneNode, true),
    
    %% Check if zone already exists
    UpdatedZones = case lists:keyfind(ZoneNode, 2, CurrentZones) of
        false ->
            %% Add new zone
            NewZone = {undefined, ZoneNode, ZoneType},
            [NewZone | CurrentZones];
        _ ->
            %% Zone already exists, don't duplicate
            CurrentZones
    end,
    
    case State#state.state_collector_pid of
        undefined -> ok;
        CollectorPid when is_pid(CollectorPid) ->
            logistics_state_collector:zone_state_changed(atom_to_list(ZoneType), #{
                status => live,
                node => ZoneNode,
                couriers => 0,
                deliveries => 0
            })
    end,
    
    io:format("Control Center: Zone ~p (~p) connected successfully. Total zones: ~p~n", 
              [ZoneType, ZoneNode, length(UpdatedZones)]),
    
    {reply, ok, State#state{zones = UpdatedZones, total_zones = length(UpdatedZones)}};

handle_call(get_system_status, _From, State) ->
    %% Check all zones
    ZoneStatuses = lists:map(fun({_PID, Node, Type}) ->
        Status = case net_adm:ping(Node) of
            pong -> 
                %% Try to get stats from zone
                try
                    gen_statem:call({zone_manager, Node}, get_stats, 1000)
                catch
                    _:_ -> #{status => unreachable}
                end;
            pang -> 
                #{status => offline}
        end,
        {Type, Status}
    end, State#state.zones),
    
    SystemStatus = #{
        simulation_state => State#state.simulation_state,
        zones => ZoneStatuses,
        total_zones => State#state.total_zones,
        total_couriers => State#state.total_couriers,
        active_couriers => State#state.active_couriers,
        total_deliveries => State#state.total_deliveries
    },
    
    {reply, {ok, SystemStatus}, State};

handle_call({remove_couriers_request, Num}, _From, State) ->
    io:format("Control Center: Request to remove ~p couriers~n", [Num]),
    broadcast_to_zones(State#state.zones, {remove_couriers, Num}),
    {reply, ok, State};

handle_call(shutdown, _From, State) ->
    io:format("Control Center: Shutting down~n"),
    {stop, shutdown, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%%--------------------------------------------------------------------

%% Start simulation - only from stopped state
handle_cast({start_simulation, MapModule}, State) when is_atom(MapModule) ->
    case State#state.simulation_state of
        stopped ->
            %% Fresh start - create everything new
            io:format("Control Center: Starting NEW simulation with map ~p~n", [MapModule]),
            start_fresh_simulation(MapModule, State);
        paused ->
            %% Should resume instead
            io:format("Control Center: Cannot start new simulation when paused. Use resume instead.~n"),
            {noreply, State};
        running ->
            io:format("Control Center: Simulation already running~n"),
            {noreply, State}
    end;

handle_cast({start_simulation}, State) ->
    handle_cast({start_simulation, State#state.current_map}, State);

%% Resume handler
handle_cast({resume_simulation}, State) ->
    case State#state.simulation_state of
        paused ->
            io:format("Control Center: Resuming simulation~n"),
            
            %% Use existing zones or find them
            ZonesToUse = case State#state.zones of
                [] -> find_connected_zones();
                Zones -> Zones
            end,
            
            broadcast_to_zones(ZonesToUse, {resume_simulation}),
            
            case State#state.visualization_node of
                undefined -> ok;
                VizNode ->
                    gen_server:cast({visualization_server, VizNode}, {resume_visualization})
            end,
            
            {noreply, State#state{simulation_state = running}};
        stopped ->
            io:format("Control Center: Cannot resume from stopped state. Starting fresh.~n"),
            handle_cast({start_simulation, State#state.current_map}, State);
        running ->
            io:format("Control Center: Simulation already running~n"),
            {noreply, State}
    end;

%% Pause simulation
handle_cast({pause_simulation}, State) ->
    case State#state.simulation_state of
        running ->
            io:format("Control Center: Pausing simulation~n"),
            
            %% Use existing zones or find them
            ZonesToUse = case State#state.zones of
                [] -> find_connected_zones();
                Zones -> Zones
            end,
            
            broadcast_to_zones(ZonesToUse, {pause_simulation}),
            
            case State#state.visualization_node of
                undefined -> ok;
                VizNode ->
                    gen_server:cast({visualization_server, VizNode}, {pause_visualization})
            end,
            
            {noreply, State#state{simulation_state = paused}};
        _ ->
            io:format("Control Center: Cannot pause - simulation not running~n"),
            {noreply, State}
    end;

%% Stop simulation - complete shutdown
handle_cast({stop_simulation}, State) ->
    case State#state.simulation_state of
        stopped ->
            io:format("Control Center: Already stopped~n"),
            {noreply, State};
        _ ->
            io:format("Control Center: Stopping simulation completely~n"),
            
            %% Use existing zones or find them
            ZonesToUse = case State#state.zones of
                [] -> find_connected_zones();
                Zones -> Zones
            end,
            
            io:format("Control Center: Broadcasting STOP to ~p zones: ~p~n", 
                      [length(ZonesToUse), ZonesToUse]),
            broadcast_to_zones(ZonesToUse, {stop_simulation}),
            
            %% Stop visualization
            case State#state.visualization_node of
                undefined -> ok;
                VizNode ->
                    gen_server:cast({visualization_server, VizNode}, {stop_visualization})
            end,
            
            %% Clean up map server
            case State#state.map_server_pid of
                undefined -> ok;
                Pid when is_pid(Pid) ->
                    io:format("Control Center: Stopping map_server~n"),
                    catch gen_server:stop(Pid)
            end,
            
            %% DON'T clear the zones list! Keep it for next time
            {noreply, State#state{
                simulation_state = stopped,
                map_server_pid = undefined,
                simulation_config = #{}
                %% zones = State#state.zones  <- Keep the zones!
            }}
    end;

handle_cast({set_mode, Mode}, State) when State#state.simulation_state == stopped ->
    io:format("Control Center: Setting mode to ~p~n", [Mode]),
    {noreply, State#state{simulation_mode = Mode}};

handle_cast({deploy_couriers, Num}, State) ->
    io:format("Control Center: Deploying ~p couriers per zone~n", [Num]),
    broadcast_to_zones(State#state.zones, {deploy_couriers, Num}),
    {noreply, State};

handle_cast({remove_couriers, Num}, State) ->
    io:format("Control Center: Removing ~p couriers per zone~n", [Num]),
    broadcast_to_zones(State#state.zones, {remove_couriers, Num}),
    {noreply, State};

handle_cast({update_load_factor, Value}, State) ->
    io:format("Control Center: Setting load factor to ~p%~n", [Value]),
    broadcast_to_zones(State#state.zones, {update_load_factor, Value}),
    {noreply, State};

handle_cast({update_stats, _ZoneNode, NewCouriers, NewDeliveries, NewFailures}, 
            State = #state{total_couriers = TC, active_couriers = AC, 
                          total_deliveries = TD, failed_deliveries = FD}) ->
    
    NewState = State#state{
        total_couriers = TC + NewCouriers,
        active_couriers = AC + NewCouriers - NewDeliveries - NewFailures,
        total_deliveries = TD + NewDeliveries,
        failed_deliveries = FD + NewFailures
    },
    
    case State#state.state_collector_pid of
        undefined -> ok;
        CollectorPid when is_pid(CollectorPid) ->
            logistics_state_collector:broadcast_message(#{
                type => <<"stats_update">>,
                total_zones => NewState#state.total_zones,
                total_couriers => NewState#state.total_couriers,
                active_couriers => NewState#state.active_couriers,
                total_deliveries => NewState#state.total_deliveries,
                failed_deliveries => NewState#state.failed_deliveries
            })
    end,
    
    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling info messages
%%--------------------------------------------------------------------
handle_info(check_and_reconnect_zones, State) ->
    %% Check if we have zones, if not try to find them
    NewZones = case State#state.zones of
        [] ->
            io:format("Control Center: No zones in list, searching...~n"),
            FoundZones = find_connected_zones(),
            case FoundZones of
                [] ->
                    io:format("Control Center: Still no zones found~n"),
                    [];
                _ ->
                    io:format("Control Center: Found ~p zones!~n", [length(FoundZones)]),
                    %% Register them
                    lists:foreach(fun({_, Node, Type}) ->
                        monitor_node(Node, true),
                        %% Update state collector
                        case State#state.state_collector_pid of
                            undefined -> ok;
                            CollectorPid when is_pid(CollectorPid) ->
                                logistics_state_collector:zone_state_changed(
                                    atom_to_list(Type), 
                                    #{status => live, node => Node, couriers => 0, deliveries => 0}
                                )
                        end
                    end, FoundZones),
                    FoundZones
            end;
        ExistingZones ->
            %% Verify existing zones are still alive
            lists:filtermap(fun({PID, Node, Type}) ->
                case net_adm:ping(Node) of
                    pong -> 
                        {true, {PID, Node, Type}};
                    pang ->
                        io:format("Control Center: Zone ~p (~p) is down, removing~n", [Type, Node]),
                        %% Update state collector
                        case State#state.state_collector_pid of
                            undefined -> ok;
                            CollectorPid when is_pid(CollectorPid) ->
                                logistics_state_collector:zone_state_changed(
                                    atom_to_list(Type),
                                    #{status => down, node => Node}
                                )
                        end,
                        false
                end
            end, ExistingZones)
    end,
    
    %% Try to add any missing zones
    AllPossibleZones = [
        {north, 'zone_north@127.0.0.1'},
        {center, 'zone_center@127.0.0.1'},
        {south, 'zone_south@127.0.0.1'}
    ],
    
    FinalZones = lists:foldl(fun({Type, Node}, AccZones) ->
        %% Check if this zone is already in our list
        case lists:keyfind(Node, 2, AccZones) of
            false ->
                %% Not in list, try to connect
                case net_adm:ping(Node) of
                    pong ->
                        io:format("Control Center: Reconnected to zone ~p at ~p~n", [Type, Node]),
                        monitor_node(Node, true),
                        [{undefined, Node, Type} | AccZones];
                    pang ->
                        AccZones
                end;
            _ ->
                %% Already in list
                AccZones
        end
    end, NewZones, AllPossibleZones),
    
    {noreply, State#state{
        zones = FinalZones,
        total_zones = length(FinalZones)
    }};

handle_info({nodedown, Node}, State = #state{visualization_node = VizNode, zones = Zones}) ->
    io:format("Control Center: Node ~p went down~n", [Node]),
    
    case Node of
        VizNode ->
            {noreply, State};
        _ ->
            case lists:keyfind(Node, 2, Zones) of
                {_PID, _Node, ZoneType} ->
                    io:format("Control Center: Zone ~p (~p) is down~n", [ZoneType, Node]),
                    case State#state.state_collector_pid of
                        undefined -> ok;
                        CollectorPid when is_pid(CollectorPid) ->
                            logistics_state_collector:zone_state_changed(atom_to_list(ZoneType), #{
                                status => down,
                                node => Node
                            })
                    end;
                false ->
                    ok
            end,
            {noreply, State}
    end;

handle_info({nodeup, Node}, State = #state{visualization_node = VizNode, zones = Zones}) ->
    io:format("Control Center: Node ~p came back up~n", [Node]),
    
    case Node of
        VizNode ->
            ok;
        _ ->
            case lists:keyfind(Node, 2, Zones) of
                {_PID, _Node, ZoneType} ->
                    io:format("Control Center: Zone ~p (~p) is back up~n", [ZoneType, Node]),
                    case State#state.state_collector_pid of
                        undefined -> ok;
                        CollectorPid when is_pid(CollectorPid) ->
                            logistics_state_collector:zone_state_changed(atom_to_list(ZoneType), #{
                                status => live,
                                node => Node
                            })
                    end;
                false ->
                    ok
            end
    end,
    {noreply, State};

handle_info(check_zones, State = #state{zones = Zones}) ->
    %% Report current status of all zones
    case Zones of
        [] ->
            io:format("Control Center: WARNING - No zones connected!~n");
        _ ->
            lists:foreach(fun({_PID, Node, ZoneType}) ->
                case net_adm:ping(Node) of
                    pong ->
                        %% Zone is alive - no need to log unless debugging
                        ok;
                    pang ->
                        io:format("Control Center: Zone ~p (~p) is not responding~n", [ZoneType, Node]),
                        case State#state.state_collector_pid of
                            undefined -> ok;
                            CollectorPid when is_pid(CollectorPid) ->
                                logistics_state_collector:zone_state_changed(
                                    atom_to_list(ZoneType), 
                                    #{status => down, node => Node}
                                )
                        end
                end
            end, Zones)
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Terminate
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    case State#state.map_server_pid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            catch gen_server:stop(Pid)
    end,
    
    case State#state.state_collector_pid of
        undefined -> ok;
        CollectorPid when is_pid(CollectorPid) ->
            catch gen_server:stop(CollectorPid)
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Code change
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

%% Start fresh simulation
start_fresh_simulation(MapModule, State) ->
    %% Make sure we have zones
    ZonesToUse = case State#state.zones of
        [] -> 
            io:format("Control Center: No zones connected, trying to find...~n"),
            find_connected_zones();
        Zones -> 
            Zones
    end,
    
    Config = #{
        map_module => MapModule,
        visualization_node => State#state.visualization_node,
        simulation_mode => State#state.simulation_mode,
        num_couriers_per_zone => 5,
        num_households_per_zone => 10
    },
    
    io:format("Control Center: Sending start_simulation to ~p zones~n", [length(ZonesToUse)]),
    
    timer:sleep(1000),
    
    broadcast_to_zones_with_config(ZonesToUse, {start_fresh_simulation, Config}),
    
    %% Start visualization
    case State#state.visualization_node of
        undefined -> ok;
        VizNode ->
            case net_adm:ping(VizNode) of
                pong ->
                    gen_server:cast({visualization_server, VizNode}, {start_visualization}),
                    gen_server:cast({visualization_server, VizNode}, {load_map, MapModule});
                pang ->
                    io:format("Control Center: Visualization node not reachable~n")
            end
    end,
    
    {noreply, State#state{
        simulation_state = running,
        current_map = MapModule,
        simulation_config = Config,
        zones = ZonesToUse  %% Make sure to keep the zones!
    }}.

%% Find connected zones
find_connected_zones() ->
    PossibleZones = [
        {'zone_north@127.0.0.1', north},
        {'zone_center@127.0.0.1', center},
        {'zone_south@127.0.0.1', south}
    ],
    lists:filtermap(fun({Node, Type}) ->
        case net_adm:ping(Node) of
            pong -> 
                io:format("Control Center: Found active zone ~p at ~p~n", [Type, Node]),
                {true, {undefined, Node, Type}};
            pang -> 
                false
        end
    end, PossibleZones).

%% Broadcast to zones with configuration
broadcast_to_zones_with_config([], _Message) -> 
    io:format("Control Center: No zones to broadcast to!~n"),
    ok;
broadcast_to_zones_with_config([{_PID, Node, Type}|Rest], Message) ->
    io:format("Control Center: Broadcasting ~p to zone ~p on ~p~n", [element(1, Message), Type, Node]),
    
    case net_adm:ping(Node) of
        pong ->
            gen_statem:cast({zone_manager, Node}, Message),
            timer:sleep(100); % Give it time to process
        pang ->
            io:format("Control Center: Zone ~p at ~p is not responding!~n", [Type, Node])
    end,
    
    broadcast_to_zones_with_config(Rest, Message).

%% Broadcast to zones
broadcast_to_zones([], Message) -> 
    io:format("Control Center: WARNING - No zones to broadcast to! Trying to find zones...~n"),
    %% Try to find zones if list is empty
    EmergencyZones = find_connected_zones(),
    case EmergencyZones of
        [] -> 
            io:format("Control Center: ERROR - No zones found at all!~n");
        _ ->
            io:format("Control Center: Found ~p zones, broadcasting...~n", [length(EmergencyZones)]),
            broadcast_to_zones_internal(EmergencyZones, Message)
    end;
broadcast_to_zones(Zones, Message) ->
    broadcast_to_zones_internal(Zones, Message).

broadcast_to_zones_internal([], _Message) -> ok;
broadcast_to_zones_internal([{_PID, Node, Type}|Rest], Message) ->
    io:format("Control Center: Broadcasting ~p to zone ~p on ~p~n", [Message, Type, Node]),
    
    %% Try multiple times with different approaches
    case net_adm:ping(Node) of
        pong ->
            %% Try both gen_statem and direct call
            gen_statem:cast({zone_manager, Node}, Message),
            %% Also try direct message passing as backup
            case rpc:call(Node, erlang, whereis, [zone_manager], 1000) of
                Pid when is_pid(Pid) ->
                    Pid ! {'$gen_cast', Message};
                _ ->
                    ok
            end;
        pang ->
            io:format("Control Center: Zone ~p at ~p is not responding!~n", [Type, Node])
    end,
    
    broadcast_to_zones_internal(Rest, Message).