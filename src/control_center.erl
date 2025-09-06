%%%-------------------------------------------------------------------
%%% @doc Control Center - Fixed for distributed zone management
%%%-------------------------------------------------------------------
-module(control_center).
-behaviour(gen_server).

-include("header.hrl").

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
    simulation_state = stopped,
    simulation_mode = visual,
    current_map = map_data_100,
    zone_monitors = #{},
    map_server_pid = undefined,
    state_collector_pid = undefined
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
            %% המתן קצת כדי לוודא שהוא נרשם גלובלית
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
                %% Launch dashboard on remote node via RPC
                case rpc:call(VisualizationNode, dashboard_server, start, []) of
                    {badrpc, RPCReason} ->
                        io:format("Control Center: Failed to start dashboard on ~p: ~p~n", 
                                 [VisualizationNode, RPCReason]);
                    {wx_ref, _, _, _} ->
                        io:format("Control Center: Dashboard started on ~p~n", [VisualizationNode]);
                    Other ->
                        io:format("Control Center: Unexpected dashboard start result: ~p~n", [Other])
                end,
                
                %% Also start visualization server on the same node
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
    
    %% Start a timer to check zone connections
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
    
    %% Determine zone type from node name
    ZoneType = case atom_to_list(ZoneNode) of
        "zone_north" ++ _ -> north;
        "zone_center" ++ _ -> center;
        "zone_south" ++ _ -> south;
        _ -> unknown
    end,
    
    %% Monitor the zone node
    monitor_node(ZoneNode, true),
    
    %% Add to zones list
    NewZone = {ZoneNode, ZoneNode, ZoneType},
    UpdatedZones = [NewZone | CurrentZones],
    
    %% Report to state collector if available
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
    
    io:format("Control Center: Zone ~p (~p) connected successfully~n", [ZoneType, ZoneNode]),
    {reply, ok, State#state{zones = UpdatedZones, total_zones = length(UpdatedZones)}};

handle_call(shutdown, _From, State) ->
    io:format("Control Center: Shutting down~n"),
    {stop, shutdown, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%%--------------------------------------------------------------------
handle_cast({start_simulation, MapModule}, State) when is_atom(MapModule) ->
    case State#state.simulation_state of
        running ->
            io:format("Control Center: Simulation already running, ignoring start request~n"),
            {noreply, State};
        _ ->
            io:format("Control Center: Starting simulation with map ~p~n", [MapModule]),
            io:format("Control Center: Currently have ~p zones connected~n", [length(State#state.zones)]),
            
            Config = #{
                map_module => MapModule,
                visualization_node => State#state.visualization_node
            },
            
            case start_simulation_components(Config, State) of
                {ok, NewState} ->
                    io:format("Control Center: Sending start_simulation to ~p connected zones~n", 
                              [length(NewState#state.zones)]),
                    
                    timer:sleep(1000),
                    
                    broadcast_to_zones(NewState#state.zones, {start_simulation}),
                    
                    case NewState#state.visualization_node of
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
                    
                    {noreply, NewState#state{
                        simulation_state = running,
                        current_map = MapModule
                    }};
                {error, Reason} ->
                    io:format("Control Center: Failed to start simulation - ~p~n", [Reason]),
                    {noreply, State}
            end
    end;

handle_cast({start_simulation}, State) ->
    handle_cast({start_simulation, State#state.current_map}, State);

handle_cast({pause_simulation}, State) ->
    io:format("Control Center: Pausing simulation~n"),
    broadcast_to_zones(State#state.zones, {pause_simulation}),
    
    case State#state.visualization_node of
        undefined -> ok;
        VizNode ->
            gen_server:cast({visualization_server, VizNode}, {pause_visualization})
    end,
    
    {noreply, State#state{simulation_state = paused}};

handle_cast({stop_simulation}, State) ->
    io:format("Control Center: Stopping simulation~n"),
    
    broadcast_to_zones(State#state.zones, {stop_simulation}),
    
    case State#state.visualization_node of
        undefined -> ok;
        VizNode ->
            gen_server:cast({visualization_server, VizNode}, {stop_visualization})
    end,
    
    case State#state.map_server_pid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            io:format("Control Center: Stopping map_server~n"),
            catch gen_server:stop(Pid)
    end,
    
    {noreply, State#state{
        simulation_state = stopped,
        map_server_pid = undefined
    }};

handle_cast({set_mode, Mode}, State) when State#state.simulation_state == stopped ->
    io:format("Control Center: Setting mode to ~p~n", [Mode]),
    {noreply, State#state{simulation_mode = Mode}};

handle_cast({deploy_couriers, Num}, State) ->
    io:format("Control Center: Deploying ~p couriers~n", [Num]),
    broadcast_to_zones(State#state.zones, {deploy_couriers, Num}),
    {noreply, State};

handle_cast({remove_couriers, Num}, State) ->
    io:format("Control Center: Removing ~p couriers~n", [Num]),
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
    lists:foreach(fun({_PID, Node, ZoneType}) ->
        case net_adm:ping(Node) of
            pong ->
                ok;
            pang ->
                io:format("Control Center: Zone ~p (~p) is not responding~n", [ZoneType, Node]),
                case State#state.state_collector_pid of
                    undefined -> ok;
                    CollectorPid when is_pid(CollectorPid) ->
                        logistics_state_collector:zone_state_changed(atom_to_list(ZoneType), #{
                            status => down,
                            node => Node
                        })
                end
        end
    end, Zones),
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

%% Start simulation components including map_server
start_simulation_components(Config, State) ->
    try
        io:format("Control Center: Starting map_server...~n"),
        
        case map_server:start_link() of
            {ok, MapServerPid} ->
                io:format("Control Center: Map server started successfully~n"),
                
                MapModule = maps:get(map_module, Config),
                case map_server:initialize_map_from_module(MapModule) of
                    {ok, map_initialized} ->
                        io:format("Control Center: Map initialized from module ~p~n", [MapModule]),
                        {ok, State#state{map_server_pid = MapServerPid}};
                    {error, InitError} ->
                        io:format("Control Center: Failed to initialize map - ~p~n", [InitError]),
                        gen_server:stop(MapServerPid),
                        {error, {map_initialization_failed, InitError}}
                end;
            {error, {already_started, MapServerPid}} ->
                io:format("Control Center: Map server already running, reinitializing...~n"),
                MapModule = maps:get(map_module, Config),
                case map_server:initialize_map_from_module(MapModule) of
                    {ok, map_initialized} ->
                        io:format("Control Center: Map reinitialized from module ~p~n", [MapModule]),
                        {ok, State#state{map_server_pid = MapServerPid}};
                    {error, InitError} ->
                        io:format("Control Center: Failed to initialize map - ~p~n", [InitError]),
                        {error, {map_initialization_failed, InitError}}
                end;
            {error, StartError} ->
                io:format("Control Center: Failed to start map_server - ~p~n", [StartError]),
                {error, {map_server_start_failed, StartError}}
        end
    catch
        Type:Error ->
            io:format("Control Center: Error starting simulation components - ~p:~p~n", [Type, Error]),
            {error, {simulation_start_failed, Error}}
    end.

%% Broadcast to zones using node names
broadcast_to_zones([], _Message) -> 
    io:format("Control Center: No zones to broadcast to!~n"),
    ok;
broadcast_to_zones([{_PID, Node, Type}|Rest], Message) ->
    io:format("Control Center: Broadcasting ~p to zone ~p on ~p~n", [Message, Type, Node]),
    gen_statem:cast({zone_manager, Node}, Message),
    broadcast_to_zones(Rest, Message).