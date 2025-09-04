%%%-------------------------------------------------------------------
%%% @doc Control Center - Clean working version
%%%-------------------------------------------------------------------
-module(control_center).
-behaviour(gen_server).

-include("header.hrl").

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    dashboard_pid,
    visualization_node,
    zones = [],
    total_zones = 0,
    total_couriers = 0,
    active_couriers = 0,
    total_deliveries = 0,
    failed_deliveries = 0,
    simulation_state = stopped,
    simulation_mode = visual
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
    
    %% Start dashboard
    DashboardPID = try
        case dashboard_server:start() of
            {wx_ref, _, _, DashPid} -> 
                io:format("Control Center: Dashboard started~n"),
                DashPid;
            _ -> 
                undefined
        end
    catch
        _:_ ->
            io:format("Control Center: Dashboard not available~n"),
            undefined
    end,
    
    %% Initialize ETS tables
    ets:new(simulation_state, [set, named_table, public]),
    ets:new(zone_stats, [set, named_table, public]),
    
    %% Connect to visualization
    try
        gen_server:call({visualization_server, VisualizationNode}, {connect, node()}, 5000),
        io:format("Control Center: Connected to visualization~n")
    catch
        _:_ ->
            io:format("Control Center: Visualization not responding~n")
    end,
    
    %% Update dashboard if exists
    case DashboardPID of
        undefined -> ok;
        ValidPid when is_pid(ValidPid) ->
            wx_object:cast(ValidPid, {update_nodes, node(), VisualizationNode})
    end,
    
    {ok, #state{
        dashboard_pid = DashboardPID,
        visualization_node = VisualizationNode,
        simulation_state = stopped,
        simulation_mode = visual
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%%--------------------------------------------------------------------
handle_call({connect, ZoneNode}, From, State = #state{zones = Zones}) ->
    {PID, _} = From,
    io:format("Control Center: Zone ~p connected~n", [ZoneNode]),
    
    %% Update dashboard if exists
    case State#state.dashboard_pid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            wx_object:cast(Pid, {update_zone_status, ZoneNode, live, 0, 0})
    end,
    
    NewZones = Zones ++ [{PID, ZoneNode, 0, 0}],
    {reply, ok, State#state{zones = NewZones, total_zones = length(NewZones)}};

handle_call(shutdown, _From, State) ->
    io:format("Control Center: Shutting down~n"),
    {stop, shutdown, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%%--------------------------------------------------------------------
handle_cast({start_simulation}, State) ->
    io:format("Control Center: Starting simulation in ~p mode~n", [State#state.simulation_mode]),
    
    %% Send to zones
    broadcast_to_zones(State#state.zones, {start_simulation}),
    
    %% Send to visualization - THIS IS THE IMPORTANT PART!
    io:format("Control Center: Telling visualization to start ~p mode~n", [State#state.simulation_mode]),
    gen_server:cast({visualization_server, State#state.visualization_node}, 
                    {start_visualization, State#state.simulation_mode}),
    
    {noreply, State#state{simulation_state = running}};

handle_cast({pause_simulation}, State) ->
    io:format("Control Center: Pausing simulation~n"),
    
    %% Send to zones
    broadcast_to_zones(State#state.zones, {pause_simulation}),
    
    %% Send to visualization
    gen_server:cast({visualization_server, State#state.visualization_node}, 
                    {pause_visualization}),
    
    {noreply, State#state{simulation_state = paused}};

handle_cast({stop_simulation}, State) ->
    io:format("Control Center: Stopping simulation~n"),
    
    %% Send to zones
    broadcast_to_zones(State#state.zones, {stop_simulation}),
    
    %% Send to visualization - this will close the window
    io:format("Control Center: Telling visualization to stop~n"),
    gen_server:cast({visualization_server, State#state.visualization_node}, 
                    {stop_visualization}),
    
    {noreply, State#state{simulation_state = stopped}};

handle_cast({set_mode, Mode}, State) when State#state.simulation_state == stopped ->
    io:format("Control Center: Setting mode to ~p~n", [Mode]),
    {noreply, State#state{simulation_mode = Mode}};

handle_cast({deploy_couriers, Num}, State) ->
    io:format("Control Center: Deploying ~p couriers~n", [Num]),
    broadcast_to_zones(State#state.zones, {deploy_couriers, Num}),
    {noreply, State};

handle_cast({update_load_factor, Value}, State) ->
    io:format("Control Center: Setting load factor to ~p%~n", [Value]),
    broadcast_to_zones(State#state.zones, {update_load_factor, Value}),
    {noreply, State};

handle_cast({update_households, Count}, State) ->
    io:format("Control Center: Setting households to ~p~n", [Count]),
    broadcast_to_zones(State#state.zones, {set_households, Count}),
    {noreply, State};

handle_cast({update_stats, ZoneNode, NewCouriers, NewDeliveries, NewFailures}, 
            State = #state{total_couriers = TC, active_couriers = AC, 
                          total_deliveries = TD, failed_deliveries = FD}) ->
    
    NewState = State#state{
        total_couriers = TC + NewCouriers,
        active_couriers = AC + NewCouriers - NewDeliveries - NewFailures,
        total_deliveries = TD + NewDeliveries,
        failed_deliveries = FD + NewFailures
    },
    
    %% Update dashboard if exists
    case State#state.dashboard_pid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            wx_object:cast(Pid, {update_stats, 
                NewState#state.total_zones,
                NewState#state.total_couriers,
                NewState#state.active_couriers,
                NewState#state.total_deliveries,
                NewState#state.failed_deliveries})
    end,
    
    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling info messages
%%--------------------------------------------------------------------
handle_info({nodedown, Node}, State) ->
    io:format("Control Center: Node ~p down~n", [Node]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Terminate
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
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
broadcast_to_zones([], _Message) -> ok;
broadcast_to_zones([{PID, _Node, _, _}|Rest], Message) ->
    gen_server:cast(PID, Message),
    broadcast_to_zones(Rest, Message).