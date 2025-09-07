%%%-------------------------------------------------------------------
%%% Zone Manager - Fixed with proper PAUSE/RESUME/STOP handling
%%% and smart courier removal
%%%-------------------------------------------------------------------
-module(zone_manager).
-behaviour(gen_statem).
-include("network_const.hrl").


-include("header.hrl").
-include("map_records.hrl").


%% API
-export([start/1, new_package/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

-record(state, {
    control_node,
    visualization_node = ?VIZ_NODE,
    zone_id,
    zone_center,
    zone_bounds,
    zone,
    waiting_packages = [],
    active_deliveries = 0,
    total_deliveries = 0,
    failed_deliveries = 0,
    total_orders = 0,
    household_pids = [],
    available_couriers = [],
    busy_couriers = [],
    location_tracker_pid = undefined,
    components_created = false,
    simulation_state = stopped,  % stopped | running | paused
    package_order_counts = #{},
    couriers_to_remove = 0  % NEW: Number of couriers to remove when they return
}).

%% Define zone configurations
-define(ZONE_CONFIG, #{
    zone_north => #{center => {50, 20}, bounds => {0, 0, 100, 33}},
    zone_center => #{center => {50, 50}, bounds => {0, 34, 100, 66}},
    zone_south => #{center => {50, 80}, bounds => {0, 67, 100, 100}}
}).

%%====================================================================
%% API
%%====================================================================

start(ControlNode) ->
    gen_statem:start_link({local, zone_manager}, ?MODULE, [ControlNode], []).

new_package(Zone, PackageId) ->
    io:format("API: Sending new_package ~p to zone ~p~n", [PackageId, Zone]),
    gen_statem:cast(zone_manager, {new_package, PackageId}).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() -> handle_event_function.

init([ControlNode]) ->
    io:format("Zone Manager ~p: Starting up as FSM with control node ~p...~n", [node(), ControlNode]),
    
    %% Determine zone configuration based on node name
    {ZoneName, ZoneAtom} = case atom_to_list(node()) of
        "zone_north" ++ _ -> {zone_north, north};
        "zone_center" ++ _ -> {zone_center, center};
        "zone_south" ++ _ -> {zone_south, south};
        _ -> {zone_north, north}
    end,
    
    #{center := ZoneCenter, bounds := ZoneBounds} = maps:get(ZoneName, ?ZONE_CONFIG),
    
    io:format("Zone Manager ~p: Zone center at ~p, bounds ~p~n", 
              [node(), ZoneCenter, ZoneBounds]),
    
    %% Initialize ETS for zone statistics
    case ets:info(zone_stats) of
        undefined -> ets:new(zone_stats, [set, named_table, public, 
                                          {read_concurrency, true}, 
                                          {write_concurrency, true}]);
        _ -> ok
    end,
    ets:insert(zone_stats, {deliveries, 0}),
    ets:insert(zone_stats, {failures, 0}),
    ets:insert(zone_stats, {couriers_active, 0}),
    ets:insert(zone_stats, {update_stats, false}),
    
    %% Connect to control center
    io:format("Zone Manager ~p: Connecting to control center at ~p~n", [node(), ControlNode]),
    case gen_server:call({control_center, ControlNode}, {connect, node()}) of
        ok ->
            io:format("Zone Manager ~p: Successfully connected to control center~n", [node()]);
        Error ->
            io:format("Zone Manager ~p: Failed to connect to control center: ~p~n", [node(), Error])
    end,
    
    %% Create initial state record
    InitialState = #state{
        control_node = ControlNode,
        visualization_node = ?VIZ_NODE,
        zone_id = node(),
        zone_center = ZoneCenter,
        zone_bounds = ZoneBounds,
        zone = atom_to_list(ZoneAtom),
        waiting_packages = [],
        active_deliveries = 0,
        total_deliveries = 0,
        failed_deliveries = 0,
        total_orders = 0,
        household_pids = [],
        available_couriers = [],
        busy_couriers = [],
        location_tracker_pid = undefined,
        components_created = false,
        simulation_state = stopped,
        package_order_counts = #{},
        couriers_to_remove = 0
    },
    
    %% Report initial zone state
    report_zone_state(atom_to_list(ZoneAtom), InitialState),
    
    {ok, monitoring, InitialState}.

%%====================================================================
%% State Machine Event Handlers
%%====================================================================

%% Handle direct cast messages
handle_event(info, {'$gen_cast', Message}, StateName, Data) ->
    io:format("Zone Manager ~p: Received direct cast message: ~p in state ~p~n", 
              [Data#state.zone_id, Message, StateName]),
    %% Forward to regular cast handler
    handle_event(cast, Message, StateName, Data);

%% Handle retry of state reporting
handle_event(info, {retry_report_state, Zone, ReportData}, _StateName, CurrentState) ->
    report_zone_state(Zone, ReportData),
    {keep_state, CurrentState};

%% Start fresh simulation - create all components
handle_event(cast, {start_fresh_simulation, Config}, _StateName, Data) ->
    io:format("Zone Manager ~p: Starting FRESH simulation~n", [Data#state.zone_id]),
    
    %% Clean up any existing components first
    cleanup_existing_components(Data),
    
    %% Load map
    MapModule = maps:get(map_module, Config, map_data_100),
    case map_server:initialize_local_map(MapModule) of
        {ok, map_initialized} ->
            io:format("Zone Manager ~p: Successfully loaded local map~n", [Data#state.zone_id]);
        {error, MapError} ->
            io:format("Zone Manager ~p: Failed to load local map: ~p~n", [Data#state.zone_id, MapError])
    end,
    
    %% Create new location tracker
    LocationTrackerPid = case location_tracker:start_link() of
        {ok, TrackerPid} ->
            io:format("Zone Manager ~p: Started location tracker~n", [Data#state.zone_id]),
            TrackerPid;
        {error, {already_started, _}} ->
            %% Stop existing and create new
            catch gen_server:stop(location_tracker),
            timer:sleep(100),
            case location_tracker:start_link() of
                {ok, NewPid} -> NewPid;
                _ -> undefined
            end;
        _ ->
            undefined
    end,
    
    %% Create new couriers
    NumCouriers = maps:get(num_couriers_per_zone, Config, 5),
    NewCouriers = create_fresh_couriers(Data#state.zone, NumCouriers, self(), LocationTrackerPid),
    
    %% Create new households
    NewHouseholds = create_fresh_households(Data#state.zone, self()),
    
    InitialCounts = lists:foldl(fun({Id, _Pid}, Acc) ->
        maps:put(Id, 0, Acc)
    end, #{}, NewHouseholds),
    
    {next_state, monitoring, Data#state{
        simulation_state = running,
        components_created = true,
        household_pids = NewHouseholds,
        available_couriers = NewCouriers,
        busy_couriers = [],
        location_tracker_pid = LocationTrackerPid,
        package_order_counts = InitialCounts,
        waiting_packages = [],
        active_deliveries = 0,
        couriers_to_remove = 0  % Reset counter
    }};

%% OLD handler for backward compatibility - redirects to fresh start
handle_event(cast, {start_simulation, Config}, _StateName, Data) ->
    case Data#state.simulation_state of
        stopped ->
            %% If stopped, do fresh start
            handle_event(cast, {start_fresh_simulation, Config}, _StateName, Data);
        paused ->
            %% If paused, should resume instead
            io:format("Zone Manager ~p: Ignoring start_simulation while paused. Waiting for resume.~n", 
                      [Data#state.zone_id]),
            {keep_state, Data};
        running ->
            io:format("Zone Manager ~p: Already running~n", [Data#state.zone_id]),
            {keep_state, Data}
    end;

handle_event(cast, {start_simulation}, StateName, Data) ->
    DefaultConfig = #{
        num_couriers_per_zone => 5,
        num_households_per_zone => 10
    },
    handle_event(cast, {start_simulation, DefaultConfig}, StateName, Data);

%% Resume handler
handle_event(cast, {resume_simulation}, _StateName, Data) ->
    case Data#state.simulation_state of
        paused ->
            io:format("Zone Manager ~p: Resuming simulation~n", [Data#state.zone_id]),
            
            %% Resume households
            lists:foreach(fun({_Id, Pid}) ->
                gen_server:cast(Pid, resume_simulation)
            end, Data#state.household_pids),
            
            %% Resume ALL couriers
            AllCouriers = Data#state.available_couriers ++ Data#state.busy_couriers,
            lists:foreach(fun({CourierId, _Pid}) ->
                courier:resume(CourierId)
            end, AllCouriers),
            
            %% Resume location tracker
            case Data#state.location_tracker_pid of
                undefined -> ok;
                TrackerPid -> location_tracker:resume()
            end,
            
            {keep_state, Data#state{simulation_state = running}};
        stopped ->
            io:format("Zone Manager ~p: Cannot resume from stopped state~n", [Data#state.zone_id]),
            {keep_state, Data};
        running ->
            io:format("Zone Manager ~p: Already running~n", [Data#state.zone_id]),
            {keep_state, Data}
    end;

%% Pause simulation
handle_event(cast, {pause_simulation}, _StateName, Data) ->
    case Data#state.simulation_state of
        running ->
            io:format("Zone Manager ~p: Pausing simulation~n", [Data#state.zone_id]),
            
            %% Pause households
            lists:foreach(fun({_Id, Pid}) ->
                gen_server:cast(Pid, pause_simulation)
            end, Data#state.household_pids),
            
            %% Pause ALL couriers
            AllCouriers = Data#state.available_couriers ++ Data#state.busy_couriers,
            lists:foreach(fun({CourierId, _Pid}) ->
                courier:pause(CourierId)
            end, AllCouriers),
            
            %% Pause location tracker
            case Data#state.location_tracker_pid of
                undefined -> ok;
                TrackerPid -> location_tracker:pause()
            end,
            
            {keep_state, Data#state{simulation_state = paused}};
        _ ->
            io:format("Zone Manager ~p: Cannot pause - not running~n", [Data#state.zone_id]),
            {keep_state, Data}
    end;

%% Stop simulation - complete cleanup
handle_event(cast, {stop_simulation}, StateName, Data) ->
    io:format("Zone Manager ~p: EXECUTING STOP - Stopping simulation completely (state: ~p)~n", 
              [Data#state.zone_id, StateName]),
    
    %% Force stop all households
    io:format("Zone Manager ~p: Stopping ~p households~n", 
              [Data#state.zone_id, length(Data#state.household_pids)]),
    lists:foreach(fun({Id, Pid}) ->
        try
            gen_server:stop(Pid, normal, 1000)
        catch
            _:_ -> 
                %% Force kill if normal stop fails
                exit(Pid, kill)
        end
    end, Data#state.household_pids),
    
    %% Force stop all couriers
    AllCouriers = Data#state.available_couriers ++ Data#state.busy_couriers,
    io:format("Zone Manager ~p: Stopping ~p couriers~n", 
              [Data#state.zone_id, length(AllCouriers)]),
    lists:foreach(fun({Id, Pid}) ->
        try
            gen_statem:stop(Pid, normal, 1000)
        catch
            _:_ -> 
                %% Force kill if normal stop fails
                exit(Pid, kill)
        end
    end, AllCouriers),
    
    %% Force stop location tracker
    case Data#state.location_tracker_pid of
        undefined -> ok;
        TrackerPid -> 
            io:format("Zone Manager ~p: Stopping location tracker~n", [Data#state.zone_id]),
            try
                gen_server:stop(TrackerPid, normal, 1000)
            catch
                _:_ -> exit(TrackerPid, kill)
            end
    end,
    
    %% Clear all waiting packages
    io:format("Zone Manager ~p: Clearing ~p waiting packages~n", 
              [Data#state.zone_id, length(Data#state.waiting_packages)]),
    
    %% Report final state
    report_zone_state(Data#state.zone, Data#state{
        simulation_state = stopped,
        available_couriers = [],
        busy_couriers = [],
        waiting_packages = [],
        active_deliveries = 0
    }),
    
    %% Move to monitoring state with clean slate
    {next_state, monitoring, Data#state{
        simulation_state = stopped,
        components_created = false,
        available_couriers = [],
        busy_couriers = [],
        location_tracker_pid = undefined,
        household_pids = [],
        waiting_packages = [],
        package_order_counts = #{},
        active_deliveries = 0,
        couriers_to_remove = 0  % Reset counter
    }};

%% Handle courier becoming available - with removal check
handle_event(cast, {courier_available, CourierId}, monitoring, Data) ->
    io:format("Zone Manager ~p: Courier ~p is now available~n", [Data#state.zone_id, CourierId]),
    
    %% Check if we should remove this courier
    case Data#state.couriers_to_remove > 0 of
        true ->
            %% This courier should be removed
            io:format("Zone Manager ~p: REMOVING returning courier ~p (marked for removal)~n", 
                     [Data#state.zone_id, CourierId]),
            
            %% Find and remove the courier
            case lists:keytake(CourierId, 1, Data#state.busy_couriers) of
                {value, {CourierId, CourierPid}, RestBusy} ->
                    %% Stop the courier process
                    catch gen_statem:stop(CourierPid),
                    
                    %% Update the counter
                    NewData = Data#state{
                        busy_couriers = RestBusy,
                        couriers_to_remove = Data#state.couriers_to_remove - 1
                    },
                    
                    io:format("Zone Manager ~p: Successfully removed courier ~p. ~p more to remove.~n", 
                             [Data#state.zone_id, CourierId, NewData#state.couriers_to_remove]),
                    
                    %% Report updated state
                    report_zone_state(Data#state.zone, NewData),
                    
                    {keep_state, NewData};
                false ->
                    io:format("Zone Manager ~p: WARNING - Courier ~p not found in busy list~n", 
                             [Data#state.zone_id, CourierId]),
                    {keep_state, Data}
            end;
        
        false ->
            %% Normal flow - move courier from busy to available
            NewData = case lists:keytake(CourierId, 1, Data#state.busy_couriers) of
                {value, CourierTuple, RestBusy} ->
                    io:format("Zone Manager ~p: Moving courier ~p from busy to available pool~n", 
                             [Data#state.zone_id, CourierId]),
                    Data#state{
                        available_couriers = [CourierTuple | Data#state.available_couriers],
                        busy_couriers = RestBusy
                    };
                false ->
                    io:format("Zone Manager ~p: WARNING - Courier ~p not found in busy list~n", 
                             [Data#state.zone_id, CourierId]),
                    Data
            end,
            
            %% Check if we have waiting packages and assign immediately
            case NewData#state.waiting_packages of
                [PackageId | RestPackages] ->
                    io:format("Zone Manager ~p: Immediately assigning waiting package ~p to courier ~p~n", 
                             [Data#state.zone_id, PackageId, CourierId]),
                    
                    [{CourierId, CourierPid} | RestAvailable] = NewData#state.available_couriers,
                    
                    gen_statem:cast(CourierPid, {assign_delivery, PackageId, Data#state.zone}),
                    
                    FinalData = NewData#state{
                        waiting_packages = RestPackages,
                        available_couriers = RestAvailable,
                        busy_couriers = [{CourierId, CourierPid} | NewData#state.busy_couriers],
                        active_deliveries = NewData#state.active_deliveries + 1
                    },
                    
                    {keep_state, FinalData};
                [] ->
                    {keep_state, NewData}
            end
    end;

%% Handle new package from household
handle_event(info, {new_package, _HouseholdPid, HouseholdId, PackageId}, monitoring, Data) ->
    handle_new_household_package_fixed(HouseholdId, PackageId, Data);

handle_event(cast, {new_package, _HouseholdPid, HouseholdId, PackageId}, monitoring, Data) ->
    handle_new_household_package_fixed(HouseholdId, PackageId, Data);

%% Handle new package (partner's version logic)
handle_event(cast, {new_package, PackageId}, monitoring, Data) ->
    Zone = Data#state.zone,
    io:format("Zone(~p) received new package: ~p~n", [Zone, PackageId]),
    
    TotalOrders = Data#state.total_orders + 1,
    DataWithTotal = Data#state{total_orders = TotalOrders},
    
    case rpc:call(Data#state.control_node, package, start_link, [PackageId, Zone]) of
        {ok, _} ->
            io:format("Zone(~p): Package ~p process started on control node~n", [Zone, PackageId]);
        {badrpc, Reason} ->
            io:format("Zone(~p): Failed to start package ~p via RPC: ~p~n", [Zone, PackageId, Reason]);
        Error ->
            io:format("Zone(~p): Failed to start package ~p: ~p~n", [Zone, PackageId, Error])
    end,
    
    AssignedData = try_assign_package_to_available_courier(PackageId, DataWithTotal),
    
    report_zone_state(Zone, AssignedData),
    {keep_state, AssignedData};

%% Handle assignment failure
handle_event(cast, {assignment_failed, PackageId, CourierId}, monitoring, Data) ->
    io:format("Zone Manager ~p: Courier ~p rejected package ~p, re-queueing~n", 
              [Data#state.zone_id, CourierId, PackageId]),
    
    NewData = Data#state{
        waiting_packages = Data#state.waiting_packages ++ [PackageId]
    },
    
    {keep_state, NewData};

%% Handle delivery complete
handle_event(cast, {package_delivered, PackageId, CourierId}, monitoring, Data) ->
    Zone = Data#state.zone,
    io:format("Zone(~p): Package ~p delivered by courier ~p!~n", [Zone, PackageId, CourierId]),
    Total = Data#state.total_deliveries,
    ActiveDeliveries = Data#state.active_deliveries,
    
    UpdatedCounts = case string:tokens(PackageId, "_") of
        [_Zone, HouseholdId | _] ->
            CurrentCount = maps:get(HouseholdId, Data#state.package_order_counts, 1),
            maps:put(HouseholdId, max(0, CurrentCount - 1), Data#state.package_order_counts);
        _ ->
            Data#state.package_order_counts
    end,
    
    NewData = Data#state{
        total_deliveries = Total + 1,
        active_deliveries = max(0, ActiveDeliveries - 1),
        package_order_counts = UpdatedCounts
    },
    report_zone_state(Zone, NewData),
    {keep_state, NewData};

%% Handle load factor update
handle_event(cast, {update_load_factor, Value}, _StateName, Data) ->
    io:format("Zone Manager ~p: Received load factor update: ~p%~n", [Data#state.zone_id, Value]),
    
    lists:foreach(fun({_Id, Pid}) ->
        gen_server:cast(Pid, {update_load_factor, Value})
    end, Data#state.household_pids),
    
    io:format("Zone Manager ~p: Updated load factor for ~p households~n", 
              [Data#state.zone_id, length(Data#state.household_pids)]),
    
    {keep_state, Data};

%% Deploy additional couriers
handle_event(cast, {deploy_couriers, Num}, _StateName, Data) ->
    io:format("Zone Manager ~p: Deploying ~p additional couriers~n", [Data#state.zone_id, Num]),
    
    ZoneManagerPid = self(),
    TrackerPid = Data#state.location_tracker_pid,
    CurrentCourierCount = length(Data#state.available_couriers) + length(Data#state.busy_couriers),
    
    NewCourierPids = lists:map(fun(N) ->
        CourierId = Data#state.zone ++ "_courier_" ++ integer_to_list(CurrentCourierCount + N),
        case courier:start_link(CourierId, ZoneManagerPid, TrackerPid) of
            {ok, Pid} ->
                io:format("Zone Manager ~p: Started additional courier ~p (PID: ~p)~n", 
                          [Data#state.zone_id, CourierId, Pid]),
                {CourierId, Pid};
            Error ->
                io:format("Zone Manager ~p: Failed to start courier ~p: ~p~n", 
                          [Data#state.zone_id, CourierId, Error]),
                {CourierId, undefined}
        end
    end, lists:seq(1, Num)),
    
    ValidNewCouriers = [{Id, Pid} || {Id, Pid} <- NewCourierPids, Pid =/= undefined],
    
    UpdatedAvailableCouriers = Data#state.available_couriers ++ ValidNewCouriers,
    
    ets:update_counter(zone_stats, couriers_active, length(ValidNewCouriers)),
    
    report_zone_state(Data#state.zone, Data#state{available_couriers = UpdatedAvailableCouriers}),
    
    {keep_state, Data#state{available_couriers = UpdatedAvailableCouriers}};

%% Remove couriers - FIXED with marking for removal
handle_event(cast, {remove_couriers, Num}, _StateName, Data) ->
    io:format("Zone Manager ~p: Request to remove ~p couriers~n", [Data#state.zone_id, Num]),
    
    %% Try to remove from available couriers first
    AvailableCount = length(Data#state.available_couriers),
    
    if
        AvailableCount >= Num ->
            %% We have enough available couriers to remove
            {ToRemove, ToKeep} = lists:split(Num, Data#state.available_couriers),
            
            lists:foreach(fun({CourierId, Pid}) ->
                io:format("Zone Manager ~p: Removing available courier ~p~n", 
                         [Data#state.zone_id, CourierId]),
                catch gen_statem:stop(Pid)
            end, ToRemove),
            
            NewData = Data#state{
                available_couriers = ToKeep,
                couriers_to_remove = 0
            },
            
            report_zone_state(Data#state.zone, NewData),
            
            {keep_state, NewData};
        
        AvailableCount > 0 ->
            %% Remove what we can from available, mark the rest for removal
            {ToRemove, ToKeep} = lists:split(AvailableCount, Data#state.available_couriers),
            RemainingToRemove = Num - AvailableCount,
            
            lists:foreach(fun({CourierId, Pid}) ->
                io:format("Zone Manager ~p: Removing available courier ~p~n", 
                         [Data#state.zone_id, CourierId]),
                catch gen_statem:stop(Pid)
            end, ToRemove),
            
            io:format("Zone Manager ~p: Marked ~p couriers for removal when they return~n", 
                     [Data#state.zone_id, RemainingToRemove]),
            
            NewData = Data#state{
                available_couriers = ToKeep,
                couriers_to_remove = RemainingToRemove
            },
            
            report_zone_state(Data#state.zone, NewData),
            
            {keep_state, NewData};
        
        true ->
            %% No available couriers, mark all for removal when they return
            io:format("Zone Manager ~p: No available couriers. Marked ~p for removal when they return~n", 
                     [Data#state.zone_id, Num]),
            
            NewData = Data#state{
                couriers_to_remove = Data#state.couriers_to_remove + Num
            },
            
            report_zone_state(Data#state.zone, NewData),
            
            {keep_state, NewData}
    end;

%% Get statistics
handle_event({call, From}, get_stats, _StateName, Data) ->
    WaitingPackages = Data#state.waiting_packages,
    Stats = #{
        zone => Data#state.zone,
        waiting_packages => WaitingPackages,
        waiting_count => length(WaitingPackages),
        active_deliveries => Data#state.active_deliveries,
        total_deliveries => Data#state.total_deliveries,
        failed_deliveries => Data#state.failed_deliveries,
        available_couriers => length(Data#state.available_couriers),
        busy_couriers => length(Data#state.busy_couriers),
        total_couriers => length(Data#state.available_couriers) + length(Data#state.busy_couriers),
        household_count => length(Data#state.household_pids),
        couriers_to_remove => Data#state.couriers_to_remove
    },
    {keep_state, Data, [{reply, From, Stats}]};

%% Catch-all handler
handle_event(EventType, Event, StateName, Data) ->
    %% Log everything we don't handle explicitly
    case Event of
        %% Filter out noisy events
        {new_package, _, _, _} -> ok;
        {package_delivered, _, _} -> ok;
        {courier_available, _} -> ok;
        _ ->
            io:format("Zone Manager ~p: Unhandled event in state ~p: ~p (~p)~n", 
                      [Data#state.zone_id, StateName, Event, EventType])
    end,
    {keep_state, Data}.

terminate(_Reason, _State, Data) ->
    io:format("Zone Manager ~p terminating~n", [Data#state.zone_id]),
    cleanup_existing_components(Data),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Cleanup existing components properly
cleanup_existing_components(Data) ->
    %% Stop households
    lists:foreach(fun({_Id, Pid}) ->
        catch gen_server:stop(Pid)
    end, Data#state.household_pids),
    
    %% Stop all couriers
    AllCouriers = Data#state.available_couriers ++ Data#state.busy_couriers,
    lists:foreach(fun({_Id, Pid}) ->
        catch gen_statem:stop(Pid)
    end, AllCouriers),
    
    %% Stop location tracker
    case Data#state.location_tracker_pid of
        undefined -> ok;
        TrackerPid -> 
            catch gen_server:stop(TrackerPid)
    end.

%% Create fresh couriers
create_fresh_couriers(Zone, NumCouriers, ZoneManagerPid, LocationTrackerPid) ->
    lists:filtermap(fun(N) ->
        CourierId = Zone ++ "_courier_" ++ integer_to_list(N),
        
        %% Stop existing courier if any
        case whereis(list_to_atom("courier_" ++ CourierId)) of
            undefined -> ok;
            OldPid -> catch gen_statem:stop(OldPid)
        end,
        
        timer:sleep(50),  %% Small delay
        
        case courier:start_link(CourierId, ZoneManagerPid, LocationTrackerPid) of
            {ok, Pid} ->
                io:format("Zone Manager: Started courier ~p~n", [CourierId]),
                {true, {CourierId, Pid}};
            Error ->
                io:format("Zone Manager: Failed to start courier ~p: ~p~n", [CourierId, Error]),
                false
        end
    end, lists:seq(1, NumCouriers)).

%% Create fresh households
create_fresh_households(Zone, ZoneManagerPid) ->
    %% Get houses in zone from map
    HousesInZone = case map_server:get_all_locations() of
        {ok, Locs} ->
            ZoneAtom = list_to_atom(Zone),
            lists:filter(fun
                (#location{type = home, zone = LocZone}) ->
                    LocZone == ZoneAtom;
                (_) -> false
            end, Locs);
        {error, _} ->
            []
    end,
    
    FinalHouses = case HousesInZone of
        [] ->
            %% Default houses if none found
            lists:map(fun(N) ->
                #location{
                    id = "household_" ++ Zone ++ "_" ++ integer_to_list(N),
                    type = home,
                    zone = list_to_atom(Zone),
                    x = 50 + N * 10,
                    y = 50
                }
            end, lists:seq(1, 3));
        Houses ->
            Houses
    end,
    
    lists:filtermap(fun(House) ->
        HouseholdId = House#location.id,
        
        %% Stop existing household if any
        case whereis(list_to_atom("household_" ++ HouseholdId)) of
            undefined -> ok;
            OldPid -> catch gen_server:stop(OldPid)
        end,
        
        timer:sleep(50),  %% Small delay
        
        case household:start_link(HouseholdId, Zone, ZoneManagerPid) of
            {ok, Pid} ->
                io:format("Zone Manager: Started household ~p~n", [HouseholdId]),
                {true, {HouseholdId, Pid}};
            Error ->
                io:format("Zone Manager: Failed to start household ~p: ~p~n", [HouseholdId, Error]),
                false
        end
    end, FinalHouses).

%% Helper functions from original implementation
handle_new_household_package_fixed(HouseholdId, PackageId, Data) ->
    io:format("Zone Manager: Received new package ~p from household ~p~n", [PackageId, HouseholdId]),
    
    TotalOrders = Data#state.total_orders + 1,
    
    CurrentCount = maps:get(HouseholdId, Data#state.package_order_counts, 0),
    NewCount = CurrentCount + 1,
    UpdatedCounts = maps:put(HouseholdId, NewCount, Data#state.package_order_counts),
    
    DataWithOrder = Data#state{
        total_orders = TotalOrders,
        package_order_counts = UpdatedCounts
    },
    
    AssignedData = try_assign_package_to_available_courier(PackageId, DataWithOrder),
    
    logistics_state_collector:zone_state_changed(Data#state.zone, #{
        status => live,
        node => node(),
        total_orders => TotalOrders,
        waiting_packages => length(AssignedData#state.waiting_packages),
        active_deliveries => AssignedData#state.active_deliveries,
        available_couriers => length(AssignedData#state.available_couriers),
        busy_couriers => length(AssignedData#state.busy_couriers)
    }),
    
    {keep_state, AssignedData}.

try_assign_package_to_available_courier(PackageId, Data) ->
    case Data#state.available_couriers of
        [{CourierId, CourierPid} | RestAvailable] ->
            io:format("Zone Manager ~p: Assigning package ~p to available courier ~p~n", 
                      [Data#state.zone_id, PackageId, CourierId]),
            
            gen_statem:cast(CourierPid, {assign_delivery, PackageId, Data#state.zone}),
            
            Data#state{
                available_couriers = RestAvailable,
                busy_couriers = [{CourierId, CourierPid} | Data#state.busy_couriers],
                active_deliveries = Data#state.active_deliveries + 1
            };
        [] ->
            io:format("Zone Manager ~p: No available couriers, queueing package ~p~n", 
                      [Data#state.zone_id, PackageId]),
            
            Waiting = Data#state.waiting_packages,
            Data#state{waiting_packages = Waiting ++ [PackageId]}
    end.

report_zone_state(Zone, Data) when is_record(Data, state) ->
    case global:whereis_name(logistics_state_collector) of
        undefined ->
            io:format("Zone Manager ~p: State collector not found, will retry~n", [Data#state.zone_id]),
            erlang:send_after(2000, self(), {retry_report_state, Zone, Data});
        Pid when is_pid(Pid) ->
            TotalCouriers = length(Data#state.available_couriers) + length(Data#state.busy_couriers),
            StateData = #{
                status => case Data#state.simulation_state of
                    stopped -> offline;
                    _ -> live
                end,
                node => node(),
                couriers => TotalCouriers,
                available_couriers => length(Data#state.available_couriers),
                busy_couriers => length(Data#state.busy_couriers),
                deliveries => Data#state.total_deliveries,
                waiting_packages => length(Data#state.waiting_packages),
                active_deliveries => Data#state.active_deliveries,
                total_delivered => Data#state.total_deliveries,
                failed_deliveries => Data#state.failed_deliveries,
                total_orders => Data#state.total_orders,
                couriers_to_remove => Data#state.couriers_to_remove
            },
            io:format("Zone Manager ~p: Reporting state to collector: ~p~n", 
                      [Data#state.zone_id, maps:get(couriers, StateData, 0)]),
            logistics_state_collector:zone_state_changed(Zone, StateData)
    end.

min(A, B) when A < B -> A;
min(_, B) -> B.

max(A, B) when A > B -> A;
max(_, B) -> B.