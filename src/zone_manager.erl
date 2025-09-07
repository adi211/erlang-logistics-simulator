%%%-------------------------------------------------------------------
%%% Zone Manager - Fixed with proper PAUSE/RESUME/STOP handling
%%% and smart courier removal
%%%-------------------------------------------------------------------
-module(zone_manager).
-behaviour(gen_statem).
-include("network_const.hrl").

-include("map_records.hrl").


%% API
-export([start/0, new_package/2]).

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
    package_order_counts = #{}
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

start() ->
    gen_statem:start_link({local, zone_manager}, ?MODULE, [], []).

new_package(Zone, PackageId) ->
    io:format("API: Sending new_package ~p to zone ~p~n", [PackageId, Zone]),
    gen_statem:cast(zone_manager, {new_package, PackageId}).

%%====================================================================
%% gen_statem callbacks 
%%====================================================================

callback_mode() -> handle_event_function.

init([]) ->
    ControlNode = ?CTRL_NODE,
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
        package_order_counts = #{}
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
    
    %% Get courier count for THIS specific zone from config
    CourierConfig = maps:get(courier_config, Config, #{}),
    ZoneAtom = list_to_atom(Data#state.zone),  % Convert "north"/"center"/"south" to atom
    NumCouriers = maps:get(ZoneAtom, CourierConfig, 5), % Get zone-specific count, default 5
    
    io:format("Zone Manager ~p (~p): Creating ~p couriers (from user config)~n", 
              [Data#state.zone_id, ZoneAtom, NumCouriers]),
    
    %% Create couriers with the zone-specific count
    NewCouriers = create_fresh_couriers(Data#state.zone, NumCouriers, self(), LocationTrackerPid),
    
    %% Create new households
    NewHouseholds = create_fresh_households(Data#state.zone, self()),
    
    InitialCounts = lists:foldl(fun({Id, _Pid}, Acc) ->
        maps:put(Id, 0, Acc)
    end, #{}, NewHouseholds),
    
    %% Report initial state with correct courier count
    report_zone_state(Data#state.zone, Data#state{
        available_couriers = NewCouriers,
        busy_couriers = [],
        total_deliveries = 0,
        active_deliveries = 0,
        waiting_packages = []
    }),
    
    {next_state, monitoring, Data#state{
        simulation_state = running,
        components_created = true,
        household_pids = NewHouseholds,
        available_couriers = NewCouriers,
        busy_couriers = [],
        location_tracker_pid = LocationTrackerPid,
        package_order_counts = InitialCounts,
        waiting_packages = [],
        active_deliveries = 0
        
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
                _TrackerPid -> location_tracker:resume()
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
                _TrackerPid -> location_tracker:pause()
            end,
            
            {keep_state, Data#state{simulation_state = paused}};
        _ ->
            io:format("Zone Manager ~p: Cannot pause - not running~n", [Data#state.zone_id]),
            {keep_state, Data}
    end;

%% Stop simulation - complete cleanup
handle_event(cast, {stop_simulation}, StateName, Data) ->
    io:format("Zone Manager ~p: EXECUTING CONCURRENT STOP - Stopping simulation completely (state: ~p)~n", 
              [Data#state.zone_id, StateName]),

    %% Concurrently stop all households
    Households = Data#state.household_pids,
    io:format("Zone Manager ~p: Concurrently stopping ~p households...~n", [Data#state.zone_id, length(Households)]),
    lists:foreach(fun({_Id, Pid}) ->
        spawn(fun() -> 
            try gen_server:cast(Pid, stop_simulation) 
            catch _:_ -> ok end 
        end)
    end, Households),

    %% Concurrently stop all couriers
    AllCouriers = Data#state.available_couriers ++ Data#state.busy_couriers,
    io:format("Zone Manager ~p: Concurrently stopping ~p couriers...~n", [Data#state.zone_id, length(AllCouriers)]),
    lists:foreach(fun({_Id, Pid}) ->
        spawn(fun() -> 
            try gen_statem:stop(Pid, normal) 
            catch _:_ -> ok end
        end)
    end, AllCouriers),

    %% Stop location tracker (this can remain synchronous as it's just one)
    case Data#state.location_tracker_pid of
        undefined -> ok;
        TrackerPid -> 
            io:format("Zone Manager ~p: Stopping location tracker~n", [Data#state.zone_id]),
            gen_server:stop(TrackerPid, normal, 500)
    end,
    
    %% Give processes a moment to terminate before cleaning up the state
    timer:sleep(200),

    io:format("Zone Manager ~p: Clearing ~p waiting packages~n", 
              [Data#state.zone_id, length(Data#state.waiting_packages)]),
    
    report_zone_state(Data#state.zone, Data#state{
        simulation_state = stopped,
        available_couriers = [],
        busy_couriers = [],
        waiting_packages = [],
        active_deliveries = 0
    }),
    
    {next_state, monitoring, Data#state{
        simulation_state = stopped,
        components_created = false,
        available_couriers = [],
        busy_couriers = [],
        location_tracker_pid = undefined,
        household_pids = [],
        waiting_packages = [],
        package_order_counts = #{},
        active_deliveries = 0
    }};

%% Handle courier becoming available - with removal check
%% Handle courier becoming available - SIMPLIFIED VERSION
handle_event(cast, {courier_available, CourierId}, monitoring, Data) ->
    io:format("Zone Manager ~p: Courier ~p is now available~n", 
              [Data#state.zone_id, CourierId]),
    
    %% פשוט העבר את השליח מbusy לavailable
    NewData = case lists:keytake(CourierId, 1, Data#state.busy_couriers) of
        {value, CourierTuple, RestBusy} ->
            Data#state{
                available_couriers = [CourierTuple | Data#state.available_couriers],
                busy_couriers = RestBusy
            };
        false ->
            io:format("Zone Manager ~p: WARNING - Courier ~p not found in busy list~n", 
                     [Data#state.zone_id, CourierId]),
            Data
    end,
    
    %% בדוק אם יש חבילות ממתינות
    case NewData#state.waiting_packages of
        [PackageId | RestPackages] ->
            io:format("Zone Manager ~p: Assigning waiting package ~p to courier ~p~n", 
                     [Data#state.zone_id, PackageId, CourierId]),
            
            [{CourierId, CourierPid} | RestAvailable] = NewData#state.available_couriers,
            
            %% שלח משימה לשליח
            gen_statem:cast(CourierPid, {assign_delivery, PackageId, Data#state.zone}),
            
            AssignedData = NewData#state{
                available_couriers = RestAvailable,
                busy_couriers = [{CourierId, CourierPid} | NewData#state.busy_couriers],
                waiting_packages = RestPackages,
                active_deliveries = NewData#state.active_deliveries + 1
            },
            
            report_zone_state(Data#state.zone, AssignedData),
            {keep_state, AssignedData};
            
        [] ->
            %% אין חבילות ממתינות
            report_zone_state(Data#state.zone, NewData),
            {keep_state, NewData}
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
            maps:put(HouseholdId, erlang:max(0, CurrentCount - 1), Data#state.package_order_counts);
        _ ->
            Data#state.package_order_counts
    end,
    
    NewData = Data#state{
        total_deliveries = Total + 1,
        active_deliveries = erlang:max(0, ActiveDeliveries - 1),
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
        household_count => length(Data#state.household_pids)
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
    Parent = self(),
    Pids = lists:map(fun(N) ->
        CourierId = Zone ++ "_courier_" ++ integer_to_list(N),
        spawn(fun() ->
            %% Stop existing courier if any
            case whereis(list_to_atom("courier_" ++ CourierId)) of
                undefined -> ok;
                OldPid -> catch gen_statem:stop(OldPid)
            end,
            
            case courier:start_link(CourierId, ZoneManagerPid, LocationTrackerPid) of
                {ok, Pid} ->
                    io:format("Zone Manager: Started courier ~p~n", [CourierId]),
                    Parent ! {{courier_started, self()}, {ok, {CourierId, Pid}}};
                Error ->
                    io:format("Zone Manager: Failed to start courier ~p: ~p~n", [CourierId, Error]),
                    Parent ! {{courier_started, self()}, {error, CourierId}}
            end
        end)
    end, lists:seq(1, NumCouriers)),
    
    %% Collect results
    collect_results(Pids, []).

%% Create fresh households
create_fresh_households(Zone, ZoneManagerPid) ->
    %% החלק הזה של מציאת הבתים במפה נשאר זהה
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

    %% כאן מתחיל השינוי המרכזי: יצירה מקבילית
    Parent = self(),
    Pids = lists:map(fun(House) ->
        HouseholdId = House#location.id,
        spawn(fun() ->
            %% הפסק תהליך ישן אם קיים
            case whereis(list_to_atom("household_" ++ HouseholdId)) of
                undefined -> ok;
                OldPid -> catch gen_server:stop(OldPid)
            end,
            
            %% התחל את התהליך החדש
            case household:start_link(HouseholdId, Zone, ZoneManagerPid) of
                {ok, Pid} ->
                    Parent ! {{household_started, self()}, {ok, {HouseholdId, Pid}}};
                _Error ->
                    Parent ! {{household_started, self()}, {error, HouseholdId}}
            end
        end)
    end, FinalHouses),

    %% איסוף התוצאות מהתהליכים שנוצרו
    collect_household_results(Pids, []).


%% פונקציית עזר לאיסוף תוצאות מהתהליכים המקביליים
collect_household_results([], Acc) -> 
    lists:reverse(Acc);
collect_household_results([Pid | Rest], Acc) ->
    receive
        {{household_started, Pid}, {ok, Result}} ->
            collect_household_results(Rest, [Result | Acc]);
        {{household_started, Pid}, {error, _}} ->
            collect_household_results(Rest, Acc)
    after 5000 ->
        io:format("Zone Manager: WARNING - Timeout collecting household start results~n"),
        lists:reverse(Acc)
    end.

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
                total_orders => Data#state.total_orders
            },
            io:format("Zone Manager ~p: Reporting state to collector: ~p~n", 
                      [Data#state.zone_id, maps:get(couriers, StateData, 0)]),
            logistics_state_collector:zone_state_changed(Zone, StateData)
    end.
	
	
	
	
%% Helper to collect results from spawned processes (Generic Version)
collect_results([], Acc) ->
    lists:reverse(Acc);
collect_results([Pid | Rest], Acc) ->
    receive
        {{courier_started, Pid}, {ok, Result}} ->
            collect_results(Rest, [Result | Acc]);
        {{household_started, Pid}, {ok, Result}} ->
            collect_results(Rest, [Result | Acc]);
        {{_, Pid}, {error, _}} ->
            collect_results(Rest, Acc)
    after 5000 ->
        io:format("Zone Manager: WARNING - Timeout collecting process start results~n"),
        lists:reverse(Acc)
    end.
	


