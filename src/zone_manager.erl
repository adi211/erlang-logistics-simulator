%%%-------------------------------------------------------------------
%%% Zone Manager - Working with zone_manager:start(ControlNode)
%%%-------------------------------------------------------------------
-module(zone_manager).
-behaviour(gen_statem).

-include("header.hrl").
-include("map_records.hrl").

%% API - Only export start/1
-export([start/1, new_package/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

-record(state, {
    control_node,
    visualization_node = 'visualization@127.0.0.1',
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
    simulation_running = false,
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
        visualization_node = 'visualization@127.0.0.1',
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
        simulation_running = false,
        package_order_counts = #{}
    },
    
    %% Report initial zone state
    report_zone_state(atom_to_list(ZoneAtom), InitialState),
    
    {ok, monitoring, InitialState}.

%%====================================================================
%% State Machine Event Handlers
%%====================================================================

%% Handle retry of state reporting
handle_event(info, {retry_report_state, Zone, Data}, StateName, CurrentState) ->
    report_zone_state(Zone, Data),
    {keep_state, CurrentState};

handle_event(info, {retry_report_state_with_households, Zone, Data}, StateName, CurrentState) ->
    report_zone_state_with_households(Zone, Data),
    {keep_state, CurrentState};

%% Start simulation - create households
handle_event(cast, {start_simulation}, _StateName, Data) ->
    io:format("Zone Manager ~p: Starting simulation and creating households~n", [Data#state.zone_id]),
    
    %% Get all locations from map_server via RPC to control node
    HousesInZone = try
        case rpc:call(Data#state.control_node, map_server, get_all_locations, []) of
            {ok, Locs} ->
                %% Filter for houses in our zone
                ZoneAtom = list_to_atom(Data#state.zone),
                lists:filter(fun
                    (#location{type = home, zone = LocZone}) ->
                        LocZone == ZoneAtom;
                    (_) -> false
                end, Locs);
            {badrpc, RPCReason} ->
                io:format("Zone Manager: RPC failed to get locations: ~p~n", [RPCReason]),
                [];
            ErrorResult ->
                io:format("Zone Manager: Failed to get locations: ~p~n", [ErrorResult]),
                []
        end
    catch
        Type:CatchReason ->
            io:format("Zone Manager: Error getting locations - ~p:~p~n", [Type, CatchReason]),
            []
    end,
    
    %% If no houses found, create some defaults
    FinalHouses = case HousesInZone of
        [] ->
            io:format("Zone Manager: No houses found, creating defaults~n"),
            ZoneStr = Data#state.zone,
            lists:map(fun(N) ->
                #location{
                    id = "household_" ++ ZoneStr ++ "_" ++ integer_to_list(N),
                    type = home,
                    zone = list_to_atom(ZoneStr),
                    x = 50 + N * 10,
                    y = 50
                }
            end, lists:seq(1, 3));
        Houses ->
            Houses
    end,
    
    io:format("Zone Manager ~p: Creating households for ~p houses in zone ~p~n", 
              [Data#state.zone_id, length(FinalHouses), Data#state.zone]),
    
    %% Start household processes for each house
    HouseholdPids = lists:map(fun(House) ->
        HouseholdId = House#location.id,
        
        case household:start_link(HouseholdId, Data#state.zone, self()) of
            {ok, Pid} ->
                io:format("Zone Manager: Started household ~p (PID: ~p)~n", [HouseholdId, Pid]),
                {HouseholdId, Pid};
            Error ->
                io:format("Zone Manager: Failed to start household ~p: ~p~n", [HouseholdId, Error]),
                {HouseholdId, undefined}
        end
    end, FinalHouses),
    
    %% Filter out failed starts
    ValidHouseholds = [{Id, Pid} || {Id, Pid} <- HouseholdPids, Pid =/= undefined],
    
    io:format("Zone Manager ~p: Successfully started ~p households~n", 
              [Data#state.zone_id, length(ValidHouseholds)]),
    
    %% Initialize package order counts
    InitialCounts = lists:foldl(fun({Id, _Pid}, Acc) ->
        maps:put(Id, 0, Acc)
    end, #{}, ValidHouseholds),
    
    {next_state, monitoring, Data#state{
        simulation_running = true,
        household_pids = ValidHouseholds,
        package_order_counts = InitialCounts
    }};

%% Handle new package from household
handle_event(info, {new_package, _HouseholdPid, HouseholdId, PackageId}, monitoring, Data) ->
    handle_new_household_package(HouseholdId, PackageId, Data);

handle_event(cast, {new_package, _HouseholdPid, HouseholdId, PackageId}, monitoring, Data) ->
    handle_new_household_package(HouseholdId, PackageId, Data);

%% Handle new package (partner's version logic)
handle_event(cast, {new_package, PackageId}, monitoring, Data) ->
    Zone = Data#state.zone,
    io:format("Zone(~p) received new package: ~p~n", [Zone, PackageId]),
    
    %% Update total orders
    TotalOrders = Data#state.total_orders + 1,
    DataWithTotal = Data#state{total_orders = TotalOrders},
    
    %% Start package process via RPC to control node
    case rpc:call(Data#state.control_node, package, start_link, [PackageId, Zone]) of
        {ok, _} ->
            io:format("Zone(~p): Package ~p process started on control node~n", [Zone, PackageId]);
        {badrpc, Reason} ->
            io:format("Zone(~p): Failed to start package ~p via RPC: ~p~n", [Zone, PackageId, Reason]);
        Error ->
            io:format("Zone(~p): Failed to start package ~p: ~p~n", [Zone, PackageId, Error])
    end,
    
    %% Add to waiting packages queue
    Waiting = DataWithTotal#state.waiting_packages,
    NewData = DataWithTotal#state{waiting_packages = Waiting ++ [PackageId]},
    report_zone_state(Zone, NewData),
    {keep_state, NewData};

%% Handle courier assignment from pool
handle_event(cast, {assign_to_waiting_package, CourierId}, monitoring, Data) ->
    Zone = Data#state.zone,
    Waiting = Data#state.waiting_packages,
    case Waiting of
        [Pkg | RestPkgs] ->
            io:format("Zone(~p): Got courier ~p for waiting package ~p~n", [Zone, CourierId, Pkg]),
            %% Assign courier via RPC
            rpc:call(Data#state.control_node, package, assign_courier, [Pkg, CourierId]),
            ActiveDeliveries = Data#state.active_deliveries,
            NewData = Data#state{
                waiting_packages = RestPkgs,
                active_deliveries = ActiveDeliveries + 1
            },
            report_zone_state(Zone, NewData),
            {keep_state, NewData};
        [] ->
            io:format("Zone(~p): Got assignment ~p but have no waiting packages~n", [Zone, CourierId]),
            {keep_state, Data}
    end;

%% Handle delivery complete
handle_event(cast, {package_delivered, PackageId, CourierId}, monitoring, Data) ->
    Zone = Data#state.zone,
    io:format("Zone(~p): Package ~p delivered by courier ~p!~n", [Zone, PackageId, CourierId]),
    Total = Data#state.total_deliveries,
    ActiveDeliveries = Data#state.active_deliveries,
    
    %% Update household package count if applicable
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

%% Handle assignment failure
handle_event(cast, {assignment_failed, PackageId, CourierId}, monitoring, Data) ->
    Zone = Data#state.zone,
    ExpectedPrefix = Zone ++ "_",
    case string:prefix(PackageId, ExpectedPrefix) of
        nomatch ->
            io:format("Zone(~p): Ignoring assignment failure for package ~p (different zone)~n", [Zone, PackageId]),
            {keep_state, Data};
        _ ->
            io:format("Zone(~p): Assignment failed - courier ~p busy, requeueing package ~p~n", [Zone, CourierId, PackageId]),
            Waiting = Data#state.waiting_packages,
            Failed = Data#state.failed_deliveries,
            ActiveDeliveries = Data#state.active_deliveries,
            NewData = Data#state{
                waiting_packages = Waiting ++ [PackageId],
                failed_deliveries = Failed + 1,
                active_deliveries = max(0, ActiveDeliveries - 1)
            },
            report_zone_state(Zone, NewData),
            {keep_state, NewData}
    end;

%% Handle load factor update
handle_event(cast, {update_load_factor, Value}, _StateName, Data) ->
    io:format("Zone Manager ~p: Received load factor update: ~p%~n", [Data#state.zone_id, Value]),
    
    %% Forward load factor update to all households
    lists:foreach(fun({_Id, Pid}) ->
        gen_server:cast(Pid, {update_load_factor, Value})
    end, Data#state.household_pids),
    
    io:format("Zone Manager ~p: Updated load factor for ~p households~n", 
              [Data#state.zone_id, length(Data#state.household_pids)]),
    
    {keep_state, Data};

%% Pause simulation
handle_event(cast, {pause_simulation}, _StateName, Data) ->
    io:format("Zone Manager ~p: Pausing simulation~n", [Data#state.zone_id]),
    lists:foreach(fun({_Id, Pid}) ->
        gen_server:cast(Pid, pause_simulation)
    end, Data#state.household_pids),
    {keep_state, Data#state{simulation_running = false}};

%% Resume simulation
handle_event(cast, {resume_simulation}, _StateName, Data) ->
    io:format("Zone Manager ~p: Resuming simulation~n", [Data#state.zone_id]),
    lists:foreach(fun({_Id, Pid}) ->
        gen_server:cast(Pid, resume_simulation)
    end, Data#state.household_pids),
    {keep_state, Data#state{simulation_running = true}};

%% Stop simulation
handle_event(cast, {stop_simulation}, _StateName, Data) ->
    io:format("Zone Manager ~p: Stopping simulation~n", [Data#state.zone_id]),
    
    %% עצור את כל ה-households אבל אל תמחק אותם
    lists:foreach(fun({_Id, Pid}) ->
        gen_server:cast(Pid, stop_simulation)
    end, Data#state.household_pids),
    
    %% חזור ל-monitoring במקום stopped - מוכן להפעלה מחדש
    {next_state, monitoring, Data#state{
        simulation_running = false,
        %% אל תאפס את household_pids! שמור אותם להפעלה מחדש
        waiting_packages = [],
        package_order_counts = #{}
    }};

%% State transitions
handle_event(cast, {start_optimization}, monitoring, Data) ->
    io:format("Zone(~p): Starting optimization cycle~n", [Data#state.zone]),
    {next_state, optimizing, Data};

handle_event(cast, {overload_detected}, monitoring, Data) ->
    io:format("Zone(~p): Overload detected, entering emergency mode~n", [Data#state.zone]),
    {next_state, emergency_mode, Data};

handle_event(cast, {optimization_complete}, optimizing, Data) ->
    io:format("Zone(~p): Optimization complete, returning to monitoring~n", [Data#state.zone]),
    {next_state, monitoring, Data};

handle_event(cast, {load_balanced}, emergency_mode, Data) ->
    io:format("Zone(~p): Load balanced, returning to normal operation~n", [Data#state.zone]),
    {next_state, monitoring, Data};

handle_event(cast, {load_balance_complete}, load_balancing, Data) ->
    io:format("Zone(~p): Load balancing complete~n", [Data#state.zone]),
    {next_state, monitoring, Data};

%% Get statistics
handle_event({call, From}, get_stats, _StateName, Data) ->
    WaitingPackages = Data#state.waiting_packages,
    Stats = #{
        zone => Data#state.zone,
        waiting_packages => WaitingPackages,
        waiting_count => length(WaitingPackages),
        active_deliveries => Data#state.active_deliveries,
        total_deliveries => Data#state.total_deliveries,
        failed_deliveries => Data#state.failed_deliveries
    },
    {keep_state, Data, [{reply, From, Stats}]};

%% Deploy couriers
handle_event(cast, {deploy_couriers, Num}, _StateName, Data) ->
    io:format("Zone Manager ~p: Deploying ~p couriers~n", [Data#state.zone_id, Num]),
    ets:update_counter(zone_stats, couriers_active, Num),
    {keep_state, Data};

%% Catch-all handler
handle_event(EventType, Event, StateName, Data) ->
    io:format("Zone(~p) in state ~p received event: ~p (~p)~n", 
              [Data#state.zone, StateName, Event, EventType]),
    {keep_state, Data}.

terminate(_Reason, _State, Data) ->
    io:format("Zone Manager ~p terminating~n", [Data#state.zone_id]),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_new_household_package(HouseholdId, PackageId, Data) ->
    io:format("Zone Manager: Received new package ~p from household ~p~n", [PackageId, HouseholdId]),
    
    %% Update total orders
    TotalOrders = Data#state.total_orders + 1,
    
    %% Update package count for this household
    CurrentCount = maps:get(HouseholdId, Data#state.package_order_counts, 0),
    NewCount = CurrentCount + 1,
    UpdatedCounts = maps:put(HouseholdId, NewCount, Data#state.package_order_counts),
    
    %% Add to waiting packages queue
    Waiting = Data#state.waiting_packages,
    
    %% Update state
    NewData = Data#state{
        total_orders = TotalOrders,
        waiting_packages = Waiting ++ [PackageId],
        package_order_counts = UpdatedCounts
    },
    
    
    %% Optional: Send a lightweight zone summary (not all details)
    logistics_state_collector:zone_state_changed(Data#state.zone, #{
        status => live,
        node => node(),
        total_orders => TotalOrders,
        waiting_packages => length(Waiting) + 1,
        active_deliveries => Data#state.active_deliveries
        %% NO household_orders map! Too heavy!
    }),
    
    {keep_state, NewData}.

report_zone_state(Zone, Data) when is_record(Data, state) ->
    case global:whereis_name(logistics_state_collector) of
        undefined ->
            erlang:send_after(2000, self(), {retry_report_state, Zone, Data});
        Pid when is_pid(Pid) ->
            StateData = #{
                status => live,  %% חשוב! תמיד שלח status
                node => node(),
                couriers => 0,
                deliveries => 0,
                waiting_packages => length(Data#state.waiting_packages),
                active_deliveries => Data#state.active_deliveries,
                total_delivered => Data#state.total_deliveries,
                failed_deliveries => Data#state.failed_deliveries,
                total_orders => Data#state.total_orders
            },
            logistics_state_collector:zone_state_changed(Zone, StateData)
    end.

%% Report state including household order counts - תיקון דומה
report_zone_state_with_households(Zone, Data) when is_record(Data, state) ->
    case global:whereis_name(logistics_state_collector) of
        undefined ->
            erlang:send_after(2000, self(), {retry_report_state_with_households, Zone, Data});
        Pid when is_pid(Pid) ->
            StateData = #{
                status => live,  %% חשוב! גם כאן
                node => node(),
                couriers => 0,
                deliveries => 0,
                waiting_packages => length(Data#state.waiting_packages),
                active_deliveries => Data#state.active_deliveries,
                total_delivered => Data#state.total_deliveries,
                failed_deliveries => Data#state.failed_deliveries,
                total_orders => Data#state.total_orders,
                household_orders => Data#state.package_order_counts
            },
            logistics_state_collector:zone_state_changed(Zone, StateData)
    end.