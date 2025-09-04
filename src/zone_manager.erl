%%%-------------------------------------------------------------------
%%% Zone Manager - Sends visualization updates to visualization node
%%%-------------------------------------------------------------------
-module(zone_manager).
-behaviour(gen_server).

-include("header.hrl").

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {
    control_center_pid,   
    visualization_node,    % Node running visualization
    total_couriers,       
    total_deliveries,     
    failed_deliveries,    
    zone_id,             
    order_queue,         
    courier_pool,
    households,
    packages,
    simulation_running,
    zone_center,         
    zone_bounds          
}).

%% Define zone configurations
-define(ZONE_CONFIG, #{
    zone_north => #{center => {50, 20}, bounds => {0, 0, 100, 33}},
    zone_center => #{center => {50, 50}, bounds => {0, 34, 100, 66}},
    zone_south => #{center => {50, 80}, bounds => {0, 67, 100, 100}}
}).

start(ControlNode) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ControlNode], []).

init([ControlNode]) ->
    io:format("Zone Manager ~p: Starting up...~n", [node()]),
    put(server, {control_center, ControlNode}),
    
    %% Determine zone configuration based on node name
    ZoneName = case atom_to_list(node()) of
        "zone_north" ++ _ -> zone_north;
        "zone_center" ++ _ -> zone_center;
        "zone_south" ++ _ -> zone_south;
        _ -> zone_north  % default
    end,
    
    #{center := ZoneCenter, bounds := ZoneBounds} = maps:get(ZoneName, ?ZONE_CONFIG),
    
    io:format("Zone Manager ~p: Zone center at ~p, bounds ~p~n", 
              [node(), ZoneCenter, ZoneBounds]),
    
    %% Initialize ETS for zone statistics
    case ets:info(zone_stats) of
		undefined -> ets:new(zone_stats, [set, named_table, public, {read_concurrency, true}, {write_concurrency, true}]);
		_ -> ok
	end,
    ets:insert(zone_stats, {deliveries, 0}),
    ets:insert(zone_stats, {failures, 0}),
    ets:insert(zone_stats, {couriers_active, 0}),
    ets:insert(zone_stats, {update_stats, false}),
    
    %% Start timer for periodic stats updates
    spawn_link(fun() -> update_control_center_timer({zone_manager, node()}) end),
    
    %% Connect to control center
    gen_server:call(get(server), {connect, node()}),
    
    %% Set visualization node (hardcoded for now, could be passed in)
    VisualizationNode = 'visualization@127.0.0.1',
    
    {ok, #state{
        control_center_pid = ControlNode,
        visualization_node = VisualizationNode,
        total_couriers = 0,
        total_deliveries = 0,
        failed_deliveries = 0,
        zone_id = node(),
        order_queue = [],
        courier_pool = [],
        households = [],
        packages = [],
        simulation_running = false,
        zone_center = ZoneCenter,
        zone_bounds = ZoneBounds
    }}.

handle_call({become_control_center, VisualizationNode}, _From, State) ->
    io:format("Zone Manager ~p: Becoming control center~n", [node()]),
    NewControlPID = spawn(control_center, start, [VisualizationNode]),
    gen_server:cast({zone_manager, node()}, {kill_zone_manager}),
    {reply, NewControlPID, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({start_simulation}, State) ->
    io:format("Zone Manager ~p: Starting simulation~n", [node()]),
    Households = start_households(?DEFAULT_HOUSEHOLD_COUNT, State#state.zone_bounds),
    {noreply, State#state{simulation_running = true, households = Households}};

handle_cast({deploy_couriers, Num}, State = #state{total_couriers = TotalCouriers, 
                                                    zone_center = ZoneCenter,
                                                    visualization_node = VizNode}) ->
    io:format("Zone Manager ~p: Deploying ~p couriers~n", [node(), Num]),
    ets:update_counter(zone_stats, couriers_active, Num),
    gen_server:cast(get(server), {update_stats, node(), Num, 0, 0}),
    
    %% Create courier processes starting from zone distribution center
    NewCouriers = create_couriers(Num, ZoneCenter, State#state.zone_id),
    
    %% Send courier positions to visualization node
    lists:foreach(fun({CourierID, _PID}) ->
        send_to_visualization(VizNode, {update_courier_position, CourierID, ZoneCenter})
    end, NewCouriers),
    
    {noreply, State#state{
        total_couriers = TotalCouriers + Num,
        courier_pool = State#state.courier_pool ++ NewCouriers
    }};

handle_cast({order_placed, OrderInfo}, State = #state{order_queue = Queue, packages = Packages, 
                                                       zone_bounds = Bounds,
                                                       visualization_node = VizNode}) ->
    io:format("Zone Manager ~p: New order received~n", [node()]),
    
    %% Generate locations within zone bounds
    {MinX, MinY, MaxX, MaxY} = Bounds,
    BusinessLoc = {MinX + rand:uniform() * (MaxX - MinX), MinY + rand:uniform() * (MaxY - MinY)},
    CustomerLoc = {MinX + rand:uniform() * (MaxX - MinX), MinY + rand:uniform() * (MaxY - MinY)},
    
    %% Update order with zone-specific locations
    UpdatedOrderInfo = OrderInfo#{
        business_location => BusinessLoc,
        customer_location => CustomerLoc
    },
    
    %% Send order to visualization node
    OrderID = maps:get(id, UpdatedOrderInfo),
    send_to_visualization(VizNode, {add_order, OrderID, BusinessLoc, CustomerLoc}),
    
    %% Create package process
    PackagePID = spawn(fun() -> package_process(UpdatedOrderInfo) end),
    NewQueue = Queue ++ [UpdatedOrderInfo],
    
    %% Try to assign to available courier
    {UpdatedQueue, UpdatedCouriers} = try_assign_orders(NewQueue, State#state.courier_pool),
    
    {noreply, State#state{
        order_queue = UpdatedQueue, 
        packages = Packages ++ [PackagePID],
        courier_pool = UpdatedCouriers
    }};

handle_cast({courier_position_update, CourierID, Position}, State = #state{visualization_node = VizNode}) ->
    %% Forward position update to visualization node
    send_to_visualization(VizNode, {update_courier_position, CourierID, Position}),
    {noreply, State};

handle_cast({delivery_complete, CourierID, OrderID}, State = #state{visualization_node = VizNode}) ->
    %% Remove order from visualization
    send_to_visualization(VizNode, {remove_order, OrderID}),
    ets:update_counter(zone_stats, deliveries, 1),
    ets:insert(zone_stats, {update_stats, true}),
    {noreply, State};

handle_cast({delivery_complete, CourierID}, State) ->
    ets:update_counter(zone_stats, deliveries, 1),
    ets:insert(zone_stats, {update_stats, true}),
    {noreply, State};

handle_cast({pause_simulation}, State) ->
    io:format("Zone Manager ~p: Pausing simulation~n", [node()]),
    broadcast_to_processes(State#state.households, pause),
    lists:foreach(fun({_ID, PID}) -> PID ! pause end, State#state.courier_pool),
    {noreply, State#state{simulation_running = false}};

handle_cast({stop_simulation}, State) ->
    io:format("Zone Manager ~p: Stopping simulation~n", [node()]),
    broadcast_to_processes(State#state.households, stop),
    lists:foreach(fun({_ID, PID}) -> PID ! stop end, State#state.courier_pool),
    {noreply, State#state{simulation_running = false, households = [], courier_pool = []}};

handle_cast({update_stats}, State = #state{total_deliveries = TotalDeliveries, 
                                           failed_deliveries = TotalFailures}) ->
    case ets:lookup(zone_stats, update_stats) of
        [{_, true}] ->
            ets:insert(zone_stats, {update_stats, false}),
            
            [{_, Deliveries}] = ets:lookup(zone_stats, deliveries),
            [{_, Failures}] = ets:lookup(zone_stats, failures),
            
            ets:update_counter(zone_stats, deliveries, -Deliveries),
            ets:update_counter(zone_stats, failures, -Failures),
            
            gen_server:cast(get(server), {update_stats, node(), 0, Deliveries, Failures}),
            
            NewTotalDeliveries = TotalDeliveries + Deliveries,
            NewTotalFailures = TotalFailures + Failures,
            io:format("Zone Manager ~p: Stats update - Deliveries: ~p, Failures: ~p~n", 
                     [node(), NewTotalDeliveries, NewTotalFailures]),
            
            {noreply, State#state{
                total_deliveries = NewTotalDeliveries, 
                failed_deliveries = NewTotalFailures
            }};
        [{_, false}] ->
            {noreply, State}
    end;

handle_cast({update_param, Param, Value}, State) ->
    io:format("Zone Manager ~p: Updating parameter ~p to ~p~n", [node(), Param, Value]),
    broadcast_to_processes(State#state.households, {update_param, Param, Value}),
    lists:foreach(fun({_ID, PID}) -> PID ! {update_param, Param, Value} end, State#state.courier_pool),
    {noreply, State};

handle_cast({update_control_node, ControlNode}, State) ->
    io:format("Zone Manager ~p: Updating control node to ~p~n", [node(), ControlNode]),
    put(server, {control_center, ControlNode}),
    {noreply, State#state{control_center_pid = ControlNode}};

handle_cast({kill_zone_manager}, State) ->
    io:format("Zone Manager ~p: Shutting down~n", [node()]),
    exit(self(), exit),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
update_control_center_timer(ZoneServer) ->
    timer:sleep(?UPDATE_INTERVAL),
    case ets:lookup(zone_stats, update_stats) of
        [{_, true}] -> 
            gen_server:cast(ZoneServer, {update_stats});
        [{_, false}] -> 
            ok
    end,
    update_control_center_timer(ZoneServer).

%% Create couriers with IDs
create_couriers(0, _Center, _Zone) -> [];
create_couriers(N, Center, Zone) ->
    CourierID = list_to_atom(lists:flatten(io_lib:format("courier_~p_~p", [Zone, N]))),
    CourierPID = spawn(fun() -> courier_process_loop(CourierID, Zone, Center, zone_manager) end),
    [{CourierID, CourierPID} | create_couriers(N-1, Center, Zone)].

start_households(0, _Bounds) -> [];
start_households(N, Bounds) ->
    HouseholdPID = spawn(fun() -> household_process(N, node(), Bounds) end),
    [HouseholdPID | start_households(N-1, Bounds)].

broadcast_to_processes([], _Message) -> ok;
broadcast_to_processes([PID|Rest], Message) ->
    PID ! Message,
    broadcast_to_processes(Rest, Message).

try_assign_orders([], Couriers) -> {[], Couriers};
try_assign_orders(Orders, []) -> {Orders, []};
try_assign_orders([Order|RestOrders], [{CourierID, CourierPID}|RestCouriers]) ->
    CourierPID ! {assign_order, Order},
    try_assign_orders(RestOrders, RestCouriers).

%% Send updates to visualization server
send_to_visualization(VizNode, Message) ->
    case whereis(visualization_server) of
        undefined ->
            %% Try remote node
            gen_server:cast({visualization_server, VizNode}, Message);
        _Pid ->
            %% Local node
            gen_server:cast(visualization_server, Message)
    end.

%% Simple process skeletons
package_process(OrderInfo) ->
    receive
        stop -> ok;
        _ -> package_process(OrderInfo)
    end.

courier_process_loop(CourierID, Zone, Location, ZoneManager) ->
    receive
        {assign_order, Order} ->
            BusinessLoc = maps:get(business_location, Order),
            CustomerLoc = maps:get(customer_location, Order),
            OrderID = maps:get(id, Order),
            
            %% Simulate movement to business
            simulate_movement(CourierID, Location, BusinessLoc, 50, ZoneManager),
            
            %% Simulate movement to customer
            simulate_movement(CourierID, BusinessLoc, CustomerLoc, 50, ZoneManager),
            
            %% Delivery complete
            gen_server:cast(ZoneManager, {delivery_complete, CourierID, OrderID}),
            
            courier_process_loop(CourierID, Zone, CustomerLoc, ZoneManager);
        pause -> 
            receive resume -> courier_process_loop(CourierID, Zone, Location, ZoneManager) end;
        stop -> ok;
        _ -> courier_process_loop(CourierID, Zone, Location, ZoneManager)
    end.

simulate_movement(CourierID, {X1, Y1}, {X2, Y2}, Steps, ZoneManager) ->
    DX = (X2 - X1) / Steps,
    DY = (Y2 - Y1) / Steps,
    move_step(CourierID, X1, Y1, DX, DY, Steps, ZoneManager).

move_step(_CourierID, X, Y, _DX, _DY, 0, _ZoneManager) ->
    {X, Y};
move_step(CourierID, X, Y, DX, DY, Steps, ZoneManager) ->
    NewX = X + DX,
    NewY = Y + DY,
    gen_server:cast(ZoneManager, {courier_position_update, CourierID, {NewX, NewY}}),
    timer:sleep(50),  % 50ms per step
    move_step(CourierID, NewX, NewY, DX, DY, Steps - 1, ZoneManager).

household_process(ID, Zone, Bounds) ->
    receive
        pause -> 
            receive resume -> household_process(ID, Zone, Bounds) end;
        stop -> ok;
        _ ->
            timer:sleep(rand:uniform(10000)),
            OrderID = erlang:unique_integer([positive]),
            gen_server:cast(zone_manager, {order_placed, #{id => OrderID, zone => Zone}}),
            household_process(ID, Zone, Bounds)
    end.