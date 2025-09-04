%%%-------------------------------------------------------------------
%%% @doc map_visual_tb - Testbench for map visualization
%%% Shows the map in action with animated deliveries
%%%-------------------------------------------------------------------
-module(map_visual_tb).
-export([run/0, demo/0, stress/0]).

%% Basic test run
run() ->
    io:format("Starting Map Visual Testbench...~n"),
    
    %% Start the map
    {ok, _MapPid} = map_visual:start(),
    timer:sleep(500),
    
    %% Phase 1: Setup initial state
    io:format("Phase 1: Setting up initial state~n"),
    setup_initial_orders(),
    timer:sleep(2000),
    
    %% Phase 2: Deploy couriers
    io:format("Phase 2: Deploying couriers~n"),
    deploy_couriers(),
    timer:sleep(2000),
    
    %% Phase 3: Simulate deliveries
    io:format("Phase 3: Starting delivery simulation~n"),
    simulate_deliveries(),
    
    io:format("Testbench complete! Map remains open.~n"),
    ok.

%% Demo with narration
demo() ->
    io:format("~n=== LOGISTICS MAP DEMO ===~n~n"),
    
    {ok, _} = map_visual:start(),
    timer:sleep(1000),
    
    %% Step 1: Show houses with orders
    io:format("Step 1: Houses receiving orders...~n"),
    lists:foreach(fun(N) ->
        HouseID = list_to_atom("house_" ++ integer_to_list(N)),
        Orders = rand:uniform(5),
        map_visual:update_house_orders(HouseID, Orders),
        timer:sleep(50)
    end, [1, 5, 10, 15, 20, 35, 40, 50, 60, 70, 80, 90]),
    
    timer:sleep(2000),
    
    %% Step 2: Deploy couriers from DCs
    io:format("Step 2: Couriers deploying from distribution centers...~n"),
    map_visual:update_courier(courier_n1, {553, 123}),
    timer:sleep(500),
    map_visual:update_courier(courier_n2, {553, 123}),
    timer:sleep(500),
    map_visual:update_courier(courier_c1, {553, 430}),
    timer:sleep(500),
    map_visual:update_courier(courier_s1, {553, 676}),
    timer:sleep(1000),
    
    %% Step 3: Couriers move to houses
    io:format("Step 3: Couriers delivering to houses...~n"),
    
    %% North courier to house 5
    spawn(fun() -> 
        animate_delivery(courier_n1, {553, 123}, {461, 61}, house_5, 3)
    end),
    
    timer:sleep(500),
    
    %% Center courier to house 50
    spawn(fun() ->
        animate_delivery(courier_c1, {553, 430}, {369, 369}, house_50, 2)
    end),
    
    timer:sleep(500),
    
    %% South courier to house 80
    spawn(fun() ->
        animate_delivery(courier_s1, {553, 676}, {1015, 615}, house_80, 1)
    end),
    
    io:format("Demo running... Watch the map!~n"),
    ok.

%% Stress test - many updates
stress() ->
    io:format("Starting stress test - many simultaneous deliveries~n"),
    {ok, _} = map_visual:start(),
    timer:sleep(500),
    
    %% Add orders to many houses
    lists:foreach(fun(N) ->
        HouseID = list_to_atom("house_" ++ integer_to_list(N)),
        map_visual:update_house_orders(HouseID, rand:uniform(3))
    end, lists:seq(1, 100)),
    
    %% Create 10 couriers
    lists:foreach(fun(N) ->
        CourierID = list_to_atom("c_" ++ integer_to_list(N)),
        Zone = case N rem 3 of
            0 -> {553, 123};  % North DC
            1 -> {553, 430};  % Center DC
            2 -> {553, 676}   % South DC
        end,
        map_visual:update_courier(CourierID, Zone)
    end, lists:seq(1, 10)),
    
    %% Random movements
    spawn(fun() -> random_courier_movements(10) end),
    
    io:format("Stress test running... Press Ctrl+C to stop~n"),
    ok.

%% Helper functions

setup_initial_orders() ->
    %% Add pending orders to various houses
    OrderDistribution = [
        {house_3, 2}, {house_7, 1}, {house_12, 3},
        {house_15, 2}, {house_22, 1}, {house_28, 4},
        {house_35, 1}, {house_42, 2}, {house_48, 1},
        {house_55, 3}, {house_61, 1}, {house_67, 2},
        {house_72, 1}, {house_78, 2}, {house_85, 1},
        {house_92, 3}, {house_95, 1}, {house_99, 2}
    ],
    
    lists:foreach(fun({HouseID, Count}) ->
        map_visual:update_house_orders(HouseID, Count),
        timer:sleep(100)
    end, OrderDistribution).

deploy_couriers() ->
    %% Deploy couriers at distribution centers
    Couriers = [
        {courier_north_1, {553, 123}},
        {courier_north_2, {553, 123}},
        {courier_center_1, {553, 430}},
        {courier_center_2, {553, 430}},
        {courier_south_1, {553, 676}},
        {courier_south_2, {553, 676}}
    ],
    
    lists:foreach(fun({ID, Pos}) ->
        map_visual:update_courier(ID, Pos),
        timer:sleep(200)
    end, Couriers).

simulate_deliveries() ->
    %% Simulate multiple deliveries
    Deliveries = [
        {courier_north_1, {553, 123}, {276, 61}, house_3, 2},
        {courier_north_2, {553, 123}, {738, 123}, house_19, 1},
        {courier_center_1, {553, 430}, {184, 369}, house_48, 1},
        {courier_south_1, {553, 676}, {461, 553}, house_72, 1}
    ],
    
    lists:foreach(fun({Courier, From, To, House, Orders}) ->
        spawn(fun() ->
            animate_delivery(Courier, From, To, House, Orders)
        end),
        timer:sleep(1000)
    end, Deliveries).

%% Animate a delivery
animate_delivery(CourierID, {FromX, FromY}, {ToX, ToY}, HouseID, OrderCount) ->
    %% Add delivery route
    RouteID = list_to_atom(atom_to_list(CourierID) ++ "_route"),
    map_visual:add_order(RouteID, {FromX, FromY}, {ToX, ToY}),
    
    %% Animate movement (10 steps)
    Steps = 15,
    DX = (ToX - FromX) / Steps,
    DY = (ToY - FromY) / Steps,
    
    lists:foreach(fun(Step) ->
        NewX = FromX + (DX * Step),
        NewY = FromY + (DY * Step),
        map_visual:update_courier(CourierID, {round(NewX), round(NewY)}),
        timer:sleep(150)
    end, lists:seq(1, Steps)),
    
    %% Delivery complete - reduce house orders
    timer:sleep(500),
    NewOrders = max(0, OrderCount - 1),
    map_visual:update_house_orders(HouseID, NewOrders),
    
    %% Return to DC (simplified - just teleport back)
    timer:sleep(1000),
    map_visual:update_courier(CourierID, {FromX, FromY}),
    
    ok.

%% Random movements for stress test
random_courier_movements(NumCouriers) ->
    lists:foreach(fun(N) ->
        CourierID = list_to_atom("c_" ++ integer_to_list(N)),
        X = rand:uniform(1100) + 50,
        Y = rand:uniform(700) + 50,
        map_visual:update_courier(CourierID, {X, Y}),
        timer:sleep(200)
    end, lists:seq(1, NumCouriers)),
    random_courier_movements(NumCouriers).