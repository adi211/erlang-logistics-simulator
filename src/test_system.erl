%%%-------------------------------------------------------------------
%%% @doc Test script for logistics system
%%% Run this to test different modes
%%%-------------------------------------------------------------------
-module(test_system).
-export([test_visual/0, test_stress/0, compile_all/0]).

%% Compile all modules
compile_all() ->
    Files = [
        control_center,
        dashboard_server,
        zone_manager,
        visualization_server,
        map_visual,
        stress_log
    ],
    
    Results = lists:map(fun(Module) ->
        case compile:file(atom_to_list(Module), [debug_info, {outdir, "."}]) of
            {ok, _} -> 
                io:format("✓ Compiled ~p successfully~n", [Module]),
                {Module, ok};
            Error -> 
                io:format("✗ Failed to compile ~p: ~p~n", [Module, Error]),
                {Module, Error}
        end
    end, Files),
    
    %% Check if all compiled successfully
    Failed = [M || {M, R} <- Results, R =/= ok],
    case Failed of
        [] -> 
            io:format("~n✅ All modules compiled successfully!~n"),
            ok;
        _ ->
            io:format("~n❌ Failed modules: ~p~n", [Failed]),
            error
    end.

%% Test visual mode
test_visual() ->
    io:format("Starting Visualization Server in VISUAL mode...~n"),
    io:format("========================================~n"),
    
    %% First compile everything
    case compile_all() of
        ok ->
            %% Start visualization server in visual mode
            case visualization_server:start(visual) of
                {ok, Pid} ->
                    io:format("✓ Visualization server started: ~p~n", [Pid]),
                    io:format("~nYou should see a map window appear.~n"),
                    io:format("The map will show 3 zones (North, Center, South).~n"),
                    
                    %% Test some courier updates
                    timer:sleep(1000),
                    io:format("~nTesting courier position updates...~n"),
                    visualization_server:update_courier_position(courier_1, {25, 25}),
                    timer:sleep(500),
                    visualization_server:update_courier_position(courier_1, {30, 30}),
                    timer:sleep(500),
                    visualization_server:update_courier_position(courier_1, {35, 35}),
                    
                    %% Test house orders
                    io:format("Testing house order updates...~n"),
                    visualization_server:update_house_orders(house_1, 3),
                    visualization_server:update_house_orders(house_2, 5),
                    
                    io:format("~n✅ Visual mode test completed!~n"),
                    {ok, Pid};
                Error ->
                    io:format("❌ Failed to start visualization server: ~p~n", [Error]),
                    Error
            end;
        error ->
            io:format("Cannot start - compilation failed~n"),
            error
    end.

%% Test stress mode
test_stress() ->
    io:format("Starting Visualization Server in STRESS mode...~n"),
    io:format("==========================================~n"),
    
    %% First compile everything
    case compile_all() of
        ok ->
            %% Start visualization server in stress mode
            case visualization_server:start(stress) of
                {ok, Pid} ->
                    io:format("✓ Visualization server started: ~p~n", [Pid]),
                    io:format("~nYou should see a log viewer window appear.~n"),
                    
                    %% Simulate some log messages
                    timer:sleep(1000),
                    io:format("~nGenerating test log messages...~n"),
                    
                    visualization_server:log_message(info, "System", "Stress test mode initialized"),
                    timer:sleep(200),
                    
                    visualization_server:log_message(info, "Zone North", "Zone manager started"),
                    timer:sleep(200),
                    
                    visualization_server:log_message(success, "Delivery", "Courier C001 completed delivery D1234"),
                    timer:sleep(200),
                    
                    visualization_server:log_message(warning, "Load", "Zone Center queue length exceeding threshold"),
                    timer:sleep(200),
                    
                    visualization_server:log_message(error, "Network", "Lost connection to Zone South"),
                    timer:sleep(200),
                    
                    %% Update some statistics
                    io:format("~nUpdating statistics...~n"),
                    gen_server:cast(visualization_server, {update_process_count, 1500}),
                    timer:sleep(500),
                    
                    gen_server:cast(visualization_server, {delivery_completed, courier_1, delivery_1}),
                    gen_server:cast(visualization_server, {delivery_completed, courier_2, delivery_2}),
                    
                    io:format("~n✅ Stress mode test completed!~n"),
                    io:format("Check the log viewer window for messages.~n"),
                    {ok, Pid};
                Error ->
                    io:format("❌ Failed to start visualization server: ~p~n", [Error]),
                    Error
            end;
        error ->
            io:format("Cannot start - compilation failed~n"),
            error
    end.