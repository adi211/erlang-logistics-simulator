%%%-------------------------------------------------------------------
%%% @doc Visualization Server - Manages display based on mode
%%% Visual mode: Shows map_visual
%%% Stress mode: Shows stress_log
%%%-------------------------------------------------------------------
-module(visualization_server).
-behaviour(gen_server).

-include("header.hrl").

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    control_node,      
    zones,            
    total_zones,
    current_display,   % undefined | {visual, Pid} | {stress, Pid}
    display_mode      % visual | stress | none
}).

%%--------------------------------------------------------------------
%% @doc Starts the visualization server
%%--------------------------------------------------------------------
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server - NO automatic display start
%%--------------------------------------------------------------------
init([]) ->
    io:format("Visualization Server: Starting (waiting for control signal)...~n"),
    
    {ok, #state{
        zones = [],
        control_node = undefined,
        total_zones = 0,
        current_display = undefined,
        display_mode = none
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%%--------------------------------------------------------------------
handle_call({connect, ControlNode}, _, State) ->
    io:format("Visualization Server: Control center ~p connected~n", [ControlNode]),
    monitor_node(ControlNode, true),
    {reply, node(), State#state{control_node = ControlNode}};

handle_call(shutdown, _From, State) ->
    io:format("Visualization Server: Shutting down~n"),
    stop_current_display(State),
    {stop, shutdown, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%%--------------------------------------------------------------------
handle_cast({start_visualization, Mode}, State) ->
    io:format("Visualization Server: Starting visualization in ~p mode~n", [Mode]),
    
    %% Stop any existing display
    NewState1 = stop_current_display(State),
    
    %% Start the appropriate display
    NewState2 = case Mode of
        visual ->
            start_visual_display(NewState1);
        stress ->
            start_stress_display(NewState1);
        _ ->
            io:format("Visualization Server: Unknown mode ~p~n", [Mode]),
            NewState1
    end,
    
    {noreply, NewState2};

handle_cast({pause_visualization}, State) ->
    io:format("Visualization Server: Pausing visualization~n"),
    case State#state.current_display of
        {stress, _} ->
            catch stress_log:simulation_pause();
        _ ->
            ok
    end,
    {noreply, State};

handle_cast({stop_visualization}, State) ->
    io:format("Visualization Server: Stopping visualization~n"),
    NewState = stop_current_display(State),
    {noreply, NewState#state{display_mode = none}};

handle_cast({new_zone, PID, ZoneNode}, State = #state{zones = Zones, total_zones = TotalZones}) ->
    io:format("Visualization Server: New zone ~p registered~n", [ZoneNode]),
    
    %% If in stress mode, log this event
    case State#state.display_mode of
        stress ->
            catch stress_log:add_entry(info, "Zone", 
                io_lib:format("Zone ~p connected", [ZoneNode]));
        _ ->
            ok
    end,
    
    {noreply, State#state{zones = Zones ++ [{PID, ZoneNode}], total_zones = TotalZones + 1}};

handle_cast({update_courier_position, CourierID, Position}, State) ->
    %% Only forward if in visual mode
    case State#state.current_display of
        {visual, _Pid} ->
            catch map_visual:update_courier(CourierID, Position);
        _ ->
            ok
    end,
    {noreply, State};

handle_cast({delivery_completed, CourierID, DeliveryID}, State) ->
    %% Log in stress mode
    case State#state.display_mode of
        stress ->
            catch stress_log:add_entry(success, "Delivery", 
                io_lib:format("Courier ~p completed delivery ~p", [CourierID, DeliveryID]));
        _ ->
            ok
    end,
    {noreply, State};

handle_cast({system_event, Level, Category, Message}, State) ->
    %% Forward to stress log if active
    case State#state.display_mode of
        stress ->
            catch stress_log:add_entry(Level, Category, Message);
        _ ->
            ok
    end,
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling info messages
%%--------------------------------------------------------------------
handle_info({nodedown, Node}, State) ->
    io:format("Visualization Server: Node ~p down~n", [Node]),
    
    case State#state.display_mode of
        stress ->
            catch stress_log:add_entry(error, "Network", 
                io_lib:format("Lost connection to node ~p", [Node]));
        _ ->
            ok
    end,
    
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Terminate function
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    stop_current_display(State),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Code change function
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

start_visual_display(State) ->
    io:format("Visualization Server: Starting visual map display~n"),
    
    try
        case map_visual:start() of
            {ok, Pid} ->
                io:format("Visualization Server: Map visual started with PID ~p~n", [Pid]),
                State#state{
                    current_display = {visual, Pid},
                    display_mode = visual
                };
            Error ->
                io:format("Visualization Server: Failed to start map visual: ~p~n", [Error]),
                State
        end
    catch
        E:R ->
            io:format("Visualization Server: Error starting map visual: ~p:~p~n", [E, R]),
            State
    end.

start_stress_display(State) ->
    io:format("Visualization Server: Starting stress log display~n"),
    
    try
        case stress_log:start() of
            {ok, Pid} ->
                io:format("Visualization Server: Stress log started with PID ~p~n", [Pid]),
                stress_log:simulation_start(),
                stress_log:add_entry(info, "System", "Stress mode initialized"),
                State#state{
                    current_display = {stress, Pid},
                    display_mode = stress
                };
            Error ->
                io:format("Visualization Server: Failed to start stress log: ~p~n", [Error]),
                State
        end
    catch
        E:R ->
            io:format("Visualization Server: Error starting stress log: ~p:~p~n", [E, R]),
            State
    end.

stop_current_display(State) ->
    case State#state.current_display of
        {visual, Pid} ->
            io:format("Visualization Server: Stopping map visual~n"),
            %% Send exit signal to kill the process
            catch exit(Pid, shutdown),
            State#state{current_display = undefined};
            
        {stress, Pid} ->
            io:format("Visualization Server: Stopping stress log~n"),
            %% Try to stop gracefully first, then force
            catch stress_log:stop(),
            catch exit(Pid, shutdown),
            State#state{current_display = undefined};
            
        undefined ->
            State
    end.