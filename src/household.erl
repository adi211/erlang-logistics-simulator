%%%-------------------------------------------------------------------
%%% @doc Household Module - Autonomous Order Generation
%%% Generates packages following a Poisson distribution pattern
%%%-------------------------------------------------------------------
-module(household).
-behaviour(gen_server).

-include("network_const.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    household_id,
    zone,
    zone_manager_pid,
    state_status = running,  % running | paused
    timer_ref = undefined,
    order_count = 0,
    load_factor = 50        % Add load_factor to state
}).

%%====================================================================
%% API
%%====================================================================

start_link(HouseholdId, Zone, ZoneManagerPid) ->
    ProcessName = list_to_atom("household_" ++ HouseholdId),
    gen_server:start_link({local, ProcessName}, ?MODULE, 
                          [HouseholdId, Zone, ZoneManagerPid], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([HouseholdId, Zone, ZoneManagerPid]) ->
    %% Seed random number generator
    rand:seed(exsplus, {erlang:phash2([HouseholdId]), 
                        erlang:monotonic_time(), 
                        erlang:unique_integer()}),
    
    %% Schedule first order with default load factor
    InitialLoadFactor = 50,
    schedule_next_order(InitialLoadFactor),
    
    {ok, #state{
        household_id = HouseholdId,
        zone = Zone,
        zone_manager_pid = ZoneManagerPid,
        state_status = running,
        timer_ref = undefined,
        order_count = 0,
        load_factor = InitialLoadFactor
    }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Handle load factor updates from zone manager
handle_cast({update_load_factor, NewLoadFactor}, State = #state{timer_ref = TimerRef}) ->
    io:format("Household ~p: Updating load factor to ~p%~n", 
              [State#state.household_id, NewLoadFactor]),
    
    %% Cancel current timer if exists
    case TimerRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TimerRef)
    end,
    
    %% Schedule new order with updated load factor if running
    NewState = State#state{load_factor = NewLoadFactor},
    case State#state.state_status of
        running ->
            %% If load factor is 0, don't schedule any orders
            if
                NewLoadFactor == 0 ->
                    {noreply, NewState#state{timer_ref = undefined}};
                true ->
                    NewTimerRef = schedule_next_order(NewLoadFactor),
                    {noreply, NewState#state{timer_ref = NewTimerRef}}
            end;
        paused ->
            {noreply, NewState}
    end;

handle_cast(pause_simulation, State = #state{timer_ref = TimerRef}) ->
    %% Cancel any pending timer
    case TimerRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TimerRef)
    end,
    {noreply, State#state{state_status = paused, timer_ref = undefined}};

handle_cast(resume_simulation, State = #state{state_status = paused, load_factor = LoadFactor}) ->
    %% Resume by scheduling next order with current load factor
    if
        LoadFactor == 0 ->
            {noreply, State#state{state_status = running, timer_ref = undefined}};
        true ->
            TimerRef = schedule_next_order(LoadFactor),
            {noreply, State#state{state_status = running, timer_ref = TimerRef}}
    end;

handle_cast(resume_simulation, State) ->
    %% Already running, ignore
    {noreply, State};

handle_cast(stop_simulation, State = #state{timer_ref = TimerRef}) ->
    %% Cancel timer and terminate
    case TimerRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TimerRef)
    end,
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(create_order, State = #state{state_status = running,
                                          household_id = HouseholdId,
                                          zone = Zone,
                                          zone_manager_pid = ZoneManagerPid,
                                          order_count = OrderCount,
                                          load_factor = LoadFactor}) ->
    %% Check if load factor is 0
    if
        LoadFactor == 0 ->
            %% Don't create orders or schedule new ones
            {noreply, State#state{timer_ref = undefined}};
        true ->
            %% Generate unique package ID
            Timestamp = erlang:system_time(millisecond),
            PackageId = lists:flatten(io_lib:format("~s_~s_~p_~p", 
                                                    [Zone, HouseholdId, OrderCount + 1, Timestamp])),
            
            io:format("Household ~p creating package ~p (Load Factor: ~p%)~n", 
                     [HouseholdId, PackageId, LoadFactor]),
            
            %% Start package process via RPC to control node where the code exists
            try
                case rpc:call(?CTRL_NODE, package, start_link, [PackageId, Zone]) of
                    {ok, _Pid} ->
                        io:format("Household ~p: Package ~p process started on control node~n", 
                                 [HouseholdId, PackageId]);
                    {error, {already_started, _}} ->
                        io:format("Household ~p: Package ~p already exists~n", 
                                 [HouseholdId, PackageId]);
                    {badrpc, RPCReason} ->
                        io:format("Household ~p: Failed to start package ~p via RPC: ~p~n", 
                                 [HouseholdId, PackageId, RPCReason]);
                    Error ->
                        io:format("Household ~p: Failed to start package ~p: ~p~n", 
                                 [HouseholdId, PackageId, Error])
                end
            catch
                Type:CatchReason ->
                    io:format("Household ~p: Error starting package ~p - ~p:~p~n", 
                             [HouseholdId, PackageId, Type, CatchReason])
            end,
            
            %% Notify zone manager about new package
            gen_server:cast(ZoneManagerPid, {new_package, self(), HouseholdId, PackageId}),
            
            %% Schedule next order with current load factor
            TimerRef = schedule_next_order(LoadFactor),
            
            {noreply, State#state{timer_ref = TimerRef, order_count = OrderCount + 1}}
    end;

handle_info(create_order, State = #state{state_status = paused}) ->
    %% Ignore if paused
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{household_id = HouseholdId}) ->
    io:format("Household ~p terminating~n", [HouseholdId]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Schedule next order based on load factor
schedule_next_order(LoadFactor) when LoadFactor == 0 ->
    %% Don't schedule if load factor is 0
    undefined;
schedule_next_order(LoadFactor) ->
    %% Calculate delay based on load factor
    %% Load factor 0-100 maps to delay 180000-10000 ms (3 minutes to 10 seconds)
    BaseMaxDelay = 180000,  % 3 minutes
    BaseMinDelay = 10000,   % 10 seconds
    
    %% Linear interpolation: higher load factor = shorter delays
    MeanDelay = BaseMaxDelay - round((BaseMaxDelay - BaseMinDelay) * LoadFactor / 100),
    
    %% Apply exponential distribution for Poisson process
    RandomValue = -math:log(rand:uniform()) * MeanDelay,
    Delay = round(max(1000, RandomValue)),  % Minimum 1 second
    
    io:format("Next order scheduled in ~p ms (Load Factor: ~p%, Mean: ~p ms)~n", 
             [Delay, LoadFactor, MeanDelay]),
    
    erlang:send_after(Delay, self(), create_order).