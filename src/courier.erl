%%%-------------------------------------------------------------------
%%% Courier Module - Modified for Zone-Local Operation
%%% Each courier reports to its parent zone manager and uses local tracker
%%%-------------------------------------------------------------------
-module(courier).
-behaviour(gen_statem).

%% API - Modified to accept 3 arguments
-export([start_link/3]).
-export([pause/1, resume/1]).

%% Callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

%% Include record definitions
-include("map_records.hrl").

%% Define fixed zones
-define(FIXED_ZONES, ["north", "center", "south"]).

%%--------------------------------------------------------------------
%% API - Modified to accept zone manager and tracker PIDs
%%--------------------------------------------------------------------
start_link(CourierId, ParentZoneManagerPID, LocationTrackerPID) ->
    gen_statem:start_link({local, list_to_atom("courier_" ++ CourierId)}, 
                          ?MODULE, 
                          [CourierId, ParentZoneManagerPID, LocationTrackerPID], 
                          []).

pause(CourierId) ->
    gen_statem:cast(list_to_atom("courier_" ++ CourierId), pause).

resume(CourierId) ->
    gen_statem:cast(list_to_atom("courier_" ++ CourierId), resume).

callback_mode() -> handle_event_function.

%%--------------------------------------------------------------------
%% init - Modified to accept and store both PIDs
%%--------------------------------------------------------------------
init([CourierId, ParentZoneManagerPID, LocationTrackerPID]) ->
    io:format("Courier ~p starting with zone manager ~p and tracker ~p~n", 
              [CourierId, ParentZoneManagerPID, LocationTrackerPID]),
    
    rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),
    
    report_state_change(CourierId, idle, #{
        delivered_packages => [],
        total_delivered => 0
    }),
    
    report_visual_status(CourierId, no_package),
    
    %% Get zone from courier ID and find business location
    Zone = case string:tokens(CourierId, "_") of
        [ZoneStr, "courier", _] -> list_to_atom(ZoneStr);
        _ -> north  % default
    end,
    
    %% Start at the business/distribution center of the zone
    {InitialLocation, HomeBase} = case map_server:get_business_in_zone(Zone) of
        {ok, Business} ->
            io:format("Courier ~p starting at distribution center: ~p~n", 
                     [CourierId, Business#location.id]),
            {Business#location.id, Business#location.id};
        {error, _} ->
            %% Fallback to random location if no business found
            case map_server:get_random_location() of
                {ok, Loc} ->
                    {Loc#location.id, Loc#location.id};
                {error, _} ->
                    {undefined, undefined}
            end
    end,
    
    {ok, idle, #{
        id => CourierId,
        parent_zone_manager => ParentZoneManagerPID,
        location_tracker => LocationTrackerPID,
        zones => ?FIXED_ZONES,
        delivered_packages => [],
        total_delivered => 0,
        paused => false,
        current_location => InitialLocation,
        home_base => HomeBase,
        pending_event => undefined
    }}.

%%--------------------------------------------------------------------
%% Event Handlers
%%--------------------------------------------------------------------

%% Handle pause
handle_event(cast, pause, StateName, Data) ->
    io:format("Courier ~p paused in state ~p~n", [maps:get(id, Data), StateName]),
    {keep_state, Data#{paused => true}};

%% Handle resume
handle_event(cast, resume, StateName, Data) ->
    io:format("Courier ~p resumed in state ~p~n", [maps:get(id, Data), StateName]),
    NewData = Data#{paused => false},
    case maps:get(pending_event, NewData) of
        undefined ->
            {keep_state, NewData};
        PendingEvent ->
            self() ! PendingEvent,
            {keep_state, NewData#{pending_event => undefined}}
    end;

%% Handle delivery assignment
handle_event(cast, {assign_delivery, PackageId, FromZone}, idle, Data) ->
    case maps:get(paused, Data) of
        true -> {keep_state, Data};
        false ->
            CourierId = maps:get(id, Data),
            LocationTrackerPID = maps:get(location_tracker, Data),
            
            io:format("Courier(~p) received new assignment: package ~p from zone ~p~n", 
                      [CourierId, PackageId, FromZone]),
            
            CurrentLocation = maps:get(current_location, Data),
            case get_package_locations(PackageId, FromZone) of
                {ok, BusinessLocation, HomeLocation} ->
                    package:update_status(PackageId, picking_up),
                    
                    CourierPid = self(),
                    StartCallback = fun() -> CourierPid ! pickup_complete end,
                    
                    %% Use local tracker via PID
                    case gen_server:call(LocationTrackerPID, 
                                       {start_tracking, CourierId, CurrentLocation, 
                                        BusinessLocation, StartCallback}) of
                        {ok, EstimatedTime} ->
                            io:format("Courier(~p) will arrive for package ~p in ~p seconds~n",
                                    [CourierId, PackageId, round(EstimatedTime)]),
                            
                            report_state_change(CourierId, picking_up, #{
                                package => PackageId,
                                eta => round(EstimatedTime * 1000),
                                destination => BusinessLocation
                            }),
                            
                            {next_state, picking_up, Data#{
                                package => PackageId,
                                zone => FromZone,
                                business_location => BusinessLocation,
                                home_location => HomeLocation
                            }};
                        Error ->
                            io:format("Courier(~p) failed to start tracking: ~p~n", [CourierId, Error]),
                            {keep_state, Data}
                    end;
                {error, Reason} ->
                    io:format("Courier(~p) failed to get package locations: ~p~n", [CourierId, Reason]),
                    {keep_state, Data}
            end
    end;

%% Handle busy state - reject assignment
handle_event(cast, {assign_delivery, PackageId, FromZone}, StateName, Data) when StateName =/= idle ->
    io:format("Courier(~p) BUSY in state ~p, rejecting package from zone ~p~n",
              [maps:get(id, Data), StateName, FromZone]),
    
    %% Notify parent zone manager directly via PID
    ParentZoneManager = maps:get(parent_zone_manager, Data),
    gen_statem:cast(ParentZoneManager, {assignment_failed, PackageId, maps:get(id, Data)}),
    {keep_state, Data};

%% Handle pickup complete
handle_event(info, pickup_complete, picking_up, Data) ->
    case maps:get(paused, Data) of
        true ->
            {keep_state, Data#{pending_event => pickup_complete}};
        false ->
            CourierId = maps:get(id, Data),
            PackageId = maps:get(package, Data),
            LocationTrackerPid = maps:get(location_tracker, Data),
            
            io:format("Courier(~p) arrived at restaurant, picking up package ~p!~n", 
                      [CourierId, PackageId]),
            package:update_status(PackageId, in_transit),
            
            %% Report carrying package
            report_visual_status(CourierId, has_package),
            
            BusinessLocation = maps:get(business_location, Data),
            HomeLocation = maps:get(home_location, Data),
            
            %% Get the FULL route for visualization
            case map_server:get_route(BusinessLocation, HomeLocation) of
                {ok, FullRoute} ->
                    %% Send full route to visualization
                    VisualizationNode = 'visualization@127.0.0.1',
                    RouteData = #{
                        courier_id => CourierId,
                        type => delivery,
                        path => FullRoute,  % Full path with all waypoints
                        from => BusinessLocation,
                        to => HomeLocation
                    },
                    catch rpc:call(VisualizationNode, visualization_server, 
                                  set_courier_route, [CourierId, RouteData]);
                _ ->
                    ok
            end,
            
            %% Stop tracking via local tracker PID
            gen_server:cast(LocationTrackerPid, {stop_tracking, CourierId}),
            
            CourierPid = self(),
            DeliveryCallback = fun() -> CourierPid ! delivery_complete end,
            
            %% Start new tracking via local tracker PID  
            case gen_server:call(LocationTrackerPid,
                               {start_tracking, CourierId, BusinessLocation, 
                                HomeLocation, DeliveryCallback}) of
                {ok, EstimatedTime} ->
                    io:format("Courier(~p) heading to customer with package ~p, ETA: ~p seconds~n",
                            [CourierId, PackageId, round(EstimatedTime)]),
                    
                    Zone = maps:get(zone, Data, "unknown"),
                    report_state_change(CourierId, delivering, #{
                        package => PackageId,
                        zone => Zone,
                        eta => round(EstimatedTime * 1000),
                        destination => HomeLocation
                    }),
                    
                    {next_state, delivering, Data#{current_location => BusinessLocation}};
                Error ->
                    io:format("Courier(~p) failed to start delivery tracking: ~p~n", 
                              [CourierId, Error]),
                    {keep_state, Data}
            end
    end;

%% Handle delivery complete
handle_event(info, delivery_complete, delivering, Data) ->
    case maps:get(paused, Data) of
        true ->
            {keep_state, Data#{pending_event => delivery_complete}};
        false ->
            CourierId = maps:get(id, Data),
            PackageId = maps:get(package, Data),
            LocationTrackerPID = maps:get(location_tracker, Data),
            ParentZoneManager = maps:get(parent_zone_manager, Data),
            HomeLocation = maps:get(home_location, Data, undefined),
            
            io:format("Courier(~p) delivered package ~p, now available!~n", 
                      [CourierId, PackageId]),
            package:update_status(PackageId, delivered),
			
			case extract_house_from_package(PackageId) of
				{ok, HouseId} ->
					VisualizationNode = 'visualization@127.0.0.1',
					catch rpc:call(VisualizationNode, visualization_server, 
								  package_delivered, [HouseId]);
				_ -> ok
			end,
            
            %% דיווח שהשליח כבר לא נושא חבילה
            report_visual_status(CourierId, no_package),
            
            %% Stop tracking via local tracker PID
            gen_server:cast(LocationTrackerPID, {stop_tracking, CourierId}),
            
            DeliveredPackages = maps:get(delivered_packages, Data),
            TotalDelivered = maps:get(total_delivered, Data),
            NewDeliveredPackages = [PackageId | DeliveredPackages],
            NewTotalDelivered = TotalDelivered + 1,
            
            report_state_change(CourierId, idle, #{
                delivered_packages => NewDeliveredPackages,
                total_delivered => NewTotalDelivered
            }),
            
            %% Report availability to parent zone manager via PID
            gen_statem:cast(ParentZoneManager, {courier_available, CourierId}),
            
            NewData = maps:remove(package, Data#{
                delivered_packages => NewDeliveredPackages,
                total_delivered => NewTotalDelivered,
                zone => null,
                current_location => HomeLocation,
                business_location => undefined,
                home_location => undefined
            }),
            
            {next_state, idle, NewData}
    end;

%% Handle unhandled events
handle_event(EventType, Event, StateName, Data) ->
    io:format("Courier(~p) in state ~p received unhandled event: ~p (~p)~n",
              [maps:get(id, Data), StateName, Event, EventType]),
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

%% Parse package locations - FIXED PARSING LOGIC
get_package_locations(PackageId, Zone) ->
    io:format("Courier: Getting locations for package ~p in zone ~p~n", [PackageId, Zone]),
    
    %% Package format: "zone_home_NUMBER_ordernum_timestamp" 
    %% Example: "north_home_32_1_1757151850956"
    case parse_home_from_package_id(PackageId) of
        {ok, HomeId} ->
            io:format("Courier: Successfully parsed home ID: ~p from package ~p~n", [HomeId, PackageId]),
            case map_server:get_business_in_zone(list_to_atom(Zone)) of
                {ok, Business} ->
                    io:format("Courier: Will deliver from ~p to ~p~n", 
                              [Business#location.id, HomeId]),
                    {ok, Business#location.id, HomeId};
                Error ->
                    io:format("Courier: Failed to get business for zone ~p: ~p~n", [Zone, Error]),
                    Error
            end;
        {error, _Reason} ->
            io:format("Courier: Could not parse home ID from package ~p, using random home in zone ~p~n", 
                      [PackageId, Zone]),
            case map_server:get_business_in_zone(list_to_atom(Zone)) of
                {ok, Business} ->
                    case map_server:get_random_home_in_zone(list_to_atom(Zone)) of
                        {ok, Home} -> {ok, Business#location.id, Home#location.id};
                        Error -> Error
                    end;
                Error -> Error
            end
    end.

%% Parse home ID from package ID - NEW ROBUST PARSER
parse_home_from_package_id(PackageId) ->
    %% Split by underscore
    Parts = string:tokens(PackageId, "_"),
    
    %% Expected format: ["zone", "home", "NUMBER", ...]
    %% We need at least 3 parts and the second part should be "home"
    case Parts of
        [_Zone, "home", NumberStr | _Rest] ->
            %% Successfully found home pattern
            HomeId = "home_" ++ NumberStr,
            {ok, HomeId};
        _ ->
            %% Try alternative parsing in case format is different
            case find_home_pattern_in_parts(Parts) of
                {ok, HomeId} -> {ok, HomeId};
                error -> {error, invalid_package_format}
            end
    end.

%% Alternative pattern finder - looks for "home" followed by a number anywhere in the parts
find_home_pattern_in_parts([]) -> error;
find_home_pattern_in_parts(["home", NumberStr | _Rest]) when length(NumberStr) > 0 ->
    %% Check if NumberStr starts with digits
    case is_numeric_start(NumberStr) of
        true -> {ok, "home_" ++ NumberStr};
        false -> error
    end;
find_home_pattern_in_parts([_Head | Rest]) ->
    find_home_pattern_in_parts(Rest).

%% Check if string starts with numeric characters
is_numeric_start([]) -> false;
is_numeric_start([H|_]) when H >= $0, H =< $9 -> true;
is_numeric_start(_) -> false.

report_state_change(CourierId, NewStatus, AdditionalData) ->
    case whereis(logistics_state_collector) of
        undefined ->
            io:format("DEBUG: State Collector not available for courier ~p state change~n", [CourierId]);
        _ ->
            StateData = maps:merge(AdditionalData, #{status => NewStatus}),
            logistics_state_collector:courier_state_changed(CourierId, StateData)
    end.

%% Helper function to report visual status to visualization server
report_visual_status(CourierId, Status) ->
    VisualizationNode = 'visualization@127.0.0.1',
    case rpc:call(VisualizationNode, visualization_server, 
                  update_courier_status, [CourierId, Status]) of
        {badrpc, Reason} ->
            io:format("Courier ~p: Failed to update visual status: ~p~n", 
                     [CourierId, Reason]);
        _ ->
            io:format("Courier ~p: Visual status updated to ~p~n", 
                     [CourierId, Status])
    end.

%%--------------------------------------------------------------------
%% Required callbacks
%%--------------------------------------------------------------------
terminate(_Reason, _State, _Data) -> ok.
code_change(_OldVsn, State, Data, _Extra) -> {ok, State, Data}.



extract_house_from_package(PackageId) ->
    %% Package format: "zone_home_NUMBER_ordernum_timestamp"
    case string:tokens(PackageId, "_") of
        [_Zone, "home", NumStr | _] ->
            {ok, NumStr};
        _ ->
            error
    end.