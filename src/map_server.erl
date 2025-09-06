-module(map_server).
-behaviour(gen_server).

%% Include the record definitions - CRITICAL FIRST STEP
-include("map_records.hrl").

%% API
-export([start_link/0, initialize_map_from_module/1]).
-export([get_location/1, get_distance/2, get_zone_info/1]).
-export([update_courier_position/2, get_courier_position/1, get_all_courier_positions/0]).
-export([get_business_in_zone/1, get_random_home_in_zone/1]).
-export([get_route_distance/2, get_neighbors/1]).
-export([get_random_location/0, get_all_locations/0]).
-export([get_route/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    initialized = false,
    courier_positions = #{}
}).

%% -----------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% New function to initialize map from Erlang module
initialize_map_from_module(MapModule) when is_atom(MapModule) ->
    gen_server:call(?MODULE, {initialize_map_from_module, MapModule}, 10000).

get_location(LocationId) ->
    gen_server:call(?MODULE, {get_location, LocationId}).

get_distance(FromId, ToId) ->
    gen_server:call(?MODULE, {get_distance, FromId, ToId}).

get_zone_info(Zone) ->
    gen_server:call(?MODULE, {get_zone_info, Zone}).

update_courier_position(CourierId, PositionData) ->
    gen_server:cast(?MODULE, {update_courier_position, CourierId, PositionData}).

get_courier_position(CourierId) ->
    gen_server:call(?MODULE, {get_courier_position, CourierId}).

get_all_courier_positions() ->
    gen_server:call(?MODULE, get_all_courier_positions).

get_business_in_zone(Zone) ->
    gen_server:call(?MODULE, {get_business_in_zone, Zone}).

get_random_home_in_zone(Zone) ->
    gen_server:call(?MODULE, {get_random_home_in_zone, Zone}).

get_random_location() ->
    gen_server:call(?MODULE, get_random_location).

get_all_locations() ->
    gen_server:call(?MODULE, get_all_locations).

get_route_distance(FromId, ToId) ->
    gen_server:call(?MODULE, {get_route_distance, FromId, ToId}).

get_neighbors(LocationId) ->
    gen_server:call(?MODULE, {get_neighbors, LocationId}).

get_route(FromId, ToId) ->
    gen_server:call(?MODULE, {get_route, FromId, ToId}).

%% -----------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------

init([]) ->
    io:format("Map Server starting...~n"),
    rand:seed(exsplus, {erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()}),
    
    %% Create ETS tables if they don't exist
    create_ets_tables(),
    
    {ok, #state{}}.

%% Handle the new initialization from module
handle_call({initialize_map_from_module, MapModule}, _From, State) ->
    io:format("Map Server: Initializing map from module ~p...~n", [MapModule]),
    
    try
        %% Load the map data from the module
        MapData = MapModule:get_map(),
        
        %% Extract components
        Homes = maps:get(homes, MapData, []),
        Businesses = maps:get(businesses, MapData, []),
        Roads = maps:get(roads, MapData, []),
        
        %% Clear existing data
        ets:delete_all_objects(map_locations),
        ets:delete_all_objects(map_roads),
        ets:delete_all_objects(map_graph),
        
        %% Convert and store locations
        LocationRecords = convert_to_location_records(Homes, Businesses),
        store_locations(LocationRecords),
        
        %% Convert and store roads (bidirectional)
        RoadRecords = convert_to_road_records(Roads, LocationRecords),
        store_roads(RoadRecords),
        
        %% Build graph structure
        build_graph_structure(LocationRecords, RoadRecords),
        
        io:format("Map Server: Successfully loaded ~p locations and ~p roads~n", 
                  [length(LocationRecords), length(RoadRecords)]),
        
        %% Report initialization if state collector exists
        report_map_initialized(MapData),
        
        {reply, {ok, map_initialized}, State#state{initialized = true}}
    catch
        Type:Error ->
            io:format("Map Server: Failed to initialize map - ~p:~p~n", [Type, Error]),
            {reply, {error, {initialization_failed, Error}}, State}
    end;

handle_call({get_location, LocationId}, _From, State) ->
    case State#state.initialized of
        false ->
            {reply, {error, map_not_initialized}, State};
        true ->
            case ets:lookup(map_locations, LocationId) of
                [{_, Location}] ->
                    {reply, {ok, Location}, State};
                [] ->
                    {reply, {error, location_not_found}, State}
            end
    end;

handle_call({get_distance, FromId, ToId}, _From, State) ->
    case State#state.initialized of
        false -> {reply, {error, map_not_initialized}, State};
        true ->
            Distance = calculate_direct_distance(FromId, ToId),
            {reply, Distance, State}
    end;

handle_call({get_zone_info, Zone}, _From, State) ->
    case State#state.initialized of
        false -> {reply, {error, map_not_initialized}, State};
        true ->
            Info = get_zone_statistics(Zone),
            {reply, {ok, Info}, State}
    end;

handle_call({get_courier_position, CourierId}, _From, State) ->
    case maps:get(CourierId, State#state.courier_positions, undefined) of
        undefined -> {reply, {error, courier_not_found}, State};
        Position -> {reply, {ok, Position}, State}
    end;

handle_call(get_all_courier_positions, _From, State) ->
    {reply, {ok, State#state.courier_positions}, State};

handle_call({get_business_in_zone, Zone}, _From, State) ->
    case State#state.initialized of
        false -> {reply, {error, map_not_initialized}, State};
        true ->
            AllLocations = ets:tab2list(map_locations),
            Businesses = [L || {_, L} <- AllLocations, 
                              L#location.type == business, 
                              L#location.zone == Zone],
            case Businesses of
                [] -> {reply, {error, business_not_found}, State};
                [Business|_] -> {reply, {ok, Business}, State}
            end
    end;

handle_call({get_random_home_in_zone, Zone}, _From, State) ->
    case State#state.initialized of
        false -> {reply, {error, map_not_initialized}, State};
        true ->
            AllLocations = ets:tab2list(map_locations),
            HomesInZone = [L || {_, L} <- AllLocations, 
                                L#location.type == home, 
                                L#location.zone == Zone],
            case HomesInZone of
                [] -> {reply, {error, no_homes_in_zone}, State};
                Homes ->
                    RandomHome = lists:nth(rand:uniform(length(Homes)), Homes),
                    {reply, {ok, RandomHome}, State}
            end
    end;

handle_call(get_all_locations, _From, State) ->
    case State#state.initialized of
        false -> {reply, {error, map_not_initialized}, State};
        true ->
            AllLocations = ets:tab2list(map_locations),
            Locations = [L || {_, L} <- AllLocations],
            {reply, {ok, Locations}, State}
    end;

handle_call(get_random_location, _From, State) ->
    case State#state.initialized of
        false -> {reply, {error, map_not_initialized}, State};
        true ->
            AllLocations = ets:tab2list(map_locations),
            case AllLocations of
                [] -> {reply, {error, no_locations_on_map}, State};
                _ ->
                    {_, RandomLocation} = lists:nth(rand:uniform(length(AllLocations)), AllLocations),
                    {reply, {ok, RandomLocation}, State}
            end
    end;

handle_call({get_route_distance, FromId, ToId}, _From, State) ->
    case State#state.initialized of
        false -> {reply, {error, map_not_initialized}, State};
        true ->
            case calculate_direct_distance(FromId, ToId) of
                {ok, DirectDistance} ->
                    RouteDistance = round(DirectDistance * 1.3),
                    {reply, {ok, RouteDistance}, State};
                Error -> {reply, Error, State}
            end
    end;

handle_call({get_neighbors, LocationId}, _From, State) ->
    case State#state.initialized of
        false -> {reply, {error, map_not_initialized}, State};
        true ->
            case ets:lookup(map_graph, LocationId) of
                [{_, Neighbors}] -> {reply, {ok, Neighbors}, State};
                [] -> {reply, {error, location_not_found}, State}
            end
    end;

handle_call({get_route, FromId, ToId}, _From, State) ->
    case State#state.initialized of
        false -> {reply, {error, map_not_initialized}, State};
        true ->
            case dijkstra(FromId, ToId) of
                {ok, Path} ->
                    Weight = calculate_path_weight(Path),
                    io:format("~n=== ROUTE CALCULATION ===~n"),
                    io:format("From: ~p~nTo:   ~p~n", [FromId, ToId]),
                    io:format("CHOSEN PATH [~p nodes, weight=~p]: ~p~n", 
                             [length(Path), Weight, Path]),
                    io:format("========================~n~n"),
                    {reply, {ok, Path}, State};
                {error, Reason} ->
                    io:format("~p: Failed to find route from ~p to ~p: ~p~n", 
                             [?MODULE, FromId, ToId, Reason]),
                    {reply, {error, Reason}, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_courier_position, CourierId, PositionData}, State) ->
    NewPositions = maps:put(CourierId, PositionData, State#state.courier_positions),
    report_courier_position_update(CourierId, PositionData),
    {noreply, State#state{courier_positions = NewPositions}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Map Server terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -----------------------------------------------------------
%% Helper Functions for Map Loading
%% -----------------------------------------------------------

create_ets_tables() ->
    %% Create tables if they don't exist
    case ets:info(map_locations) of
        undefined -> ets:new(map_locations, [set, named_table, public]);
        _ -> ok
    end,
    case ets:info(map_roads) of
        undefined -> ets:new(map_roads, [set, named_table, public]);
        _ -> ok
    end,
    case ets:info(map_graph) of
        undefined -> ets:new(map_graph, [set, named_table, public]);
        _ -> ok
    end.

%% Convert homes and businesses to location records
convert_to_location_records(Homes, Businesses) ->
    HomeRecords = [
        #location{
            id = "home_" ++ integer_to_list(maps:get(id, H)),
            type = home,
            zone = maps:get(zone, H),
            x = maps:get(x, H),
            y = maps:get(y, H),
            address = "Home " ++ integer_to_list(maps:get(id, H))
        } || H <- Homes
    ],
    
    BusinessRecords = [
        #location{
            id = "business_" ++ atom_to_list(maps:get(zone, B)),
            type = business,
            zone = maps:get(zone, B),
            x = maps:get(x, B),
            y = maps:get(y, B),
            address = "Distribution Center " ++ atom_to_list(maps:get(zone, B))
        } || B <- Businesses
    ],
    
    HomeRecords ++ BusinessRecords.

%% Store locations in ETS
store_locations(Locations) ->
    lists:foreach(fun(Loc) ->
        ets:insert(map_locations, {Loc#location.id, Loc})
    end, Locations).

%% Convert road segments to bidirectional road records
convert_to_road_records(RoadSegments, Locations) ->
    lists:flatmap(fun(Segment) ->
        X1 = maps:get(x1, Segment),
        Y1 = maps:get(y1, Segment),
        X2 = maps:get(x2, Segment),
        Y2 = maps:get(y2, Segment),
        
        %% Find closest locations to road endpoints
        Loc1 = find_closest_location(X1, Y1, Locations),
        Loc2 = find_closest_location(X2, Y2, Locations),
        
        case {Loc1, Loc2} of
            {undefined, _} -> [];
            {_, undefined} -> [];
            {L1, L2} when L1#location.id == L2#location.id -> [];
            {L1, L2} ->
                Distance = calculate_distance_between_points(X1, Y1, X2, Y2),
                BaseTime = round(Distance / 10), % Assume 10 meters per second
                
                %% Create bidirectional roads
                [
                    #road{
                        id = L1#location.id ++ "_to_" ++ L2#location.id,
                        from = L1#location.id,
                        to = L2#location.id,
                        distance = Distance,
                        base_time = BaseTime
                    },
                    #road{
                        id = L2#location.id ++ "_to_" ++ L1#location.id,
                        from = L2#location.id,
                        to = L1#location.id,
                        distance = Distance,
                        base_time = BaseTime
                    }
                ]
        end
    end, RoadSegments).

%% Find the closest location to a given point
find_closest_location(X, Y, Locations) ->
    case Locations of
        [] -> undefined;
        _ ->
            {_MinDist, ClosestLoc} = lists:foldl(
                fun(Loc, {MinDist, MinLoc}) ->
                    Dist = calculate_distance_between_points(X, Y, Loc#location.x, Loc#location.y),
                    if
                        Dist < MinDist -> {Dist, Loc};
                        true -> {MinDist, MinLoc}
                    end
                end,
                {999999, undefined},
                Locations
            ),
            ClosestLoc
    end.

%% Calculate distance between two points
calculate_distance_between_points(X1, Y1, X2, Y2) ->
    DX = X2 - X1,
    DY = Y2 - Y1,
    round(math:sqrt(DX * DX + DY * DY)).

%% Store roads in ETS
store_roads(Roads) ->
    lists:foreach(fun(Road) ->
        ets:insert(map_roads, {Road#road.id, Road})
    end, Roads).

%% Build graph structure for pathfinding
build_graph_structure(Locations, Roads) ->
    %% Initialize graph with empty neighbor lists for all locations
    lists:foreach(fun(Loc) ->
        ets:insert(map_graph, {Loc#location.id, []})
    end, Locations),
    
    %% Add neighbors based on roads
    lists:foreach(fun(Road) ->
        %% Get current neighbors for the 'from' location
        case ets:lookup(map_graph, Road#road.from) of
            [{From, CurrentNeighbors}] ->
                %% Add this road as a neighbor connection
                NewNeighbor = {Road#road.to, Road#road.distance, Road#road.base_time},
                UpdatedNeighbors = [NewNeighbor | CurrentNeighbors],
                ets:insert(map_graph, {From, UpdatedNeighbors});
            [] ->
                %% Location doesn't exist, skip
                ok
        end
    end, Roads).

%% -----------------------------------------------------------
%% Original Helper Functions (unchanged)
%% -----------------------------------------------------------

calculate_path_weight([_]) -> 0;
calculate_path_weight([Node1, Node2 | Rest]) ->
    case get_segment_distance(Node1, Node2) of
        {ok, Dist} ->
            Dist + calculate_path_weight([Node2 | Rest]);
        {error, _} ->
            infinity
    end;
calculate_path_weight([]) -> 0.

get_segment_distance(From, To) ->
    case ets:lookup(map_graph, From) of
        [{_, Neighbors}] ->
            case lists:keyfind(To, 1, Neighbors) of
                {To, Distance, _Time} -> {ok, Distance};
                false -> {error, {segment_not_found, From, To}}
            end;
        [] -> {error, {node_not_found, From}}
    end.

calculate_direct_distance(FromId, ToId) ->
    case {ets:lookup(map_locations, FromId), ets:lookup(map_locations, ToId)} of
        {[{_, From}], [{_, To}]} ->
            DX = From#location.x - To#location.x,
            DY = From#location.y - To#location.y,
            Distance = round(math:sqrt(DX*DX + DY*DY)),
            {ok, Distance};
        _ -> {error, location_not_found}
    end.

get_zone_statistics(Zone) ->
    AllLocations = ets:tab2list(map_locations),
    LocationsInZone = [L || {_, L} <- AllLocations, L#location.zone == Zone],
    HomesInZone = [L || L <- LocationsInZone, L#location.type == home],
    BusinessesInZone = [L || L <- LocationsInZone, L#location.type == business],
    {CenterX, CenterY} = case LocationsInZone of
        [] -> {0, 0};
        Locs ->
            AvgX = lists:sum([L#location.x || L <- Locs]) div length(Locs),
            AvgY = lists:sum([L#location.y || L <- Locs]) div length(Locs),
            {AvgX, AvgY}
    end,
    #{
        zone => Zone,
        total_locations => length(LocationsInZone),
        homes => length(HomesInZone),
        businesses => length(BusinessesInZone),
        center => {CenterX, CenterY},
        home_ids => [H#location.id || H <- HomesInZone],
        business_ids => [B#location.id || B <- BusinessesInZone]
    }.

report_map_initialized(MapData) ->
    case whereis(logistics_state_collector) of
        undefined -> ok;
        _ ->
            %% Convert map data to JSON-safe format
            JsonData = #{
                <<"type">> => <<"map_initialized">>,
                <<"data">> => #{
                    <<"dimensions">> => maps:get(dimensions, MapData, #{}),
                    <<"stats">> => maps:get(stats, MapData, #{})
                }
            },
            Message = jsx:encode(JsonData),
            logistics_state_collector:broadcast_message(Message)
    end.

report_courier_position_update(CourierId, PositionData) ->
    case whereis(logistics_state_collector) of
        undefined -> ok;
        _ ->
            SafePositionData = convert_position_data_to_safe(PositionData),
            Message = jsx:encode(#{
                type => <<"courier_position_update">>,
                data => maps:merge(#{courier_id => list_to_binary(CourierId)}, SafePositionData)
            }),
            logistics_state_collector:broadcast_message(Message)
    end.

convert_position_data_to_safe(PositionData) ->
    maps:fold(fun(Key, Value, Acc) ->
        SafeKey = convert_to_binary(Key),
        SafeValue = convert_value_to_safe(Value),
        maps:put(SafeKey, SafeValue, Acc)
    end, #{}, PositionData).

convert_value_to_safe(Value) when is_atom(Value) -> 
    atom_to_binary(Value, utf8);
convert_value_to_safe(Value) when is_list(Value) -> 
    case io_lib:printable_list(Value) of 
        true -> list_to_binary(Value);
        false -> list_to_binary(io_lib:format("~p", [Value]))
    end;
convert_value_to_safe(Value) when is_map(Value) -> 
    maps:fold(fun(K, V, Acc) -> 
        maps:put(convert_to_binary(K), convert_value_to_safe(V), Acc) 
    end, #{}, Value);
convert_value_to_safe(Value) when is_binary(Value) -> Value;
convert_value_to_safe(Value) when is_number(Value) -> Value;
convert_value_to_safe(Value) -> 
    list_to_binary(io_lib:format("~p", [Value])).

convert_to_binary(Value) when is_atom(Value) -> 
    atom_to_binary(Value, utf8);
convert_to_binary(Value) when is_list(Value) -> 
    list_to_binary(Value);
convert_to_binary(Value) when is_binary(Value) -> Value;
convert_to_binary(Value) -> 
    list_to_binary(io_lib:format("~p", [Value])).

%% -----------------------------------------------------------
%% Dijkstra's Algorithm Implementation
%% -----------------------------------------------------------

dijkstra(StartNode, EndNode) ->
    Nodes = [Id || {Id, _} <- ets:tab2list(map_locations)],
    Distances = maps:from_list([{Node, infinity} || Node <- Nodes]),
    DistancesWithStart = maps:put(StartNode, 0, Distances),
    PriorityQueue = Nodes,
    Previous = maps:new(),
    
    case dijkstra_loop(EndNode, PriorityQueue, DistancesWithStart, Previous) of
        {ok, FinalPrev} -> {ok, reconstruct_path(EndNode, FinalPrev, [])};
        {error, Reason} -> {error, Reason}
    end.

dijkstra_loop(EndNode, PQ, Distances, Previous) ->
    case find_closest_node(PQ, Distances) of
        {U, _Dist} when U == EndNode -> {ok, Previous};
        {_U, infinity} -> {error, no_path_found};
        {U, DistU} ->
            NewPQ = lists:delete(U, PQ),
            case ets:lookup(map_graph, U) of
                [{_, Neighbors}] ->
                    {NewDistances, NewPrevious} = lists:foldl(
                        fun({V, EdgeWeight, _}, {D, P}) ->
                            Alt = DistU + EdgeWeight,
                            CurrentDistV = maps:get(V, D),
                            if 
                                Alt < CurrentDistV ->
                                    {maps:put(V, Alt, D), maps:put(V, U, P)};
                                true -> 
                                    {D, P}
                            end
                        end, 
                        {Distances, Previous}, 
                        Neighbors
                    ),
                    dijkstra_loop(EndNode, NewPQ, NewDistances, NewPrevious);
                [] ->
                    dijkstra_loop(EndNode, NewPQ, Distances, Previous)
            end;
        none -> {error, no_path_found_in_pq}
    end.

find_closest_node(PQ, Distances) ->
    lists:foldl(
        fun(Node, {ClosestNode, ClosestDist}) ->
            Dist = maps:get(Node, Distances),
            if 
                Dist < ClosestDist -> {Node, Dist};
                true -> {ClosestNode, ClosestDist}
            end
        end,
        {none, infinity},
        PQ
    ).

reconstruct_path(Current, Previous, Path) ->
    case maps:get(Current, Previous, undefined) of
        undefined -> [Current | Path];
        PrevNode -> reconstruct_path(PrevNode, Previous, [Current | Path])
    end.