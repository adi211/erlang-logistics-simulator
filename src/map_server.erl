%%%-------------------------------------------------------------------
%%% Map Server - STANDALONE VERSION with Road-Only Routing
%%% Works without gen_server - Direct ETS operations
%%%-------------------------------------------------------------------
-module(map_server).

-include("map_records.hrl").

%% API
-export([initialize_local_map/1]).
-export([get_location/1, get_distance/2, get_zone_info/1]).
-export([get_business_in_zone/1, get_random_home_in_zone/1]).
-export([get_route_distance/2, get_neighbors/1]).
-export([get_random_location/0, get_all_locations/0]).
-export([get_route/2]).
-export([update_courier_position/2]).

%% ETS table names
-define(MAP_LOCATIONS, map_locations).
-define(MAP_ROADS, map_roads).
-define(MAP_GRAPH, map_graph).

%%====================================================================
%% Initialize Local Map - STANDALONE VERSION
%%====================================================================

initialize_local_map(MapModule) when is_atom(MapModule) ->
    io:format("Map Server: Initializing local map with road network...~n"),
    
    try
        create_local_ets_tables(),
        
        %% Get map data
        MapData = MapModule:get_map(),
        
        Homes = maps:get(homes, MapData, []),
        Businesses = maps:get(businesses, MapData, []),
        Roads = maps:get(roads, MapData, []),
        
        %% Clear existing data
        ets:delete_all_objects(?MAP_LOCATIONS),
        ets:delete_all_objects(?MAP_ROADS),
        ets:delete_all_objects(?MAP_GRAPH),
        
        %% Convert to location records
        LocationRecords = convert_to_location_records(Homes, Businesses),
        
        %% Create junctions at road intersections
        Junctions = create_road_junctions(Roads),
        AllLocations = LocationRecords ++ Junctions,
        
        %% Build road network along actual roads
        AllRoads = build_road_network_proper(Roads, AllLocations),
        
        %% Store everything
        store_locations(AllLocations),
        store_roads(AllRoads),
        build_graph_structure(AllLocations, AllRoads),
        
        %% Verify connectivity
        verify_basic_connectivity(AllLocations),
        
        io:format("Map Server: Loaded ~p locations (~p homes, ~p businesses, ~p junctions) with ~p roads~n", 
                  [length(AllLocations), 
                   length([L || L <- LocationRecords, L#location.type == home]),
                   length([L || L <- LocationRecords, L#location.type == business]),
                   length(Junctions),
                   length(AllRoads)]),
        
        {ok, map_initialized}
    catch
        Type:Error:Stacktrace ->
            io:format("Map Server: Failed - ~p:~p~n~p~n", [Type, Error, Stacktrace]),
            {error, {initialization_failed, Error}}
    end.

%%====================================================================
%% Create road junctions - COMPREHENSIVE APPROACH
%%====================================================================
create_road_junctions(RoadSegments) ->
    %% Step 1: Create junctions at ALL road endpoints
    Endpoints = lists:foldl(fun(Segment, Acc) ->
        X1 = maps:get(x1, Segment, 0),
        Y1 = maps:get(y1, Segment, 0),
        X2 = maps:get(x2, Segment, 0),
        Y2 = maps:get(y2, Segment, 0),
        sets:add_element({X1, Y1}, sets:add_element({X2, Y2}, Acc))
    end, sets:new(), RoadSegments),
    
    %% Step 2: Find all intersection points
    Intersections = find_segment_intersections(RoadSegments),
    
    %% Step 3: Add intermediate junctions along long road segments
    IntermediatePoints = create_intermediate_junctions(RoadSegments, 50), % Every 50 pixels
    
    %% Combine all junction points
    AllJunctionPoints = sets:union(sets:union(Endpoints, Intersections), IntermediatePoints),
    
    %% Create junction locations
    {Junctions, _} = lists:mapfoldl(fun({X, Y}, Index) ->
        Junction = #location{
            id = "junction_" ++ integer_to_list(Index),
            type = junction,
            zone = road,
            x = round(X),
            y = round(Y),
            address = "Junction " ++ integer_to_list(Index)
        },
        {Junction, Index + 1}
    end, 1, sets:to_list(AllJunctionPoints)),
    
    Junctions.

%%====================================================================
%% Create intermediate junctions along long segments
%%====================================================================
create_intermediate_junctions(RoadSegments, MaxDistance) ->
    lists:foldl(fun(Segment, Acc) ->
        X1 = maps:get(x1, Segment, 0),
        Y1 = maps:get(y1, Segment, 0),
        X2 = maps:get(x2, Segment, 0),
        Y2 = maps:get(y2, Segment, 0),
        
        Distance = math:sqrt((X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1)),
        
        if
            Distance > MaxDistance ->
                %% Create intermediate points
                NumPoints = round(Distance / MaxDistance),
                IntPoints = lists:map(fun(I) ->
                    T = I / (NumPoints + 1),
                    IX = X1 + T * (X2 - X1),
                    IY = Y1 + T * (Y2 - Y1),
                    {IX, IY}
                end, lists:seq(1, NumPoints)),
                lists:foldl(fun(P, AccIn) -> sets:add_element(P, AccIn) end, Acc, IntPoints);
            true ->
                Acc
        end
    end, sets:new(), RoadSegments).

%%====================================================================
%% Build road network - IMPROVED VERSION
%%====================================================================
build_road_network_proper(RoadSegments, AllLocations) ->
    %% Step 1: Connect all junctions along road segments
    SegmentRoads = lists:flatmap(fun(Segment) ->
        connect_locations_along_segment(Segment, AllLocations)
    end, RoadSegments),
    
    %% Step 2: Connect homes/businesses to nearest junctions
    ConnectionRoads = connect_homes_and_businesses(AllLocations),
    
    %% Combine and remove duplicates
    AllRoads = SegmentRoads ++ ConnectionRoads,
    remove_duplicate_roads(AllRoads).

%%====================================================================
%% Connect all locations along a road segment
%%====================================================================
connect_locations_along_segment(Segment, AllLocations) ->
    X1 = maps:get(x1, Segment, 0),
    Y1 = maps:get(y1, Segment, 0),
    X2 = maps:get(x2, Segment, 0),
    Y2 = maps:get(y2, Segment, 0),
    
    %% Find ALL locations on this segment (junctions and buildings)
    LocationsOnSegment = lists:filter(fun(Loc) ->
        is_on_segment(Loc#location.x, Loc#location.y, X1, Y1, X2, Y2, 20)
    end, AllLocations),
    
    %% Sort by distance from start
    SortedLocations = lists:sort(fun(A, B) ->
        DistA = point_distance(A#location.x, A#location.y, X1, Y1),
        DistB = point_distance(B#location.x, B#location.y, X1, Y1),
        DistA =< DistB
    end, LocationsOnSegment),
    
    %% Create bidirectional roads between consecutive locations
    create_consecutive_roads(SortedLocations).

%%====================================================================
%% Check if point is on segment with tolerance
%%====================================================================
is_on_segment(PX, PY, X1, Y1, X2, Y2, Tolerance) ->
    %% Check if point is close to the line segment
    
    %% First check bounding box
    MinX = min(X1, X2) - Tolerance,
    MaxX = max(X1, X2) + Tolerance,
    MinY = min(Y1, Y2) - Tolerance,
    MaxY = max(Y1, Y2) + Tolerance,
    
    if
        PX >= MinX, PX =< MaxX, PY >= MinY, PY =< MaxY ->
            %% Calculate perpendicular distance to line
            LineLen = math:sqrt((X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1)),
            if
                LineLen < 0.001 -> 
                    point_distance(PX, PY, X1, Y1) =< Tolerance;
                true ->
                    %% Project point onto line
                    T = ((PX-X1)*(X2-X1) + (PY-Y1)*(Y2-Y1)) / (LineLen*LineLen),
                    T2 = max(0, min(1, T)),
                    ProjX = X1 + T2 * (X2-X1),
                    ProjY = Y1 + T2 * (Y2-Y1),
                    point_distance(PX, PY, ProjX, ProjY) =< Tolerance
            end;
        true ->
            false
    end.

%%====================================================================
%% Connect homes and businesses to nearest junctions
%%====================================================================
connect_homes_and_businesses(AllLocations) ->
    Homes = [L || L <- AllLocations, L#location.type == home],
    Businesses = [L || L <- AllLocations, L#location.type == business],
    Junctions = [L || L <- AllLocations, L#location.type == junction],
    AllRoadSegments = ets:tab2list(?MAP_ROADS), % Get all road segments

    lists:flatmap(fun(Building) ->
        %% 1. Find the road segment closest to this building
        case find_closest_road_segment(Building, AllRoadSegments) of
            {error, _} ->
                %% Fallback: if no road found, connect to absolute nearest junction
                case find_k_nearest(Building, Junctions, 1) of
                    [] -> [];
                    [NearestJunction] -> create_connection_roads(Building, [NearestJunction])
                end;
            {ok, ClosestSegment} ->
                %% 2. Find all junctions that lie ON that specific road segment
                {_Id, RoadRec} = ClosestSegment,
                FromId = RoadRec#road.from,
                ToId = RoadRec#road.to,
                case {get_location(FromId), get_location(ToId)} of
                    {{ok, FromLoc}, {ok, ToLoc}} ->
                        JunctionsOnSegment = find_locations_on_segment(
                            {FromLoc#location.x, FromLoc#location.y},
                            {ToLoc#location.x, ToLoc#location.y},
                            Junctions,
                            20
                        ),
                        %% 3. Connect to the nearest junction ON that segment
                        case find_k_nearest(Building, JunctionsOnSegment, 1) of
                            [] -> []; % No junction found on the segment
                            [NearestJunctionOnSegment] ->
                                create_connection_roads(Building, [NearestJunctionOnSegment])
                        end;
                    _ -> []
                end
        end
    end, Homes ++ Businesses).
	
	
find_closest_road_segment(Location, RoadSegments) ->
    case RoadSegments of
        [] -> {error, no_roads};
        _ ->
            {MinSeg, _MinDist} = lists:foldl(
                fun({_Id, Road} = Segment, {BestSeg, MinDist}) ->
                    Dist = distance_point_to_segment(Location, Road),
                    if
                        Dist < MinDist -> {Segment, Dist};
                        true -> {BestSeg, MinDist}
                    end
                end,
                {{}, infinity},
                RoadSegments
            ),
            {ok, MinSeg}
    end.

distance_point_to_segment(Location, Road) ->
    PX = Location#location.x,
    PY = Location#location.y,
    case {get_location(Road#road.from), get_location(Road#road.to)} of
        {{ok, FromLoc}, {ok, ToLoc}} ->
            X1 = FromLoc#location.x,
            Y1 = FromLoc#location.y,
            X2 = ToLoc#location.x,
            Y2 = ToLoc#location.y,
            
            LineLenSq = (X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1),
            if
                LineLenSq < 0.001 ->
                    point_distance(PX, PY, X1, Y1);
                true ->
                    T = max(0, min(1, ((PX-X1)*(X2-X1) + (PY-Y1)*(Y2-Y1)) / LineLenSq)),
                    ProjX = X1 + T * (X2 - X1),
                    ProjY = Y1 + T * (Y2 - Y1),
                    point_distance(PX, PY, ProjX, ProjY)
            end;
        _ ->
            infinity
    end.

%%====================================================================
%% Find only endpoints where 3+ roads meet (real junctions)
%%====================================================================
find_critical_endpoints(RoadSegments) ->
    %% Count how many segments connect to each endpoint
    EndpointCounts = lists:foldl(fun(Segment, Acc) ->
        X1 = maps:get(x1, Segment, 0),
        Y1 = maps:get(y1, Segment, 0),
        X2 = maps:get(x2, Segment, 0),
        Y2 = maps:get(y2, Segment, 0),
        
        Acc1 = maps:update_with({X1, Y1}, fun(V) -> V + 1 end, 1, Acc),
        maps:update_with({X2, Y2}, fun(V) -> V + 1 end, 1, Acc1)
    end, #{}, RoadSegments),
    
    %% Keep only points where 3+ roads meet
    CriticalPoints = maps:fold(fun(Point, Count, Acc) ->
        if
            Count >= 3 -> sets:add_element(Point, Acc);
            true -> Acc
        end
    end, sets:new(), EndpointCounts),
    
    CriticalPoints.

%%====================================================================
%% Find only real intersections (where roads cross)
%%====================================================================
find_real_intersections(Segments) ->
    find_intersections_recursive(Segments, sets:new()).

%%====================================================================
%% Find all junction points (endpoints and intersections)
%%====================================================================
find_all_junction_points(RoadSegments) ->
    %% Get all endpoints
    Endpoints = lists:foldl(fun(Segment, Acc) ->
        X1 = maps:get(x1, Segment, 0),
        Y1 = maps:get(y1, Segment, 0),
        X2 = maps:get(x2, Segment, 0),
        Y2 = maps:get(y2, Segment, 0),
        sets:add_element({X1, Y1}, sets:add_element({X2, Y2}, Acc))
    end, sets:new(), RoadSegments),
    
    %% Find intersections between segments
    Intersections = find_segment_intersections(RoadSegments),
    
    %% Combine all points
    sets:union(Endpoints, Intersections).

%%====================================================================
%% Find intersection points between road segments
%%====================================================================
find_segment_intersections(Segments) ->
    find_intersections_recursive(Segments, sets:new()).

find_intersections_recursive([], Intersections) ->
    Intersections;
find_intersections_recursive([Seg1 | Rest], Intersections) ->
    NewIntersections = lists:foldl(fun(Seg2, Acc) ->
        case calculate_intersection(Seg1, Seg2) of
            {ok, Point} -> sets:add_element(Point, Acc);
            none -> Acc
        end
    end, Intersections, Rest),
    find_intersections_recursive(Rest, NewIntersections).

calculate_intersection(Seg1, Seg2) ->
    %% Get segment coordinates
    X1 = maps:get(x1, Seg1), Y1 = maps:get(y1, Seg1),
    X2 = maps:get(x2, Seg1), Y2 = maps:get(y2, Seg1),
    X3 = maps:get(x1, Seg2), Y3 = maps:get(y1, Seg2),
    X4 = maps:get(x2, Seg2), Y4 = maps:get(y2, Seg2),
    
    %% Check if segments actually intersect
    Denom = (X1-X2)*(Y3-Y4) - (Y1-Y2)*(X3-X4),
    
    if
        abs(Denom) < 0.001 -> none;
        true ->
            T = ((X1-X3)*(Y3-Y4) - (Y1-Y3)*(X3-X4)) / Denom,
            U = -((X1-X2)*(Y1-Y3) - (Y1-Y2)*(X1-X3)) / Denom,
            
            if
                T >= 0, T =< 1, U >= 0, U =< 1 ->
                    IX = round(X1 + T * (X2 - X1)),
                    IY = round(Y1 + T * (Y2 - Y1)),
                    {ok, {IX, IY}};
                true ->
                    none
            end
    end.



%%====================================================================
%% Create roads along a road segment
%%====================================================================
create_roads_on_segment(Segment, AllLocations) ->
    X1 = maps:get(x1, Segment, 0),
    Y1 = maps:get(y1, Segment, 0),
    X2 = maps:get(x2, Segment, 0),
    Y2 = maps:get(y2, Segment, 0),
    
    %% Find all locations on this segment
    LocationsOnSegment = find_locations_on_segment({X1,Y1}, {X2,Y2}, AllLocations, 15),
    
    %% Sort by distance from start
    SortedLocations = lists:sort(fun(A, B) ->
        DistA = point_distance(A#location.x, A#location.y, X1, Y1),
        DistB = point_distance(B#location.x, B#location.y, X1, Y1),
        DistA =< DistB
    end, LocationsOnSegment),
    
    %% Create roads between consecutive locations
    create_consecutive_roads(SortedLocations).

%%====================================================================
%% Find locations on or near a segment
%%====================================================================
find_locations_on_segment({X1,Y1}, {X2,Y2}, AllLocations, Tolerance) ->
    lists:filter(fun(Loc) ->
        is_location_on_segment(Loc, X1, Y1, X2, Y2, Tolerance)
    end, AllLocations).

is_location_on_segment(Loc, X1, Y1, X2, Y2, Tolerance) ->
    PX = Loc#location.x,
    PY = Loc#location.y,
    
    %% Check bounding box
    MinX = min(X1, X2) - Tolerance,
    MaxX = max(X1, X2) + Tolerance,
    MinY = min(Y1, Y2) - Tolerance,
    MaxY = max(Y1, Y2) + Tolerance,
    
    if
        PX >= MinX, PX =< MaxX, PY >= MinY, PY =< MaxY ->
            %% Check distance to line
            Dist = distance_point_to_line(PX, PY, X1, Y1, X2, Y2),
            Dist =< Tolerance;
        true ->
            false
    end.

distance_point_to_line(PX, PY, X1, Y1, X2, Y2) ->
    LineLen = math:sqrt((X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1)),
    if
        LineLen < 0.001 -> 
            point_distance(PX, PY, X1, Y1);
        true ->
            T = max(0, min(1, ((PX-X1)*(X2-X1) + (PY-Y1)*(Y2-Y1)) / (LineLen*LineLen))),
            ProjX = X1 + T * (X2-X1),
            ProjY = Y1 + T * (Y2-Y1),
            point_distance(PX, PY, ProjX, ProjY)
    end.

%%====================================================================
%% Create consecutive roads between locations
%%====================================================================
create_consecutive_roads([]) -> [];
create_consecutive_roads([_]) -> [];
create_consecutive_roads([L1, L2 | Rest]) ->
    Distance = calculate_distance_between_locations(L1, L2),
    Roads = [
        #road{
            id = L1#location.id ++ "_to_" ++ L2#location.id,
            from = L1#location.id,
            to = L2#location.id,
            distance = Distance,
            base_time = round(Distance / 10)
        },
        #road{
            id = L2#location.id ++ "_to_" ++ L1#location.id,
            from = L2#location.id,
            to = L1#location.id,
            distance = Distance,
            base_time = round(Distance / 10)
        }
    ],
    Roads ++ create_consecutive_roads([L2 | Rest]).

%%====================================================================
%% Connect homes/businesses to road network
%%====================================================================
connect_locations_to_road_network(AllLocations) ->
    Homes = [L || L <- AllLocations, L#location.type == home],
    Businesses = [L || L <- AllLocations, L#location.type == business],
    Junctions = [L || L <- AllLocations, L#location.type == junction],
    
    %% Connect each home/business to nearest junctions
    lists:flatmap(fun(Loc) ->
        NearestJunctions = find_k_nearest(Loc, Junctions, 2),
        create_connection_roads(Loc, NearestJunctions)
    end, Homes ++ Businesses).

create_connection_roads(Location, Junctions) ->
    lists:flatmap(fun(Junction) ->
        Distance = calculate_distance_between_locations(Location, Junction),
        [
            #road{
                id = Location#location.id ++ "_to_" ++ Junction#location.id,
                from = Location#location.id,
                to = Junction#location.id,
                distance = Distance,
                base_time = round(Distance / 10)
            },
            #road{
                id = Junction#location.id ++ "_to_" ++ Location#location.id,
                from = Junction#location.id,
                to = Location#location.id,
                distance = Distance,
                base_time = round(Distance / 10)
            }
        ]
    end, Junctions).

%%====================================================================
%% OPTIMAL PATH FINDING - Dijkstra's Algorithm
%%====================================================================

get_route(FromId, ToId) ->
    case dijkstra_shortest_path(FromId, ToId) of
        {ok, Path} ->
            io:format("Route found from ~p to ~p: ~p nodes~n", 
                     [FromId, ToId, length(Path)]),
            {ok, Path};
        {error, Reason} ->
            io:format("Failed to find route from ~p to ~p: ~p~n", 
                     [FromId, ToId, Reason]),
            {error, Reason}
    end.

dijkstra_shortest_path(StartNode, EndNode) ->
    %% Get all nodes
    AllNodes = [Id || {Id, _} <- ets:tab2list(?MAP_GRAPH)],
    
    case {lists:member(StartNode, AllNodes), lists:member(EndNode, AllNodes)} of
        {false, _} -> {error, {start_node_not_found, StartNode}};
        {_, false} -> {error, {end_node_not_found, EndNode}};
        {true, true} ->
            %% Initialize
            Distances = maps:from_list([{Node, infinity} || Node <- AllNodes]),
            DistancesWithStart = maps:put(StartNode, 0, Distances),
            Previous = maps:new(),
            Unvisited = gb_sets:from_list(AllNodes),
            
            %% Run Dijkstra
            case dijkstra_loop(EndNode, Unvisited, DistancesWithStart, Previous) of
                {ok, FinalPrev} ->
                    Path = reconstruct_path(EndNode, FinalPrev, []),
                    {ok, Path};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

dijkstra_loop(EndNode, Unvisited, Distances, Previous) ->
    case gb_sets:is_empty(Unvisited) of
        true -> {error, no_path_found};
        false ->
            %% Find minimum distance unvisited node
            case find_min_unvisited(gb_sets:to_list(Unvisited), Distances) of
                {none, _} -> {error, no_path_found};
                {_, infinity} -> {error, no_path_found};
                {U, _} when U == EndNode -> {ok, Previous};
                {U, DistU} ->
                    %% Remove from unvisited
                    NewUnvisited = gb_sets:delete(U, Unvisited),
                    
                    %% Update neighbors
                    case ets:lookup(?MAP_GRAPH, U) of
                        [{_, Neighbors}] ->
                            {NewDist, NewPrev} = update_neighbor_distances(
                                Neighbors, U, DistU, Distances, Previous, NewUnvisited),
                            dijkstra_loop(EndNode, NewUnvisited, NewDist, NewPrev);
                        [] ->
                            dijkstra_loop(EndNode, NewUnvisited, Distances, Previous)
                    end
            end
    end.

find_min_unvisited(Nodes, Distances) ->
    lists:foldl(fun(Node, {MinNode, MinDist}) ->
        Dist = maps:get(Node, Distances, infinity),
        if
            Dist < MinDist -> {Node, Dist};
            true -> {MinNode, MinDist}
        end
    end, {none, infinity}, Nodes).

update_neighbor_distances([], _U, _DistU, Distances, Previous, _Unvisited) ->
    {Distances, Previous};
update_neighbor_distances([{V, Weight, _Time} | Rest], U, DistU, Distances, Previous, Unvisited) ->
    case gb_sets:is_member(V, Unvisited) of
        true ->
            CurrentDistV = maps:get(V, Distances, infinity),
            Alt = DistU + Weight,
            {NewDist, NewPrev} = if
                Alt < CurrentDistV ->
                    {maps:put(V, Alt, Distances), maps:put(V, U, Previous)};
                true ->
                    {Distances, Previous}
            end,
            update_neighbor_distances(Rest, U, DistU, NewDist, NewPrev, Unvisited);
        false ->
            update_neighbor_distances(Rest, U, DistU, Distances, Previous, Unvisited)
    end.

reconstruct_path(Current, Previous, Path) ->
    case maps:get(Current, Previous, undefined) of
        undefined -> [Current | Path];
        PrevNode -> reconstruct_path(PrevNode, Previous, [Current | Path])
    end.

%%====================================================================
%% Other API Functions
%%====================================================================

get_location(LocationId) ->
    case ets:lookup(?MAP_LOCATIONS, LocationId) of
        [{_, Location}] -> {ok, Location};
        [] -> {error, location_not_found}
    end.

get_all_locations() ->
    AllLocations = ets:tab2list(?MAP_LOCATIONS),
    Locations = [L || {_, L} <- AllLocations],
    {ok, Locations}.

get_random_location() ->
    AllLocations = ets:tab2list(?MAP_LOCATIONS),
    case AllLocations of
        [] -> {error, no_locations_on_map};
        _ ->
            {_, RandomLocation} = lists:nth(rand:uniform(length(AllLocations)), AllLocations),
            {ok, RandomLocation}
    end.

get_business_in_zone(Zone) ->
    AllLocations = ets:tab2list(?MAP_LOCATIONS),
    Businesses = [L || {_, L} <- AllLocations, 
                      L#location.type == business, 
                      L#location.zone == Zone],
    case Businesses of
        [] -> {error, business_not_found};
        [Business|_] -> {ok, Business}
    end.

get_random_home_in_zone(Zone) ->
    AllLocations = ets:tab2list(?MAP_LOCATIONS),
    HomesInZone = [L || {_, L} <- AllLocations, 
                        L#location.type == home, 
                        L#location.zone == Zone],
    case HomesInZone of
        [] -> {error, no_homes_in_zone};
        Homes ->
            RandomHome = lists:nth(rand:uniform(length(Homes)), Homes),
            {ok, RandomHome}
    end.

get_distance(FromId, ToId) ->
    case {ets:lookup(?MAP_LOCATIONS, FromId), ets:lookup(?MAP_LOCATIONS, ToId)} of
        {[{_, From}], [{_, To}]} ->
            DX = From#location.x - To#location.x,
            DY = From#location.y - To#location.y,
            Distance = round(math:sqrt(DX*DX + DY*DY)),
            {ok, Distance};
        _ -> {error, location_not_found}
    end.

get_route_distance(FromId, ToId) ->
    case dijkstra_shortest_path(FromId, ToId) of
        {ok, Path} when length(Path) > 1 ->
            TotalDistance = calculate_path_distance(Path, 0),
            {ok, TotalDistance};
        {ok, _} ->
            {ok, 0};
        Error ->
            Error
    end.

calculate_path_distance([_], Acc) -> Acc;
calculate_path_distance([From, To | Rest], Acc) ->
    case ets:lookup(?MAP_GRAPH, From) of
        [{_, Neighbors}] ->
            case lists:keyfind(To, 1, Neighbors) of
                {To, Distance, _} ->
                    calculate_path_distance([To | Rest], Acc + Distance);
                false ->
                    calculate_path_distance([To | Rest], Acc)
            end;
        [] ->
            calculate_path_distance([To | Rest], Acc)
    end.

get_neighbors(LocationId) ->
    case ets:lookup(?MAP_GRAPH, LocationId) of
        [{_, Neighbors}] -> {ok, Neighbors};
        [] -> {error, location_not_found}
    end.

get_zone_info(Zone) ->
    AllLocations = ets:tab2list(?MAP_LOCATIONS),
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
    
    Info = #{
        zone => Zone,
        total_locations => length(LocationsInZone),
        homes => length(HomesInZone),
        businesses => length(BusinessesInZone),
        center => {CenterX, CenterY},
        home_ids => [H#location.id || H <- HomesInZone],
        business_ids => [B#location.id || B <- BusinessesInZone]
    },
    {ok, Info}.

update_courier_position(CourierId, PositionData) ->
    case ets:info(courier_positions) of
        undefined -> 
            ets:new(courier_positions, [set, named_table, public]);
        _ -> 
            ok
    end,
    
    Position = maps:get(position, PositionData, #{x => 0, y => 0}),
    X = maps:get(x, Position, 0),
    Y = maps:get(y, Position, 0),
    
    ets:insert(courier_positions, {CourierId, {X, Y, PositionData}}),
    
    VisualizationNode = 'visualization@127.0.0.1',
    case net_adm:ping(VisualizationNode) of
        pong ->
            catch gen_server:cast({visualization_server, VisualizationNode}, 
                                  {update_courier_position, CourierId, PositionData});
        pang ->
            ok
    end,
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

create_local_ets_tables() ->
    ensure_table(?MAP_LOCATIONS, [set, named_table, public]),
    ensure_table(?MAP_ROADS, [set, named_table, public]),
    ensure_table(?MAP_GRAPH, [set, named_table, public]).

ensure_table(Name, Options) ->
    case ets:info(Name) of
        undefined -> ets:new(Name, Options);
        _ -> ok
    end.

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

store_locations(Locations) ->
    lists:foreach(fun(Loc) ->
        ets:insert(?MAP_LOCATIONS, {Loc#location.id, Loc})
    end, Locations).

store_roads(Roads) ->
    lists:foreach(fun(Road) ->
        ets:insert(?MAP_ROADS, {Road#road.id, Road})
    end, Roads).

build_graph_structure(Locations, Roads) ->
    %% Initialize empty neighbor lists
    lists:foreach(fun(Loc) ->
        ets:insert(?MAP_GRAPH, {Loc#location.id, []})
    end, Locations),
    
    %% Add edges
    lists:foreach(fun(Road) ->
        FromId = Road#road.from,
        ToId = Road#road.to,
        Weight = Road#road.distance,
        Time = Road#road.base_time,
        
        case ets:lookup(?MAP_GRAPH, FromId) of
            [{_, Neighbors}] ->
                case lists:keyfind(ToId, 1, Neighbors) of
                    false ->
                        ets:insert(?MAP_GRAPH, {FromId, [{ToId, Weight, Time} | Neighbors]});
                    _ ->
                        ok
                end;
            [] ->
                ets:insert(?MAP_GRAPH, {FromId, [{ToId, Weight, Time}]})
        end
    end, Roads).

find_k_nearest(Location, Candidates, K) ->
    WithDistances = [{L, calculate_distance_between_locations(Location, L)} 
                     || L <- Candidates, L#location.id =/= Location#location.id],
    Sorted = lists:sort(fun({_, D1}, {_, D2}) -> D1 =< D2 end, WithDistances),
    {Nearest, _} = lists:split(min(K, length(Sorted)), Sorted),
    [L || {L, _} <- Nearest].

calculate_distance_between_locations(L1, L2) ->
    DX = L1#location.x - L2#location.x,
    DY = L1#location.y - L2#location.y,
    round(math:sqrt(DX * DX + DY * DY)).

point_distance(X1, Y1, X2, Y2) ->
    math:sqrt((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2)).

remove_duplicate_roads(Roads) ->
    RoadMap = lists:foldl(fun(Road, Map) ->
        Key = {Road#road.from, Road#road.to},
        maps:put(Key, Road, Map)
    end, #{}, Roads),
    maps:values(RoadMap).

verify_basic_connectivity(Locations) ->
    Businesses = [L || L <- Locations, L#location.type == business],
    Homes = [L || L <- Locations, L#location.type == home],
    
    lists:foreach(fun(Business) ->
        HomesInZone = [H || H <- Homes, H#location.zone == Business#location.zone],
        io:format("Map Server: Business ~s has ~p homes in zone ~p~n", 
                  [Business#location.id, length(HomesInZone), Business#location.zone]),
        
        case ets:lookup(?MAP_GRAPH, Business#location.id) of
            [{_, Neighbors}] ->
                io:format("  -> Business has ~p neighbors~n", [length(Neighbors)]);
            [] ->
                io:format("  -> WARNING: Business has NO neighbors!~n")
        end
    end, Businesses).

min(A, B) when A < B -> A;
min(_, B) -> B.

max(A, B) when A > B -> A;
max(_, B) -> B.