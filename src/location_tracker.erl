%% -----------------------------------------------------------
%% מודול מעקב אחר מיקומי שליחים (Location Tracker)
%% מנהל את התנועה של שליחים על המפה בזמן אמת
%% -----------------------------------------------------------
-module(location_tracker).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start_tracking/4, stop_tracking/1, get_courier_status/1]).
-export([update_all_positions/0]).

-export([pause/0, resume/0]).

-export([clear_all_trackings/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-include("map_records.hrl").


-record(tracking, {
    courier_id,          % מזהה השליח
    start_location_id,   % מזהה נקודת מוצא
    end_location_id,     % מזהה נקודת יעד
    route = [],          % המסלול כרשימת מזהי מיקומים
    total_route_distance = 0, % מרחק כולל לאורך כל המסלול
    traveled_distance = 0,    % מרחק כולל שנסע מתחילת המסלול
    speed,               % מהירות נוכחית (מטר/שנייה)
    start_time,          % זמן התחלת הנסיעה
    estimated_arrival,   % זמן הגעה משוער
    status,              % moving | arrived
    update_callback,     % פונקציה להפעיל בהגעה
    current_segment_index = 1, % אינדקס המקטע הנוכחי במסלול
    distance_on_segment = 0    % מרחק שהשליח עבר על המקטע הנוכחי
}).


-record(state, {
    active_trackings = #{},  % מעקבים פעילים
    update_timer,            % טיימר לעדכון תקופתי
    paused = false
}).

%% קבועים
-define(UPDATE_INTERVAL_MS, 1000).  % עדכון כל שנייה (1000 מילי-שניות)
-define(BASE_SPEED, 11.11).      % מהירות בסיסית: 40 קמ"ש = 11.11 מ/ש
-define(SPEED_VARIATION, 0.2).   % וריאציה במהירות: ±20%

%% -----------------------------------------------------------
%% API Functions
%% -----------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


pause() ->
    gen_server:cast(?MODULE, pause).

resume() ->
    gen_server:cast(?MODULE, resume).


clear_all_trackings() ->
    gen_server:cast(?MODULE, clear_all).

%% התחלת מעקב אחר שליח
%% CourierId - מזהה השליח
%% FromLocation - מיקום התחלה
%% ToLocation - מיקום יעד
%% Callback - פונקציה להפעיל בהגעה {M, F, A}
start_tracking(CourierId, FromLocation, ToLocation, Callback) ->
    gen_server:call(?MODULE, {start_tracking, CourierId, FromLocation, ToLocation, Callback}).

%% הפסקת מעקב אחר שליח
stop_tracking(CourierId) ->
    gen_server:cast(?MODULE, {stop_tracking, CourierId}).

%% קבלת סטטוס נוכחי של שליח
get_courier_status(CourierId) ->
    gen_server:call(?MODULE, {get_courier_status, CourierId}).

%% עדכון ידני של כל המיקומים
update_all_positions() ->
    gen_server:cast(?MODULE, update_all_positions).

%% -----------------------------------------------------------
%% gen_server callbacks
%% -----------------------------------------------------------

init([]) ->
    io:format("Location Tracker starting...~n"),

    %% התחלת טיימר לעדכונים תקופתיים
    Timer = erlang:send_after(?UPDATE_INTERVAL_MS, self(), update_positions),

    {ok, #state{update_timer = Timer}}.

%% התחלת מעקב חדש
handle_call({start_tracking, CourierId, FromLocationId, ToLocationId, Callback}, _From, State) ->
    io:format("Location Tracker: Starting tracking for courier ~p from ~p to ~p~n",
              [CourierId, FromLocationId, ToLocationId]),

    if FromLocationId == ToLocationId ->
        io:format("Location Tracker: Source and destination are the same. Triggering immediate arrival.~n"),
        handle_immediate_arrival(CourierId, FromLocationId, Callback),
        {reply, {ok, 0}, State};
    true ->
        case map_server:get_route(FromLocationId, ToLocationId) of
            {ok, Route} ->
                Speed = calculate_speed(),
                {ok, TotalDistance} = calculate_route_distance(Route),
                EstimatedTime = TotalDistance / Speed,
                
                %% Determine route type
                RouteType = case map_server:get_location(ToLocationId) of
                    {ok, #location{type = business}} -> pickup;
                    {ok, #location{type = home}} -> delivery;
                    _ -> delivery
                end,
                
                %% Send FULL route to visualization
                VisualizationNode = 'visualization@127.0.0.1',
                RouteData = #{
                    courier_id => CourierId,
                    type => RouteType,
                    path => Route,  % Full route with all waypoints
                    from => FromLocationId,
                    to => ToLocationId,
                    total_distance => TotalDistance,
                    estimated_time => round(EstimatedTime)
                },
                
                catch rpc:call(VisualizationNode, visualization_server, 
                              set_courier_route, [CourierId, RouteData]),

                Tracking = #tracking{
                    courier_id = CourierId,
                    start_location_id = FromLocationId,
                    end_location_id = ToLocationId,
                    route = Route,
                    total_route_distance = TotalDistance,
                    speed = Speed,
                    start_time = erlang:system_time(second),
                    estimated_arrival = erlang:system_time(second) + round(EstimatedTime),
                    status = moving,
                    update_callback = Callback
                },

                NewTrackings = maps:put(CourierId, Tracking, State#state.active_trackings),
                update_courier_position(Tracking),
                {reply, {ok, EstimatedTime}, State#state{active_trackings = NewTrackings}};
            {error, Reason} ->
                io:format("Location Tracker: Could not find route for ~p -> ~p. Reason: ~p~n", 
                         [FromLocationId, ToLocationId, Reason]),
                {reply, {error, {no_route_found, Reason}}, State}
        end
    end;


%% קבלת סטטוס שליח
handle_call({get_courier_status, CourierId}, _From, State) ->
    case maps:get(CourierId, State#state.active_trackings, undefined) of
        undefined ->
            {reply, {error, not_tracking}, State};
        Tracking ->
            Status = build_status_report(Tracking),
            {reply, {ok, Status}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% >> הערה חדשה: טיפול בפקודות החדשות להשהיה והמשך <<
handle_cast(pause, State) ->
    io:format("Location Tracker: Pausing updates.~n"),
    {noreply, State#state{paused = true}};

handle_cast(resume, State) ->
    io:format("Location Tracker: Resuming updates.~n"),
    {noreply, State#state{paused = false}};

%% @@ הערה בעברית: טיפול בקריאת ה-API החדשה לאיפוס המעקבים. @@
%% @@ הוא פשוט מחליף את מפת המעקבים במפה ריקה. @@
handle_cast(clear_all, State) ->
    io:format("Location Tracker: Clearing all active trackings.~n"),
    {noreply, State#state{active_trackings = #{}}};

%% הפסקת מעקב
handle_cast({stop_tracking, CourierId}, State) ->
    io:format("Location Tracker: Stopping tracking for courier ~p~n", [CourierId]),
    NewTrackings = maps:remove(CourierId, State#state.active_trackings),
    {noreply, State#state{active_trackings = NewTrackings}};

%% עדכון ידני של כל המיקומים
handle_cast(update_all_positions, State) ->
    update_all_courier_positions(State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(update_positions, State) ->
    %% בדיקה אם המערכת מושהית
    NewState = case State#state.paused of
        true ->
            %% אם כן, לא מעדכנים מיקומים
            State;
        false ->
            %% אם לא, מעדכנים כרגיל
            update_all_courier_positions(State)
    end,

    %% תזמון העדכון הבא בכל מקרה
    Timer = erlang:send_after(?UPDATE_INTERVAL_MS, self(), update_positions),

    {noreply, NewState#state{update_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% ביטול הטיימר
    case State#state.update_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    io:format("Location Tracker terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% @doc פונקציית העל שמעדכנת את המיקום של כל השליחים הפעילים.
update_all_courier_positions(State) ->
    %% מחשבים את המרחק שהשליחים צריכים לעבור בפעימה הנוכחית.
    DistanceThisTick = ?BASE_SPEED * (?UPDATE_INTERVAL_MS / 1000),

    UpdatedTrackings = maps:map(
        fun(_CourierId, Tracking) ->
            case Tracking#tracking.status of
                moving ->
                    %% עבור כל שליח בתנועה, קוראים לפונקציה הרקורסיבית שמזיזה אותו.
                    move_courier(Tracking, DistanceThisTick);
                arrived ->
                    %% שליח שהגיע נשאר במקומו.
                    Tracking
            end
        end,
        State#state.active_trackings
    ),

    State#state{active_trackings = UpdatedTrackings}.

%% @doc הפונקציה המרכזית שמזיזה שליח בודד לאורך המסלול שלו.
move_courier(Tracking, DistanceToMove) ->
    Route = Tracking#tracking.route,
    SegmentIndex = Tracking#tracking.current_segment_index,

    if SegmentIndex >= length(Route) ->
        %% אם השליח כבר נמצא במקטע האחרון (או מעבר), נטפל בו כהגעה.
        handle_arrival(Tracking);
    true ->
        FromNodeId = lists:nth(SegmentIndex, Route),
        ToNodeId = lists:nth(SegmentIndex + 1, Route),

        {ok, SegmentDistance} = get_segment_distance(FromNodeId, ToNodeId),

        RemainingOnSegment = SegmentDistance - Tracking#tracking.distance_on_segment,

        if DistanceToMove >= RemainingOnSegment ->
            %% השליח יסיים את המקטע הנוכחי בפעימה זו.
            LeftoverDistance = DistanceToMove - RemainingOnSegment,
            

            NewTracking = Tracking#tracking{
                current_segment_index = SegmentIndex + 1,
                distance_on_segment = 0,  % נתחיל מ-0 זמנית
                traveled_distance = Tracking#tracking.traveled_distance + RemainingOnSegment
            },
            
            %% עדכון המיקום לסוף המקטע הנוכחי לפני המעבר
            update_courier_position(NewTracking#tracking{
                current_segment_index = SegmentIndex,
                distance_on_segment = SegmentDistance
            }),
            
            %% עכשיו נמשיך עם היתרה במקטע החדש
            if SegmentIndex + 1 < length(Route) ->
                move_courier(NewTracking#tracking{
                    distance_on_segment = LeftoverDistance
                }, 0);
            true ->
                handle_arrival(NewTracking)
            end;
        true ->
            %% השליח נשאר במקטע הנוכחי.
            NewDistanceOnSegment = Tracking#tracking.distance_on_segment + DistanceToMove,
            FinalTracking = Tracking#tracking{
                distance_on_segment = NewDistanceOnSegment,
                traveled_distance = Tracking#tracking.traveled_distance + DistanceToMove
            },
            %% מעדכנים את מיקום השליח במפה.
            update_courier_position(FinalTracking),
            FinalTracking
        end
    end.


update_courier_position(Tracking) ->
    SegmentIndex = Tracking#tracking.current_segment_index,
    Route = Tracking#tracking.route,


    if SegmentIndex > length(Route) - 1 ->
        %% אם חרגנו מהמסלול, נישאר בנקודה האחרונה
        LastLocationId = lists:last(Route),
        case map_server:get_location(LastLocationId) of
            {ok, LastLoc} ->
                PositionData = #{
                    position => #{x => LastLoc#location.x, y => LastLoc#location.y},
                    destination => list_to_binary(LastLocationId),
                    progress => 1.0,
                    eta => 0,
                    status => arrived
                },
                map_server:update_courier_position(Tracking#tracking.courier_id, PositionData),
                
                VisualizationNode = 'visualization@127.0.0.1',
                catch rpc:call(VisualizationNode, visualization_server, 
                              update_courier_position, 
                              [Tracking#tracking.courier_id, PositionData]);
            _ -> ok
        end;
    true ->
        FromNodeId = lists:nth(SegmentIndex, Route),
        ToNodeId = if 
            SegmentIndex < length(Route) -> lists:nth(SegmentIndex + 1, Route);
            true -> lists:last(Route)
        end,

        %% Get actual locations from map_server
        case {map_server:get_location(FromNodeId), map_server:get_location(ToNodeId)} of
            {{ok, FromLoc}, {ok, ToLoc}} ->
                {ok, SegmentDistance} = get_segment_distance(FromNodeId, ToNodeId),
                
                %% וודא שה-progress לא חורג מ-1.0
                ProgressOnSegment = if
                    SegmentDistance > 0 -> 
                        erlang:min(1.0, Tracking#tracking.distance_on_segment / SegmentDistance);
                    true -> 1.0
                end,

                %% Linear interpolation on current segment - חלקה יותר
                CurrentX = FromLoc#location.x + (ToLoc#location.x - FromLoc#location.x) * ProgressOnSegment,
                CurrentY = FromLoc#location.y + (ToLoc#location.y - FromLoc#location.y) * ProgressOnSegment,

                RemainingDistance = max(0, Tracking#tracking.total_route_distance - Tracking#tracking.traveled_distance),
                ETA_Seconds = case Tracking#tracking.speed of
                    S when S > 0 -> round(RemainingDistance / S);
                    _ -> 0
                end,

                PositionData = #{
                    position => #{x => round(CurrentX), y => round(CurrentY)},
                    destination => list_to_binary(Tracking#tracking.end_location_id),
                    progress => erlang:min(1.0, Tracking#tracking.traveled_distance / max(1, Tracking#tracking.total_route_distance)),
                    eta => ETA_Seconds * 1000,
                    speed => round(Tracking#tracking.speed * 3.6),
                    status => moving
                },
                
                %% Update both map_server and visualization
                map_server:update_courier_position(Tracking#tracking.courier_id, PositionData),
                
                VisualizationNode = 'visualization@127.0.0.1',
                catch rpc:call(VisualizationNode, visualization_server, 
                              update_courier_position, 
                              [Tracking#tracking.courier_id, PositionData]);
            _ ->
                io:format("Location Tracker: Failed to get locations for segment ~p -> ~p~n", 
                         [FromNodeId, ToNodeId])
        end
    end.



%% @doc מטפל בהגעה ליעד.
handle_arrival(Tracking) ->
    io:format("Location Tracker: Courier ~p arrived at ~p!~n",
              [Tracking#tracking.courier_id, Tracking#tracking.end_location_id]),

    %% עדכון מיקום סופי ומדויק
    case map_server:get_location(Tracking#tracking.end_location_id) of
        {ok, FinalLoc} ->
            FinalPosition = #{
                position => #{
                    x => FinalLoc#location.x,
                    y => FinalLoc#location.y
                },
                destination => list_to_binary(Tracking#tracking.end_location_id),
                progress => 1.0,
                eta => 0,
                status => arrived
            },
            map_server:update_courier_position(Tracking#tracking.courier_id, FinalPosition);
        _ -> ok
    end,

    %% הפעלת ה-callback אם קיים
    case Tracking#tracking.update_callback of
        undefined -> ok;
        {M, F, A} ->
            spawn(fun() -> apply(M, F, A) end);
        Fun when is_function(Fun) ->
            spawn(Fun)
    end,

    %% מחזירים את רשומת המעקב עם סטטוס "הגיע".
    Tracking#tracking{status = arrived}.



%% חישוב מהירות עם וריאציה רנדומלית
calculate_speed() ->
    Variation = (rand:uniform() * 2 - 1) * ?SPEED_VARIATION,
    ?BASE_SPEED * (1 + Variation).

%% @doc מחשב את המרחק הכולל של מסלול (רשימת צמתים).
calculate_route_distance([_]) -> {ok, 0};
calculate_route_distance([H1, H2 | T]) ->
    case get_segment_distance(H1, H2) of
        {ok, Dist} ->
            case calculate_route_distance([H2 | T]) of
                {ok, RestDist} -> {ok, Dist + RestDist};
                Error -> Error
            end;
        Error -> Error
    end;
calculate_route_distance([]) -> {ok, 0}.


%% @doc מחלץ את המרחק של מקטע כביש בודד מהגרף.
get_segment_distance(From, To) ->
    case ets:lookup(map_graph, From) of
        [{_, Neighbors}] ->
            case lists:keyfind(To, 1, Neighbors) of
                {To, Distance, _Time} -> {ok, Distance};
                false -> {error, {segment_not_found, From, To}}
            end;
        [] -> {error, {node_not_found, From}}
    end.

%% @doc פונקציית עזר לטיפול בהגעה מיידית כאשר המוצא והיעד זהים.
handle_immediate_arrival(CourierId, LocationId, Callback) ->
    case map_server:get_location(LocationId) of
        {ok, Loc} ->
            PositionData = #{
                position => #{x => Loc#location.x, y => Loc#location.y},
                destination => list_to_binary(LocationId),
                progress => 1.0,
                eta => 0,
                status => arrived
            },
            map_server:update_courier_position(CourierId, PositionData);
        _ -> ok
    end,
    %% הפעלת ה-callback
    case Callback of
        undefined -> ok;
        {M, F, A} -> spawn(fun() -> apply(M, F, A) end);
        Fun when is_function(Fun) -> spawn(Fun)
    end.

%% בניית דוח סטטוס
build_status_report(Tracking) ->
    RemainingDistance = max(0, Tracking#tracking.total_route_distance - Tracking#tracking.traveled_distance),
    ETA = case Tracking#tracking.speed of
        S when S > 0 -> round(RemainingDistance / S);
        _ -> 0
    end,

    #{
        courier_id => Tracking#tracking.courier_id,
        from => Tracking#tracking.start_location_id,
        to => Tracking#tracking.end_location_id,
        total_distance => round(Tracking#tracking.total_route_distance),
        traveled_distance => round(Tracking#tracking.traveled_distance),
        progress => if Tracking#tracking.total_route_distance > 0 ->
                         Tracking#tracking.traveled_distance / Tracking#tracking.total_route_distance;
                     true -> 1.0
                  end,
        speed_kmh => round(Tracking#tracking.speed * 3.6),
        eta_seconds => ETA,
        status => Tracking#tracking.status
    }.