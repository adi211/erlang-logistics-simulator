%%%-------------------------------------------------------------------
%%% @doc Package FSM Module
%%% מנהל את מחזור החיים של חבילה מיצירה ועד מסירה
%%%-------------------------------------------------------------------
-module(package).
-behaviour(gen_statem).

%% API
-export([start_link/2, assign_courier/2, update_status/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

%%====================================================================
%% API Functions
%%====================================================================

start_link(PkgId, Zone) ->
    gen_statem:start_link({local, list_to_atom("package_" ++ PkgId)}, ?MODULE, [PkgId, Zone], []).

assign_courier(PkgId, Courier) ->
    gen_statem:cast(list_to_atom("package_" ++ PkgId), {assign_courier, Courier}).

update_status(PkgId, Status) ->
    gen_statem:cast(list_to_atom("package_" ++ PkgId), {update_status, Status}).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() -> handle_event_function.

init([PkgId, Zone]) ->
    %% מאתרים את ה-PID של מנהל האזור לפי שמו
    ZoneManagerPid = list_to_atom("zone_manager_" ++ Zone),
    
    %% דיווח למערכת הניטור על יצירת חבילה חדשה
    report_state_change(PkgId, ordered, #{zone => Zone, created_at => erlang:system_time(second)}),
    
    {ok, ordered, #{id => PkgId, zone_manager => ZoneManagerPid, zone => Zone}}.

%%--------------------------------------------------------------------
%% State: ordered - ממתין להקצאת שליח
%%--------------------------------------------------------------------
handle_event(cast, {assign_courier, Courier}, ordered, Data) ->
    PkgId = maps:get(id, Data),
    Zone = maps:get(zone, Data),
    
    %% שלח הודעה לשליח
    gen_statem:cast(list_to_atom("courier_" ++ Courier), {assign_delivery, PkgId, Zone}),
    
    %% דווח על השינוי
    report_state_change(PkgId, assigned, #{courier => Courier, zone => Zone}),
    
    {next_state, assigned, Data#{courier => Courier}};

%%--------------------------------------------------------------------
%% State: assigned - הוקצה שליח, ממתין לאיסוף
%%--------------------------------------------------------------------
handle_event(cast, {update_status, picking_up}, assigned, Data) ->
    PkgId = maps:get(id, Data),
    
    report_state_change(PkgId, picking_up, #{
        courier => maps:get(courier, Data),
        zone => maps:get(zone, Data)
    }),
    
    {next_state, picking_up, Data};

%%--------------------------------------------------------------------
%% State: picking_up - השליח בדרך לאסוף את החבילה
%%--------------------------------------------------------------------
handle_event(cast, {update_status, in_transit}, picking_up, Data) ->
    PkgId = maps:get(id, Data),
    
    report_state_change(PkgId, in_transit, #{
        courier => maps:get(courier, Data),
        zone => maps:get(zone, Data)
    }),
    
    {next_state, in_transit, Data};

%%--------------------------------------------------------------------
%% State: in_transit - בדרך ללקוח
%%--------------------------------------------------------------------
handle_event(cast, {update_status, delivered}, in_transit, Data) ->
    PkgId = maps:get(id, Data),
    ZoneManager = maps:get(zone_manager, Data),
    
    %% הודע למנהל האזור על סיום המשלוח
    gen_statem:cast(ZoneManager, {package_delivered, PkgId, maps:get(courier, Data)}),
    
    report_state_change(PkgId, delivered, #{
        courier => maps:get(courier, Data),
        zone => maps:get(zone, Data),
        delivered_at => erlang:system_time(second)
    }),
    
    {next_state, delivered, Data};

%%--------------------------------------------------------------------
%% State: failed - כשל במשלוח
%%--------------------------------------------------------------------
handle_event(cast, {update_status, failed}, _AnyState, Data) ->
    PkgId = maps:get(id, Data),
    
    report_state_change(PkgId, failed, #{
        courier => maps:get(courier, Data, null),
        zone => maps:get(zone, Data)
    }),
    
    {next_state, failed, Data};

%%--------------------------------------------------------------------
%% Catch-all handlers
%%--------------------------------------------------------------------
handle_event(cast, {update_status, _Status}, _CurrentState, Data) ->
    %% התעלם מעדכוני סטטוס לא צפויים
    {keep_state, Data};

handle_event(_EventType, _Event, delivered, Data) ->
    %% מצב סופי - התעלם מכל אירוע
    {keep_state, Data};

handle_event({call, From}, get_state, StateName, Data) ->
    {keep_state, Data, [{reply, From, StateName}]};
	
handle_event(info, {retry_report, PackageId, NewStatus, AdditionalData}, _State, Data) ->
    report_state_change(PackageId, NewStatus, AdditionalData),
    {keep_state, Data};

handle_event(_EventType, _Event, _StateName,Data) ->
    %% Catch-all לאירועים לא מטופלים
    {keep_state, Data}.

%%====================================================================
%% Internal Functions
%%====================================================================

report_state_change(PackageId, NewStatus, AdditionalData) ->
    case global:whereis_name(logistics_state_collector) of
        undefined ->
			 io:format("DEBUG: State Collector not found globally for package ~p state change~n", [PackageId]),
            %% נסה שוב אחרי 2 שניות
            erlang:send_after(2000, self(), {retry_report, PackageId, NewStatus, AdditionalData});
        Pid when is_pid(Pid) ->
            StateData = maps:merge(AdditionalData, #{
                status => NewStatus,
                id => PackageId  %% וודא ש-id נשלח!
            }),
            logistics_state_collector:package_state_changed(PackageId, StateData)
    end.

%%====================================================================
%% Callbacks
%%====================================================================

terminate(_Reason, _State, _Data) -> 
    ok.

code_change(_OldVsn, State, Data, _Extra) -> 
    {ok, State, Data}.