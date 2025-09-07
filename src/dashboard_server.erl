%%%-------------------------------------------------------------------
%%% @doc Dashboard Server - Fixed for distributed communication
%%%-------------------------------------------------------------------
-module(dashboard_server).
-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("header.hrl").
-include("network_const.hrl").

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, 
         terminate/2, code_change/3]).

-record(state, {
    frame,
    panel,
    % Controls
    start_btn,
    pause_btn,
    stop_btn,
    deploy_btn,
    remove_btn,
    courier_input,
    map_choice,
    load_slider,
    % Display
    stats_labels,
    zones_list,
    % State
    simulation_state = stopped,
    current_map = map_data_100,
    % Stats
    total_zones = 0,
    total_couriers = 0,
    active_couriers = 0,
    total_deliveries = 0,
    failed_deliveries = 0,
    % Zone tracking
    zone_status = #{},  % Track zone statuses
    % Node tracking
    control_node = ?CTRL_NODE,  % Fixed control node name
    visualization_node = node()  % Current node
}).

%%--------------------------------------------------------------------
%% @doc Starts the dashboard
%%--------------------------------------------------------------------
start() ->
    Wx = wx:new(),
    wx_object:start(?MODULE, [Wx], []).

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the dashboard
%%--------------------------------------------------------------------
init([Wx]) ->
    %% Create frame
    Frame = wxFrame:new(Wx, -1, "Logistics Control Center", 
                       [{size, {1000, 700}}]),
    
    %% Create panel
    Panel = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(Panel, {245, 245, 250}),
    
    %% Create main sizer
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    
    %% === TITLE ===
    Title = wxStaticText:new(Panel, ?wxID_ANY, "LOGISTICS CONTROL CENTER", 
                            [{style, ?wxALIGN_CENTER}]),
    TitleFont = wxFont:new(20, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    wxStaticText:setFont(Title, TitleFont),
    wxSizer:add(MainSizer, Title, [{flag, ?wxALL bor ?wxALIGN_CENTER}, {border, 15}]),
    
    wxSizer:add(MainSizer, wxStaticLine:new(Panel), [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    
    %% === CONTROL SECTION ===
    ControlBox = wxStaticBox:new(Panel, ?wxID_ANY, "Simulation Control"),
    ControlSizer = wxStaticBoxSizer:new(ControlBox, ?wxVERTICAL),
    
    %% Row 1: Main buttons and map selection
    Row1 = wxBoxSizer:new(?wxHORIZONTAL),
    
    StartBtn = wxButton:new(Panel, 1001, [{label, "START"}, {size, {100, 35}}]),
    PauseBtn = wxButton:new(Panel, 1002, [{label, "PAUSE"}, {size, {100, 35}}]),
    StopBtn = wxButton:new(Panel, 1003, [{label, "STOP"}, {size, {100, 35}}]),
    
    wxButton:connect(StartBtn, command_button_clicked),
    wxButton:connect(PauseBtn, command_button_clicked),
    wxButton:connect(StopBtn, command_button_clicked),
    
    wxSizer:add(Row1, StartBtn, [{flag, ?wxALL}, {border, 5}]),
    wxSizer:add(Row1, PauseBtn, [{flag, ?wxALL}, {border, 5}]),
    wxSizer:add(Row1, StopBtn, [{flag, ?wxALL}, {border, 5}]),
    wxSizer:addStretchSpacer(Row1),
    
    %% Map selection
    wxSizer:add(Row1, wxStaticText:new(Panel, ?wxID_ANY, "Map: "), 
                [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]),
    
    MapChoice = wxChoice:new(Panel, 1010, [
        {choices, ["Small (100 homes)", "Large (200 homes)"]}, 
        {size, {150, -1}}
    ]),
    wxChoice:setSelection(MapChoice, 0),
    wxChoice:connect(MapChoice, command_choice_selected),
    wxSizer:add(Row1, MapChoice, [{flag, ?wxALL}, {border, 5}]),
    
    wxSizer:add(ControlSizer, Row1, [{flag, ?wxEXPAND}]),
    
    wxSizer:add(MainSizer, ControlSizer, [{flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    
    %% === DYNAMIC CONTROLS ===
    DynamicBox = wxStaticBox:new(Panel, ?wxID_ANY, "Dynamic Controls"),
    DynamicSizer = wxStaticBoxSizer:new(DynamicBox, ?wxVERTICAL),
    
    %% Courier management
    CourierRow = wxBoxSizer:new(?wxHORIZONTAL),
    
    CourierLabel = wxStaticText:new(Panel, ?wxID_ANY, "Couriers: "),
    CourierInput = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "5"}, {size, {60, -1}}]),
    DeployBtn = wxButton:new(Panel, 1004, [{label, "Deploy"}, {size, {80, 30}}]),
    RemoveBtn = wxButton:new(Panel, 1005, [{label, "Remove"}, {size, {80, 30}}]),
    
    wxButton:connect(DeployBtn, command_button_clicked),
    wxButton:connect(RemoveBtn, command_button_clicked),
    
    wxSizer:add(CourierRow, CourierLabel, [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]),
    wxSizer:add(CourierRow, CourierInput, [{flag, ?wxALL}, {border, 5}]),
    wxSizer:add(CourierRow, DeployBtn, [{flag, ?wxALL}, {border, 5}]),
    wxSizer:add(CourierRow, RemoveBtn, [{flag, ?wxALL}, {border, 5}]),
    
    %% Order load
    LoadRow = wxBoxSizer:new(?wxHORIZONTAL),
    
    LoadLabel = wxStaticText:new(Panel, ?wxID_ANY, "Order Load: "),
    LoadSlider = wxSlider:new(Panel, 1011, 50, 0, 100, 
                             [{size, {300, -1}}, {style, ?wxSL_HORIZONTAL bor ?wxSL_LABELS}]),
    wxSlider:connect(LoadSlider, command_slider_updated),
    
    LoadText = wxStaticText:new(Panel, ?wxID_ANY, "50%"),
    put(load_text, LoadText),
    
    wxSizer:add(LoadRow, LoadLabel, [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]),
    wxSizer:add(LoadRow, LoadSlider, [{flag, ?wxALL}, {border, 5}]),
    wxSizer:add(LoadRow, LoadText, [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]),
    
    wxSizer:add(DynamicSizer, CourierRow, [{flag, ?wxEXPAND}]),
    wxSizer:add(DynamicSizer, LoadRow, [{flag, ?wxEXPAND}]),
    
    wxSizer:add(MainSizer, DynamicSizer, [{flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    
    %% === STATISTICS ===
    StatsBox = wxStaticBox:new(Panel, ?wxID_ANY, "Statistics"),
    StatsSizer = wxStaticBoxSizer:new(StatsBox, ?wxHORIZONTAL),
    
    StatsLabels = create_stats_labels(Panel, StatsSizer),
    
    wxSizer:add(MainSizer, StatsSizer, [{flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    
    %% === ZONE LIST ===
    ZonesBox = wxStaticBox:new(Panel, ?wxID_ANY, "Zones Status"),
    ZonesSizer = wxStaticBoxSizer:new(ZonesBox, ?wxVERTICAL),
    
    ZonesList = wxListCtrl:new(Panel, [{style, ?wxLC_REPORT}]),
    wxListCtrl:insertColumn(ZonesList, 0, "Node/Zone", [{width, 250}]),
    wxListCtrl:insertColumn(ZonesList, 1, "Status", [{width, 100}]),
    wxListCtrl:insertColumn(ZonesList, 2, "Couriers", [{width, 100}]),
    wxListCtrl:insertColumn(ZonesList, 3, "Deliveries", [{width, 100}]),
    
    %% Add fixed zone rows (0-2 for zones, 3-4 for control/viz)
    lists:foreach(fun(Index) ->
        wxListCtrl:insertItem(ZonesList, Index, "")
    end, lists:seq(0, 4)),
    
    %% Initialize zone display
    wxListCtrl:setItem(ZonesList, 0, 0, "North Zone"),
    wxListCtrl:setItem(ZonesList, 0, 1, "Not Connected"),
    wxListCtrl:setItem(ZonesList, 0, 2, "0"),
    wxListCtrl:setItem(ZonesList, 0, 3, "0"),
    wxListCtrl:setItemTextColour(ZonesList, 0, {128, 128, 128}),
    
    wxListCtrl:setItem(ZonesList, 1, 0, "Center Zone"),
    wxListCtrl:setItem(ZonesList, 1, 1, "Not Connected"),
    wxListCtrl:setItem(ZonesList, 1, 2, "0"),
    wxListCtrl:setItem(ZonesList, 1, 3, "0"),
    wxListCtrl:setItemTextColour(ZonesList, 1, {128, 128, 128}),
    
    wxListCtrl:setItem(ZonesList, 2, 0, "South Zone"),
    wxListCtrl:setItem(ZonesList, 2, 1, "Not Connected"),
    wxListCtrl:setItem(ZonesList, 2, 2, "0"),
    wxListCtrl:setItem(ZonesList, 2, 3, "0"),
    wxListCtrl:setItemTextColour(ZonesList, 2, {128, 128, 128}),
    
    %% Add control and visualization nodes
    wxListCtrl:setItem(ZonesList, 3, 0, "Control Node"),
    wxListCtrl:setItem(ZonesList, 3, 1, "Active"),
    wxListCtrl:setItemTextColour(ZonesList, 3, {0, 100, 200}),
    
    wxListCtrl:setItem(ZonesList, 4, 0, "Visualization"),
    wxListCtrl:setItem(ZonesList, 4, 1, "Not Connected"),
    wxListCtrl:setItemTextColour(ZonesList, 4, {128, 128, 128}),
    
    wxSizer:add(ZonesSizer, ZonesList, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, ZonesSizer, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    
    %% Set sizer
    wxPanel:setSizer(Panel, MainSizer),
    
    %% Status bar
    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "System: STOPPED | Map: Small (100 homes)"),
    
    %% Show frame
    wxFrame:show(Frame),
    
    %% Update the visualization node's own status in the list
    wxListCtrl:setItem(ZonesList, 4, 0, atom_to_list(node())),
    wxListCtrl:setItem(ZonesList, 4, 1, "Active"),
    wxListCtrl:setItemTextColour(ZonesList, 4, {0, 100, 200}),
    
    %% Subscribe to state collector after frame is created
    try
        logistics_state_collector:subscribe(self()),
        io:format("Dashboard: Successfully subscribed to state collector~n")
    catch
        Error:Reason ->
            io:format("Dashboard: Failed to subscribe to state collector: ~p:~p~n", [Error, Reason]),
            %% Try again after a short delay
            timer:send_after(1000, self(), retry_subscribe)
    end,
    
    {Frame, #state{
        frame = Frame,
        panel = Panel,
        start_btn = StartBtn,
        pause_btn = PauseBtn,
        stop_btn = StopBtn,
        deploy_btn = DeployBtn,
        remove_btn = RemoveBtn,
        courier_input = CourierInput,
        map_choice = MapChoice,
        load_slider = LoadSlider,
        stats_labels = StatsLabels,
        zones_list = ZonesList,
        simulation_state = stopped,
        current_map = map_data_100,
        zone_status = #{},
        control_node = ?CTRL_NODE,
        visualization_node = node()
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc Create statistics labels
%%--------------------------------------------------------------------
create_stats_labels(Panel, Sizer) ->
    Font = wxFont:new(10, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
    BoldFont = wxFont:new(10, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    
    Stats = [
        {zones_label, "Zones: ", "0"},
        {couriers_label, "Couriers: ", "0"},
        {active_label, "Active: ", "0"},
        {deliveries_label, "Deliveries: ", "0"},
        {failed_label, "Failed: ", "0"}
    ],
    
    lists:map(fun({Tag, Text, Initial}) ->
        Label = wxStaticText:new(Panel, ?wxID_ANY, Text),
        wxStaticText:setFont(Label, Font),
        Value = wxStaticText:new(Panel, ?wxID_ANY, Initial),
        wxStaticText:setFont(Value, BoldFont),
        wxStaticText:setForegroundColour(Value, {0, 100, 200}),
        
        wxSizer:add(Sizer, Label, [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]),
        wxSizer:add(Sizer, Value, [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]),
        
        {Tag, Value}
    end, Stats).

%%--------------------------------------------------------------------
%% @private
%% @doc Handle events
%%--------------------------------------------------------------------
handle_event(#wx{id = Id, event = #wxCommand{type = command_button_clicked}}, 
             State = #state{courier_input = CourierInput, control_node = ControlNode}) ->
    NewState = case Id of
        1001 -> % Start
            handle_start(State);
            
        1002 -> % Pause  
            handle_pause(State);
            
        1003 -> % Stop
            handle_stop(State);
            
        1004 -> % Deploy
            Value = wxTextCtrl:getValue(CourierInput),
            Num = try list_to_integer(Value) catch _:_ -> 5 end,
            io:format("Dashboard: Deploying ~p couriers to remote control center~n", [Num]),
            %% Remote cast to control center
            gen_server:cast({control_center, ControlNode}, {deploy_couriers, Num}),
            State;
            
        1005 -> % Remove
            Value = wxTextCtrl:getValue(CourierInput),
            Num = try list_to_integer(Value) catch _:_ -> 5 end,
            io:format("Dashboard: Removing ~p couriers via remote control center~n", [Num]),
            %% Remote cast to control center
            gen_server:cast({control_center, ControlNode}, {remove_couriers, Num}),
            State;
            
        _ ->
            State
    end,
    {noreply, NewState};

handle_event(#wx{id = 1010, event = #wxCommand{type = command_choice_selected}}, 
             State = #state{map_choice = Choice, simulation_state = stopped}) ->
    Selection = wxChoice:getSelection(Choice),
    
    MapModule = case Selection of
        0 -> map_data_100;
        1 -> map_data_200;
        _ -> map_data_100
    end,
    
    io:format("Dashboard: Map changed to ~p~n", [MapModule]),
    
    MapLabel = case Selection of
        0 -> "Small (100 homes)";
        1 -> "Large (200 homes)";
        _ -> "Unknown"
    end,
    
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: STOPPED | Map: ~s", [MapLabel])),
    
    {noreply, State#state{current_map = MapModule}};

handle_event(#wx{id = 1010, event = #wxCommand{type = command_choice_selected}}, State) ->
    io:format("Dashboard: Cannot change map while simulation is running~n"),
    wxChoice:setSelection(State#state.map_choice, get_map_index(State#state.current_map)),
    {noreply, State};

handle_event(#wx{id = 1011, event = #wxCommand{type = command_slider_updated}}, 
             State = #state{load_slider = Slider, control_node = ControlNode}) ->
    Value = wxSlider:getValue(Slider),
    
    case get(load_text) of
        undefined -> ok;
        Text -> wxStaticText:setLabel(Text, io_lib:format("~p%", [Value]))
    end,
    
    %% Remote cast to control center
    gen_server:cast({control_center, ControlNode}, {update_load_factor, Value}),
    
    {noreply, State};

handle_event(#wx{event = #wxClose{}}, State) ->
    io:format("Dashboard: Closing~n"),
    {stop, normal, State};

handle_event(_Event, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Control handlers
%%--------------------------------------------------------------------
handle_start(State = #state{current_map = MapModule, control_node = ControlNode, 
                            simulation_state = CurrentState}) ->
    case CurrentState of
        stopped ->
            %% Fresh start
            io:format("Dashboard: Starting NEW simulation with map ~p~n", [MapModule]),
            gen_server:cast({control_center, ControlNode}, {start_simulation, MapModule}),
            wxChoice:enable(State#state.map_choice, [{enable, false}]),
            
            MapLabel = get_map_label(MapModule),
            wxFrame:setStatusText(State#state.frame, 
                                 io_lib:format("System: RUNNING | Map: ~s", [MapLabel])),
            
            State#state{simulation_state = running};
        
        paused ->
            %% Resume from pause
            io:format("Dashboard: Resuming simulation~n"),
            gen_server:cast({control_center, ControlNode}, {resume_simulation}),
            
            MapLabel = get_map_label(State#state.current_map),
            wxFrame:setStatusText(State#state.frame, 
                                 io_lib:format("System: RUNNING | Map: ~s", [MapLabel])),
            
            State#state{simulation_state = running};
        
        running ->
            %% Already running
            io:format("Dashboard: Simulation already running~n"),
            State
    end.

%% The pause handler stays the same:
handle_pause(State = #state{control_node = ControlNode}) ->
    io:format("Dashboard: Pausing simulation~n"),
    gen_server:cast({control_center, ControlNode}, {pause_simulation}),
    
    MapLabel = get_map_label(State#state.current_map),
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: PAUSED | Map: ~s", [MapLabel])),
    State#state{simulation_state = paused}.

%% The stop handler stays the same:
handle_stop(State = #state{control_node = ControlNode}) ->
    io:format("Dashboard: Stopping simulation completely~n"),
    gen_server:cast({control_center, ControlNode}, {stop_simulation}),
    wxChoice:enable(State#state.map_choice, [{enable, true}]),
    
    MapLabel = get_map_label(State#state.current_map),
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: STOPPED | Map: ~s", [MapLabel])),
    State#state{simulation_state = stopped}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle cast messages - cleaned up, no longer used for direct updates
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle info messages - NEW handler for state collector updates
%%--------------------------------------------------------------------

handle_info({state_update, UpdateType, Data}, State = #state{zones_list = List, 
                                                              zone_status = ZoneStatusMap,
                                                              stats_labels = StatsLabels}) ->
    case UpdateType of
        <<"zone_update">> ->
            %% Extract zone data
            Zone = case maps:get(<<"zone">>, Data, maps:get(zone, Data, undefined)) of
                undefined -> maps:get(zone, Data, undefined);
                ZoneBin when is_binary(ZoneBin) -> binary_to_atom(ZoneBin, utf8);
                ZoneAtom when is_atom(ZoneAtom) -> ZoneAtom;
                ZoneStr when is_list(ZoneStr) -> list_to_atom(ZoneStr);
                "north" -> north;
                "center" -> center;
                "south" -> south
            end,
            
            Status = case maps:get(<<"status">>, Data, maps:get(status, Data, undefined)) of
                StatusBin when is_binary(StatusBin) -> binary_to_atom(StatusBin, utf8);
                StatusAtom when is_atom(StatusAtom) -> StatusAtom;
                _ -> unknown
            end,
            
            %% Get ALL the stats from the zone update
            Couriers = maps:get(<<"couriers">>, Data, maps:get(couriers, Data, 0)),
            Deliveries = maps:get(<<"deliveries">>, Data, maps:get(deliveries, Data, 0)),
            ActiveDeliveries = maps:get(<<"active_deliveries">>, Data, maps:get(active_deliveries, Data, 0)),
            WaitingPackages = maps:get(<<"waiting_packages">>, Data, maps:get(waiting_packages, Data, 0)),
            TotalOrders = maps:get(<<"total_orders">>, Data, maps:get(total_orders, Data, 0)),
            
            %% Update zone display
            case Zone of
                undefined -> {noreply, State};
                _ ->
                    Index = case Zone of
                        north -> 0;
                        <<"north">> -> 0;
                        center -> 1;
                        <<"center">> -> 1;
                        south -> 2;
                        <<"south">> -> 2;
                        _ -> -1
                    end,
                    
                    case Index of
                        -1 -> {noreply, State};
                        _ ->
                            ZoneName = case Index of
                                0 -> "North Zone";
                                1 -> "Center Zone";
                                2 -> "South Zone"
                            end,
                            
                            StatusText = case Status of
                                live -> "Connected";
                                down -> "Disconnected";
                                offline -> "Offline";
                                _ -> "Unknown"
                            end,
                            
                            %% Update zone list display
                            wxListCtrl:setItem(List, Index, 0, ZoneName),
                            wxListCtrl:setItem(List, Index, 1, StatusText),
                            wxListCtrl:setItem(List, Index, 2, integer_to_list(Couriers)),
                            wxListCtrl:setItem(List, Index, 3, integer_to_list(Deliveries)),
                            
                            Color = case Status of
                                live -> {0, 150, 0};
                                down -> {200, 0, 0};
                                offline -> {128, 128, 128};
                                _ -> {128, 128, 128}
                            end,
                            wxListCtrl:setItemTextColour(List, Index, Color),
                            
                            %% Update zone status map with ALL data
                            NewZoneStatusMap = maps:put(Zone, {Status, Couriers, Deliveries, ActiveDeliveries}, ZoneStatusMap),
                            
                            %% Calculate TOTALS from all zones
                            {TotalZones, TotalCouriers, TotalActive, TotalDeliveries, TotalFailed} = 
                                maps:fold(fun(_ZoneKey, {ZStatus, ZCouriers, ZDeliveries, ZActive}, 
                                             {AccZones, AccCouriers, AccActive, AccDeliveries, AccFailed}) ->
                                    case ZStatus of
                                        live -> 
                                            {AccZones + 1, AccCouriers + ZCouriers, AccActive + ZActive, 
                                             AccDeliveries + ZDeliveries, AccFailed};
                                        _ -> 
                                            {AccZones, AccCouriers, AccActive, AccDeliveries, AccFailed}
                                    end
                                end, {0, 0, 0, 0, 0}, NewZoneStatusMap),
                            
                            %% UPDATE THE STATS LABELS!
                            update_label(StatsLabels, zones_label, integer_to_list(TotalZones)),
                            update_label(StatsLabels, couriers_label, integer_to_list(TotalCouriers)),
                            update_label(StatsLabels, active_label, integer_to_list(TotalActive)),
                            update_label(StatsLabels, deliveries_label, integer_to_list(TotalDeliveries)),
                            update_label(StatsLabels, failed_label, integer_to_list(TotalFailed)),
                            
                            {noreply, State#state{
                                zone_status = NewZoneStatusMap,
                                total_zones = TotalZones,
                                total_couriers = TotalCouriers,
                                active_couriers = TotalActive,
                                total_deliveries = TotalDeliveries,
                                failed_deliveries = TotalFailed
                            }}
                    end
            end;
            
        <<"stats_update">> ->
            %% Direct stats update from control center
            TotalZones = maps:get(total_zones, Data, State#state.total_zones),
            TotalCouriers = maps:get(total_couriers, Data, State#state.total_couriers),
            ActiveCouriers = maps:get(active_couriers, Data, State#state.active_couriers),
            TotalDeliveries = maps:get(total_deliveries, Data, State#state.total_deliveries),
            FailedDeliveries = maps:get(failed_deliveries, Data, State#state.failed_deliveries),
            
            %% Update labels directly
            update_label(StatsLabels, zones_label, integer_to_list(TotalZones)),
            update_label(StatsLabels, couriers_label, integer_to_list(TotalCouriers)),
            update_label(StatsLabels, active_label, integer_to_list(ActiveCouriers)),
            update_label(StatsLabels, deliveries_label, integer_to_list(TotalDeliveries)),
            update_label(StatsLabels, failed_label, integer_to_list(FailedDeliveries)),
            
            {noreply, State#state{
                total_zones = TotalZones,
                total_couriers = TotalCouriers,
                active_couriers = ActiveCouriers,
                total_deliveries = TotalDeliveries,
                failed_deliveries = FailedDeliveries
            }};
            
        _ ->
            {noreply, State}
    end;

handle_info(retry_subscribe, State) ->
    %% Retry subscription to state collector
    try
        logistics_state_collector:subscribe(self()),
        io:format("Dashboard: Successfully subscribed to state collector on retry~n")
    catch
        Error:Reason ->
            io:format("Dashboard: Failed to subscribe on retry: ~p:~p~n", [Error, Reason]),
            %% Try again later
            timer:send_after(5000, self(), retry_subscribe)
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Terminate
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    %% Unsubscribe from state collector if it exists
    try
        logistics_state_collector:unsubscribe(self())
    catch
        _:_ -> ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Code change
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================
update_label(Labels, Tag, Value) ->
    case lists:keyfind(Tag, 1, Labels) of
        {_, Label} -> wxStaticText:setLabel(Label, Value);
        false -> ok
    end.

%% Helper functions for map management
get_map_label(map_data_100) -> "Small (100 homes)";
get_map_label(map_data_200) -> "Large (200 homes)";
get_map_label(_) -> "Custom".

get_map_index(map_data_100) -> 0;
get_map_index(map_data_200) -> 1;
get_map_index(_) -> 0.