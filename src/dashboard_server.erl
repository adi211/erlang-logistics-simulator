%%%-------------------------------------------------------------------
%%% @doc Dashboard Server - Control panel with map selection
%%% Fixed to pass map file name to visualization on START
%%%-------------------------------------------------------------------
-module(dashboard_server).
-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include("header.hrl").

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
    current_map = map_data_100,  % Default map as atom
    % Stats
    total_zones = 0,
    total_couriers = 0,
    active_couriers = 0,
    total_deliveries = 0,
    failed_deliveries = 0
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
    wxChoice:setSelection(MapChoice, 0),  % Default to small map
    wxChoice:connect(MapChoice, command_choice_selected),
    wxSizer:add(Row1, MapChoice, [{flag, ?wxALL}, {border, 5}]),
    
    wxSizer:add(ControlSizer, Row1, [{flag, ?wxEXPAND}]),
    
    %% Row 2: Simulation info
    Row2 = wxBoxSizer:new(?wxHORIZONTAL),
    
    InfoLabel = wxStaticText:new(Panel, ?wxID_ANY, 
                                 "Distributed simulation with 3 zones: North, Center, South"),
    InfoFont = wxFont:new(10, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_ITALIC, ?wxFONTWEIGHT_NORMAL),
    wxStaticText:setFont(InfoLabel, InfoFont),
    wxStaticText:setForegroundColour(InfoLabel, {100, 100, 100}),
    
    wxSizer:add(Row2, InfoLabel, [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 10}]),
    wxSizer:add(ControlSizer, Row2, [{flag, ?wxEXPAND}]),
    
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
    
    %% Add fixed zones
    lists:foreach(fun({Zone, Index}) ->
        wxListCtrl:insertItem(ZonesList, Index, ""),
        wxListCtrl:setItem(ZonesList, Index, 0, Zone),
        wxListCtrl:setItem(ZonesList, Index, 1, "Ready"),
        wxListCtrl:setItem(ZonesList, Index, 2, "0"),
        wxListCtrl:setItem(ZonesList, Index, 3, "0")
    end, [{"North Zone", 0}, {"Center Zone", 1}, {"South Zone", 2}]),
    
    wxSizer:add(ZonesSizer, ZonesList, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, ZonesSizer, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    
    %% Set sizer
    wxPanel:setSizer(Panel, MainSizer),
    
    %% Status bar
    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "System: STOPPED | Map: Small (100 homes)"),
    
    %% Show frame
    wxFrame:show(Frame),
    
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
        current_map = map_data_100
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc Create statistics labels
%%--------------------------------------------------------------------
create_stats_labels(Panel, Sizer) ->
    Font = wxFont:new(10, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
    BoldFont = wxFont:new(10, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    
    Stats = [
        {zones_label, "Zones: ", "3"},
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
             State = #state{courier_input = CourierInput}) ->
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
            io:format("Dashboard: Deploying ~p couriers~n", [Num]),
            gen_server:cast(control_center, {deploy_couriers, Num}),
            State;
            
        1005 -> % Remove
            Value = wxTextCtrl:getValue(CourierInput),
            Num = try list_to_integer(Value) catch _:_ -> 5 end,
            io:format("Dashboard: Removing ~p couriers~n", [Num]),
            gen_server:cast(control_center, {remove_couriers, Num}),
            State;
            
        _ ->
            State
    end,
    {noreply, NewState};

handle_event(#wx{id = 1010, event = #wxCommand{type = command_choice_selected}}, 
             State = #state{map_choice = Choice, simulation_state = stopped}) ->
    Selection = wxChoice:getSelection(Choice),
    
    %% Determine which map module to use
    MapModule = case Selection of
        0 -> map_data_100;      % Small map (100 homes)
        1 -> map_data_200;      % Large map (200 homes)
        _ -> map_data_100
    end,
    
    io:format("Dashboard: Map changed to ~p~n", [MapModule]),
    
    %% Update status bar
    MapLabel = case Selection of
        0 -> "Small (100 homes)";
        1 -> "Large (200 homes)";
        _ -> "Unknown"
    end,
    
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: STOPPED | Map: ~s", [MapLabel])),
    
    {noreply, State#state{current_map = MapModule}};

handle_event(#wx{id = 1010, event = #wxCommand{type = command_choice_selected}}, State) ->
    %% Map change disabled during simulation
    io:format("Dashboard: Cannot change map while simulation is running~n"),
    wxChoice:setSelection(State#state.map_choice, get_map_index(State#state.current_map)),
    {noreply, State};

handle_event(#wx{id = 1011, event = #wxCommand{type = command_slider_updated}}, 
             State = #state{load_slider = Slider}) ->
    Value = wxSlider:getValue(Slider),
    
    case get(load_text) of
        undefined -> ok;
        Text -> wxStaticText:setLabel(Text, io_lib:format("~p%", [Value]))
    end,
    
    gen_server:cast(control_center, {update_load_factor, Value}),
    
    {noreply, State};

handle_event(#wx{event = #wxClose{}}, State) ->
    io:format("Dashboard: Closing~n"),
    {stop, normal, State};

handle_event(_Event, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Control handlers - FIXED TO PASS MAP FILE
%%--------------------------------------------------------------------
handle_start(State = #state{current_map = MapModule}) ->
    io:format("Dashboard: Starting simulation with map ~p~n", [MapModule]),
    
    %% Tell control to start with the specific map
    gen_server:cast(control_center, {start_simulation, MapModule}),
    
    %% Disable map selection during simulation
    wxChoice:enable(State#state.map_choice, [{enable, false}]),
    
    MapLabel = get_map_label(MapModule),
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: RUNNING | Map: ~s", [MapLabel])),
    
    State#state{simulation_state = running}.

handle_pause(State) ->
    io:format("Dashboard: Pausing simulation~n"),
    gen_server:cast(control_center, {pause_simulation}),
    
    MapLabel = get_map_label(State#state.current_map),
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: PAUSED | Map: ~s", [MapLabel])),
    State#state{simulation_state = paused}.

handle_stop(State) ->
    io:format("Dashboard: Stopping simulation~n"),
    gen_server:cast(control_center, {stop_simulation}),
    
    %% Re-enable map selection
    wxChoice:enable(State#state.map_choice, [{enable, true}]),
    
    MapLabel = get_map_label(State#state.current_map),
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: STOPPED | Map: ~s", [MapLabel])),
    State#state{simulation_state = stopped}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle cast messages
%%--------------------------------------------------------------------
handle_cast({update_stats, Zones, Couriers, Active, Deliveries, Failed}, 
            State = #state{stats_labels = Labels}) ->
    update_label(Labels, zones_label, integer_to_list(Zones)),
    update_label(Labels, couriers_label, integer_to_list(Couriers)),
    update_label(Labels, active_label, integer_to_list(Active)),
    update_label(Labels, deliveries_label, integer_to_list(Deliveries)),
    update_label(Labels, failed_label, integer_to_list(Failed)),
    
    {noreply, State#state{
        total_zones = Zones,
        total_couriers = Couriers,
        active_couriers = Active,
        total_deliveries = Deliveries,
        failed_deliveries = Failed
    }};

handle_cast({update_zone_status, Zone, Status, Couriers, Deliveries}, 
            State = #state{zones_list = List}) ->
    update_zone_in_list(List, Zone, Status, Couriers, Deliveries),
    {noreply, State};

handle_cast({update_nodes, Control, Viz}, State = #state{zones_list = List}) ->
    %% Update the visualization node info
    wxListCtrl:setItem(List, 0, 1, "Connected"),
    wxListCtrl:setItem(List, 1, 1, "Connected"),
    wxListCtrl:setItem(List, 2, 1, "Connected"),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Other handlers
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

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

update_zone_in_list(List, Zone, Status, Couriers, Deliveries) ->
    %% Find zone index based on zone name
    Index = case Zone of
        north -> 0;
        center -> 1;
        south -> 2;
        _ -> -1
    end,
    
    case Index of
        -1 -> ok;
        _ ->
            ZoneName = case Zone of
                north -> "North Zone";
                center -> "Center Zone";
                south -> "South Zone"
            end,
            
            StatusText = case Status of
                live -> "Active";
                down -> "Down";
                ready -> "Ready";
                _ -> atom_to_list(Status)
            end,
            
            wxListCtrl:setItem(List, Index, 0, ZoneName),
            wxListCtrl:setItem(List, Index, 1, StatusText),
            wxListCtrl:setItem(List, Index, 2, integer_to_list(Couriers)),
            wxListCtrl:setItem(List, Index, 3, integer_to_list(Deliveries)),
            
            %% Set color based on status
            Color = case Status of
                live -> {0, 150, 0};
                down -> {200, 0, 0};
                ready -> {100, 100, 100};
                _ -> {0, 0, 0}
            end,
            wxListCtrl:setItemTextColour(List, Index, Color)
    end.

%% Helper functions for map management
get_map_label(map_data_100) -> "Small (100 homes)";
get_map_label(map_data_200) -> "Large (200 homes)";
get_map_label(_) -> "Custom".

get_map_index(map_data_100) -> 0;
get_map_index(map_data_200) -> 1;
get_map_index(_) -> 0.