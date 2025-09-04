%%%-------------------------------------------------------------------
%%% @doc Working Dashboard - All features, no wx errors
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
    mode_choice,
    household_text,  % Changed to text control
    load_slider,
    % Display
    stats_labels,
    zones_list,
    % State
    simulation_state = stopped,
    simulation_mode = visual,
    households = 1000,
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
    
    %% Row 1: Main buttons and mode
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
    
    %% Mode selection
    wxSizer:add(Row1, wxStaticText:new(Panel, ?wxID_ANY, "Mode: "), 
                [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]),
    
    ModeChoice = wxChoice:new(Panel, 1010, [{choices, ["Visual (100 houses)", "Stress Test"]}, 
                                            {size, {150, -1}}]),
    wxChoice:setSelection(ModeChoice, 0),
    wxChoice:connect(ModeChoice, command_choice_selected),
    wxSizer:add(Row1, ModeChoice, [{flag, ?wxALL}, {border, 5}]),
    
    wxSizer:add(ControlSizer, Row1, [{flag, ?wxEXPAND}]),
    
    %% Row 2: Stress mode settings
    Row2 = wxBoxSizer:new(?wxHORIZONTAL),
    
    HouseholdLabel = wxStaticText:new(Panel, ?wxID_ANY, "Households (Stress mode): "),
    HouseholdText = wxTextCtrl:new(Panel, 1012, [{value, "1000"}, {size, {80, -1}}]),
    wxTextCtrl:enable(HouseholdText, [{enable, false}]),
    
    UpdateHouseholdsBtn = wxButton:new(Panel, 1013, [{label, "Set"}, {size, {60, 25}}]),
    wxButton:connect(UpdateHouseholdsBtn, command_button_clicked),
    wxButton:enable(UpdateHouseholdsBtn, [{enable, false}]),
    put(update_households_btn, UpdateHouseholdsBtn),
    
    wxSizer:add(Row2, HouseholdLabel, [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]),
    wxSizer:add(Row2, HouseholdText, [{flag, ?wxALL}, {border, 5}]),
    wxSizer:add(Row2, UpdateHouseholdsBtn, [{flag, ?wxALL}, {border, 5}]),
    
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
    
    wxSizer:add(ZonesSizer, ZonesList, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, ZonesSizer, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    
    %% Set sizer
    wxPanel:setSizer(Panel, MainSizer),
    
    %% Status bar
    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "System: STOPPED | Mode: Visual"),
    
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
        mode_choice = ModeChoice,
        household_text = HouseholdText,
        load_slider = LoadSlider,
        stats_labels = StatsLabels,
        zones_list = ZonesList,
        simulation_state = stopped,
        simulation_mode = visual
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
             State = #state{courier_input = CourierInput, household_text = HText}) ->
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
            
        1013 -> % Update households
            Value = wxTextCtrl:getValue(HText),
            Num = try list_to_integer(Value) catch _:_ -> 1000 end,
            io:format("Dashboard: Setting households to ~p~n", [Num]),
            gen_server:cast(control_center, {update_households, Num}),
            State#state{households = Num};
            
        _ ->
            State
    end,
    {noreply, NewState};

handle_event(#wx{id = 1010, event = #wxCommand{type = command_choice_selected}}, 
             State = #state{mode_choice = Choice, household_text = HText, 
                           simulation_state = stopped}) ->
    Selection = wxChoice:getSelection(Choice),
    NewMode = case Selection of
        0 -> visual;
        1 -> stress;
        _ -> visual
    end,
    
    io:format("Dashboard: Mode changed to ~p~n", [NewMode]),
    
    %% Enable/disable household controls
    Enable = NewMode == stress,
    wxTextCtrl:enable(HText, [{enable, Enable}]),
    case get(update_households_btn) of
        undefined -> ok;
        Btn -> wxButton:enable(Btn, [{enable, Enable}])
    end,
    
    gen_server:cast(control_center, {set_mode, NewMode}),
    
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: STOPPED | Mode: ~p", [NewMode])),
    
    {noreply, State#state{simulation_mode = NewMode}};

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
%% @doc Control handlers
%%--------------------------------------------------------------------
handle_start(State = #state{simulation_mode = Mode, households = Households}) ->
    io:format("Dashboard: Starting simulation in ~p mode~n", [Mode]),
    
    %% Send households count if in stress mode
    case Mode of
        stress ->
            gen_server:cast(control_center, {update_households, Households});
        _ -> ok
    end,
    
    %% ONLY tell control to start - it will tell visualization what to do
    gen_server:cast(control_center, {start_simulation}),
    
    %% Update UI state
    wxChoice:enable(State#state.mode_choice, [{enable, false}]),
    wxTextCtrl:enable(State#state.household_text, [{enable, false}]),
    case get(update_households_btn) of
        undefined -> ok;
        Btn -> wxButton:enable(Btn, [{enable, false}])
    end,
    
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: RUNNING | Mode: ~p", [Mode])),
    
    State#state{simulation_state = running}.

handle_pause(State) ->
    io:format("Dashboard: Pausing simulation~n"),
    gen_server:cast(control_center, {pause_simulation}),
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: PAUSED | Mode: ~p", [State#state.simulation_mode])),
    State#state{simulation_state = paused}.

handle_stop(State = #state{simulation_mode = Mode}) ->
    io:format("Dashboard: Stopping simulation~n"),
    gen_server:cast(control_center, {stop_simulation}),
    
    wxChoice:enable(State#state.mode_choice, [{enable, true}]),
    
    Enable = Mode == stress,
    wxTextCtrl:enable(State#state.household_text, [{enable, Enable}]),
    case get(update_households_btn) of
        undefined -> ok;
        Btn -> wxButton:enable(Btn, [{enable, Enable}])
    end,
    
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: STOPPED | Mode: ~p", [Mode])),
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
    wxListCtrl:insertItem(List, 0, ""),
    wxListCtrl:setItem(List, 0, 0, atom_to_list(Control)),
    wxListCtrl:setItem(List, 0, 1, "Control"),
    wxListCtrl:setItemTextColour(List, 0, {0, 100, 200}),
    
    case Viz of
        [] -> ok;
        Node ->
            wxListCtrl:insertItem(List, 1, ""),
            wxListCtrl:setItem(List, 1, 0, atom_to_list(Node)),
            wxListCtrl:setItem(List, 1, 1, "Visual"),
            wxListCtrl:setItemTextColour(List, 1, {0, 150, 100})
    end,
    
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
    Count = wxListCtrl:getItemCount(List),
    Index = find_or_add_zone(List, Zone, 0, Count),
    
    wxListCtrl:setItem(List, Index, 0, atom_to_list(Zone)),
    
    StatusText = case Status of
        live -> "Live";
        down -> "Down";
        _ -> atom_to_list(Status)
    end,
    
    wxListCtrl:setItem(List, Index, 1, StatusText),
    wxListCtrl:setItem(List, Index, 2, integer_to_list(Couriers)),
    wxListCtrl:setItem(List, Index, 3, integer_to_list(Deliveries)).

find_or_add_zone(List, _Zone, Current, Max) when Current >= Max ->
    wxListCtrl:insertItem(List, Max, ""),
    Max;
find_or_add_zone(List, Zone, Current, Max) ->
    Item = wxListCtrl:getItemText(List, Current, [{col, 0}]),
    case Item of
        "" -> find_or_add_zone(List, Zone, Current + 1, Max);
        _ ->
            try 
                case list_to_atom(Item) of
                    Zone -> Current;
                    _ -> find_or_add_zone(List, Zone, Current + 1, Max)
                end
            catch
                _:_ -> find_or_add_zone(List, Zone, Current + 1, Max)
            end
    end.