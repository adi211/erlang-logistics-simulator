%%%-------------------------------------------------------------------
%%% @doc Dashboard Server - Fixed for distributed communication
%%%-------------------------------------------------------------------
-module(dashboard_server).
-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
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
    % Courier configuration inputs
    north_input,
    center_input,
    south_input,
    % Other controls
    map_choice,
    load_slider,
    % Display
    zones_list,
    % State
    simulation_state = stopped,
    current_map = map_data_100,
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
                       [{size, {1000, 1000}}]),
    
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
    
    %% === COURIER CONFIGURATION (Only editable when stopped) ===
    CourierBox = wxStaticBox:new(Panel, ?wxID_ANY, "Courier Configuration (Set before START)"),
    CourierSizer = wxStaticBoxSizer:new(CourierBox, ?wxVERTICAL),
    
    %% Courier configuration grid
    CourierGrid = wxFlexGridSizer:new(3, 3, 5, 10),
    
    %% North Zone
    NorthLabel = wxStaticText:new(Panel, ?wxID_ANY, "North Zone Couriers: "),
    NorthInput = wxTextCtrl:new(Panel, 2001, [{value, "5"}, {size, {60, -1}}]),
    wxFlexGridSizer:add(CourierGrid, NorthLabel, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
    wxFlexGridSizer:add(CourierGrid, NorthInput, [{flag, ?wxEXPAND}]),
    wxFlexGridSizer:add(CourierGrid, wxStaticText:new(Panel, ?wxID_ANY, "(1-20)"), 
                        [{flag, ?wxALIGN_CENTER_VERTICAL}]),
    
    %% Center Zone  
    CenterLabel = wxStaticText:new(Panel, ?wxID_ANY, "Center Zone Couriers: "),
    CenterInput = wxTextCtrl:new(Panel, 2002, [{value, "5"}, {size, {60, -1}}]),
    wxFlexGridSizer:add(CourierGrid, CenterLabel, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
    wxFlexGridSizer:add(CourierGrid, CenterInput, [{flag, ?wxEXPAND}]),
    wxFlexGridSizer:add(CourierGrid, wxStaticText:new(Panel, ?wxID_ANY, "(1-20)"), 
                        [{flag, ?wxALIGN_CENTER_VERTICAL}]),
    
    %% South Zone
    SouthLabel = wxStaticText:new(Panel, ?wxID_ANY, "South Zone Couriers: "),
    SouthInput = wxTextCtrl:new(Panel, 2003, [{value, "5"}, {size, {60, -1}}]),
    wxFlexGridSizer:add(CourierGrid, SouthLabel, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
    wxFlexGridSizer:add(CourierGrid, SouthInput, [{flag, ?wxEXPAND}]),
    wxFlexGridSizer:add(CourierGrid, wxStaticText:new(Panel, ?wxID_ANY, "(1-20)"), 
                        [{flag, ?wxALIGN_CENTER_VERTICAL}]),
    
    wxSizer:add(CourierSizer, CourierGrid, [{flag, ?wxALL bor ?wxEXPAND}, {border, 10}]),
    
    %% Configuration note
    Note = wxStaticText:new(Panel, ?wxID_ANY, 
                           "Note: Courier configuration can only be changed when simulation is STOPPED"),
    NoteFont = wxFont:new(9, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_ITALIC, ?wxFONTWEIGHT_NORMAL),
    wxStaticText:setFont(Note, NoteFont),
    wxStaticText:setForegroundColour(Note, {100, 100, 100}),
    wxSizer:add(CourierSizer, Note, [{flag, ?wxALL}, {border, 5}]),
    
    wxSizer:add(MainSizer, CourierSizer, [{flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    
    %% === ORDER LOAD CONTROL ===
    LoadBox = wxStaticBox:new(Panel, ?wxID_ANY, "Dynamic Controls"),
    LoadSizer = wxStaticBoxSizer:new(LoadBox, ?wxVERTICAL),
    
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
    
    wxSizer:add(LoadSizer, LoadRow, [{flag, ?wxEXPAND}]),
    
    wxSizer:add(MainSizer, LoadSizer, [{flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    
    %% === ZONE LIST ===
    ZonesBox = wxStaticBox:new(Panel, ?wxID_ANY, "Zones Status"),
    ZonesSizer = wxStaticBoxSizer:new(ZonesBox, ?wxVERTICAL),
    
    ZonesList = wxListCtrl:new(Panel, [{style, ?wxLC_REPORT}]),
    wxListCtrl:insertColumn(ZonesList, 0, "Node/Zone", [{width, 250}]),
    wxListCtrl:insertColumn(ZonesList, 1, "Status", [{width, 100}]),
    
    %% Add fixed zone rows (0-2 for zones, 3-4 for control/viz)
    lists:foreach(fun(Index) ->
        wxListCtrl:insertItem(ZonesList, Index, "")
    end, lists:seq(0, 4)),
    
    %% Initialize zone display
    wxListCtrl:setItem(ZonesList, 0, 0, "North Zone"),
    wxListCtrl:setItem(ZonesList, 0, 1, "Not Connected"),
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
        north_input = NorthInput,
        center_input = CenterInput,
        south_input = SouthInput,
        map_choice = MapChoice,
        load_slider = LoadSlider,
        zones_list = ZonesList,
        simulation_state = stopped,
        current_map = map_data_100,
        zone_status = #{},
        control_node = ?CTRL_NODE,
        visualization_node = node()
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle events
%%--------------------------------------------------------------------
handle_event(#wx{id = Id, event = #wxCommand{type = command_button_clicked}}, State) ->
    NewState = case Id of
        1001 -> % Start
            handle_start(State);
            
        1002 -> % Pause  
            handle_pause(State);
            
        1003 -> % Stop
            handle_stop(State);
            
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
                            simulation_state = CurrentState,
                            north_input = NorthInput,
                            center_input = CenterInput,
                            south_input = SouthInput}) ->
    case CurrentState of
        stopped ->
            %% Read courier configuration values
            NorthCouriers = get_courier_count(NorthInput),
            CenterCouriers = get_courier_count(CenterInput),
            SouthCouriers = get_courier_count(SouthInput),
            
            %% Lock the input fields
            wxTextCtrl:enable(NorthInput, [{enable, false}]),
            wxTextCtrl:enable(CenterInput, [{enable, false}]),
            wxTextCtrl:enable(SouthInput, [{enable, false}]),
            
            %% Create configuration with courier counts per zone
            Config = #{
                courier_config => #{
                    north => NorthCouriers,
                    center => CenterCouriers,
                    south => SouthCouriers
                },
                map_module => MapModule,
                num_households_per_zone => 10
            },
            
            io:format("Dashboard: Starting simulation with courier config: North=~p, Center=~p, South=~p~n", 
                     [NorthCouriers, CenterCouriers, SouthCouriers]),
            
            %% Send configuration to control center
            gen_server:cast({control_center, ControlNode}, {start_simulation_with_config, Config}),
            
            wxChoice:enable(State#state.map_choice, [{enable, false}]),
            
            MapLabel = get_map_label(MapModule),
            wxFrame:setStatusText(State#state.frame, 
                                 io_lib:format("System: RUNNING | Map: ~s | Couriers: N=~p C=~p S=~p", 
                                              [MapLabel, NorthCouriers, CenterCouriers, SouthCouriers])),
            
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

handle_pause(State = #state{control_node = ControlNode}) ->
    io:format("Dashboard: Pausing simulation~n"),
    gen_server:cast({control_center, ControlNode}, {pause_simulation}),
    
    MapLabel = get_map_label(State#state.current_map),
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("System: PAUSED | Map: ~s", [MapLabel])),
    State#state{simulation_state = paused}.

handle_stop(State = #state{control_node = ControlNode,
                           north_input = NorthInput,
                           center_input = CenterInput,
                           south_input = SouthInput}) ->
    io:format("Dashboard: Stopping simulation completely~n"),
    
    %% Unlock the courier input fields
    wxTextCtrl:enable(NorthInput, [{enable, true}]),
    wxTextCtrl:enable(CenterInput, [{enable, true}]),
    wxTextCtrl:enable(SouthInput, [{enable, true}]),
    
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
%% @doc Handle cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle info messages
%%--------------------------------------------------------------------
handle_info({state_update, <<"zone_update">>, Data}, State = #state{zones_list = List}) ->
    %% Extract zone data
    Zone = case maps:get(<<"zone">>, Data, maps:get(zone, Data, undefined)) of
        "north" -> north;
        "center" -> center;
        "south" -> south;
        <<"north">> -> north;
        <<"center">> -> center;
        <<"south">> -> south;
        ZoneAtom when is_atom(ZoneAtom) -> ZoneAtom;
        _ -> undefined
    end,
    
    case Zone of
        undefined -> {noreply, State};
        _ ->
            Index = case Zone of
                north -> 0;
                center -> 1;
                south -> 2;
                _ -> -1
            end,
            
            case Index of
                -1 -> {noreply, State};
                _ ->
                    %% Get zone status
                    Status = case maps:get(<<"status">>, Data, maps:get(status, Data, offline)) of
                        <<"live">> -> "Connected";
                        <<"offline">> -> "Offline";
                        live -> "Connected";
                        offline -> "Offline";
                        _ -> "Unknown"
                    end,
                    
                    
                    %% Update the list
                    ZoneName = case Index of
                        0 -> "North Zone";
                        1 -> "Center Zone";
                        2 -> "South Zone"
                    end,
                    
                    wxListCtrl:setItem(List, Index, 0, ZoneName),
                    wxListCtrl:setItem(List, Index, 1, Status),
                    
                    %% Set color based on status
                    Color = case Status of
                        "Connected" -> {0, 150, 0};   % Green
                        "Offline" -> {128, 128, 128}; % Gray
                        _ -> {200, 0, 0}              % Red
                    end,
                    wxListCtrl:setItemTextColour(List, Index, Color),
                    
                    {noreply, State}
            end
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

%% Helper function to get courier count from input
get_courier_count(Input) ->
    case wxTextCtrl:getValue(Input) of
        "" -> 5;  % Default value
        Val -> 
            case string:to_integer(Val) of
                {Num, _} when Num > 0, Num =< 20 -> Num;
                _ -> 5  % Default if invalid
            end
    end.

%% Helper functions for map management
get_map_label(map_data_100) -> "Small (100 homes)";
get_map_label(map_data_200) -> "Large (200 homes)";
get_map_label(_) -> "Custom".

get_map_index(map_data_100) -> 0;
get_map_index(map_data_200) -> 1;
get_map_index(_) -> 0.