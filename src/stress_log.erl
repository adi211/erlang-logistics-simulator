%%%-------------------------------------------------------------------
%%% @doc stress_log - User-friendly log viewer for stress mode
%%% Shows system messages in organized wxWidgets interface
%%%-------------------------------------------------------------------
-module(stress_log).

-include_lib("wx/include/wx.hrl").

-export([start/0, stop/0]).
-export([add_entry/3, update_stats/1]).
-export([init/0]).  % For spawn

-define(ID_FILTER_ALL, 1001).
-define(ID_FILTER_INFO, 1002).
-define(ID_FILTER_SUCCESS, 1003).
-define(ID_FILTER_WARNING, 1004).
-define(ID_FILTER_ERROR, 1005).
-define(ID_CLEAR_LOG, 1006).

-record(state, {
    frame,
    panel,
    log_list,
    stats_panel,
    entries = [],
    max_entries = 1000,
    filters = all,
    start_time
}).

%% Start the log viewer
start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.

%% Stop the log viewer
stop() ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> Pid ! stop
    end.

%% Add log entry
add_entry(Level, Category, Message) ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> 
            Pid ! {add_entry, Level, Category, Message, erlang:timestamp()}
    end.

%% Update statistics display
update_stats(Stats) ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> Pid ! {update_stats, Stats}
    end.

%% Initialize the log viewer window
init() ->
    %% Create wx application
    Wx = wx:new(),
    
    %% Create main frame
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Logistics System - Stress Mode Log Viewer", 
                       [{size, {1000, 700}},
                        {style, ?wxDEFAULT_FRAME_STYLE}]),
    
    %% Create main panel
    Panel = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(Panel, {245, 245, 245}),
    
    %% Create sizer for layout
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    
    %% Title
    Title = wxStaticText:new(Panel, ?wxID_ANY, "STRESS MODE - System Activity Monitor", 
                            [{style, ?wxALIGN_CENTER}]),
    TitleFont = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    wxStaticText:setFont(Title, TitleFont),
    wxSizer:add(MainSizer, Title, [{flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    
    %% Statistics panel
    StatsPanel = create_stats_panel(Panel),
    wxSizer:add(MainSizer, StatsPanel, [{proportion, 0}, 
                                         {flag, ?wxEXPAND bor ?wxALL}, 
                                         {border, 10}]),
    
    %% Filter buttons
    FilterPanel = create_filter_panel(Panel),
    wxSizer:add(MainSizer, FilterPanel, [{proportion, 0}, 
                                         {flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT}, 
                                         {border, 10}]),
    
    %% Log list control
    LogList = wxListCtrl:new(Panel, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
    
    %% Configure columns
    wxListCtrl:insertColumn(LogList, 0, "Time", [{width, 100}]),
    wxListCtrl:insertColumn(LogList, 1, "Level", [{width, 80}]),
    wxListCtrl:insertColumn(LogList, 2, "Category", [{width, 150}]),
    wxListCtrl:insertColumn(LogList, 3, "Message", [{width, 600}]),
    
    %% Set colors
    wxListCtrl:setBackgroundColour(LogList, {255, 255, 255}),
    
    wxSizer:add(MainSizer, LogList, [{proportion, 1}, 
                                     {flag, ?wxEXPAND bor ?wxALL}, 
                                     {border, 10}]),
    
    %% Status bar
    StatusBar = wxFrame:createStatusBar(Frame),
    wxStatusBar:setStatusText(StatusBar, "Ready - Monitoring system activity..."),
    
    %% Set panel sizer
    wxPanel:setSizer(Panel, MainSizer),
    
    %% Connect frame close event
    wxFrame:connect(Frame, close_window),
    
    %% Show frame
    wxFrame:show(Frame),
    
    io:format("Stress Log Viewer: Started~n"),
    
    %% Create initial state with start time
    State = #state{
        frame = Frame,
        panel = Panel,
        log_list = LogList,
        stats_panel = StatsPanel,
        entries = [],
        max_entries = 1000,
        filters = all,
        start_time = erlang:timestamp()
    },
    
    %% Start timer for uptime updates
    timer:send_interval(1000, self(), update_uptime),
    
    %% Start message loop
    loop(State).

%% Create statistics panel
create_stats_panel(Parent) ->
    Panel = wxPanel:new(Parent),
    wxPanel:setBackgroundColour(Panel, {230, 240, 250}),
    
    Sizer = wxGridSizer:new(1, 6, 5, 20),
    
    %% Create stat displays - removed Uptime from here
    Stats = [
        {"Active Processes:", "0", processes_text},
        {"Total Deliveries:", "0", deliveries_text},
        {"Messages/sec:", "0", messages_text}
    ],
    
    lists:foreach(fun({Label, Value, Tag}) ->
        %% Label
        LabelText = wxStaticText:new(Panel, ?wxID_ANY, Label),
        Font = wxFont:new(11, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
        wxStaticText:setFont(LabelText, Font),
        wxSizer:add(Sizer, LabelText, [{flag, ?wxALIGN_RIGHT bor ?wxALIGN_CENTER_VERTICAL}]),
        
        %% Value
        ValueText = wxStaticText:new(Panel, ?wxID_ANY, Value),
        wxStaticText:setForegroundColour(ValueText, {0, 100, 200}),
        wxStaticText:setFont(ValueText, Font),
        
        %% Store reference for updates
        put(Tag, ValueText),
        
        wxSizer:add(Sizer, ValueText, [{flag, ?wxALIGN_LEFT bor ?wxALIGN_CENTER_VERTICAL}])
    end, Stats),
    
    wxPanel:setSizer(Panel, Sizer),
    Panel.

%% Create filter panel with unique IDs for each button
create_filter_panel(Parent) ->
    Panel = wxPanel:new(Parent),
    
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    
    %% Filter label
    Label = wxStaticText:new(Panel, ?wxID_ANY, "Filter: "),
    wxSizer:add(Sizer, Label, [{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxRIGHT}, {border, 5}]),
    
    %% Create buttons with specific IDs
    AllBtn = wxButton:new(Panel, ?ID_FILTER_ALL, [{label, "All"}, {size, {80, 25}}]),
    InfoBtn = wxButton:new(Panel, ?ID_FILTER_INFO, [{label, "Info"}, {size, {80, 25}}]),
    SuccessBtn = wxButton:new(Panel, ?ID_FILTER_SUCCESS, [{label, "Success"}, {size, {80, 25}}]),
    WarningBtn = wxButton:new(Panel, ?ID_FILTER_WARNING, [{label, "Warning"}, {size, {80, 25}}]),
    ErrorBtn = wxButton:new(Panel, ?ID_FILTER_ERROR, [{label, "Error"}, {size, {80, 25}}]),
    
    %% Connect ALL events to the panel (not individual buttons)
    wxPanel:connect(Panel, command_button_clicked),
    
    %% Add buttons to sizer
    wxSizer:add(Sizer, AllBtn, [{flag, ?wxRIGHT}, {border, 5}]),
    wxSizer:add(Sizer, InfoBtn, [{flag, ?wxRIGHT}, {border, 5}]),
    wxSizer:add(Sizer, SuccessBtn, [{flag, ?wxRIGHT}, {border, 5}]),
    wxSizer:add(Sizer, WarningBtn, [{flag, ?wxRIGHT}, {border, 5}]),
    wxSizer:add(Sizer, ErrorBtn, [{flag, ?wxRIGHT}, {border, 5}]),
    
    %% Clear button
    ClearBtn = wxButton:new(Panel, ?ID_CLEAR_LOG, [{label, "Clear Log"}, {size, {100, 25}}]),
    wxSizer:add(Sizer, ClearBtn, [{flag, ?wxLEFT}, {border, 20}]),
    
    %% Auto-scroll checkbox
    AutoScroll = wxCheckBox:new(Panel, ?wxID_ANY, "Auto-scroll"),
    wxCheckBox:setValue(AutoScroll, true),
    put(auto_scroll, AutoScroll),
    wxSizer:add(Sizer, AutoScroll, [{flag, ?wxLEFT bor ?wxALIGN_CENTER_VERTICAL}, {border, 20}]),
    
    wxPanel:setSizer(Panel, Sizer),
    Panel.

%% Main message loop
loop(State) ->
    receive
        {add_entry, Level, Category, Message, Timestamp} ->
            NewState = add_log_entry(State, Level, Category, Message, Timestamp),
            loop(NewState);
            
        {update_stats, Stats} ->
            update_statistics_display(Stats),
            loop(State);
        
        %% Handle wxWidgets events
        #wx{id = Id, event = #wxCommand{type = command_button_clicked}} ->
            io:format("Button clicked: ID = ~p~n", [Id]),  % Debug output
            NewState = case Id of
                ?ID_FILTER_ALL -> 
                    io:format("Applying ALL filter~n"),
                    apply_filter(State, all);
                ?ID_FILTER_INFO -> 
                    io:format("Applying INFO filter~n"),
                    apply_filter(State, info);
                ?ID_FILTER_SUCCESS -> 
                    io:format("Applying SUCCESS filter~n"),
                    apply_filter(State, success);
                ?ID_FILTER_WARNING -> 
                    io:format("Applying WARNING filter~n"),
                    apply_filter(State, warning);
                ?ID_FILTER_ERROR -> 
                    io:format("Applying ERROR filter~n"),
                    apply_filter(State, error);
                ?ID_CLEAR_LOG -> 
                    io:format("Clearing log~n"),
                    clear_log(State);
                _ -> 
                    io:format("Unknown button ID: ~p~n", [Id]),
                    State
            end,
            loop(NewState);
            
        #wx{event = #wxClose{}} ->
            wxFrame:destroy(State#state.frame),
            ok;
            
        update_uptime ->
            %% No need to update uptime display anymore
            loop(State);
            
        stop ->
            wxFrame:destroy(State#state.frame),
            ok;
            
        Msg ->
            io:format("Unhandled message: ~p~n", [Msg]),
            loop(State)
    end.

%% Add entry to log
add_log_entry(State = #state{log_list = List, entries = Entries, max_entries = MaxEntries}, 
              Level, Category, Message, Timestamp) ->
    
    %% Format timestamp
    {{_, _, _}, {H, M, S}} = calendar:now_to_local_time(Timestamp),
    TimeStr = io_lib:format("~2..0B:~2..0B:~2..0B", [H, M, S]),
    
    %% Create entry
    Entry = {TimeStr, Level, Category, Message},
    
    %% Add to list control if matches filter
    case should_display(Level, State#state.filters) of
        true ->
            Index = wxListCtrl:getItemCount(List),
            wxListCtrl:insertItem(List, Index, ""),
            
            %% Set item data
            wxListCtrl:setItem(List, Index, 0, lists:flatten(TimeStr)),
            wxListCtrl:setItem(List, Index, 1, atom_to_list(Level)),
            wxListCtrl:setItem(List, Index, 2, Category),
            wxListCtrl:setItem(List, Index, 3, lists:flatten(Message)),
            
            %% Set color based on level
            Color = case Level of
                info -> {0, 0, 0};
                success -> {0, 150, 0};
                warning -> {200, 100, 0};
                error -> {200, 0, 0};
                _ -> {0, 0, 0}
            end,
            wxListCtrl:setItemTextColour(List, Index, Color),
            
            %% Auto-scroll if enabled
            case get(auto_scroll) of
                undefined -> ok;
                CheckBox ->
                    case wxCheckBox:getValue(CheckBox) of
                        true -> wxListCtrl:ensureVisible(List, Index);
                        false -> ok
                    end
            end;
        false ->
            ok
    end,
    
    %% Manage entry list size
    NewEntries = case length(Entries) >= MaxEntries of
        true -> [Entry | lists:sublist(Entries, MaxEntries - 1)];
        false -> [Entry | Entries]
    end,
    
    %% Update status bar with counts
    Frame = State#state.frame,
    StatusBar = wxFrame:getStatusBar(Frame),
    
    %% Count entries by level
    InfoCount = length([E || E <- NewEntries, element(2, E) == info]),
    SuccessCount = length([E || E <- NewEntries, element(2, E) == success]),
    WarningCount = length([E || E <- NewEntries, element(2, E) == warning]),
    ErrorCount = length([E || E <- NewEntries, element(2, E) == error]),
    
    StatusText = io_lib:format("Total: ~p | Info: ~p | Success: ~p | Warning: ~p | Error: ~p | Filter: ~p", 
                               [length(NewEntries), InfoCount, SuccessCount, WarningCount, ErrorCount, State#state.filters]),
    wxStatusBar:setStatusText(StatusBar, lists:flatten(StatusText)),
    
    State#state{entries = NewEntries}.

%% Check if entry should be displayed
should_display(_, all) -> true;
should_display(Level, Filter) -> Level == Filter.

%% Apply filter to log
apply_filter(State = #state{log_list = List, entries = Entries}, Filter) ->
    io:format("Applying filter: ~p to ~p entries~n", [Filter, length(Entries)]),
    
    %% Clear list
    wxListCtrl:deleteAllItems(List),
    
    %% Count how many will be shown
    FilteredCount = length([E || E <- Entries, should_display(element(2, E), Filter)]),
    io:format("Showing ~p entries after filtering~n", [FilteredCount]),
    
    %% Re-add filtered entries
    lists:foreach(fun(Entry) ->
        {Time, Level, Category, Message} = Entry,
        case should_display(Level, Filter) of
            true ->
                Index = wxListCtrl:getItemCount(List),
                wxListCtrl:insertItem(List, Index, ""),
                wxListCtrl:setItem(List, Index, 0, lists:flatten(Time)),
                wxListCtrl:setItem(List, Index, 1, atom_to_list(Level)),
                wxListCtrl:setItem(List, Index, 2, Category),
                wxListCtrl:setItem(List, Index, 3, lists:flatten(Message)),
                
                Color = case Level of
                    info -> {0, 0, 0};
                    success -> {0, 150, 0};
                    warning -> {200, 100, 0};
                    error -> {200, 0, 0};
                    _ -> {0, 0, 0}
                end,
                wxListCtrl:setItemTextColour(List, Index, Color);
            false ->
                ok
        end
    end, lists:reverse(Entries)),
    
    %% Update status bar
    update_filter_status(State#state{filters = Filter}),
    
    State#state{filters = Filter}.

%% Clear the log
clear_log(State = #state{log_list = List}) ->
    wxListCtrl:deleteAllItems(List),
    
    %% Update status bar
    Frame = State#state.frame,
    StatusBar = wxFrame:getStatusBar(Frame),
    wxStatusBar:setStatusText(StatusBar, "Log cleared"),
    
    State#state{entries = []}.

%% Update filter status in status bar
update_filter_status(State = #state{entries = Entries}) ->
    Frame = State#state.frame,
    StatusBar = wxFrame:getStatusBar(Frame),
    
    %% Count entries by level
    InfoCount = length([E || E <- Entries, element(2, E) == info]),
    SuccessCount = length([E || E <- Entries, element(2, E) == success]),
    WarningCount = length([E || E <- Entries, element(2, E) == warning]),
    ErrorCount = length([E || E <- Entries, element(2, E) == error]),
    
    StatusText = io_lib:format("Total: ~p | Info: ~p | Success: ~p | Warning: ~p | Error: ~p | Filter: ~p", 
                               [length(Entries), InfoCount, SuccessCount, WarningCount, ErrorCount, State#state.filters]),
    wxStatusBar:setStatusText(StatusBar, lists:flatten(StatusText)).

%% Update statistics display
update_statistics_display(Stats) ->
    %% Update process count
    case maps:find(processes, Stats) of
        {ok, P} -> 
            case get(processes_text) of
                undefined -> ok;
                ProcWidget -> wxStaticText:setLabel(ProcWidget, integer_to_list(P))
            end;
        error -> ok
    end,
    
    %% Update deliveries
    case maps:find(deliveries, Stats) of
        {ok, D} ->
            case get(deliveries_text) of
                undefined -> ok;
                DelWidget -> wxStaticText:setLabel(DelWidget, integer_to_list(D))
            end;
        error -> ok
    end,
    
    %% Update messages/sec
    case maps:find(messages_per_sec, Stats) of
        {ok, M} ->
            case get(messages_text) of
                undefined -> ok;
                MsgWidget -> wxStaticText:setLabel(MsgWidget, io_lib:format("~.1f", [M]))
            end;
        error -> ok
    end.