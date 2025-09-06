%%%-------------------------------------------------------------------
%%% @doc Visualization Server - Updated with State Collector Integration
%%%-------------------------------------------------------------------
-module(visualization_server).
-behaviour(gen_server).

-include_lib("wx/include/wx.hrl").

-export([start/0, start/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([update_courier_position/2, log_message/3, update_house_orders/2]).

-define(SERVER, ?MODULE).

-record(state, {
    frame,
    notebook,
    map_panel,
    log_panel,
    control_node,
    current_map = "map_data_100",  % Default map module name
    wx_env
}).

%%--------------------------------------------------------------------
%% API Functions
%%--------------------------------------------------------------------

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start(MapModuleName) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [MapModuleName], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update_courier_position(CourierId, Position) ->
    gen_server:cast(?SERVER, {update_courier_position, CourierId, Position}).

update_house_orders(HouseId, Orders) ->
    gen_server:cast(?SERVER, {update_house_orders, HouseId, Orders}).

log_message(Level, Category, Message) ->
    gen_server:cast(?SERVER, {log_message, Level, Category, Message}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(Args) ->
    io:format("Visualization Server: Starting unified GUI with tabs~n"),
    
    %% Get initial map module name if provided
    InitialMapModule = case Args of
        [MapName] when is_list(MapName) -> MapName;
        [MapName] when is_atom(MapName) -> atom_to_list(MapName);
        _ -> "map_data_100"
    end,
    
    %% Initialize wx
    Wx = wx:new(),
    WxEnv = wx:get_env(),
    
    %% Create ETS tables for map data
    ets:new(map_data, [set, named_table, public]),
    ets:new(courier_positions, [set, named_table, public]),
    ets:new(house_orders, [set, named_table, public]),
    
    %% Load initial map data from external file
    load_map_from_file(InitialMapModule),
    
    %% Create main frame
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Logistics System - Unified View", 
                       [{size, {1400, 900}},
                        {style, ?wxDEFAULT_FRAME_STYLE}]),
    
    %% Create main panel
    MainPanel = wxPanel:new(Frame),
    
    %% Create main sizer
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    
    %% Create title
    Title = wxStaticText:new(MainPanel, ?wxID_ANY, "LOGISTICS VISUALIZATION CENTER", 
                            [{style, ?wxALIGN_CENTER}]),
    TitleFont = wxFont:new(18, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    wxStaticText:setFont(Title, TitleFont),
    wxSizer:add(MainSizer, Title, [{flag, ?wxALL bor ?wxALIGN_CENTER}, {border, 10}]),
    
    %% Add separator line
    wxSizer:add(MainSizer, wxStaticLine:new(MainPanel), 
                [{flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT}, {border, 10}]),
    
    %% Create notebook for tabs
    Notebook = wxNotebook:new(MainPanel, ?wxID_ANY),
    
    %% Tab 1: Map View
    MapPanel = wxPanel:new(Notebook),
    wxPanel:setBackgroundColour(MapPanel, {240, 240, 240}),
    wxNotebook:addPage(Notebook, MapPanel, "Map View", [{bSelect, true}]),
    
    %% Tab 2: System Logs  
    LogPanel = wxPanel:new(Notebook),
    wxNotebook:addPage(Notebook, LogPanel, "System Logs", [{bSelect, false}]),
    
    %% Add notebook to main sizer
    wxSizer:add(MainSizer, Notebook, [{proportion, 1}, 
                                      {flag, ?wxEXPAND bor ?wxALL}, 
                                      {border, 10}]),
    
    %% Create status bar
    StatusBar = wxFrame:createStatusBar(Frame, [{number, 3}]),
    wxStatusBar:setStatusText(StatusBar, "System: READY", [{number, 0}]),
    wxStatusBar:setStatusText(StatusBar, io_lib:format("Map: ~s", [InitialMapModule]), [{number, 1}]),
    wxStatusBar:setStatusText(StatusBar, "Connected Nodes: 0", [{number, 2}]),
    
    %% Set sizer
    wxPanel:setSizer(MainPanel, MainSizer),
    
    %% Connect paint event for map panel
    wxPanel:connect(MapPanel, paint, [{callback, fun on_paint_map/2}]),
    
    %% Initialize log panel
    init_log_in_panel(LogPanel),
    
    %% Show frame
    wxFrame:show(Frame),
    
    %% Subscribe to logistics state collector for updates
    try
        case global:whereis_name(logistics_state_collector) of
            undefined ->
                io:format("Visualization Server: State collector not found, will retry~n"),
                timer:send_after(2000, self(), retry_subscribe);
            CollectorPid when is_pid(CollectorPid) ->
                gen_server:call(CollectorPid, {subscribe, self()}),
                io:format("Visualization Server: Successfully subscribed to state collector~n")
        end
    catch
        Type:Error ->
            io:format("Visualization Server: Error subscribing to state collector: ~p:~p~n", [Type, Error]),
            timer:send_after(2000, self(), retry_subscribe)
    end,
    
    %% Force initial paint
    wxPanel:refresh(MapPanel),
    
    {ok, #state{
        frame = Frame,
        notebook = Notebook,
        map_panel = MapPanel,
        log_panel = LogPanel,
        control_node = undefined,
        current_map = InitialMapModule,
        wx_env = WxEnv
    }}.

%%--------------------------------------------------------------------
%% Handle state updates from logistics_state_collector
%%--------------------------------------------------------------------
%% הקוד המתוקן - משתמש במפתחות אטומים
handle_info({state_update, <<"package_update">>, Data}, State) ->
    %% Get data - השתמש באטומים במקום בבינאריים
    PackageId = maps:get(id, Data, <<"unknown">>),
    Status = maps:get(status, Data, <<"unknown">>),
    
    %% Convert binary to string for display
    PkgStr = binary_to_list(PackageId),
    StatusStr = binary_to_list(Status),
    
    %% Create log message
    LogMsg = io_lib:format("Package ~s: ~s", [PkgStr, StatusStr]),
    
    %% Add to log
    add_log_entry(State#state.log_panel, info, "Package", LogMsg),
    
    %% Update map display based on status
    case StatusStr of
        "ordered" ->
            %% Extract house number from package ID: "north_home_18_1_timestamp"
            case string:tokens(PkgStr, "_") of
                [_Zone, "home", NumStr | _] ->
                    HouseKey = list_to_atom("house_" ++ NumStr),
                    CurrentOrders = case ets:lookup(house_orders, HouseKey) of
                        [{_, Count}] -> Count;
                        [] -> 0
                    end,
                    ets:insert(house_orders, {HouseKey, CurrentOrders + 1});
                _ -> ok
            end;
        "delivered" ->
            case string:tokens(PkgStr, "_") of
                [_Zone, "home", NumStr | _] ->
                    HouseKey = list_to_atom("house_" ++ NumStr),
                    CurrentOrders = case ets:lookup(house_orders, HouseKey) of
                        [{_, Count}] when Count > 0 -> Count;
                        _ -> 1
                    end,
                    ets:insert(house_orders, {HouseKey, CurrentOrders - 1});
                _ -> ok
            end;
        _ -> ok
    end,
    
    %% Refresh map
    wxPanel:refresh(State#state.map_panel),
    {noreply, State};

handle_info({state_update, <<"zone_update">>, _Data}, State) ->
    wxPanel:refresh(State#state.map_panel),
    {noreply, State};

handle_info({state_update, <<"courier_update">>, _Data}, State) ->
    {noreply, State};

handle_info(retry_subscribe, State) ->
    io:format("Visualization Server: Retrying subscription~n"),
    try
        case global:whereis_name(logistics_state_collector) of
            undefined ->
                timer:send_after(5000, self(), retry_subscribe);
            CollectorPid when is_pid(CollectorPid) ->
                logistics_state_collector:subscribe(self()),
                io:format("Visualization Server: Successfully subscribed~n")
        end
    catch
        _:_ ->
            timer:send_after(5000, self(), retry_subscribe)
    end,
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    io:format("Visualization Server: Node ~p down~n", [Node]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

	
extract_house_from_package_id(PackageId) ->
    PackageStr = case PackageId of
        Bin when is_binary(Bin) -> binary_to_list(Bin);
        Str when is_list(Str) -> Str;
        _ -> ""
    end,
    
    %% Package ID בפורמט: "north_home_3_1_timestamp"
    case string:tokens(PackageStr, "_") of
        [_Zone, "home", NumStr | _] ->
            {ok, list_to_atom("house_" ++ NumStr)};
        _ ->
            error
    end.
	


%% --------------------------------------------------------------------
%% Helper function to safely format different data types for display
%% --------------------------------------------------------------------
format_for_display(undefined) -> "unassigned";
format_for_display(null) -> "unassigned";  
format_for_display(<<"null">>) -> "unassigned";
format_for_display(Value) when is_binary(Value) -> binary_to_list(Value);
format_for_display(Value) when is_list(Value) -> Value;
format_for_display(Value) when is_atom(Value) -> atom_to_list(Value);
format_for_display(Value) when is_integer(Value) -> integer_to_list(Value);
format_for_display(_Other) -> "unknown".



%%--------------------------------------------------------------------
%% Load map data from external .erl module file
%%--------------------------------------------------------------------
load_map_from_file(MapModuleName) ->
    io:format("Loading map from module: ~s~n", [MapModuleName]),
    
    %% Convert to atom if needed
    ModuleAtom = if
        is_list(MapModuleName) -> list_to_atom(MapModuleName);
        is_atom(MapModuleName) -> MapModuleName
    end,
    
    try
        %% First try to compile the module if needed
        case code:ensure_loaded(ModuleAtom) of
            {error, _} ->
                %% Try to compile the file
                FileName = atom_to_list(ModuleAtom) ++ ".erl",
                io:format("Attempting to compile ~s~n", [FileName]),
                case compile:file(FileName, []) of
                    {ok, _} ->
                        io:format("Successfully compiled ~s~n", [FileName]);
                    CompileError ->
                        io:format("Failed to compile ~s: ~p~n", [FileName, CompileError]),
                        throw({compilation_failed, CompileError})
                end;
            _ ->
                ok
        end,
        
        %% Call the get_map/0 function from the module
        case erlang:function_exported(ModuleAtom, get_map, 0) of
            true ->
                MapData = ModuleAtom:get_map(),
                
                %% Extract components from the map
                Homes = maps:get(homes, MapData, []),
                Businesses = maps:get(businesses, MapData, []),
                Roads = maps:get(roads, MapData, []),  %% Extract roads
                Dimensions = maps:get(dimensions, MapData, #{width => 1200, height => 800}),
                Stats = maps:get(stats, MapData, #{}),
                
                %% Convert home format from module to internal format
                ConvertedHomes = convert_homes(Homes),
                ConvertedBusinesses = convert_businesses(Businesses),
                
                %% Store in ETS
                ets:insert(map_data, {homes, ConvertedHomes}),
                ets:insert(map_data, {businesses, ConvertedBusinesses}),
                ets:insert(map_data, {roads, Roads}),  %% Store roads in ETS
                ets:insert(map_data, {dimensions, Dimensions}),
                ets:insert(map_data, {stats, Stats}),
                
                io:format("Map loaded: ~p homes, ~p businesses, ~p roads~n", 
                          [length(ConvertedHomes), length(ConvertedBusinesses), length(Roads)]),
                ok;
            false ->
                io:format("Module ~p doesn't export get_map/0~n", [ModuleAtom]),
                throw({no_get_map_function, ModuleAtom})
        end
    catch
        Type:CatchError ->
            io:format("Error loading map from ~s: ~p:~p~n", [MapModuleName, Type, CatchError]),
            %% Load a default fallback map
            load_default_map()
    end.

%%--------------------------------------------------------------------
%% Convert homes from module format to internal format
%%--------------------------------------------------------------------
convert_homes(Homes) when is_list(Homes) ->
    lists:map(fun(Home) ->
        #{
            id => maps:get(id, Home),
            x => maps:get(x, Home),
            y => maps:get(y, Home),
            zone => maps:get(zone, Home)
        }
    end, Homes).

%%--------------------------------------------------------------------
%% Convert businesses from module format to internal format  
%%--------------------------------------------------------------------
convert_businesses(Businesses) when is_list(Businesses) ->
    lists:map(fun(Business) ->
        #{
            id => maps:get(id, Business),
            x => maps:get(x, Business),
            y => maps:get(y, Business),
            zone => maps:get(zone, Business),
            type => maps:get(type, Business, business)
        }
    end, Businesses).

%%--------------------------------------------------------------------
%% Load default map as fallback
%%--------------------------------------------------------------------
load_default_map() ->
    io:format("Loading default fallback map~n"),
    
    DefaultHomes = [
        #{id => 1, x => 92, y => 61, zone => north},
        #{id => 2, x => 184, y => 61, zone => north},
        #{id => 3, x => 276, y => 61, zone => north},
        #{id => 35, x => 92, y => 307, zone => center},
        #{id => 50, x => 369, y => 369, zone => center},
        #{id => 68, x => 92, y => 553, zone => south},
        #{id => 80, x => 92, y => 615, zone => south}
    ],
    
    DefaultBusinesses = [
        #{id => dc_north, x => 553, y => 123, zone => north, type => distribution_center},
        #{id => dc_center, x => 553, y => 430, zone => center, type => distribution_center},
        #{id => dc_south, x => 553, y => 676, zone => south, type => distribution_center}
    ],
    
    ets:insert(map_data, {homes, DefaultHomes}),
    ets:insert(map_data, {businesses, DefaultBusinesses}),
    ets:insert(map_data, {roads, []}),  %% Empty roads list for default
    ets:insert(map_data, {dimensions, #{width => 1200, height => 800}}),
    ets:insert(map_data, {stats, #{total_homes => 7, total_businesses => 3}}).

%%--------------------------------------------------------------------
%% Paint callback for map
%%--------------------------------------------------------------------
on_paint_map(#wx{obj = Panel}, _) ->
    DC = wxPaintDC:new(Panel),
    
    try
        wxDC:clear(DC),
        
        %% Get panel size
        {W, H} = wxPanel:getSize(Panel),
        
        %% Draw zones
        draw_zones(DC, W, H),
        
        %% Draw roads from ETS
        case ets:lookup(map_data, roads) of
            [{roads, Roads}] when is_list(Roads) ->
                draw_roads(DC, Roads);
            _ -> ok
        end,
        
        %% Draw homes from ETS
        case ets:lookup(map_data, homes) of
            [{homes, Homes}] when is_list(Homes) -> 
                draw_map_homes(DC, Homes);
            _ -> 
                io:format("No homes found in ETS~n")
        end,
        
        %% Draw businesses from ETS
        case ets:lookup(map_data, businesses) of
            [{businesses, Businesses}] when is_list(Businesses) -> 
                draw_map_businesses(DC, Businesses);
            _ -> 
                io:format("No businesses found in ETS~n")
        end,
        
        %% Draw couriers from ETS
        case ets:tab2list(courier_positions) of
            [] -> ok;
            Couriers -> draw_couriers_from_ets(DC, Couriers)
        end,
        
        %% Draw legend
        draw_legend(DC, W)
    catch
        _:Error ->
            io:format("Paint error: ~p~n", [Error])
    end,
    
    wxPaintDC:destroy(DC),
    ok.

%%--------------------------------------------------------------------
%% Draw roads from map data
%%--------------------------------------------------------------------
draw_roads(DC, Roads) ->
    %% Set pen for roads - gray color, 2 pixels width
    wxDC:setPen(DC, wxPen:new({140, 140, 140}, [{width, 2}])),
    lists:foreach(fun(Road) ->
        X1 = maps:get(x1, Road),
        Y1 = maps:get(y1, Road),
        X2 = maps:get(x2, Road),
        Y2 = maps:get(y2, Road),
        wxDC:drawLine(DC, {X1, Y1}, {X2, Y2})
    end, Roads).

%%--------------------------------------------------------------------
%% Draw homes from map data
%%--------------------------------------------------------------------
draw_map_homes(DC, Homes) ->
    lists:foreach(fun(Home) ->
        X = maps:get(x, Home),
        Y = maps:get(y, Home),
        Zone = maps:get(zone, Home),
        Id = maps:get(id, Home),
        
        %% Zone color
        Color = case Zone of
            north -> {100, 100, 200};
            center -> {100, 200, 100};
            south -> {200, 100, 100};
            _ -> {150, 150, 150}
        end,
        
        %% Draw house
        wxDC:setBrush(DC, wxBrush:new(Color)),
        wxDC:setPen(DC, wxPen:new({50, 50, 50}, [{width, 1}])),
        wxDC:drawRectangle(DC, {X - 8, Y - 8, 16, 16}),
        
        %% Draw roof triangle
        wxDC:setBrush(DC, wxBrush:new({139, 69, 19})),
        Points = [{X - 10, Y - 8}, {X, Y - 15}, {X + 10, Y - 8}],
        wxDC:drawPolygon(DC, Points),
        
        %% Check for orders in ETS
        HouseKey = list_to_atom("house_" ++ integer_to_list(Id)),
        case ets:lookup(house_orders, HouseKey) of
            [{_, Orders}] when Orders > 0 ->
                %% Draw order indicator
                wxDC:setBrush(DC, wxBrush:new({255, 0, 0})),
                wxDC:setPen(DC, wxPen:new({200, 0, 0}, [{width, 2}])),
                wxDC:drawCircle(DC, {X + 12, Y - 12}, 10),
                
                Font = wxFont:new(9, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
                wxDC:setFont(DC, Font),
                wxDC:setTextForeground(DC, {255, 255, 255}),
                wxDC:drawText(DC, integer_to_list(Orders), {X + 8, Y - 18});
            _ -> ok
        end
    end, Homes).

%%--------------------------------------------------------------------
%% Draw businesses from map data
%%--------------------------------------------------------------------
draw_map_businesses(DC, Businesses) ->
    lists:foreach(fun(Business) ->
        X = maps:get(x, Business),
        Y = maps:get(y, Business),
        Zone = maps:get(zone, Business),
        
        %% Draw business as larger golden square
        wxDC:setBrush(DC, wxBrush:new({255, 215, 0})),
        wxDC:setPen(DC, wxPen:new({0, 0, 0}, [{width, 3}])),
        wxDC:drawRectangle(DC, {X - 30, Y - 30, 60, 60}),
        
        %% Label
        Font = wxFont:new(12, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
        wxDC:setFont(DC, Font),
        wxDC:setTextForeground(DC, {0, 0, 0}),
        
        Label = case Zone of
            north -> "DC-N";
            center -> "DC-C";
            south -> "DC-S";
            _ -> "DC"
        end,
        wxDC:drawText(DC, Label, {X - 18, Y - 5})
    end, Businesses).

%%--------------------------------------------------------------------
%% Draw couriers from ETS
%%--------------------------------------------------------------------
draw_couriers_from_ets(DC, CourierList) ->
    lists:foreach(fun({CourierId, {X, Y}}) ->
        %% Draw courier as vehicle
        wxDC:setBrush(DC, wxBrush:new({0, 150, 0})),
        wxDC:setPen(DC, wxPen:new({0, 0, 0}, [{width, 2}])),
        wxDC:drawRoundedRectangle(DC, {X - 15, Y - 10, 30, 20}, 3),
        
        %% ID
        Font = wxFont:new(8, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
        wxDC:setFont(DC, Font),
        wxDC:setTextForeground(DC, {255, 255, 255}),
        
        IDStr = atom_to_list(CourierId),
        ShortID = lists:sublist(IDStr, 1, 6),
        wxDC:drawText(DC, ShortID, {X - 12, Y - 3})
    end, CourierList).

%%--------------------------------------------------------------------
%% Draw zones
%%--------------------------------------------------------------------
draw_zones(DC, Width, _Height) ->
    %% Use fixed zone heights based on the map data
    %% North: 0-266, Center: 267-533, South: 534-800
    
    %% North Zone
    wxDC:setBrush(DC, wxBrush:new({220, 230, 250})),
    wxDC:setPen(DC, wxPen:new({150, 150, 150}, [{width, 2}])),
    wxDC:drawRectangle(DC, {0, 0, Width, 266}),
    
    %% Center Zone
    wxDC:setBrush(DC, wxBrush:new({220, 250, 220})),
    wxDC:drawRectangle(DC, {0, 267, Width, 266}),
    
    %% South Zone
    wxDC:setBrush(DC, wxBrush:new({250, 220, 220})),
    wxDC:drawRectangle(DC, {0, 534, Width, 266}),
    
    %% Zone labels
    Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    wxDC:setFont(DC, Font),
    
    wxDC:setTextForeground(DC, {50, 50, 150}),
    wxDC:drawText(DC, "NORTH ZONE", {20, 10}),
    
    wxDC:setTextForeground(DC, {50, 150, 50}),
    wxDC:drawText(DC, "CENTER ZONE", {20, 277}),
    
    wxDC:setTextForeground(DC, {150, 50, 50}),
    wxDC:drawText(DC, "SOUTH ZONE", {20, 544}).

%%--------------------------------------------------------------------
%% Draw legend
%%--------------------------------------------------------------------
draw_legend(DC, Width) ->
    wxDC:setBrush(DC, wxBrush:new({255, 255, 255, 240})),
    wxDC:setPen(DC, wxPen:new({100, 100, 100})),
    wxDC:drawRectangle(DC, {Width - 150, 30, 140, 200}),
    
    Font = wxFont:new(9, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
    wxDC:setFont(DC, Font),
    wxDC:setTextForeground(DC, {0, 0, 0}),
    
    wxDC:drawText(DC, "Legend:", {Width - 140, 40}),
    
    %% House
    wxDC:setBrush(DC, wxBrush:new({100, 100, 200})),
    wxDC:drawRectangle(DC, {Width - 140, 60, 10, 10}),
    wxDC:drawText(DC, "House", {Width - 125, 60}),
    
    %% DC
    wxDC:setBrush(DC, wxBrush:new({255, 215, 0})),
    wxDC:drawRectangle(DC, {Width - 140, 80, 12, 12}),
    wxDC:drawText(DC, "Dist. Center", {Width - 125, 80}),
    
    %% Courier
    wxDC:setBrush(DC, wxBrush:new({0, 150, 0})),
    wxDC:drawRectangle(DC, {Width - 140, 100, 12, 8}),
    wxDC:drawText(DC, "Courier", {Width - 125, 98}),
    
    %% Orders
    wxDC:setBrush(DC, wxBrush:new({255, 0, 0})),
    wxDC:drawCircle(DC, {Width - 135, 125}, 5),
    wxDC:drawText(DC, "Orders", {Width - 125, 120}),
    
    %% Roads
    wxDC:setPen(DC, wxPen:new({140, 140, 140}, [{width, 2}])),
    wxDC:drawLine(DC, {Width - 140, 145}, {Width - 100, 145}),
    wxDC:setTextForeground(DC, {0, 0, 0}),
    wxDC:drawText(DC, "Road", {Width - 125, 140}),
    
    %% Delivery route
    wxDC:setPen(DC, wxPen:new({255, 140, 0}, [{width, 2}, {style, ?wxDOT}])),
    wxDC:drawLine(DC, {Width - 140, 165}, {Width - 100, 165}),
    wxDC:setTextForeground(DC, {0, 0, 0}),
    wxDC:drawText(DC, "Route", {Width - 125, 160}).

%%--------------------------------------------------------------------
%% Initialize log panel
%%--------------------------------------------------------------------
init_log_in_panel(Panel) ->
    io:format("Initializing log panel~n"),
    
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    
    LogList = wxListCtrl:new(Panel, [{style, ?wxLC_REPORT}]),
    wxListCtrl:insertColumn(LogList, 0, "Time", [{width, 100}]),
    wxListCtrl:insertColumn(LogList, 1, "Level", [{width, 80}]),
    wxListCtrl:insertColumn(LogList, 2, "Category", [{width, 150}]),
    wxListCtrl:insertColumn(LogList, 3, "Message", [{width, 600}]),
    
    wxSizer:add(Sizer, LogList, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    
    wxPanel:setSizer(Panel, Sizer),
    
    put(log_list, LogList),
    
    add_log_entry(Panel, info, "System", "Log viewer initialized"),
    
    ok.

%%--------------------------------------------------------------------
%% Add log entry
%%--------------------------------------------------------------------
add_log_entry(_Panel, Level, Category, Message) ->
    case get(log_list) of
        undefined -> ok;
        List ->
            {{_, _, _}, {H, M, S}} = calendar:now_to_local_time(erlang:timestamp()),
            TimeStr = io_lib:format("~2..0B:~2..0B:~2..0B", [H, M, S]),
            
            Index = wxListCtrl:getItemCount(List),
            wxListCtrl:insertItem(List, Index, ""),
            
            wxListCtrl:setItem(List, Index, 0, lists:flatten(TimeStr)),
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
            wxListCtrl:setItemTextColour(List, Index, Color),
            
            wxListCtrl:ensureVisible(List, Index)
    end.

%%--------------------------------------------------------------------
%% Handle calls
%%--------------------------------------------------------------------
handle_call({connect, ControlNode}, _From, State) ->
    io:format("Visualization Server: Control center ~p connected~n", [ControlNode]),
    monitor_node(ControlNode, true),
    update_status_bar(State#state.frame, "Connected to control", 2),
    {reply, node(), State#state{control_node = ControlNode}};

handle_call(shutdown, _From, State) ->
    io:format("Visualization Server: Shutting down~n"),
    {stop, shutdown, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Handle casts
%%--------------------------------------------------------------------
handle_cast({start_visualization}, State) ->
    io:format("Visualization Server: Starting visualization~n"),
    wxPanel:refresh(State#state.map_panel),
    update_status_bar(State#state.frame, "System: RUNNING", 0),
    log_message(info, "System", "Visualization started"),
    {noreply, State};

handle_cast({stop_visualization}, State) ->
    io:format("Visualization Server: Stopping visualization~n"),
    ets:delete_all_objects(courier_positions),
    ets:delete_all_objects(house_orders),
    wxPanel:refresh(State#state.map_panel),
    update_status_bar(State#state.frame, "System: STOPPED", 0),
    log_message(info, "System", "Visualization stopped"),
    {noreply, State};

handle_cast({update_courier_position, CourierId, Position}, State) ->
    ets:insert(courier_positions, {CourierId, Position}),
    wxPanel:refresh(State#state.map_panel),
    {noreply, State};

handle_cast({update_house_orders, HouseId, Orders}, State) ->
    ets:insert(house_orders, {HouseId, Orders}),
    wxPanel:refresh(State#state.map_panel),
    {noreply, State};

handle_cast({log_message, Level, Category, Message}, State) ->
    add_log_entry(State#state.log_panel, Level, Category, Message),
    {noreply, State};

handle_cast({load_map, MapModuleName}, State) ->
    io:format("Visualization Server: Loading map ~p~n", [MapModuleName]),
    load_map_from_file(MapModuleName),
    wxPanel:refresh(State#state.map_panel),
    update_status_bar(State#state.frame, io_lib:format("Map: ~s", [MapModuleName]), 1),
    log_message(info, "Map", io_lib:format("Loaded map: ~s", [MapModuleName])),
    {noreply, State#state{current_map = MapModuleName}};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Terminate
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("Visualization Server: Terminating~n"),
    %% Unsubscribe from state collector if possible
    try
        case global:whereis_name(logistics_state_collector) of
            undefined -> ok;
            CollectorPid when is_pid(CollectorPid) ->
                gen_server:call(CollectorPid, {unsubscribe, self()})
        end
    catch
        _:_ -> ok
    end,
    ok.

%%--------------------------------------------------------------------
%% Code change
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Helper: Update status bar
%%--------------------------------------------------------------------
update_status_bar(Frame, Text, Column) ->
    case wxFrame:getStatusBar(Frame) of
        false -> ok;
        StatusBar -> 
            wxStatusBar:setStatusText(StatusBar, Text, [{number, Column}])
    end.