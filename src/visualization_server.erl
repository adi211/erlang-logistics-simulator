%%%-------------------------------------------------------------------
%%% @doc Visualization Server with State Collector Integration
%%%-------------------------------------------------------------------
-module(visualization_server).
-behaviour(gen_server).

-include_lib("wx/include/wx.hrl").
-include("map_records.hrl").  % For location record

-export([start/0, start/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([update_courier_position/2, log_message/3, update_house_orders/2]).
-export([update_courier_status/2, set_courier_route/2]).
-export([package_delivered/1]).

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

update_courier_status(CourierId, Status) ->
    gen_server:cast(?SERVER, {update_courier_status, CourierId, Status}).

set_courier_route(CourierId, RouteData) ->
    gen_server:cast(?SERVER, {set_courier_route, CourierId, RouteData}).

update_house_orders(HouseId, Orders) ->
    gen_server:cast(?SERVER, {update_house_orders, HouseId, Orders}).

log_message(Level, Category, Message) ->
    gen_server:cast(?SERVER, {log_message, Level, Category, Message}).
	
package_delivered(HouseId) ->
    gen_server:cast(?SERVER, {package_delivered, HouseId}).

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
    
    %% New ETS tables for enhanced visualization
    ets:new(courier_routes, [set, named_table, public]),      % מסלולי שליחים
    ets:new(courier_status, [set, named_table, public]),      % סטטוס חבילה
    ets:new(courier_animations, [set, named_table, public]),  % אנימציות
    
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
    
    %% Start animation timer for smooth courier movement (20 FPS) - FIXED
    timer:send_interval(50, refresh_animation),
    
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
handle_info({state_update, <<"package_update">>, Data}, State) ->
    %% Get data
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

handle_info(refresh_animation, State) ->
    %% Update all courier animations for smooth movement
    CurrentTime = erlang:monotonic_time(millisecond),
    
    AllAnimations = ets:tab2list(courier_animations),
    lists:foreach(fun({CourierId, AnimData}) ->
        From = maps:get(from, AnimData),
        To = maps:get(to, AnimData),
        StartTime = maps:get(start_time, AnimData),
        Duration = maps:get(duration, AnimData, 1000),
        
        %% Calculate animation progress
        Progress = erlang:min(1.0, (CurrentTime - StartTime) / Duration),
        
        %% Interpolate position
        {FromX, FromY} = From,
        {ToX, ToY} = To,
        CurrentX = FromX + (ToX - FromX) * Progress,
        CurrentY = FromY + (ToY - FromY) * Progress,
        
        %% Update displayed position
        case ets:lookup(courier_positions, CourierId) of
            [{_, {_, _, Data}}] ->
                ets:insert(courier_positions, {CourierId, {round(CurrentX), round(CurrentY), Data}});
            _ ->
                ok
        end,
        
        %% Remove completed animations
        if Progress >= 1.0 ->
            ets:delete(courier_animations, CourierId);
        true ->
            ok
        end
    end, AllAnimations),
    
    %% Refresh display only if there are animations or couriers
    case ets:info(courier_positions, size) of
        0 -> ok;
        _ -> wxPanel:refresh(State#state.map_panel)
    end,
    
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

        case code:ensure_loaded(ModuleAtom) of
            {error, _} ->
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
    ets:insert(map_data, {roads, []}), 
    ets:insert(map_data, {dimensions, #{width => 1200, height => 800}}),
    ets:insert(map_data, {stats, #{total_homes => 7, total_businesses => 3}}).

%%--------------------------------------------------------------------
%% Paint callback for map - FIXED VERSION
%%--------------------------------------------------------------------
on_paint_map(#wx{obj = Panel}, _) ->
    DC = wxPaintDC:new(Panel),
    
    try
        wxDC:clear(DC),
        
        %% Get panel size
        {W, H} = wxPanel:getSize(Panel),
        
        %% Draw zones
        draw_zones(DC, W, H),
        
        %% Draw roads from ETS - with check
        case ets:lookup(map_data, roads) of
            [{roads, Roads}] when is_list(Roads) ->
                try
                    draw_roads(DC, Roads)
                catch
                    _:RoadErr ->
                        io:format("Error drawing roads: ~p~n", [RoadErr])
                end;
            _ -> ok
        end,
        
        %% Draw courier routes - with try-catch
        try
            draw_courier_routes(DC)
        catch
            _:RouteError ->
                io:format("Error drawing routes: ~p~n", [RouteError])
        end,
        
        %% Draw homes from ETS
        case ets:lookup(map_data, homes) of
            [{homes, Homes}] when is_list(Homes) -> 
                try
                    draw_map_homes(DC, Homes)
                catch
                    _:HomeErr ->
                        io:format("Error drawing homes: ~p~n", [HomeErr])
                end;
            _ -> 
                io:format("No homes found in ETS~n")
        end,
        
        %% Draw businesses from ETS
        case ets:lookup(map_data, businesses) of
            [{businesses, Businesses}] when is_list(Businesses) -> 
                try
                    draw_map_businesses(DC, Businesses)
                catch
                    _:BizErr ->
                        io:format("Error drawing businesses: ~p~n", [BizErr])
                end;
            _ -> 
                io:format("No businesses found in ETS~n")
        end,
        
        %% Draw couriers - with try-catch
        try
            case ets:tab2list(courier_positions) of
                [] -> ok;
                Couriers -> draw_enhanced_couriers(DC, Couriers)
            end
        catch
            _:CourierError ->
                io:format("Error drawing couriers: ~p~n", [CourierError])
        end,
        
        %% Draw legend
        try
            draw_legend(DC, W)
        catch
            _:LegendErr ->
                io:format("Error drawing legend: ~p~n", [LegendErr])
        end
    catch
        Type:Error ->
            io:format("Paint error details: ~p:~p~n", [Type, Error])
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
        try
            X1 = maps:get(x1, Road),
            Y1 = maps:get(y1, Road),
            X2 = maps:get(x2, Road),
            Y2 = maps:get(y2, Road),
            wxDC:drawLine(DC, {X1, Y1}, {X2, Y2})
        catch
            _:_ -> ok
        end
    end, Roads).

%%--------------------------------------------------------------------
%% Draw homes from map data
%%--------------------------------------------------------------------
draw_map_homes(DC, Homes) ->
    lists:foreach(fun(Home) ->
        try
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
        catch
            _:_ -> ok
        end
    end, Homes).

%%--------------------------------------------------------------------
%% Draw businesses from map data
%%--------------------------------------------------------------------
draw_map_businesses(DC, Businesses) ->
    lists:foreach(fun(Business) ->
        try
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
        catch
            _:_ -> ok
        end
    end, Businesses).

%%--------------------------------------------------------------------
%% Draw courier routes on map - SIMPLE VERSION
%%--------------------------------------------------------------------
draw_courier_routes(DC) ->
    Routes = ets:tab2list(courier_routes),
    lists:foreach(fun({CourierId, RouteData}) ->
        try
            case RouteData of
                #{type := Type, path := Path} when is_list(Path), length(Path) > 1 ->
                    Color = case Type of
                        pickup -> {255, 140, 0};    % Orange
                        delivery -> {0, 200, 0};     % Green  
                        _ -> {100, 100, 100}         % Gray
                    end,
                    draw_full_route_path(DC, Path, Color, CourierId);
                _ ->
                    ok
            end
        catch
            _:_ -> ok
        end
    end, Routes).

%%--------------------------------------------------------------------
%% Draw route path 
%%--------------------------------------------------------------------
draw_full_route_path(DC, [StartNode | _] = Path, Color, _CourierId) ->
    EndNode = lists:last(Path),

    %% קבל קואורדינטות רק לנקודת ההתחלה והסיום
    case {get_location_coordinates_safe(StartNode), get_location_coordinates_safe(EndNode)} of
        {{ok, StartX, StartY}, {ok, EndX, EndY}} ->
            
            wxDC:setPen(DC, wxPen:new(Color, [{width, 3}, {style, ?wxSOLID}])),
            
            
            wxDC:drawLine(DC, {StartX, StartY}, {EndX, EndY});
        _ ->
            ok 
    end;
draw_full_route_path(_DC, _, _, _) ->
    
    ok.



%%--------------------------------------------------------------------
%% Safe helper to get coordinates
%%--------------------------------------------------------------------
get_location_coordinates_safe(LocationId) when is_list(LocationId) ->
    try
        case LocationId of
            "home_" ++ NumStr ->
                Num = list_to_integer(NumStr),
                case ets:lookup(map_data, homes) of
                    [{homes, Homes}] ->
                        case lists:filter(fun(H) -> 
                            maps:get(id, H) == Num 
                        end, Homes) of
                            [Home|_] ->
                                {ok, maps:get(x, Home), maps:get(y, Home)};
                            _ ->
                                error
                        end;
                    _ ->
                        error
                end;
            "business_" ++ Zone ->
                case ets:lookup(map_data, businesses) of
                    [{businesses, Businesses}] ->
                        ZoneAtom = list_to_atom(Zone),
                        case lists:filter(fun(B) ->
                            maps:get(zone, B) == ZoneAtom
                        end, Businesses) of
                            [Business|_] ->
                                {ok, maps:get(x, Business), maps:get(y, Business)};
                            _ ->
                                error
                        end;
                    _ ->
                        error
                end;
            "junction_" ++ _ ->
                %% For real junctions, get from map_server
                case map_server:get_location(LocationId) of
                    {ok, Location} ->
                        {ok, Location#location.x, Location#location.y};
                    _ -> error
                end;
            _ ->
                error
        end
    catch
        _:_ -> error
    end;
get_location_coordinates_safe(_) -> error.

%%--------------------------------------------------------------------
%% Draw enhanced couriers with status indication
%%--------------------------------------------------------------------
draw_enhanced_couriers(DC, CourierList) ->
    lists:foreach(fun(CourierData) ->
        try
            {CourierId, PositionInfo} = case CourierData of
                {Id, Info} -> {Id, Info};
                _ -> throw(invalid_courier_data)
            end,
            
            %% Extract position safely
            {X, Y} = case PositionInfo of
                {PX, PY, _Data} -> {PX, PY};
                {PX, PY} -> {PX, PY};
                #{position := #{x := PX, y := PY}} -> {PX, PY};
                _ -> {500, 400}  % Default position
            end,
            
            %% Get courier status (has_package/no_package)
            Status = case ets:lookup(courier_status, CourierId) of
                [{_, S}] -> S;
                _ -> no_package
            end,
            
            %% Draw courier with status
            draw_courier_with_status(DC, CourierId, X, Y, Status)
        catch
            _:Err ->
                io:format("Error drawing courier ~p: ~p~n", [CourierData, Err])
        end
    end, CourierList).

draw_courier_with_status(DC, CourierId, X, Y, Status) ->
    try
        %% Base vehicle color
        BaseColor = case Status of
            has_package -> {0, 100, 200};  % Blue when carrying
            no_package -> {0, 150, 0};     % Green when empty
            _ -> {100, 100, 100}           % Gray otherwise
        end,
        
        %% Draw vehicle body
        wxDC:setBrush(DC, wxBrush:new(BaseColor)),
        wxDC:setPen(DC, wxPen:new({0, 0, 0}, [{width, 2}])),
        wxDC:drawRoundedRectangle(DC, {X - 15, Y - 10, 30, 20}, 5),
        
        %% Draw package indicator if carrying
        case Status of
            has_package ->
                %% Draw package box on top of vehicle
                wxDC:setBrush(DC, wxBrush:new({139, 69, 19})),  % Brown
                wxDC:setPen(DC, wxPen:new({101, 67, 33}, [{width, 1}])),
                wxDC:drawRectangle(DC, {X - 8, Y - 18, 16, 8}),
                
                %% Add blinking indicator
                Time = erlang:monotonic_time(millisecond),
                if (Time rem 1000) < 500 ->
                    %% Draw red dot when blinking on
                    wxDC:setBrush(DC, wxBrush:new({255, 0, 0})),
                    wxDC:setPen(DC, wxPen:new({200, 0, 0}, [{width, 1}])),
                    wxDC:drawCircle(DC, {X + 12, Y - 5}, 3);
                true ->
                    ok
                end;
            no_package ->
                %% Draw empty indicator
                wxDC:setBrush(DC, wxBrush:new({255, 255, 255})),
                wxDC:setPen(DC, wxPen:new({0, 150, 0}, [{width, 1}])),
                wxDC:drawCircle(DC, {X, Y - 15}, 4);
            _ ->
                ok
        end,
        
        %% Draw courier ID
        Font = wxFont:new(8, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
        wxDC:setFont(DC, Font),
        wxDC:setTextForeground(DC, {255, 255, 255}),
        
        %% Shorten ID for display
        IDStr = case CourierId of
            Atom when is_atom(Atom) -> atom_to_list(Atom);
            List when is_list(List) -> List;
            _ -> "?"
        end,
        ShortID = case string:tokens(IDStr, "_") of
            [_Zone, "courier", Num] -> "C" ++ Num;
            _ -> 
                case length(IDStr) > 6 of
                    true -> string:sub_string(IDStr, 1, 6);
                    false -> IDStr
                end
        end,
        
        wxDC:drawText(DC, ShortID, {X - 10, Y - 3})
    catch
        _:_ -> ok
    end.

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
    wxDC:drawRectangle(DC, {Width - 150, 30, 140, 250}),
    
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
    
    %% Courier Empty
    wxDC:setBrush(DC, wxBrush:new({0, 150, 0})),
    wxDC:drawRectangle(DC, {Width - 140, 100, 12, 8}),
    wxDC:drawText(DC, "Courier (empty)", {Width - 125, 98}),
    
    %% Courier with Package
    wxDC:setBrush(DC, wxBrush:new({0, 100, 200})),
    wxDC:drawRectangle(DC, {Width - 140, 120, 12, 8}),
    wxDC:setBrush(DC, wxBrush:new({139, 69, 19})),
    wxDC:drawRectangle(DC, {Width - 138, 116, 8, 4}),
    wxDC:drawText(DC, "Courier (package)", {Width - 125, 118}),
    
    %% Orders
    wxDC:setBrush(DC, wxBrush:new({255, 0, 0})),
    wxDC:drawCircle(DC, {Width - 135, 145}, 5),
    wxDC:drawText(DC, "Orders", {Width - 125, 140}),
    
    %% Roads
    wxDC:setPen(DC, wxPen:new({140, 140, 140}, [{width, 2}])),
    wxDC:drawLine(DC, {Width - 140, 165}, {Width - 100, 165}),
    wxDC:setTextForeground(DC, {0, 0, 0}),
    wxDC:drawText(DC, "Road", {Width - 125, 160}),
    
    %% Pickup Route
    wxDC:setPen(DC, wxPen:new({255, 140, 0}, [{width, 3}, {style, ?wxDOT}])),
    wxDC:drawLine(DC, {Width - 140, 185}, {Width - 100, 185}),
    wxDC:setTextForeground(DC, {0, 0, 0}),
    wxDC:drawText(DC, "Pickup Route", {Width - 125, 180}),
    
    %% Delivery Route
    wxDC:setPen(DC, wxPen:new({0, 200, 0}, [{width, 3}, {style, ?wxDOT}])),
    wxDC:drawLine(DC, {Width - 140, 205}, {Width - 100, 205}),
    wxDC:setTextForeground(DC, {0, 0, 0}),
    wxDC:drawText(DC, "Delivery Route", {Width - 125, 200}).

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
    ets:delete_all_objects(courier_routes),
    ets:delete_all_objects(courier_status),
    wxPanel:refresh(State#state.map_panel),
    update_status_bar(State#state.frame, "System: STOPPED", 0),
    log_message(info, "System", "Visualization stopped"),
    {noreply, State};

handle_cast({update_courier_position, CourierId, PositionData}, State) ->
    %% Extract position from the data
    Position = maps:get(position, PositionData, #{x => 0, y => 0}),
    X = maps:get(x, Position, 0),
    Y = maps:get(y, Position, 0),
    
    %% Store the position with animation data
    CurrentTime = erlang:monotonic_time(millisecond),
    
    %% Check if we need to create smooth animation
    case ets:lookup(courier_positions, CourierId) of
        [{_, {OldX, OldY, _}}] ->
            %% Create animation from old to new position
            ets:insert(courier_animations, {CourierId, #{
                from => {OldX, OldY},
                to => {X, Y},
                start_time => CurrentTime,
                duration => 1000  % 1 second animation
            }});
        _ ->
            ok
    end,
    
    ets:insert(courier_positions, {CourierId, {X, Y, PositionData}}),
    
    %% Log the update
    Progress = maps:get(progress, PositionData, 0),
    Speed = maps:get(speed, PositionData, 0),
    LogMsg = io_lib:format("Courier ~s: Position (~p,~p), Progress: ~.1f%, Speed: ~p km/h", 
                           [CourierId, X, Y, Progress * 100, Speed]),
    add_log_entry(State#state.log_panel, info, "Movement", LogMsg),
    
    wxPanel:refresh(State#state.map_panel),
    {noreply, State};

handle_cast({set_courier_route, CourierId, RouteData}, State) ->
    %% Store the route for visualization
    ets:insert(courier_routes, {CourierId, RouteData}),
    
    %% Log route information
    Type = maps:get(type, RouteData, unknown),
    Path = maps:get(path, RouteData, []),
    Distance = maps:get(total_distance, RouteData, 0),
    EstTime = maps:get(estimated_time, RouteData, 0),
    
    TypeStr = case Type of
        pickup -> "Pickup";
        delivery -> "Delivery";
        _ -> "Unknown"
    end,
    
    LogMsg = io_lib:format("Courier ~s: New ~s route with ~p waypoints, ~pm distance, ETA: ~ps", 
                           [CourierId, TypeStr, length(Path), Distance, EstTime]),
    add_log_entry(State#state.log_panel, success, "Route", LogMsg),
    
    wxPanel:refresh(State#state.map_panel),
    {noreply, State};

handle_cast({update_courier_status, CourierId, Status}, State) ->
    %% Store courier package status
    ets:insert(courier_status, {CourierId, Status}),
    
    %% Log status change
    StatusStr = case Status of
        has_package -> "Carrying package";
        no_package -> "Empty";
        _ -> "Unknown"
    end,
    
    LogMsg = io_lib:format("Courier ~s: Status changed to ~s", [CourierId, StatusStr]),
    add_log_entry(State#state.log_panel, info, "Status", LogMsg),
    
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
	
handle_cast({package_delivered, HouseId}, State) ->
    HouseKey = list_to_atom("house_" ++ HouseId),
    case ets:lookup(house_orders, HouseKey) of
        [{_, Count}] when Count > 0 ->
            ets:insert(house_orders, {HouseKey, Count - 1});
        _ -> ok
    end,
    wxPanel:refresh(State#state.map_panel),
    {noreply, State};	

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

