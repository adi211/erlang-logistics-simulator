%%%-------------------------------------------------------------------
%%% @doc map_visual - Map with 100 fixed houses from JSON
%%% Shows all houses physically on the map with order counts
%%%-------------------------------------------------------------------
-module(map_visual).

-include_lib("wx/include/wx.hrl").

-export([start/0, start_link/0]).
-export([update_courier/2, add_order/3, update_house_orders/2]).
-export([test/0]).

%% Record for house info
-record(house, {
    id,
    x,
    y,
    zone,
    pending_orders = 0
}).

%% Start function
start() ->
    Pid = spawn(fun() -> init() end),
    timer:sleep(100),
    {ok, Pid}.

start_link() ->
    Pid = spawn_link(fun() -> init() end),
    timer:sleep(100),
    {ok, Pid}.

%% Test function
test() ->
    %% Add some couriers
    update_courier(courier_n1, {600, 133}),
    update_courier(courier_c1, {600, 400}),
    update_courier(courier_s1, {600, 666}),
    
    %% Update some house orders
    update_house_orders(house_1, 3),
    update_house_orders(house_5, 2),
    update_house_orders(house_10, 1),
    update_house_orders(house_50, 4),
    
    %% Add delivery route
    add_order(order_1, {600, 133}, {92, 61}),
    ok.

%% Update courier position
update_courier(CourierID, Position) ->
    case whereis(map_visual_server) of
        undefined -> {error, not_running};
        Pid -> 
            Pid ! {update_courier, CourierID, Position},
            ok
    end.

%% Add order route
add_order(OrderID, FromPos, ToPos) ->
    case whereis(map_visual_server) of
        undefined -> {error, not_running};
        Pid -> 
            Pid ! {add_order, OrderID, FromPos, ToPos},
            ok
    end.

%% Update pending orders for a house
update_house_orders(HouseID, Count) ->
    case whereis(map_visual_server) of
        undefined -> {error, not_running};
        Pid -> 
            Pid ! {update_house_orders, HouseID, Count},
            ok
    end.

%% Parse JSON houses
parse_houses_from_json() ->
    %% The 100 houses from your JSON
    Houses = [
        %% North Zone houses
        {1, 92, 61, north}, {2, 184, 61, north}, {3, 276, 61, north},
        {4, 369, 61, north}, {5, 461, 61, north}, {6, 553, 61, north},
        {7, 646, 61, north}, {8, 738, 61, north}, {9, 830, 61, north},
        {10, 923, 61, north}, {11, 1015, 61, north}, {12, 1107, 61, north},
        {13, 92, 123, north}, {14, 184, 123, north}, {15, 276, 123, north},
        {16, 369, 123, north}, {17, 461, 123, north}, {18, 646, 123, north},
        {19, 738, 123, north}, {20, 830, 123, north}, {21, 923, 123, north},
        {22, 1015, 123, north}, {23, 1107, 123, north},
        {24, 92, 184, north}, {25, 184, 184, north}, {26, 276, 184, north},
        {27, 369, 184, north}, {28, 461, 184, north}, {29, 646, 184, north},
        {30, 738, 184, north}, {31, 830, 184, north}, {32, 923, 184, north},
        {33, 1015, 184, north}, {34, 1107, 184, north},
        
        %% Center Zone houses
        {35, 92, 307, center}, {36, 184, 307, center}, {37, 276, 307, center},
        {38, 369, 307, center}, {39, 461, 307, center}, {40, 553, 307, center},
        {41, 646, 307, center}, {42, 738, 307, center}, {43, 830, 307, center},
        {44, 923, 307, center}, {45, 1015, 307, center}, {46, 1107, 307, center},
        {47, 92, 369, center}, {48, 184, 369, center}, {49, 276, 369, center},
        {50, 369, 369, center}, {51, 461, 369, center}, {52, 553, 369, center},
        {53, 646, 369, center}, {54, 738, 369, center}, {55, 830, 369, center},
        {56, 923, 369, center}, {57, 1015, 369, center}, {58, 1107, 369, center},
        {59, 92, 430, center}, {60, 184, 430, center}, {61, 276, 430, center},
        {62, 1107, 430, center}, {63, 1015, 430, center}, {64, 923, 430, center},
        {65, 415, 430, center}, {66, 692, 430, center}, {67, 553, 492, center},
        
        %% South Zone houses
        {68, 92, 553, south}, {69, 184, 553, south}, {70, 276, 553, south},
        {71, 369, 553, south}, {72, 461, 553, south}, {73, 553, 553, south},
        {74, 646, 553, south}, {75, 738, 553, south}, {76, 830, 553, south},
        {77, 923, 553, south}, {78, 1015, 553, south}, {79, 1107, 553, south},
        {80, 1015, 615, south}, {81, 92, 615, south}, {82, 184, 615, south},
        {83, 369, 615, south}, {84, 461, 615, south}, {85, 646, 615, south},
        {86, 738, 615, south}, {87, 276, 676, south}, {88, 830, 676, south},
        {89, 92, 738, south}, {90, 184, 738, south}, {91, 276, 738, south},
        {92, 369, 738, south}, {93, 461, 738, south}, {94, 553, 738, south},
        {95, 646, 738, south}, {96, 738, 738, south}, {97, 830, 738, south},
        {98, 923, 738, south}, {99, 1015, 738, south}, {100, 1107, 738, south}
    ],
    
    %% Convert to house records
    lists:map(fun({ID, X, Y, Zone}) ->
        #house{
            id = list_to_atom("house_" ++ integer_to_list(ID)),
            x = X,
            y = Y,
            zone = Zone,
            pending_orders = 0
        }
    end, Houses).

init() ->
    %% Load all 100 houses
    Houses = parse_houses_from_json(),
    HousesMap = lists:foldl(fun(H, Acc) ->
        maps:put(H#house.id, H, Acc)
    end, #{}, Houses),
    
    %% Initialize wx
    Wx = wx:new(),
    
    %% Create frame (1200x800 based on JSON)
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Logistics Map - 100 Houses", 
                       [{size, {1250, 850}}]),
    
    %% Create panel
    Panel = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(Panel, {240, 240, 240}),
    
    %% Show frame
    wxFrame:show(Frame),
    
    %% Register
    register(map_visual_server, self()),
    
    io:format("Map Visual: Started with 100 houses~n"),
    
    %% Initial state
    InitialState = #{
        houses => HousesMap,
        couriers => #{},
        orders => [],
        businesses => [
            {north_dc, 553, 123},
            {center_dc, 553, 430},
            {south_dc, 553, 676}
        ]
    },
    
    %% Connect paint event
    connect_paint(Panel, InitialState),
    
    %% Force initial paint
    wxPanel:refresh(Panel),
    
    %% Refresh timer
    timer:send_interval(100, self(), timer_refresh),
    
    %% Event loop
    loop(Panel, Frame, InitialState).

connect_paint(Panel, State) ->
    wxPanel:disconnect(Panel, paint),
    Callback = fun(#wx{obj = P, event = #wxPaint{}}, _) ->
        draw_full_map(P, State)
    end,
    wxPanel:connect(Panel, paint, [{callback, Callback}]).

loop(Panel, Frame, State) ->
    receive
        {update_courier, CourierID, Position} ->
            Couriers = maps:get(couriers, State, #{}),
            NewCouriers = maps:put(CourierID, Position, Couriers),
            NewState = State#{couriers := NewCouriers},
            connect_paint(Panel, NewState),
            wxPanel:refresh(Panel),
            loop(Panel, Frame, NewState);
            
        {add_order, OrderID, FromPos, ToPos} ->
            Orders = maps:get(orders, State, []),
            NewOrders = [{OrderID, FromPos, ToPos} | Orders],
            NewState = State#{orders := NewOrders},
            connect_paint(Panel, NewState),
            wxPanel:refresh(Panel),
            loop(Panel, Frame, NewState);
            
        {update_house_orders, HouseID, Count} ->
            Houses = maps:get(houses, State, #{}),
            case maps:find(HouseID, Houses) of
                {ok, House} ->
                    UpdatedHouse = House#house{pending_orders = Count},
                    NewHouses = maps:put(HouseID, UpdatedHouse, Houses),
                    NewState = State#{houses := NewHouses},
                    connect_paint(Panel, NewState),
                    wxPanel:refresh(Panel),
                    loop(Panel, Frame, NewState);
                error ->
                    loop(Panel, Frame, State)
            end;
            
        timer_refresh ->
            wxPanel:refresh(Panel),
            loop(Panel, Frame, State);
            
        stop ->
            wxFrame:destroy(Frame),
            ok;
            
        _ ->
            loop(Panel, Frame, State)
    end.

%% Main drawing function
draw_full_map(Panel, State) ->
    DC = wxPaintDC:new(Panel),
    
    try
        wxDC:clear(DC),
        
        %% Draw layers
        draw_zones(DC),
        draw_roads(DC),
        draw_all_houses(DC, maps:get(houses, State, #{})),
        draw_businesses(DC, maps:get(businesses, State, [])),
        draw_couriers(DC, maps:get(couriers, State, #{})),
        draw_delivery_routes(DC, maps:get(orders, State, [])),
        draw_statistics(DC),
        draw_legend(DC)
    catch
        _:Error ->
            io:format("Drawing error: ~p~n", [Error])
    end,
    
    wxPaintDC:destroy(DC),
    ok.

%% Draw zones
draw_zones(DC) ->
    %% North Zone (0-266)
    wxDC:setBrush(DC, wxBrush:new({220, 230, 250})),
    wxDC:setPen(DC, wxPen:new({150, 150, 150}, [{width, 2}])),
    wxDC:drawRectangle(DC, {0, 0, 1200, 266}),
    
    %% Center Zone (267-533)
    wxDC:setBrush(DC, wxBrush:new({220, 250, 220})),
    wxDC:drawRectangle(DC, {0, 267, 1200, 266}),
    
    %% South Zone (534-800)
    wxDC:setBrush(DC, wxBrush:new({250, 220, 220})),
    wxDC:drawRectangle(DC, {0, 534, 1200, 266}),
    
    %% Zone labels
    Font = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    wxDC:setFont(DC, Font),
    
    wxDC:setTextForeground(DC, {50, 50, 150}),
    wxDC:drawText(DC, "NORTH ZONE", {20, 10}),
    
    wxDC:setTextForeground(DC, {50, 150, 50}),
    wxDC:drawText(DC, "CENTER ZONE", {20, 277}),
    
    wxDC:setTextForeground(DC, {150, 50, 50}),
    wxDC:drawText(DC, "SOUTH ZONE", {20, 544}).

%% Draw road grid
draw_roads(DC) ->
    wxDC:setPen(DC, wxPen:new({180, 180, 180}, [{width, 1}])),
    
    %% Main horizontal roads from JSON
    lists:foreach(fun(Y) ->
        wxDC:drawLine(DC, {46, Y}, {1153, Y})
    end, [30, 92, 153, 215, 276, 338, 400, 461, 523, 584, 646, 707, 769]),
    
    %% Main vertical roads from JSON
    lists:foreach(fun(X) ->
        wxDC:drawLine(DC, {X, 30}, {X, 769})
    end, [46, 230, 323, 507, 600, 784, 876, 1153]).

%% Draw all 100 houses
draw_all_houses(DC, HousesMap) ->
    maps:fold(fun(_ID, House, _) ->
        X = House#house.x,
        Y = House#house.y,
        
        %% House color based on zone
        Color = case House#house.zone of
            north -> {100, 100, 200};
            center -> {100, 200, 100};
            south -> {200, 100, 100}
        end,
        
        %% Draw house
        wxDC:setBrush(DC, wxBrush:new(Color)),
        wxDC:setPen(DC, wxPen:new({50, 50, 50}, [{width, 1}])),
        wxDC:drawRectangle(DC, {X - 8, Y - 8, 16, 16}),
        
        %% Draw roof
        wxDC:setBrush(DC, wxBrush:new({139, 69, 19})),
        Points = [{X - 10, Y - 8}, {X, Y - 15}, {X + 10, Y - 8}],
        wxDC:drawPolygon(DC, Points),
        
        %% Draw pending orders count if > 0
        case House#house.pending_orders of
            0 -> ok;
            Count ->
                %% Draw red circle with number
                wxDC:setBrush(DC, wxBrush:new({255, 0, 0})),
                wxDC:setPen(DC, wxPen:new({200, 0, 0}, [{width, 2}])),
                wxDC:drawCircle(DC, {X + 12, Y - 12}, 10),
                
                Font = wxFont:new(9, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
                wxDC:setFont(DC, Font),
                wxDC:setTextForeground(DC, {255, 255, 255}),
                wxDC:drawText(DC, integer_to_list(Count), {X + 8, Y - 18})
        end,
        ok
    end, ok, HousesMap).

%% Draw businesses/DCs
draw_businesses(DC, Businesses) ->
    wxDC:setBrush(DC, wxBrush:new({255, 215, 0})),
    wxDC:setPen(DC, wxPen:new({0, 0, 0}, [{width, 3}])),
    
    lists:foreach(fun({Name, X, Y}) ->
        %% Large square for DC
        wxDC:drawRectangle(DC, {X - 30, Y - 30, 60, 60}),
        
        %% Label
        Font = wxFont:new(12, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
        wxDC:setFont(DC, Font),
        wxDC:setTextForeground(DC, {0, 0, 0}),
        
        Label = case Name of
            north_dc -> "DC-N";
            center_dc -> "DC-C";
            south_dc -> "DC-S";
            _ -> "DC"
        end,
        wxDC:drawText(DC, Label, {X - 18, Y - 5})
    end, Businesses).

%% Draw couriers
draw_couriers(DC, Couriers) ->
    maps:fold(fun(ID, {X, Y}, _) ->
        %% Courier as vehicle
        wxDC:setBrush(DC, wxBrush:new({0, 150, 0})),
        wxDC:setPen(DC, wxPen:new({0, 0, 0}, [{width, 2}])),
        wxDC:drawRoundedRectangle(DC, {X - 15, Y - 10, 30, 20}, 3),
        
        %% ID
        Font = wxFont:new(8, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
        wxDC:setFont(DC, Font),
        wxDC:setTextForeground(DC, {255, 255, 255}),
        
        IDStr = atom_to_list(ID),
        ShortID = lists:sublist(IDStr, 1, 6),
        wxDC:drawText(DC, ShortID, {X - 12, Y - 3}),
        ok
    end, ok, Couriers).

%% Draw delivery routes
draw_delivery_routes(DC, Orders) ->
    wxDC:setPen(DC, wxPen:new({255, 140, 0}, [{width, 3}, {style, ?wxDOT}])),
    
    lists:foreach(fun({_ID, {X1, Y1}, {X2, Y2}}) ->
        wxDC:drawLine(DC, {X1, Y1}, {X2, Y2})
    end, Orders).

%% Draw statistics
draw_statistics(DC) ->
    wxDC:setBrush(DC, wxBrush:new({50, 50, 50})),
    wxDC:setPen(DC, wxPen:new({50, 50, 50})),
    wxDC:drawRectangle(DC, {0, 800, 1200, 30}),
    
    Font = wxFont:new(11, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
    wxDC:setFont(DC, Font),
    wxDC:setTextForeground(DC, {255, 255, 255}),
    
    wxDC:drawText(DC, "Total Houses: 100 | North: 34 | Center: 33 | South: 33", {20, 807}).

%% Draw legend
draw_legend(DC) ->
    wxDC:setBrush(DC, wxBrush:new({255, 255, 255, 240})),
    wxDC:setPen(DC, wxPen:new({100, 100, 100})),
    wxDC:drawRectangle(DC, {1050, 30, 140, 180}),
    
    Font = wxFont:new(9, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL),
    wxDC:setFont(DC, Font),
    wxDC:setTextForeground(DC, {0, 0, 0}),
    
    wxDC:drawText(DC, "Legend:", {1060, 40}),
    
    %% House
    wxDC:setBrush(DC, wxBrush:new({100, 100, 200})),
    wxDC:drawRectangle(DC, {1060, 60, 10, 10}),
    wxDC:drawText(DC, "House", {1075, 60}),
    
    %% House with orders
    wxDC:setBrush(DC, wxBrush:new({255, 0, 0})),
    wxDC:drawCircle(DC, {1065, 85}, 5),
    wxDC:drawText(DC, "Pending Orders", {1075, 80}),
    
    %% DC
    wxDC:setBrush(DC, wxBrush:new({255, 215, 0})),
    wxDC:drawRectangle(DC, {1060, 100, 12, 12}),
    wxDC:drawText(DC, "Distribution", {1075, 100}),
    wxDC:drawText(DC, "Center", {1075, 112}),
    
    %% Courier
    wxDC:setBrush(DC, wxBrush:new({0, 150, 0})),
    wxDC:drawRectangle(DC, {1060, 130, 12, 8}),
    wxDC:drawText(DC, "Courier", {1075, 128}),
    
    %% Route
    wxDC:setPen(DC, wxPen:new({255, 140, 0}, [{width, 2}, {style, ?wxDOT}])),
    wxDC:drawLine(DC, {1060, 150}, {1072, 150}),
    wxDC:drawText(DC, "Delivery", {1075, 145}),
    wxDC:drawText(DC, "Route", {1075, 157}).