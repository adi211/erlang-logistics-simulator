%%%-------------------------------------------------------------------
%%% @doc Network Constants - Example for real network deployment
%%%-------------------------------------------------------------------

%% ====================================================================
%%  Local IP
%% ====================================================================
-define(VIZ_IP,    "127.0.0.1").
-define(CTRL_IP,   "127.0.0.1").
-define(NORTH_IP,  "127.0.0.1").
-define(CENTER_IP, "127.0.0.1").
-define(SOUTH_IP,  "127.0.0.1").

%% ====================================================================
%% Real network IP
%% ====================================================================
 %%-define(VIZ_IP,    "132.72.52.84").   % Computer 1
 %%-define(CTRL_IP,   "132.72.54.72").   % Computer 2
 %%-define(NORTH_IP,  "132.72.81.226").   % Computer 3
 %%-define(CENTER_IP, "132.72.80.235").   % Computer 4
 %%-define(SOUTH_IP,  "132.72.81.94").   % Computer 5

%% ====================================================================
%% DO NOT EDIT BELOW
%% ====================================================================

-define(VIZ_NODE,    list_to_atom("visualization@" ++ ?VIZ_IP)).
-define(CTRL_NODE,   list_to_atom("control@" ++ ?CTRL_IP)).
-define(NORTH_NODE,  list_to_atom("zone_north@" ++ ?NORTH_IP)).
-define(CENTER_NODE, list_to_atom("zone_center@" ++ ?CENTER_IP)).
-define(SOUTH_NODE,  list_to_atom("zone_south@" ++ ?SOUTH_IP)).