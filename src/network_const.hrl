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
 %%-define(VIZ_IP,    "192.168.1.100").   % Computer 1
 %%-define(CTRL_IP,   "192.168.1.101").   % Computer 2
 %%-define(NORTH_IP,  "192.168.1.102").   % Computer 3
 %%-define(CENTER_IP, "192.168.1.103").   % Computer 4
 %%-define(SOUTH_IP,  "192.168.1.104").   % Computer 5

%% ====================================================================
%% DO NOT EDIT BELOW
%% ====================================================================

-define(VIZ_NODE,    list_to_atom("visualization@" ++ ?VIZ_IP)).
-define(CTRL_NODE,   list_to_atom("control@" ++ ?CTRL_IP)).
-define(NORTH_NODE,  list_to_atom("zone_north@" ++ ?NORTH_IP)).
-define(CENTER_NODE, list_to_atom("zone_center@" ++ ?CENTER_IP)).
-define(SOUTH_NODE,  list_to_atom("zone_south@" ++ ?SOUTH_IP)).