%%%-------------------------------------------------------------------
%%% @author Adi Shlomo, Dolev Ishay
%%% @doc Common header file for logistics system
%%%-------------------------------------------------------------------

%% Common records used across the system
-record(zone_info, {
    node_name,
    status,      % live/down
    couriers,
    deliveries,
    zone_id      % north/center/south
}).

-record(courier_info, {
    id,
    zone,
    status,      % idle/picking_up/delivering/returning
    location,
    current_package
}).

-record(package_info, {
    id,
    origin,
    destination,
    status,      % ordered/assigned/in_transit/delivered/failed
    courier_id,
    timestamp
}).

%% System states
-define(STOPPED, stopped).
-define(RUNNING, running).
-define(PAUSED, paused).
-define(NO_NEW_ORDERS, no_new_orders).

%% Default configurations
-define(DEFAULT_COURIER_COUNT, 5).
-define(DEFAULT_HOUSEHOLD_COUNT, 10).
-define(UPDATE_INTERVAL, 1000).  % milliseconds