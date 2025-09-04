%%%-------------------------------------------------------------------
%%% @doc Map data for 100 homes configuration
%%%-------------------------------------------------------------------
-module(map_data_100).

-export([get_map/0]).

get_map() ->
    #{
        dimensions => #{width => 1200, height => 800},
        homes => get_homes(),
        businesses => get_businesses(),
        stats => #{
            total_homes => 100,
            total_businesses => 3,
            homes_by_zone => #{
                north => 34,
                center => 33,
                south => 33
            }
        }
    }.

get_homes() ->
    [
        %% North Zone - Row 1
        #{id => 1, x => 92, y => 61, zone => north},
        #{id => 2, x => 184, y => 61, zone => north},
        #{id => 3, x => 276, y => 61, zone => north},
        #{id => 4, x => 369, y => 61, zone => north},
        #{id => 5, x => 461, y => 61, zone => north},
        #{id => 6, x => 553, y => 61, zone => north},
        #{id => 7, x => 646, y => 61, zone => north},
        #{id => 8, x => 738, y => 61, zone => north},
        #{id => 9, x => 830, y => 61, zone => north},
        #{id => 10, x => 923, y => 61, zone => north},
        #{id => 11, x => 1015, y => 61, zone => north},
        #{id => 12, x => 1107, y => 61, zone => north},
        
        %% North Zone - Row 2
        #{id => 13, x => 92, y => 123, zone => north},
        #{id => 14, x => 184, y => 123, zone => north},
        #{id => 15, x => 276, y => 123, zone => north},
        #{id => 16, x => 369, y => 123, zone => north},
        #{id => 17, x => 461, y => 123, zone => north},
        #{id => 18, x => 646, y => 123, zone => north},
        #{id => 19, x => 738, y => 123, zone => north},
        #{id => 20, x => 830, y => 123, zone => north},
        #{id => 21, x => 923, y => 123, zone => north},
        #{id => 22, x => 1015, y => 123, zone => north},
        #{id => 23, x => 1107, y => 123, zone => north},
        
        %% North Zone - Row 3
        #{id => 24, x => 92, y => 184, zone => north},
        #{id => 25, x => 184, y => 184, zone => north},
        #{id => 26, x => 276, y => 184, zone => north},
        #{id => 27, x => 369, y => 184, zone => north},
        #{id => 28, x => 461, y => 184, zone => north},
        #{id => 29, x => 646, y => 184, zone => north},
        #{id => 30, x => 738, y => 184, zone => north},
        #{id => 31, x => 830, y => 184, zone => north},
        #{id => 32, x => 923, y => 184, zone => north},
        #{id => 33, x => 1015, y => 184, zone => north},
        #{id => 34, x => 1107, y => 184, zone => north},
        
        %% Center Zone - Row 1
        #{id => 35, x => 92, y => 307, zone => center},
        #{id => 36, x => 184, y => 307, zone => center},
        #{id => 37, x => 276, y => 307, zone => center},
        #{id => 38, x => 369, y => 307, zone => center},
        #{id => 39, x => 461, y => 307, zone => center},
        #{id => 40, x => 553, y => 307, zone => center},
        #{id => 41, x => 646, y => 307, zone => center},
        #{id => 42, x => 738, y => 307, zone => center},
        #{id => 43, x => 830, y => 307, zone => center},
        #{id => 44, x => 923, y => 307, zone => center},
        #{id => 45, x => 1015, y => 307, zone => center},
        #{id => 46, x => 1107, y => 307, zone => center},
        
        %% Center Zone - Row 2
        #{id => 47, x => 92, y => 369, zone => center},
        #{id => 48, x => 184, y => 369, zone => center},
        #{id => 49, x => 276, y => 369, zone => center},
        #{id => 50, x => 369, y => 369, zone => center},
        #{id => 51, x => 461, y => 369, zone => center},
        #{id => 52, x => 553, y => 369, zone => center},
        #{id => 53, x => 646, y => 369, zone => center},
        #{id => 54, x => 738, y => 369, zone => center},
        #{id => 55, x => 830, y => 369, zone => center},
        #{id => 56, x => 923, y => 369, zone => center},
        #{id => 57, x => 1015, y => 369, zone => center},
        #{id => 58, x => 1107, y => 369, zone => center},
        
        %% Center Zone - Row 3
        #{id => 59, x => 92, y => 430, zone => center},
        #{id => 60, x => 184, y => 430, zone => center},
        #{id => 61, x => 276, y => 430, zone => center},
        #{id => 62, x => 415, y => 430, zone => center},
        #{id => 63, x => 553, y => 492, zone => center},
        #{id => 64, x => 692, y => 430, zone => center},
        #{id => 65, x => 923, y => 430, zone => center},
        #{id => 66, x => 1015, y => 430, zone => center},
        #{id => 67, x => 1107, y => 430, zone => center},
        
        %% South Zone - Row 1
        #{id => 68, x => 92, y => 553, zone => south},
        #{id => 69, x => 184, y => 553, zone => south},
        #{id => 70, x => 276, y => 553, zone => south},
        #{id => 71, x => 369, y => 553, zone => south},
        #{id => 72, x => 461, y => 553, zone => south},
        #{id => 73, x => 553, y => 553, zone => south},
        #{id => 74, x => 646, y => 553, zone => south},
        #{id => 75, x => 738, y => 553, zone => south},
        #{id => 76, x => 830, y => 553, zone => south},
        #{id => 77, x => 923, y => 553, zone => south},
        #{id => 78, x => 1015, y => 553, zone => south},
        #{id => 79, x => 1107, y => 553, zone => south},
        
        %% South Zone - Row 2
        #{id => 80, x => 92, y => 615, zone => south},
        #{id => 81, x => 184, y => 615, zone => south},
        #{id => 82, x => 369, y => 615, zone => south},
        #{id => 83, x => 461, y => 615, zone => south},
        #{id => 84, x => 646, y => 615, zone => south},
        #{id => 85, x => 738, y => 615, zone => south},
        #{id => 86, x => 1015, y => 615, zone => south},
        
        %% South Zone - Row 3
        #{id => 87, x => 276, y => 676, zone => south},
        #{id => 88, x => 830, y => 676, zone => south},
        
        %% South Zone - Row 4
        #{id => 89, x => 92, y => 738, zone => south},
        #{id => 90, x => 184, y => 738, zone => south},
        #{id => 91, x => 276, y => 738, zone => south},
        #{id => 92, x => 369, y => 738, zone => south},
        #{id => 93, x => 461, y => 738, zone => south},
        #{id => 94, x => 553, y => 738, zone => south},
        #{id => 95, x => 646, y => 738, zone => south},
        #{id => 96, x => 738, y => 738, zone => south},
        #{id => 97, x => 830, y => 738, zone => south},
        #{id => 98, x => 923, y => 738, zone => south},
        #{id => 99, x => 1015, y => 738, zone => south},
        #{id => 100, x => 1107, y => 738, zone => south}
    ].

get_businesses() ->
    [
        #{id => dc_north, x => 553, y => 123, zone => north, type => distribution_center},
        #{id => dc_center, x => 553, y => 430, zone => center, type => distribution_center},
        #{id => dc_south, x => 553, y => 676, zone => south, type => distribution_center}
    ].