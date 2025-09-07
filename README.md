delivery_simulator
=====
Using Erlang/OTP, this project implements a distributed and fault-tolerant logistics management system, simulating a 24/7 delivery company with real-time control, live visualization, and dynamic parameter adjustment. The simulator operates across multiple maps (100, 200, 1,000 houses), each divided into three zones; each zone contains a fixed set of businesses providing deliveries to nearby houses and is supervised by a dedicated regional management process. The architecture showcases core features of distributed systems, supervision trees, process distribution, automatic failover, and scalability, while implementing optimal routing, load balancing, and resource management to evaluate system performance under varying loads and failure scenarios.

An OTP application

Build
-----

$ rebar3 compile

Visualization node: 
$ rebar3 shell --name visualization@127.0.0.1 --setcookie logistics
$ visualization_server:start().

control node:
$ rebar3 shell --name control@127.0.0.1 --setcookie logistics
$ control_center:start().


zone north node:
$ rebar3 shell --name zone_north@127.0.0.1 --setcookie logistics
$ zone_manager:start().


zone center node:
$ rebar3 shell --name zone_center@127.0.0.1 --setcookie logistics
$ zone_manager:start().

zone south node:
$ rebar3 shell --name zone_south@127.0.0.1 --setcookie logistics
$ zone_manager:start().

backup node:
$ rebar3 shell --name backup@127.0.0.1 --setcookie logistics
$ backup_node:start().

