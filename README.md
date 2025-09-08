# Delivery Simulator

> 🎥 [Watch Demo Video](https://youtu.be/wCwnGQa-tug)

## Final Project - Delivery Simulator

**Creators:** Adi Shlomo and Dolev Ishay  
**Course:** Functional Programming in Concurrent and Distributed Systems, Ben Gurion University  
**Professor:** Yehuda Ben-Shimol  
**Teaching Assistant:** Guy Perets

---

Distributed logistics management system built with Erlang/OTP, simulating a 24/7 delivery company with real-time visualization and automatic failover.

## Overview

- **6 distributed nodes** (Visualization, Control, 3 Zones, Backup)
- **Fault-tolerant** with automatic failover
- **Real-time visualization** of deliveries
- **Scalable** to 1000+ concurrent operations

## Requirements

- Erlang/OTP 24+
- Rebar3

## Installation

```bash
git clone https://github.com/adi211/erlang-logistics-simulator.git
cd erlang-logistics-simulator
rebar3 compile
```

## Running the System

### Option 1: Single Machine (6 terminals)

**Terminal 1 - Visualization:**
```bash
rebar3 shell --name visualization@127.0.0.1 --setcookie logistics
> visualization_server:start().
```

**Terminal 2 - Control:**
```bash
rebar3 shell --name control@127.0.0.1 --setcookie logistics
> control_center:start().
```

**Terminal 3 - Zone North:**
```bash
rebar3 shell --name zone_north@127.0.0.1 --setcookie logistics
> zone_manager:start().
```

**Terminal 4 - Zone Center:**
```bash
rebar3 shell --name zone_center@127.0.0.1 --setcookie logistics
> zone_manager:start().
```

**Terminal 5 - Zone South:**
```bash
rebar3 shell --name zone_south@127.0.0.1 --setcookie logistics
> zone_manager:start().
```

**Terminal 6 - Backup:**
```bash
rebar3 shell --name backup@127.0.0.1 --setcookie logistics
> backup_node:start().
```

### Option 2: Multiple Machines (Example Setup)

1. Edit `include/network_const.hrl`:
```erlang
%% Comment local IPs and uncomment real IPs:
%% Example configuration:
-define(VIZ_IP,    "132.72.52.84").   % Computer 1
-define(CTRL_IP,   "132.72.54.72").   % Computer 2
-define(NORTH_IP,  "132.72.81.226").  % Computer 3
-define(CENTER_IP, "132.72.80.235").  % Computer 4
-define(SOUTH_IP,  "132.72.81.94").   % Computer 5
```

2. Recompile: `rebar3 clean && rebar3 compile`

3. Run on each machine (example IPs):

**Computer 1 (132.72.52.84):**
```bash
rebar3 shell --name visualization@132.72.52.84 --setcookie logistics
> visualization_server:start().
```

**Computer 2 (132.72.54.72):**
```bash
rebar3 shell --name control@132.72.54.72 --setcookie logistics
> control_center:start().
```

**Computer 3 (132.72.81.226):**
```bash
rebar3 shell --name zone_north@132.72.81.226 --setcookie logistics
> zone_manager:start().
```

**Computer 4 (132.72.80.235):**
```bash
rebar3 shell --name zone_center@132.72.80.235 --setcookie logistics
> zone_manager:start().
```

**Computer 5 (132.72.81.94):**
```bash
rebar3 shell --name zone_south@132.72.81.94 --setcookie logistics
> zone_manager:start().
```

**Computer 6 (Any available IP):**
```bash
rebar3 shell --name backup@<YOUR_IP> --setcookie logistics
> backup_node:start().
```

## Usage

Once all nodes are running:
- The visualization window opens automatically
- Use the dashboard to start/stop simulation


## Authors

Adi Shlomo & Dolev Ishay
