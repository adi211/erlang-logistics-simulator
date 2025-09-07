#!/bin/bash

# הגדרת נתיב הפרוייקט
PROJECT_PATH="/home/dolev/projects/erlang-logistics-simulator"

# מעבר לתיקיית הפרוייקט
cd $PROJECT_PATH

# קימפול הפרוייקט
echo "Compiling the project..."
rebar3 compile

# פתיחת הטרמינלים והרצת הצמתים
echo "Starting the nodes..."

# Visualization node
gnome-terminal -- bash -c "cd $PROJECT_PATH && rebar3 shell --name visualization@127.0.0.1 --setcookie logistics -run visualization_server start; exec bash"

# Control node
gnome-terminal -- bash -c "cd $PROJECT_PATH && rebar3 shell --name control@127.0.0.1 --setcookie logistics -run control_center start; exec bash"

# Zone north node
gnome-terminal -- bash -c "cd $PROJECT_PATH && rebar3 shell --name zone_north@127.0.0.1 --setcookie logistics -run zone_manager start; exec bash"

# Zone center node
gnome-terminal -- bash -c "cd $PROJECT_PATH && rebar3 shell --name zone_center@127.0.0.1 --setcookie logistics -run zone_manager start; exec bash"

# Zone south node
gnome-terminal -- bash -c "cd $PROJECT_PATH && rebar3 shell --name zone_south@127.0.0.1 --setcookie logistics -run zone_manager start; exec bash"

# Backup node
gnome-terminal -- bash -c "cd $PROJECT_PATH && rebar3 shell --name backup@127.0.0.1 --setcookie logistics -run backup_node start; exec bash"

echo "All nodes have been started in new terminals."