#!/bin/bash

run_bot () {
    while true; do
        python3 bot.py bot
        echo "Bot crashed, restarting in 5 seconds..."
        sleep 5
    done
}

run_server () {
    while true; do
        python -m gunicorn --access-logfile - -w 1 -b 0.0.0.0:8080 'bot:application()'
        echo "Server crashed, restarting in 5 seconds..."
        sleep 5
    done
}

run_bot &
run_server
