#!/bin/bash
while [ 1 ]
do
    resourcerer-example &
    PID="$!"
    inotifywait -e modify -e attrib "$(which resourcerer-example)"
    kill "$!"
    sleep 1
done
