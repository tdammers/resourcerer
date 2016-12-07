#!/bin/bash

function watch-example() {
    while [ 1 ]
    do
        resourcerer-example &
        PID="$!"
        sleep 0.1
        curl 'http://localhost:5000/'
        inotifywait -e modify -e attrib "$(which resourcerer-example)"
        kill "$!"
        sleep 1
    done
}

function watch-build() {
    stack install --file-watch --test --ghc-options '-O0'
}

watch-example &
watch-build
