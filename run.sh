#!/bin/bash

time=0
while true; do
  newtime=$(find ./src ./test -name "[A-Z]*.purs" -exec stat -f "%m" \{} \; | sort -n -r | head -1)
  if [ "$newtime" -gt "$time" ]; then
    clear
    spago build && cp output/Main/index.js ./public/index.js
  fi

  time=$newtime;
  sleep 1
done
