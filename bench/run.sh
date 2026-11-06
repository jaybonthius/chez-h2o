#!/bin/sh
set -u
run=$1
port=$2
log=build/bench/server.log
client=build/bench/client

measure() {
  $run ./build/obj/bench/server.so "$port" $1 > "$log" 2>&1 &
  server=$!
  until grep -q ready "$log" 2> /dev/null; do
    kill -0 "$server" 2> /dev/null || { cat "$log"; exit 1; }
    sleep 0.1
  done
  echo "== /commands as a ${1:-worker} handler"
  $client rtt "$port" 2000 > /dev/null
  for i in 1 2 3; do $client rtt "$port" 10000; done
  for i in 1 2; do $client sse "$port" 1 300 32768 10; done
  for i in 1 2; do $client sse "$port" 20 300 32768 10; done
  HOLD=3 $client sse "$port" 20 50 32768 10 > /dev/null &
  held=$!
  sleep 2
  echo "with 20 streams: $(grep VmRSS /proc/$server/status), $(grep Threads /proc/$server/status)"
  wait "$held"
  kill "$server"
  wait "$server" 2> /dev/null || :
}

measure ""
measure loop
