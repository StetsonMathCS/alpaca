#!/bin/sh

curl -X "POST" "http://127.0.0.1:10333/alpaca" \
     -H 'Content-Type: application/json; charset=utf-8' \
     -d $'[
  "createAllPaths",
  "[server_access_root]",
  "[]",
  "server_access_root"
]'


curl -X "POST" "http://127.0.0.1:10333/alpaca" \
     -H 'Content-Type: application/json; charset=utf-8' \
     -d $'[
  "allPossiblePaths"
]'



curl -X "POST" "http://127.0.0.1:10333/alpaca" \
     -H 'Content-Type: application/json; charset=utf-8' \
     -d $'[
  "allPaths",
  "[server_access_root]",
  "[]",
  "server_access_root/lattice.gv"
]'


curl -X "POST" "http://127.0.0.1:10333/alpaca" \
     -H 'Content-Type: application/json; charset=utf-8' \
     -d $'[
  "createRange",
  "alpaca"
]'


curl -X "POST" "http://127.0.0.1:10333/alpaca" \
     -H 'Content-Type: application/json; charset=utf-8' \
     -d $'[
  "createAllPaths",
  "[server_access_root]",
  "[]",
  "server_access_root"
]'

