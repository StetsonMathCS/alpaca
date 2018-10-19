#!/bin/sh

curl -X "POST" "http://alpaca.artifice.cc:8080/milestone4" \
     -H 'Content-Type: application/json; charset=utf-8' \
     -d $'[
  "createAllPaths",
  "[server_access_root]",
  "[]",
  "server_access_root"
]'


curl -X "POST" "http://alpaca.artifice.cc:8080/milestone4" \
     -H 'Content-Type: application/json; charset=utf-8' \
     -d $'[
  "allPossiblePaths"
]'



curl -X "POST" "http://alpaca.artifice.cc:8080/milestone4" \
     -H 'Content-Type: application/json; charset=utf-8' \
     -d $'[
  "allPaths",
  "[server_access_root]",
  "[]",
  "server_access_root/lattice.gv"
]'

#http://127.0.0.1:10333/alpaca"\
curl -X "POST" "http://alpaca.artifice.cc:8080/milestone4" \
     -H 'Content-Type: application/json; charset=utf-8' \
     -d $'[
  "createRange",
  "alpaca"
]'
