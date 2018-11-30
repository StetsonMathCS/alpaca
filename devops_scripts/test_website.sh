#!/bin/bash

# curl -s http://alpaca.artifice.cc:8080 | grep -q "ALPACA" ; if [ $? -ne 0 ] ; then echo "oh no!"; fi

 curl -s http://alpaca.artifice.cc:8080 | grep -q "ALPACA";
 if [ $? -ne 1 ];
 then echo "failed at main page";
 fi

	
 curl -s http://alpaca.artifice.cc:8080 | grep -q "ALPACA";
 if [ $? -ne 1 ];
 then echo "failed at main page";
 fi



