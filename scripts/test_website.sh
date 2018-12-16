#!/bin/bash

# curl -s http://alpaca.artifice.cc:8080 | grep -q "ALPACA" ; if [ $? -ne 0 ] ; then echo "oh no!"; fi

 curl -s http://alpaca.artifice.cc:8080 | grep -q "ALPACA";
 if [ $? -ne 0 ];
 then echo "failed at main page";
 fi

# curl -s http://alpaca.artifice.cc:8080/register | grep -q "password";
# if [ $? -ne 0 ];
# then echo "failed at registration page";
# fi
#
# curl -s http://alpaca.artifice.cc:8080/page2 | grep -q "Select";
# if [ $? -ne 0 ];
# then echo "failed at machine download page";
# fi
