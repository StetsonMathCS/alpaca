#!/bin/bash

#checks if swipl is running

if pgrep -x "swipl" > /dev/null
then
	echo "Running"
else
	echo "Stopped"
	sh nohup swipl -s /home/noah/alpaca/devops_scripts/alpaca_http_api.pl -g "server(10333)" &
fi
