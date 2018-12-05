# DevOps setup for ALPACA:

## Requisite software dependencies

* Jenkins
* Java 8
* Java Runtime Environment
* Docker

## Integration testing scripts

In alpaca/devops_scripts, you will find several scripts that can be used to test if a certain feature of ALPACA is working. 

These scripts include:

1. api_examples.sh for testing the alpaca_http_api
2. check_http_server.sh which happens at the beginning of the Jenkins pipeline, and is used to ensure that the alpaca_http_api is running and if not, start it for testing and usage.
3. run_http_server.sh runs the http_api_server. This is called by the alpaca_http.service Unix daemon/service which needs to be installed in systemd to keep the server running between reboots. The service needs to be installed appropriately on the target machine.
4. alpaca_tests.pl


## Setting up Jenkins

