# DevOps setup for ALPACA:

## Requisite software dependencies

* Jenkins
* Java 8
* Java Runtime Environment
* Docker
* ALPACA
* Expect
* Ubuntu or a Unix-based OS(duh)

## Integration testing scripts

In alpaca/devops_scripts, you will find several scripts that can be used to test if a certain feature of ALPACA is working. 

These scripts include:

1. api_examples.sh for testing the alpaca_http_api. The results of this test are printed to the console and httpd.log
2. check_http_server.sh which happens at the beginning of the Jenkins pipeline, and is used to ensure that the alpaca_http_api is running and if not, start it for testing and usage.
3. run_http_server.sh runs the http_api_server. This is called by the alpaca_http.service Unix daemon/service which needs to be installed in systemd to keep the server running between reboots. The service needs to be installed appropriately on the target machine.
4. alpaca_tests.pl runs a couple of simple tests to ensure that prolog is installed and running correctly and ALPACA is doing some simple tasks. This script is not exhaustive of ALPACA's capabilities.
5. testDatabase.sh uses Expect to simulate logging in to the mysql server as a user and displaying the available database tables. This is not exhaustive either, but it will fail if there are problems with the installation of either Expect or the mysql database.
6. test_website.sh uses curl and grep to check the website for connectivity and a response to ensure that the site is up and functional. Again, this is not exhaustive, but it will fail if the website is down or horribly broken.

## Setting up Jenkins

Jenkins is used to confirm that the ALPACA system and its dependencies are operational in a user-friendly way. It allows us to build and test ALPACA quickly. If something fails, we can see where in the process the failure was and the relevant test case. 

Jenkins is oriented around the "pipeline", which is a special script that interfaces with Jenkins and the above devops_scripts to systematically test each stage in isolation from the main project. To extend ALPACA in the future, it is important to add the new testing scripts to the existing pipeline to ensure that the tested features are still operational.

To set up Jenkins, follow [this guide](https://jenkins.io/doc/book/installing/). After setup, you may need to create the ALPACA pipeline. In this case, follow [these steps](https://jenkins.io/pipeline/getting-started-pipelines/#creating-a-simple-pipeline) to create the pipeline, and paste final_jenkins_pipeline code from devops_scripts into the Jenkinsfile under the configure tab of the pipeline. You should now be able to run the pipeline. If a program is not configured correctly, the build will likely fail. If this is the case, read the console output from the build carefully to understand what is not installed or configured. Make sure that the final ALPACA installation is organized correctly in your local file structure, and if not, either correct the file structure or modify the Jenkinsfile to work with the existing hierarchy.


## Manually running scripts

Most of the scripts relevant to integration testing are simple bash scripts, but here are the commands for running each one:

1. check_http_server.sh
> bash /home/YOUR_USERNAME/alpaca/devops_scripts/check_http_server.sh  

2. api_examples
> bash /home/YOUR_USERNAME/alpaca/devops_scripts/api_examples.sh

3. alpaca_tests.pl
> swipl -s /home/YOUR_USERNAME/alpaca/alpaca_tests.pl -t run_tests

4. testDatabase.sh
> expect /home/YOUR_USERNAME/alpaca/devops_scripts/testDatabase.sh

5. test_website.sh
> bash /home/YOUR_USERNAME/alpaca/devops_scripts/test_website.sh"

## Utility commands

If Jenkins or the HTTP_API need to be restarted, treat them like any other Unix service.

For example:

Restart the HTTP_API: "sudo service alpaca_http restart"

Get the running status of Jenkins: "sudo service jenkins status"
