#!/usr/bin/expect -f
 
spawn mysql -u mimi -p

#send "mysql -u mimi -p\r"

expect "Enter password: "

send "mimi123\r"

#send "show databases;\r"

send "show databases;\r"

send "exit\r"

interact

expect eof

