#!/usr/bin/expect
#set user [lindex $argv 0]
#set password [lindex $argv 1]

spawn mysql -u mimi -p

expect "Enter password: \r"
send "mimi123\r"

interact
