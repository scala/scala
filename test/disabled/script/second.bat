@echo off

scala -nocompdaemon -e "println(\"My second argument is \" + args(1))"  arg1 arg2
