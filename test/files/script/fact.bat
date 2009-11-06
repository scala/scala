::#!
:: fact - A simple Scala batch file that prints out the factorial
::        of the argument specified on the command line.

@echo off
call scala -nocompdaemon %0 %*
goto :eof
::!#


val x = argv(0).toInt

def fact(x: Int):Int =
  if(x==0) 1
  else x*fact(x-1)

Console.println("fact(" + x + ") = " + fact(x))
