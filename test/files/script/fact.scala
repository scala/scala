#!/bin/sh
# fact - A simple Scala script that prints out the factorial of
#        the argument specified on the command line.

exec scalascript "$0" "$@"
!#


val x = Integer.parseInt(argv(0))

def fact(x: Int):Int =
  if(x==0) 1
  else x*fact(x-1)

Console.println("fact(" + x + ") = " + fact(x))
