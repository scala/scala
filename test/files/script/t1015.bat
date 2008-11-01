::#!
:: t1015 - <description>.

@echo off
call scala -nocompdaemon %0 %*
goto :eof
::!#

case class Test(one : Int, two : Int)
object Test{
 def apply(one : Int): Test = Test(one, 2);
}
