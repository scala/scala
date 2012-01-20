@echo off

set scalahome=%~dp0\..\..\..
set scaladeps=%scalahome%\lib\jline.jar;%scalahome%\lib\fjbg.jar
set scalalib=%scalahome%\build\pack\lib\scala-library.jar
if not exist "%scalalib%" set scalalib=%scalahome%\build\locker\classes\library
set scalacomp="%scalahome%\build\pack\lib\scala-compiler.jar"
if not exist "%scalacomp%" set scalacomp=%scalahome%\build\locker\classes\compiler
set stdcp=%scaladeps%;%scalalib%;%scalacomp%

echo Compiling macros...
set cp=%stdcp%
call :scalac -Xmacros "%~dp0\Printf.scala"

echo Compiling the program...
set cp=%stdcp%;%~dp0.
call :scalac "%~dp0\Test.scala"

echo.
echo NOW LOOK!!!
echo ===============================================
set cp=%stdcp%;%~dp0.
call :scala Test
echo.
echo ===============================================
goto :eof

:scalac
setlocal
call set args=%*
rem echo java -cp "%cp%" -Dscala.usejavacp=true scala.tools.nsc.Main %args%
java -cp "%cp%" -Dscala.usejavacp=true scala.tools.nsc.Main %args%
endlocal&goto :eof

:scala
setlocal
call set args=%*
rem echo java -cp "%cp%" -Dscala.usejavacp=true scala.tools.nsc.MainGenericRunner %args%
java -cp "%cp%" -Dscala.usejavacp=true scala.tools.nsc.MainGenericRunner %args%
endlocal&goto :eof
