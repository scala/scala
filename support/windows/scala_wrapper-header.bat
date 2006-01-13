@echo off

rem Copyright (C) 2002-2006 LAMP/EPFL
rem
rem This is free software; see the distribution for copying conditions.
rem There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
rem PARTICULAR PURPOSE.

if "%OS%"=="Windows_NT" (
  @setlocal
  set _PREFIX=%~dp0..
) else (
  set _PREFIX=%SCALA_HOME%
)

set _JAVACMD=%JAVACMD%
if "%_JAVACMD%"=="" set _JAVACMD=java

