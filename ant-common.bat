@echo off
rem ####################################################-*-Batch-script-*-####
rem # Common settings
rem ##########################################################################
rem # $Id$

rem ##########################################################################
rem # Apache Ant

set _ANTCMD=ant
set ANT_OPTS=-Xmx256M
rem set ANT_ARGS=-verbose

rem ##########################################################################
rem # Shell commands

set CP=copy /y
set ECHO=echo
set RM=del /s /q

rem ##########################################################################
rem # environment setup

echo Setting up the build environment..
echo.
call %_ANTCMD% -Dplatform=win -q -f setenv-nsc.xml >NUL
call env.bat

rem ##########################################################################

set ANT_CONFIG_BUILDFILE=%1.xml
set ANT_BUILDFILE=concrete-%ANT_CONFIG_BUILDFILE%
set ANT_EXCLUDEFILE=developer/%USERNAME%/%1-excludes.xml

if exist "%ANT_EXCLUDEFILE%" goto exclude
%CP% %ANT_CONFIG_BUILDFILE% %ANT_BUILDFILE%
goto next
:exclude
echo %ANT_EXCLUDEFILE% file not yet supported
%CP% %ANT_CONFIG_BUILDFILE% %ANT_BUILDFILE%
:next

rem ##########################################################################
