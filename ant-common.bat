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

set CP=cp
set ECHO=echo
set RM=del /s /q

rem ##########################################################################
rem # environment setup

echo Setting up the build environment..
echo.
call %_ANTCMD% -Dplatform=win -q -f setenv-nsc.xml >NUL
call env.bat

rem ##########################################################################
