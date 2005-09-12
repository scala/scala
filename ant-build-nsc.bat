@echo off 
rem ####################################################-*-Batch-script-*-####
rem # Build nsc
rem ##########################################################################
rem # $Id$

if "%OS%"=="Windows_NT" @setlocal

set _ANTCMD_ARGS=%1
if ""%1""=="""" goto doneStart
shift
:setupArgs
if ""%1""=="""" goto doneStart
set _ANTCMD_ARGS=%_ANTCMD_ARGS% %1
shift
goto setupArgs

:doneStart
call ant-common.bat build-nsc

rem ##########################################################################
rem # ant build

set CLASSPATH=%nsc_fjbg_jar%;%nsc_scala_jar%;%nsc_tools_jar%;%nsc_jaco_jar%
%_ANTCMD% -Dplatform=win -f %ANT_BUILDFILE% %_ANTCMD_ARGS%

%RM% %ANT_BUILDFILE%

rem ##########################################################################

if "%OS%"=="Windows_NT" @endlocal
