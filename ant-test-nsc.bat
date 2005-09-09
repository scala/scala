@echo off
rem #####################################################-*-Bash-script-*-####
rem # Test nsc
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
call ant-common.bat

rem ##########################################################################
rem # ant build

set ANT_CONFIG_BUILDFILE=test-nsc.xml
set ANT_BUILDFILE=concrete-%ANT_CONFIG_BUILDFILE%
set ANT_EXCLUDEFILE=developer/%USERNAME%/test-nsc-excludes.xml

%CP% %ANT_CONFIG_BUILDFILE% %ANT_BUILDFILE%

set CLASSPATH=%nsc_fjbg_jar%;%nsc_scala_jar%;%nsc_tools_jar%;%nsc_nsc_jar%;%nsc_nsc4ant_jar%
%_ANTCMD% -Dplatform=win -f %ANT_BUILDFILE% %_ANTCMD_ARGS%

%RM% %ANT_BUILDFILE%

##############################################################################

if "%OS%"=="Windows_NT" @endlocal
