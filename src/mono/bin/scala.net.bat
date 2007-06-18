@echo off

rem ##########################################################################
rem # @NAME@ @VERSION@
rem ##########################################################################
rem # @COPYRIGHT@
rem #
rem # This is free software; see the distribution for copying conditions.
rem # There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
rem # PARTICULAR PURPOSE.
rem ##########################################################################

rem We adopt the following conventions:
rem - System/user environment variables start with a letter
rem - Local batch variables start with an underscore ('_')

if "%OS%"=="Windows_NT" (
  @setlocal
  call :set_home
  set _ARGS=%*
) else (
  set _SCALA_HOME=%SCALA_HOME%
  rem The following line tests SCALA_HOME instead of _SCALA_HOME, because
  rem the above change to _SCALA_HOME is not visible within this block.
  if "%SCALA_HOME%"=="" goto error1
  call :set_args
)

rem We use the value of the MONO environment variable if defined
set _MONO=%MONO%
if "%_MONO%"=="" (
  if exist "%MONO_HOME%" (
    set _MONO=%MONO_HOME%\bin\mono.exe
  ) else (
    call :find_mono mono.exe
  )
)

set _MSIL_LIBPATH=%_SCALA_HOME%\lib
if not "%MONO_PATH%"=="" (
  set _MSIL_LIBPATH=%_MSIL_LIBPATH%;%MONO_PATH%
)

set MONO_PATH=%_MSIL_LIBPATH%
%_MONO% %_ARGS%
goto end

rem ##########################################################################
rem # subroutines

rem Variable "%~dps0" works on WinXP SP2 or newer
rem (see http://support.microsoft.com/?kbid=833431)
rem set _SCALA_HOME=%~dps0..
:set_home
  set _BIN_DIR=
  for %%i in (%~sf0) do set _BIN_DIR=%_BIN_DIR%%%~dpsi
  set _SCALA_HOME=%_BIN_DIR%..
goto :eof

:find_mono
  set _MONO=%~$PATH:1
goto :eof

:set_args
  set _ARGS=
  :loop
  rem Argument %1 may contain quotes so we use parentheses here
  if (%1)==() goto :eof
  set _ARGS=%_ARGS% %1
  shift
  goto loop

rem ##########################################################################
rem # errors

:error1
echo ERROR: environment variable SCALA_HOME is undefined. It should point to your installation directory.
goto end

:end
if "%OS%"=="Windows_NT" @endlocal
