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

rem We use the value of the JAVACMD environment variable if defined
set _JAVACMD=%JAVACMD%
if "%_JAVACMD%"=="" set _JAVACMD=java

rem We use the value of the JAVA_OPTS environment variable if defined
set _JAVA_OPTS=%JAVA_OPTS%
if "%_JAVA_OPTS%"=="" set _JAVA_OPTS=-Xmx256M -Xms16M

set _EXTENSION_CLASSPATH=
if "%_EXTENSION_CLASSPATH%"=="" (
  for %%f in ("%_SCALA_HOME%\lib\*") do call :add_cpath "%%f"
  if "%OS%"=="Windows_NT" (
    for /d %%f in ("%_SCALA_HOME%\lib\*") do call :add_cpath "%%f"
  )
)

set _BOOT_CLASSPATH=
if "%_BOOT_CLASSPATH%"=="" (
  if exist "%_SCALA_HOME%\lib\scala-library.jar" (
    set _BOOT_CLASSPATH=%_SCALA_HOME%\lib\scala-library.jar
  )
  if exist "%_SCALA_HOME%\lib\library" (
    set _BOOT_CLASSPATH=%_SCALA_HOME%\lib\library
  )
)

rem We use the value of the ILASM environment variable if defined
set _ILASM=%ILASM%
if "%_ILASM%"=="" (
  if exist "%MONO_HOME%" (
    set _ILASM=%MONO_HOME%\bin\ilasm.bat
  ) else (
    call :find_ilasm ilasm.exe
  )
)

set _MSIL_LIBPATH=%_SCALA_HOME%\lib\predef.dll;%_SCALA_HOME%\lib\scalaruntime.dll;%_SCALA_HOME%\lib\mscorlib.dll
if not "%MONO_PATH%"=="" (
  set _MSIL_LIBPATH=%_MSIL_LIBPATH%;%MONO_PATH%
)

set _PROPS=-Dscala.home="%_SCALA_HOME%" -Denv.classpath="%CLASSPATH%" -Dmsil.libpath="%_MSIL_LIBPATH%" -Dmsil.ilasm="%_ILASM%" 

%_JAVACMD% -Xbootclasspath/a:"%_BOOT_CLASSPATH%" %_JAVA_OPTS% %_PROPS% -cp "%_EXTENSION_CLASSPATH%" scala.tools.nsc.Main -target:msil %_ARGS%
goto end

rem ##########################################################################
rem # subroutines

:add_cpath
  if "%_EXTENSION_CLASSPATH%"=="" (
    set _EXTENSION_CLASSPATH=%~1
  ) else (
    set _EXTENSION_CLASSPATH=%_EXTENSION_CLASSPATH%;%~1
  )
goto :eof

rem Variable "%~dps0" works on WinXP SP2 or newer
rem (see http://support.microsoft.com/?kbid=833431)
rem set _SCALA_HOME=%~dps0..
:set_home
  set _BIN_DIR=
  for %%i in (%~sf0) do set _BIN_DIR=%_BIN_DIR%%%~dpsi
  set _SCALA_HOME=%_BIN_DIR%..
goto :eof

:find_ilasm
  set _ILASM=%~$PATH:1
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
