
if "%SCALA_HOME%" == "" goto error1
if not exist "%SCALA_HOME%\VERSION-%VERSION%" goto error2

set ARGS=

:loop
if '%1' == '' goto exec
set ARGS=%ARGS% %1
shift
goto loop

:exec
%COMMAND% %ARGS%
goto end

:error1
echo ERROR: environment variable SCALA_HOME is undefined. It should point to the directory containing the file "VERSION-%VERSION%".
goto end

:error2
echo ERROR: environment variable SCALA_HOME points to the wrong directory "%SCALA_HOME%". It should point to the directory containing the file "VERSION-%VERSION%".
goto end

:end
set VERSION=
set COMMAND=

if "%OS%"=="Windows_NT" @endlocal
