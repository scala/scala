
if "%_PREFIX%"=="" goto error1
if not exist "%_PREFIX%\VERSION-%_VERSION%" goto error2

set _ARGS=

:loop
if "%1"=="" goto exec
set _ARGS=%_ARGS% %1
shift
goto loop

:exec
%_COMMAND% %_ARGS%
goto end

:error1
echo ERROR: environment variable SCALA_HOME is undefined. It should point to the directory containing the file "VERSION-%_VERSION%".
goto end

:error2
echo ERROR: environment variable SCALA_HOME points to the wrong directory "%_PREFIX%". It should point to the directory containing the file "VERSION-%_VERSION%".
goto end

:end
set _VERSION=
set _COMMAND=

if "%OS%"=="Windows_NT" @endlocal
