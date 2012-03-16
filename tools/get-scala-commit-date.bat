@echo off
rem
rem Usage: get-scala-revison.bat [dir]
rem Figures out current scala commit date of a git clone.
rem
rem If no dir is given, current working dir is used.

@setlocal
set _DIR=
if "%*"=="" (
  for /f "delims=;" %%i in ('cd') do set "_DIR=%%i"
) else (
  set "_DIR=%~1"
)
cd %_DIR%

rem  TODO - Check with a real windows user that this works!
if exist .git\NUL (
  for /f "tokens=1delims= " in ('git log --format="%ci" -1') do set commitdate=%%a
  echo %commitdate%
)

:end
@endlocal
