@echo off
rem
rem Usage: get-scala-commit-drift.bat [dir]
rem Figures out current scala commit drift, of a clone.
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

rem  TODO - truncate chars.
git log -1 --format="%T"

:end
@endlocal
