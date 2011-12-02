@echo off
rem
rem Usage: get-scala-revison.bat [dir]
rem Figures out current scala revision of a git clone.
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

if exist .git\NUL (
  git describe head --abbrev=7 --match dev
  echo 0
)

:end
@endlocal
