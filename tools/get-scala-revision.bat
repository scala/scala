@echo off
rem
rem Usage: get-scala-revison.bat [dir]
rem Figures out current scala revision of an svn checkout or
rem a git-svn mirror (or a git clone.)
rem
rem If no dir is given, current working dir is used.

if "%OS%" NEQ "Windows_NT" (
  echo "Sorry, your version of Windows is too old to run Scala."
  goto :eof
)
@setlocal

set _DIR=
if "%*"=="" (
  for /f %%i in ('cd') do set _DIR=%%i
) else (
  set _DIR=%~1
)
cd %_DIR%

if exist .svn\NUL (
  rem 2>&1 to catch also error output (e.g. svn warnings)
  for /f "skip=4 tokens=2" %%i in ('svn info') do (
    echo %%i
    goto :end
  )
) else ( if exist .git\NUL (
  set _GIT_PAGER=type
  rem this grabs more than one line because otherwise if you have local
  rem commits which aren't in git-svn it won't see any revision.
  rem TODO: git log -10 | findstr git-svn-id | ...
  echo 0
) else (
  echo %_DIR% doesn't appear to be git or svn dir.
  echo 0
  exit 1
))

:end
@endlocal
