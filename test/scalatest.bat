@echo off

rem ##########################################################################
rem # Copyright 2002-2009 LAMP/EPFL
rem #
rem # This is free software; see the distribution for copying conditions.
rem # There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
rem # PARTICULAR PURPOSE.
rem ##########################################################################

rem We adopt the following conventions:
rem - System/user environment variables start with a letter
rem - Local batch variables start with an underscore ('_')

echo ######################################################################
echo ## DEPRECATION WARNING                                              ##
echo ######################################################################
echo ## The 'scalatest' runner script has been deprecated. Please use    ##
echo ## 'partest' instead.                                               ##
echo ######################################################################
echo.

if "%OS%"=="Windows_NT" (
  @setlocal
  call :set_home
) else (
  set _SCALA_HOME=%SCALA_HOME%
  if "%_SCALA_HOME%"=="" goto err_home
)

rem We use the value of the JAVACMD environment variable if defined
set _JAVACMD=%JAVACMD%
if "%_JAVACMD%"=="" set _JAVACMD=java

set _BIN=dists\latest\bin
set _NORUN=
set _TYPE=auto
set _SHOWLOG=
set _SHOWDIFF=
set _FAILED=

set _ARGS=
:loop
rem Argument %1 may contain quotes so we use parentheses here
if (%1)==() goto exec
if (%1)==(--auto)      set _TYPE=auto& goto next
if (%1)==(--run)       set _TYPE=run& goto next
if (%1)==(--jvm)       set _TYPE=jvm& goto next
if (%1)==(--pos)       set _TYPE=pos& goto next
if (%1)==(--neg)       set _TYPE=neg& goto next
if (%1)==(--msil)      set _TYPE=msil& goto next
if (%1)==(--quick)     set _BIN=build\quick\bin& goto next
if (%1)==(--no-run)    set _NORUN=1& goto next
if (%1)==(--show-log)  set _SHOWLOG=1& goto next
if (%1)==(--show-diff) set _SHOWDIFF=1& goto next
if (%1)==(--failed)    set _FAILED=1& goto next
if (%1)==(--help)      call :prt_help & goto :eof
if (%1)==(-h)          call :prt_help & goto :eof
if (%1)==(-?)          call :prt_help & goto :eof
if (%1)==(--version)   call :prt_version & goto :eof
if (%1)==(-v)          call :prt_version & goto :eof
call :chk_option %1
if errorlevel 0 goto err_opt
set _ARGS=%_ARGS% %1
:next
shift
goto loop

:exec
if exist "%_SCALA_HOME%\meta\universe" (
  set _BINDIR=%_SCALA_HOME%\bin
  set _SRCDIR=%_SCALA_HOME%\misc\scala-test\files
  set _DIFFDIR=%_SCALA_HOME%\bin\diff
) else (
  set _BINDIR=%_SCALA_HOME%\%_BIN%
  set _SRCDIR=%_SCALA_HOME%\test\files
  set _DIFFDIR=%_SCALA_HOME%\test\diff
)

set _SCALA=%_BINDIR%\scala
set _SCALAC=%_BINDIR%\scalac
set _SCALAP=%_BINDIR%\scalap
set _DIFF=%_DIFFDIR%\diff.exe --text --strip-trailing-cr

if not exist "%_BINDIR%" goto err_bin

set _OBJDIR=
set _TMPDIR=%TEMP%

if not "%_OBJDIR%"=="" (
  if not exist "%_OBJDIR%" mkdir "%_OBJDIR%"
)
if not "%_TMPDIR%"=="" (
  if not exist "%_TMPDIR%" mkdir "%_TMPDIR%"
)

call :prt_dir "Source directory is  :" "%_SRCDIR%"
call :prt_dir "Scala binaries are in:" "%_BINDIR%"
call :set_version
call :prt_dir "Scala version is     :" "%_NSC_VERSION%"
call :prt_dir "Java runtime is      :" "%_JVM_VERSION%"

set _FILES_POS=
set _FILES_RUN=
set _FILES_JVM=
set _FILES_NEG=
set _FILES_MSIL=

if %_TYPE%==pos (
  set _FILES_POS=%_SRCDIR%\pos
) else if %_TYPE%==run (
  set _FILES_RUN=%_SRCDIR%\run
) else if %_TYPE%==jvm (
  set _FILES_JVM=%_SRCDIR%\jvm
  set _FILES_RUN=%_SRCDIR%\run
) else if %_TYPE%==neg (
  set _FILES_NEG=%_SRCDIR%\neg
) else if %_TYPE%==msil (
  set _FILES_MSIL=%_SRCDIR%\msil
) else if %_TYPE%==auto (
  set _FILES_POS=%_SRCDIR%\pos
  set _FILES_NEG=%_SRCDIR%\neg
  set _FILES_JVM=%_SRCDIR%\jvm
  set _FILES_RUN=%_SRCDIR%\run
) else (
  goto err_test
)
:start
call :chk_all
goto end

rem ##########################################################################
rem # subroutines
rem # NB. goto/call commands use only the first 8 characters of a label

:prt_dir
  echo %~1 %~2
  goto :eof

:prt_header
  echo.
  echo %~1
  goto :eof

:prt_help
  echo Usage: scalatest [OPTION]...
  echo.
  echo --auto          use filenames to select the test to run
  echo --run           next files test the interpreter and all backends
  echo --jvm           next files test the JVM backend
  echo --pos           next files test a compilation success
  echo --neg           next files test a compilation failure
  echo --msil          next files test the .NET 
  echo --quick         use the 'quick' build instead of the distribution
  echo --no-run        run no test, use results of last run
  echo --show-log      show output of failed tests
  echo --show-diff     show differences between actual and expected output
  echo --failed        test only files that failed last time
  echo --help, -h, -?  display this help and exit
  echo --version, -v   output version information and exit
  goto :eof

:prt_version
  echo Scala test suite 0.9.3 -- (c) 2002-2009 LAMP/EPFL
  goto :eof

:prt_status
  set _MSG=testing: [...]\%~1\%2
  if not "%_STATUS%"=="0" goto failed
  set /a _CNT_SUCCESS=_CNT_SUCCESS+1
  echo %_MSG%  [OK]
  goto :eof
  :failed
  set /a _CNT_FAILURE=_CNT_FAILURE+1
  echo %_MSG%  [FAILED]
  if not "%_SHOWLOG%"=="" type %_LOGFILE%
  goto :eof

rem Variable "%~dps0" works on WinXP SP2 or newer
rem (see http://support.microsoft.com/?kbid=833431)
rem set _SCALA_HOME=%~dps0..
:set_home
  set _BINDIR=
  for %%i in (%~sf0) do set _BINDIR=%_BINDIR%%%~dpsi
  set _SCALA_HOME=%_BINDIR%..
  goto :eof

:set_version
  set _TMPFILE=%_TMPDIR%\.version
  call %_SCALAC% -version 2> %_TMPFILE%
  for /f "tokens=*" %%f in (%_TMPFILE%) do @set _VERSION=%%f
  set _NSC_VERSION=%_VERSION%
  %_JAVACMD% -version 2> %_TMPFILE%
  for /f "skip=2 tokens=*" %%f in (%_TMPFILE%) do @set _VERSION=%%f
  set _JVM_VERSION=%_VERSION%
  goto :eof

:chk_option
  echo %~1 | findstr /r /c:"-.*" 1>NUL
  goto :eof

rem Tests a compilation success.
:test_pos
  rmdir /s/q %_DSTBASE%.obj 2>NUL
  mkdir %_DSTBASE%.obj
  call %_SCALAC% -d %_DSTBASE%.obj %1 1>NUL 2>NUL
  if errorlevel 1 goto status_pos
  set _STATUS=0
  goto next_pos
  :status_pos
  set _STATUS=1
  :next_pos
  rmdir /s/q %_DSTBASE%.obj
  goto :eof

rem Tests a compilation failure.
:test_neg
  rmdir /s/q %_DSTBASE%.obj 2>NUL
  mkdir %_DSTBASE%.obj
  call %_SCALAC% -d %_DSTBASE%.obj %1 1>NUL 2>NUL
  if errorlevel 1 goto status_neg
  set _STATUS=1
  goto next_neg
  :status_neg
  set _STATUS=0
  :next_neg
  rmdir /s/q %_DSTBASE%.obj
  goto :eof

rem Tests the JVM backend.
:test_jvm
  rmdir /s/q %_DSTBASE%.obj 2>NUL
  mkdir %_DSTBASE%.obj
  call %_SCALAC% -d %_DSTBASE%.obj %1 2>NUL
  if errorlevel 1 goto status_jvm
  call %_SCALA% -cp %_DSTBASE%.obj Test "jvm" 1>%_LOGFILE% 2>NUL
  if errorlevel 1 goto status_jvm
  set _STATUS=0
  goto next_jvm
  :status_jvm
  set _STATUS=1
  :next_jvm
  rmdir /s/q %_DSTBASE%.obj
  goto :eof

:chk_test
  if "%_OBJDIR%"=="" (
    set _DSTDIR=%_SRCDIR%
  ) else (
    set _DSTDIR=%_OBJDIR%
  )
  set _DSTBASE=%_DSTDIR%\%~n1-%_KIND%
  set _LOGFILE=%_DSTBASE%.log
  set _CHKFILE=%~dpn1.check

  if not '%_HEADER%'=='' call :prt_header %_HEADER% & set _HEADER=

  if "%_KIND%"=="jvm" call :test_jvm %1 & goto status
  if "%_KIND%"=="pos" call :test_pos %1 & goto status
  if "%_KIND%"=="neg" call :test_neg %1 & goto status
  goto :eof
  :status
  if exist %_LOGFILE% %_DIFF% %_LOGFILE% %_CHKFILE% 2>NUL
  call :prt_status %_KIND% %~nx1
  del /s/q %_LOGFILE% 2>NUL 1>NUL
  goto :eof

:chk_file
  set _CNT_SUCCESS=0
  set _CNT_FAILURE=0
  for %%f in (%1\*.scala) do call :chk_test %%f
  set /a _CNT_TOTAL=_CNT_SUCCESS+_CNT_FAILURE
  if "%_CNT_FAILURE%"=="0" goto success
  echo %_CNT_FAILURE% of %_CNT_TOTAL% tests failed
  goto :eof
  :success
  echo All of %_CNT_TOTAL% tests were successful
  goto :eof

:chk_kind
  set _HEADER=%1
  shift
  set _KIND=%1
  shift
  :loop_kind
  if "%1"=="" goto done
  call :chk_file %1
  shift
  goto loop_kind
  :done
  goto :eof

rem Checks everything.
:chk_all
  call :chk_kind "Testing JVM backend" jvm %_FILES_RUN% %_FILES_JVM%
  call :chk_kind "Testing compiler (on files whose compilation should succeed)" pos %_FILES_POS%
  call :chk_kind "Testing compiler (on files whose compilation should fail)" neg %_FILES_NEG%
  call :chk_kind "Testing .NET backend" msil %_FILES_MSIL%
  goto :eof

rem ##########################################################################
rem # errors

:err_bin
echo ERROR: missing command "%_SCALAC%; run "ant build".
goto end

:err_home
echo ERROR: Windows NT or newer is required to run this batch command.
goto end

:err_test
echo ERROR: Illegal test type %_TYPE%.
goto end

:err_opt
echo ERROR: Unknown option %1
goto end

:end
if "%OS%"=="Windows_NT" @endlocal
