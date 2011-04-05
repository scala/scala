@echo off

rem ##########################################################################
rem # Author  : Stephane Micheloud
rem ##########################################################################

rem # For building the -64.dll, you need: Visual C++ Express, Microsoft SDK (to
rem # get the 64bit compiler / libraries), adapt this script.

rem ##########################################################################
rem # variables

if "%OS%"=="Windows_NT" @setlocal

rem debug switches are: off=0, on=1
set DEBUG=0
set STDOUT=NUL
if %DEBUG%==1 set STDOUT=CON

set CLASS_NAME=Test$
set CLASS_DIR=.

set OBJ_NAME=natives
set LIB_NAME=natives-32

if "%JAVA_HOME%"=="" goto error1
if "%VSINSTALLDIR%"=="" goto error2

set JAVAH=%JAVA_HOME%\bin\javah
set JAVAH_OPTIONS=-jni -force -classpath %CLASS_DIR% -o %OBJ_NAME%.h

set CC=%VSINSTALLDIR%\vc\bin\cl
set CC_OPTIONS=/nologo /c
set CC_INCLUDES=-I%VSINSTALLDIR%\vc\include -I%JAVA_HOME%\include -I%JAVA_HOME%\include\win32

set LNK_OPTIONS=/nologo /MT /LD

rem variable LIB is used by the C++ linker to find libcmt.lib, ..
set LIB=%VSINSTALLDIR%\vc\lib

rem ##########################################################################
rem # commands

del /s/q *.obj *.exp *.lib *.dll 1>%STDOUT%

if %DEBUG%==1 echo %JAVAH% %JAVAH_OPTIONS% %CLASS_NAME%
%JAVAH% %JAVAH_OPTIONS% %CLASS_NAME%

if %DEBUG%==1 echo %CC% %CC_OPTIONS% %CC_INCLUDES% /Fo%OBJ_NAME%.obj natives.c
%CC% %CC_OPTIONS% %CC_INCLUDES% /Fo%OBJ_NAME%.obj natives.c 1>%STDOUT%

if %DEBUG%==1 echo %CC% %LNK_OPTIONS% /Fe%LIB_NAME%.dll %OBJ_NAME%.obj
%CC% %LNK_OPTIONS% /Fe%LIB_NAME%.dll %OBJ_NAME%.obj 1>%STDOUT%

goto end

rem ##########################################################################
rem # subroutines

:error1
echo ERROR: environment variable JAVA_HOME is undefined. It should point to your JDK installation.
goto end

:error2
echo ERROR: environment variable VSINSTALLDIR is undefined. It should point to your MS Visual Studio installation.
goto end

:end
if "%OS%"=="Windows_NT" @endlocal

