@ECHO OFF
SETLOCAL EnableDelayedExpansion

REM NOTES:
REM "toc"                     -> treated just like another page, its location can be changed
REM "--window-status loaded"  -> when window.status is set to "loaded", wkhtmltopdf knows js is loaded

SET THIS_SCRIPT_DIR=%~dp0
SET ROOT_DIR=%THIS_SCRIPT_DIR%..
SET SPEC_SRC_DIR=%ROOT_DIR%\spec
SET SPEC_BUILD_DIR=%ROOT_DIR%\build\spec

SET WKHTML_OPTS=--print-media-type --window-status loaded --footer-center [page] --javascript-delay 1000 --footer-font-name "Luxi Sans"
SET WKHTML_TOC=toc --xsl-style-sheet %SPEC_SRC_DIR%\spec-toc.xslt

SET HTML_FILES=
FOR /F "tokens=*" %%a IN ('dir %SPEC_BUILD_DIR%\*.html /B /O:N ^| findstr /v /i "index.*"') DO (
  SET HTML_FILES=!HTML_FILES! %SPEC_BUILD_DIR%\%%a
)
ECHO Making Spec.pdf with HTML files:
ECHO %SPEC_BUILD_DIR%\index.html %HTML_FILES%

REM first goes index.html, then TOC, then rest
wkhtmltopdf %WKHTML_OPTS% %SPEC_BUILD_DIR%\index.html %WKHTML_TOC% %HTML_FILES% %SPEC_BUILD_DIR%\Spec.pdf
