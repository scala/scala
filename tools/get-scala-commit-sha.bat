@echo off
for %%X in (bash.exe) do (set FOUND=%%~$PATH:X)
if defined FOUND (
  bash "%~dp0\get-scala-commit-sha" 2>NUL
) else (
  rem echo this script does not work with cmd.exe. please, install bash
  echo unknown
  exit 1
)
