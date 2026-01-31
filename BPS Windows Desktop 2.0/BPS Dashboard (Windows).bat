@echo off
setlocal EnableExtensions EnableDelayedExpansion

REM ==========================================================
REM BPS Dashboard Launcher (Windows) - Portable R + Seeded r_lib
REM - Prefers bundled portable R: .\R\bin\Rscript.exe
REM - Seeds per-user lib in %LOCALAPPDATA%\BPS Dashboard\r_lib
REM - Runs launch_app.R (which finds app.R inside the helper folder)
REM - Logs to %APPDATA%\BPS Dashboard\
REM ==========================================================

set "APPNAME=BPS Dashboard"
set "ROOT=%~dp0"
if "%ROOT:~-1%"=="\" set "ROOT=%ROOT:~0,-1%"

REM --- Logging folder ---
set "LOG_DIR=%APPDATA%\%APPNAME%"
if not exist "%LOG_DIR%" mkdir "%LOG_DIR%" >nul 2>nul

REM Build a timestamp-ish log filename (avoid illegal chars)
for /f %%i in ('powershell -NoProfile -Command "Get-Date -Format yyyyMMdd_HHmmss"') do set "TS=%%i"
set "LOG_FILE=%LOG_DIR%\launch_%TS%.log"

echo ================================================================ >> "%LOG_FILE%"
echo %APPNAME% Launcher start: %DATE% %TIME% >> "%LOG_FILE%"
echo ROOT=%ROOT% >> "%LOG_FILE%"

REM ------------------------------------------------
REM 1) Find Rscript.exe (prefer bundled portable R)
REM ------------------------------------------------
set "RSCRIPT_PATH=%ROOT%\R\bin\Rscript.exe"

if exist "%RSCRIPT_PATH%" (
  echo [OK] Using bundled portable R: %RSCRIPT_PATH% >> "%LOG_FILE%"
) else (
  set "RSCRIPT_PATH="

  where Rscript.exe >nul 2>nul
  if %errorlevel% equ 0 (
    set "RSCRIPT_PATH=Rscript.exe"
    echo [OK] Found system R in PATH >> "%LOG_FILE%"
  ) else (
    echo [WARN] Bundled R not found and system R not in PATH >> "%LOG_FILE%"
  )
)

if not defined RSCRIPT_PATH (
  echo ================================================================ >> "%LOG_FILE%"
  echo ERROR: Could not find Rscript.exe >> "%LOG_FILE%"
  echo Expected bundled: %ROOT%\R\bin\Rscript.exe >> "%LOG_FILE%"
  echo Or system R on PATH >> "%LOG_FILE%"
  echo ================================================================ >> "%LOG_FILE%"

  echo.
  echo ================================================================
  echo ERROR: R was not found.
  echo ================================================================
  echo This bundle expects portable R at:
  echo   %ROOT%\R\bin\Rscript.exe
  echo If missing, re-download the built artifact zip.
  echo Log: "%LOG_FILE%"
  echo.
  pause
  exit /b 1
)

REM ------------------------------------------------
REM 2) Seed per-user library from shipped r_lib (no admin)
REM ------------------------------------------------
set "SRC_RLIB=%ROOT%\r_lib"
set "DST_RLIB=%LOCALAPPDATA%\%APPNAME%\r_lib"

if not exist "%DST_RLIB%" mkdir "%DST_RLIB%" >nul 2>nul

echo [INFO] SRC_RLIB=%SRC_RLIB% >> "%LOG_FILE%"
echo [INFO] DST_RLIB=%DST_RLIB% >> "%LOG_FILE%"

REM If shipped r_lib exists and user lib looks empty, seed it once
if exist "%SRC_RLIB%" (
  set "HAS_PKGS="
  for /d %%D in ("%DST_RLIB%\*") do (
    set "HAS_PKGS=1"
    goto :after_check
  )
  :after_check

  if not defined HAS_PKGS (
    echo [INFO] Seeding user r_lib from shipped r_lib... >> "%LOG_FILE%"
    robocopy "%SRC_RLIB%" "%DST_RLIB%" /E /NFL /NDL /NJH /NJS /NP >> "%LOG_FILE%" 2>&1
  ) else (
    echo [INFO] User r_lib already populated; skipping seed. >> "%LOG_FILE%"
  )
) else (
  echo [WARN] Shipped r_lib not found at %SRC_RLIB% >> "%LOG_FILE%"
)

REM ------------------------------------------------
REM 3) Set env vars used by launch_app.R
REM ------------------------------------------------
set "BPS_APP_DIR=%ROOT%"
set "BPS_DASH_LIB=%DST_RLIB%"
set "BPS_R_LIB=%DST_RLIB%"
set "R_LIBS_USER=%DST_RLIB%"
set "R_LIBS=%DST_RLIB%"
set "R_ENVIRON_USER="
set "R_PROFILE_USER="

REM ------------------------------------------------
REM 4) Run launch_app.R (no fragile cd assumptions)
REM ------------------------------------------------
set "LAUNCHER=%ROOT%\_BPS_Dashboard_App_Files (do not modify)\launch_app.R"

if not exist "%LAUNCHER%" (
  echo ================================================================ >> "%LOG_FILE%"
  echo ERROR: launch_app.R not found at: %LAUNCHER% >> "%LOG_FILE%"
  echo ================================================================ >> "%LOG_FILE%"

  echo.
  echo ================================================================
  echo ERROR: Could not find launch_app.R
  echo ================================================================
  echo Expected:
  echo   %LAUNCHER%
  echo Make sure you extracted the whole zip before running.
  echo Log: "%LOG_FILE%"
  echo.
  pause
  exit /b 1
)

echo [INFO] Running: "%RSCRIPT_PATH%" --vanilla "%LAUNCHER%" >> "%LOG_FILE%"
"%RSCRIPT_PATH%" --vanilla "%LAUNCHER%" >> "%LOG_FILE%" 2>&1

set "EXITCODE=%ERRORLEVEL%"

echo [INFO] Exit code: %EXITCODE% >> "%LOG_FILE%"

if not "%EXITCODE%"=="0" (
  echo.
  echo ================================================================
  echo The dashboard exited with an error (code %EXITCODE%).
  echo ================================================================
  echo Log file:
  echo   "%LOG_FILE%"
  echo.
  pause
  exit /b %EXITCODE%
)

echo.
echo Dashboard has closed normally.
echo Log file:
echo   "%LOG_FILE%"
echo.
pause
exit /b 0
