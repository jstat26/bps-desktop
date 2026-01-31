@echo off
REM BPS Dashboard Launcher - Manual R Path Version
REM Use this file if the automatic launcher cannot find R

echo ================================================================
echo BPS Dashboard Launcher (Manual R Path)
echo ================================================================
echo.
echo This launcher requires you to specify where R is installed.
echo.

REM Prompt user for R installation path
set /p "R_PATH=Enter the path to your R installation (e.g., C:\Program Files\R\R-4.3.2): "

REM Remove quotes if user added them
set "R_PATH=%R_PATH:"=%"

REM Try to find Rscript.exe in the provided path
if exist "%R_PATH%\bin\Rscript.exe" (
    set "RSCRIPT_PATH=%R_PATH%\bin\Rscript.exe"
    goto :found_r
)

if exist "%R_PATH%\bin\x64\Rscript.exe" (
    set "RSCRIPT_PATH=%R_PATH%\bin\x64\Rscript.exe"
    goto :found_r
)

REM Try direct path if they specified bin folder
if exist "%R_PATH%\Rscript.exe" (
    set "RSCRIPT_PATH=%R_PATH%\Rscript.exe"
    goto :found_r
)

echo.
echo ERROR: Could not find Rscript.exe in the specified path.
echo Please check the path and try again.
echo.
pause
exit /b 1

:found_r
echo [OK] Found Rscript at: %RSCRIPT_PATH%
echo.

REM Go to the folder this script lives in
cd /d "%~dp0"

REM Go into the internal app folder
cd "_BPS_Dashboard_App_Files (do not modify)"
if %errorlevel% neq 0 (
    echo ERROR: Could not find dashboard files folder.
    pause
    exit /b 1
)

echo Starting BPS Dashboard...
echo.
echo NOTE: Keep this window open while using the dashboard.
echo       Close the browser tab when finished.
echo.
echo ================================================================
echo.

REM Run the launcher
"%RSCRIPT_PATH%" --vanilla "launch_app.R"

if %errorlevel% neq 0 (
    echo.
    echo An error occurred.
    pause
)
