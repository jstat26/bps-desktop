@echo off
REM R Installation Diagnostic Tool
REM This will help find where R is installed on your system

echo ================================================================
echo R Installation Diagnostic Tool
echo ================================================================
echo.

echo Checking if Rscript is in PATH...
where Rscript.exe 2>nul
if %errorlevel% equ 0 (
    echo [FOUND] Rscript is in your system PATH
    echo.
) else (
    echo [NOT FOUND] Rscript is NOT in your system PATH
    echo.
)

echo ----------------------------------------------------------------
echo Searching common installation locations...
echo ----------------------------------------------------------------
echo.

set "FOUND_ANY=0"

REM Check Program Files
if exist "C:\Program Files\R" (
    echo [FOUND] C:\Program Files\R
    dir /b /ad "C:\Program Files\R\R-*" 2>nul
    set "FOUND_ANY=1"
) else (
    echo [NOT FOUND] C:\Program Files\R
)
echo.

REM Check Program Files (x86)
if exist "C:\Program Files (x86)\R" (
    echo [FOUND] C:\Program Files ^(x86^)\R
    dir /b /ad "C:\Program Files (x86)\R\R-*" 2>nul
    set "FOUND_ANY=1"
) else (
    echo [NOT FOUND] C:\Program Files ^(x86^)\R
)
echo.

REM Check Local AppData
if exist "%LOCALAPPDATA%\Programs\R" (
    echo [FOUND] %LOCALAPPDATA%\Programs\R
    dir /b /ad "%LOCALAPPDATA%\Programs\R\R-*" 2>nul
    set "FOUND_ANY=1"
) else (
    echo [NOT FOUND] %LOCALAPPDATA%\Programs\R
)
echo.

REM Check if R is installed via Scoop
if exist "%USERPROFILE%\scoop\apps\r" (
    echo [FOUND] %USERPROFILE%\scoop\apps\r ^(Scoop installation^)
    dir /b /ad "%USERPROFILE%\scoop\apps\r\*" 2>nul
    set "FOUND_ANY=1"
) else (
    echo [NOT FOUND] Scoop installation
)
echo.

REM Check if R is installed via Chocolatey
if exist "C:\ProgramData\chocolatey\lib\R.Project" (
    echo [FOUND] Chocolatey R installation
    set "FOUND_ANY=1"
) else (
    echo [NOT FOUND] Chocolatey installation
)
echo.

echo ----------------------------------------------------------------
echo Manual Search Instructions
echo ----------------------------------------------------------------
echo.
echo If R was not found above, try these steps:
echo.
echo 1. Open File Explorer
echo 2. Search for "Rscript.exe" on your C: drive
echo 3. Note the full path where it's located
echo 4. Look for the parent folder structure like: R\R-4.x.x\bin
echo.
echo Common non-standard locations to check:
echo    - D:\Program Files\R
echo    - C:\R
echo    - Your Documents folder
echo    - Custom installation directories
echo.
echo ----------------------------------------------------------------
echo.

if "%FOUND_ANY%"=="0" (
    echo RESULT: No R installations found in standard locations.
    echo         R may be installed in a custom location.
    echo         Use the manual search instructions above.
) else (
    echo RESULT: R installation^(s^) found!
    echo         See paths listed above.
)

echo.
echo ================================================================
echo Press any key to close this window...
pause >nul
