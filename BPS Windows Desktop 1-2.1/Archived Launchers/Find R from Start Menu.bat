@echo off
REM Find R by checking Start Menu shortcuts

echo ================================================================
echo Finding R Installation from Start Menu Shortcuts
echo ================================================================
echo.

set "STARTMENU_R=C:\ProgramData\Microsoft\Windows\Start Menu\Programs\R"
set "STARTMENU_USER=%APPDATA%\Microsoft\Windows\Start Menu\Programs\R"

echo Checking Start Menu shortcuts...
echo.

REM Check system-wide Start Menu
if exist "%STARTMENU_R%" (
    echo Found R shortcuts in: %STARTMENU_R%
    echo.
    echo Shortcut files:
    dir /b "%STARTMENU_R%\*.lnk" 2>nul
    echo.
    echo To find the actual R installation:
    echo 1. Navigate to: %STARTMENU_R%
    echo 2. Right-click on any R shortcut
    echo 3. Select "Properties"
    echo 4. Look at the "Target:" field
    echo 5. Copy the path up to (but NOT including) "\bin\"
    echo.
    echo Example: If Target shows:
    echo "C:\Program Files\R\R-4.3.2\bin\x64\Rgui.exe"
    echo.
    echo Then your R installation path is:
    echo C:\Program Files\R\R-4.3.2
    echo.
)

REM Check user Start Menu
if exist "%STARTMENU_USER%" (
    echo Found R shortcuts in: %STARTMENU_USER%
    dir /b "%STARTMENU_USER%\*.lnk" 2>nul
    echo.
)

REM Try to extract target from shortcut using PowerShell
echo Attempting to read shortcut target automatically...
echo.

powershell -Command "$sh = New-Object -ComObject WScript.Shell; Get-ChildItem '%STARTMENU_R%\*.lnk' -ErrorAction SilentlyContinue | ForEach-Object { $target = $sh.CreateShortcut($_.FullName).TargetPath; if ($target -match 'R\\R-[0-9.]+') { Write-Host 'Found R at:' $matches[0] } }" 2>nul

echo.
echo ================================================================
echo.
pause
