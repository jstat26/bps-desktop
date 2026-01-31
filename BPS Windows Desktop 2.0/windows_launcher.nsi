!include "MUI2.nsh"

Name "BPS Dashboard"
OutFile "BPS_Dashboard.exe"
Icon "bps_dashboard.ico"
RequestExecutionLevel user
Unicode True

# No install UI; just launch
SilentInstall silent
AutoCloseWindow true

Section
  # Work out where the EXE is running from
  StrCpy $0 "$EXEDIR"

  # Expect the bundle folder to be alongside the EXE as:
  #   $EXEDIR\<BUNDLE_DIR>\
  # We pass BUNDLE_DIR in via /D when running makensis.

  IfFileExists "$0\${BUNDLE_DIR}\R\bin\Rscript.exe" ok missing

  missing:
    MessageBox MB_ICONSTOP "Missing bundle contents. Please extract the ZIP fully and keep the folder '${BUNDLE_DIR}' next to this launcher."
    Quit

  ok:
    # Launch your existing .bat in the bundle folder.
    # (If you prefer, you can directly run Rscript launch_app.R instead.)
    Exec '"$0\${BUNDLE_DIR}\BPS Dashboard (Windows).bat"'
SectionEnd
