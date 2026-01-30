#!/bin/bash
set -e

cd "$(dirname "$0")"
cd "_BPS_Dashboard_App_Files (do not modify)" || { echo "Missing app folder"; exit 1; }

# Prefer standard macOS R install location
RSCRIPT="/Library/Frameworks/R.framework/Resources/bin/Rscript"
if [ -x "$RSCRIPT" ]; then
  :
else
  # Fall back to PATH
  RSCRIPT="$(command -v Rscript || true)"
fi

if [ -z "$RSCRIPT" ] || [ ! -x "$RSCRIPT" ]; then
  echo "ERROR: R is not installed (or Rscript not found)."
  echo "Install R from: https://cran.r-project.org/bin/macosx/"
  read -n 1 -s -r -p "Press any key to exit..."
  exit 1
fi

"$RSCRIPT" --vanilla "launch_app.R"


