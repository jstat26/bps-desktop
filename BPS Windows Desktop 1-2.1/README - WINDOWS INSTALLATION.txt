BPS Dashboard for Windows
==========================

STEP 1: INSTALL R (REQUIRED)
-----------------------------
Before running the dashboard, you MUST install R on your Windows computer.

**If R is already installed but not found:**
   The launcher may not find R if it's not in your system PATH or installed in a non-standard location.
   
   TO FIND YOUR R INSTALLATION:
   1. Run the included "FIND R INSTALLATION.bat" diagnostic tool, OR
   2. Click Start menu and search for "R" - right-click the R icon and "Open file location", OR
   3. If you have RStudio, open it and type: R.home()
   4. Search your C: drive for "Rscript.exe" in File Explorer
   
   Once you find R, use the Manual launcher and enter the path.

**If R is NOT installed yet:**
1. Go to: https://cran.r-project.org/bin/windows/base/
2. Click "Download R-X.X.X for Windows" (latest version)
3. Run the installer and use these settings:
   - Accept the default installation location (typically C:\Program Files\R\R-X.X.X)
   - Select "Install for all users" if prompted
   - Keep all default options checked
4. Click "Finish" when installation completes
5. **RESTART YOUR COMPUTER** after installing R

Optional: Install RStudio for editing/development
   - Download from: https://posit.co/download/rstudio-desktop/


STEP 2: CHOOSE A LAUNCHER
--------------------------
Two launcher files are included:

OPTION A (RECOMMENDED): "BPS Dashboard (Windows).bat"
   - Automatically searches for R installation
   - Best for most users
   - Try this one first!

OPTION B: "BPS Dashboard (Windows) - Manual.bat"
   - Use this if Option A cannot find R
   - You will be prompted to enter your R installation path
   - Example path: C:\Program Files\R\R-4.3.2


STEP 3: START THE DASHBOARD
----------------------------
1. Double-click one of the launcher files (try Option A first)
2. A command window will open showing:
   - Search for R installation
   - Package installation progress (first time only - may take 5-10 minutes)
   - Dashboard startup messages
3. Your default web browser will automatically open with the dashboard
4. **IMPORTANT:** Keep the command window open while using the dashboard


WHILE USING THE DASHBOARD
--------------------------
• The dashboard runs in your web browser
• Do NOT close the command window (black console window)
• The command window must stay open for the dashboard to work


HOW TO CLOSE THE DASHBOARD
---------------------------
1. Close the browser tab/window with the dashboard
2. The command window will automatically close after a few seconds
3. If needed, you can manually close the command window


TROUBLESHOOTING
---------------

Problem: "R is not installed or cannot be found"
Solution: 
  1. Make sure R is installed (see STEP 1 above)
  2. Restart your computer after installing R
  3. Try the Manual launcher (Option B) and enter your R path
  4. To find your R installation path:
     - Look in: C:\Program Files\R\
     - You should see folders like "R-4.3.2" or similar
     - The full path would be: C:\Program Files\R\R-4.3.2

Problem: Automatic launcher cannot find R, but R is installed
Solution:
  1. Use the Manual launcher: "BPS Dashboard (Windows) - Manual.bat"
  2. When prompted, enter the full path to your R installation
  3. Example: C:\Program Files\R\R-4.3.2
  4. Do NOT include "\bin" or "\bin\x64" - just the main R folder

Problem: "Rscript is not recognized as an internal or external command"
Solution: This means R is not in your system PATH. Use the Manual launcher instead.

Problem: Dashboard doesn't open in browser
Solution: 
  1. Look at the command window for the local address (usually http://127.0.0.1:####)
  2. Manually copy and paste this address into your web browser

Problem: Package installation fails
Solution:
  1. Check your internet connection (packages download from CRAN)
  2. Try running the launcher as Administrator (right-click → "Run as administrator")
  3. Close all other R/RStudio windows before running the launcher
  4. Update R to the latest version if yours is very old

Problem: "Could not find dashboard files folder"
Solution:
  1. Make sure you extracted the ENTIRE zip file, not just the .bat file
  2. Keep the .bat file in the same folder as "_BPS_Dashboard_App_Files (do not modify)"
  3. Do not move or rename any files or folders


FOLDER STRUCTURE
----------------
DO NOT modify or move files inside the "_BPS_Dashboard_App_Files (do not modify)" folder.
The dashboard requires all files to remain in their original locations.


SUPPORT
-------
For technical support or questions, contact your IT administrator or the dashboard developer.
