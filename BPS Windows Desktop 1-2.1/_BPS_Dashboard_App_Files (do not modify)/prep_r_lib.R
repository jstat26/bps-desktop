# Prep script (CI/build machine)
# Creates / updates the r_lib that you will SHIP inside the bundle.

app_name <- "BPS Dashboard"

# ---------- helper: find directory of this script ----------
get_script_dir <- function() {
  # When run via: Rscript path/to/prep_r_lib.R
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1) {
    script_path <- sub("^--file=", "", file_arg)
    return(normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE))
  }
  
  # Fallback (interactive/source)
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(dirname(sys.frames()[[1]]$ofile), winslash = "/", mustWork = FALSE))
  }
  
  # Last resort
  return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
}

script_dir <- get_script_dir()

# ---------- choose library location ----------
# Prefer CI env var (your workflow sets this)
local_lib <- Sys.getenv("BPS_R_LIB", unset = NA_character_)

# Back-compat: allow BPS_DASH_LIB too
if (is.na(local_lib) || !nzchar(local_lib)) {
  local_lib <- Sys.getenv("BPS_DASH_LIB", unset = NA_character_)
}

# Default: sibling r_lib at the bundle root
# (prep_r_lib.R lives in: <BUNDLE_DIR>/_BPS_Dashboard_App_Files (do not modify)/prep_r_lib.R)
if (is.na(local_lib) || !nzchar(local_lib)) {
  local_lib <- file.path(script_dir, "..", "r_lib")
}

local_lib <- normalizePath(local_lib, winslash = "/", mustWork = FALSE)

dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)

# Put shipped lib FIRST
.libPaths(c(local_lib, .libPaths()))
Sys.setenv(R_LIBS_USER = local_lib)

options(timeout = 600)

if (.Platform$OS.type == "windows") {
  options(pkgType = "win.binary")
  try(options(download.file.method = "wininet"), silent = TRUE)
}

# Use your workflow env if provided, else fall back
repo1 <- Sys.getenv("R_REPO", unset = "https://packagemanager.posit.co/cran/latest")
options(repos = c(
  CRAN  = repo1,
  CRAN2 = "https://cloud.r-project.org"
))

required_packages <- c(
  "shiny","bs4Dash","shinydashboard","shinyWidgets","fresh",
  "DT","plotly","ggiraph","ggtext","patchwork","viridis","scales","gridExtra",
  "data.table","dplyr","tidyr","tibble","tidyselect","purrr","stringr","ggplot2","readr","forcats","magrittr",
  "readxl","openxlsx","Matrix","glue","jsonlite","htmltools",
  "lubridate","later","calendar",
  "shinymanager","blastula","sodium","digest","uuid",
  "qs","gtools","truncnorm","fGarch","shinyscreenshot","googledrive"
)

message("Installing into: ", local_lib)
message("LibPaths:\n", paste0(" - ", .libPaths(), collapse = "\n"))

installed <- tryCatch(rownames(installed.packages(lib.loc = local_lib)),
                      error = function(e) character(0))
missing <- setdiff(required_packages, installed)

if (length(missing)) {
  message("Missing packages (", length(missing), "): ", paste(missing, collapse = ", "))
  install.packages(missing, lib = local_lib, dependencies = TRUE)
}

# Verify again — fail CI if anything still missing
installed2 <- tryCatch(rownames(installed.packages(lib.loc = local_lib)),
                       error = function(e) character(0))
missing2 <- setdiff(required_packages, installed2)

if (length(missing2)) {
  message("\nFAILED to install (", length(missing2), "): ", paste(missing2, collapse = ", "))
  quit(status = 2)
}

message("\nDone. Installed packages in lib (sample):")
print(head(sort(installed2), 50))