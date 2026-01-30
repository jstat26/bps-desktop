# Prep script (CI/build machine)
# Creates / updates the r_lib that you will SHIP inside the bundle.

app_name <- "BPS Dashboard"

# ---------- helper: find directory of this script ----------
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1) {
    script_path <- sub("^--file=", "", file_arg)
    return(normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(dirname(sys.frames()[[1]]$ofile), winslash = "/", mustWork = FALSE))
  }
  return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
}

script_dir <- get_script_dir()

# ---------- choose library location ----------
local_lib <- Sys.getenv("BPS_R_LIB", unset = NA_character_)
if (is.na(local_lib) || !nzchar(local_lib)) {
  local_lib <- Sys.getenv("BPS_DASH_LIB", unset = NA_character_)
}
if (is.na(local_lib) || !nzchar(local_lib)) {
  # Default: sibling r_lib at the bundle root
  # (prep_r_lib.R lives in: <BUNDLE_DIR>/_BPS_Dashboard_App_Files (do not modify)/prep_r_lib.R)
  local_lib <- file.path(script_dir, "..", "r_lib")
}

local_lib <- normalizePath(local_lib, winslash = "/", mustWork = FALSE)
dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)

# Put shipped lib FIRST
.libPaths(c(local_lib, .libPaths()))
Sys.setenv(R_LIBS_USER = local_lib)

options(timeout = 600)

# ---- Windows: force binaries only ----
if (.Platform$OS.type == "windows") {
  options(pkgType = "win.binary")
  # wininet is deprecated but leaving this doesn't break; you can remove if you want
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
  "qs2","gtools","truncnorm","fGarch","shinyscreenshot","googledrive"
)

message("Installing into: ", local_lib)
message("LibPaths:\n", paste0(" - ", .libPaths(), collapse = "\n"))
message("Repos:\n"); print(getOption("repos"))

installed <- tryCatch(rownames(installed.packages(lib.loc = local_lib)),
                      error = function(e) character(0))
missing <- setdiff(required_packages, installed)

# ---- helper: install packages with captured warnings ----
install_with_log <- function(pkgs) {
  if (!length(pkgs)) return(invisible(TRUE))
  
  w <- character(0)
  withCallingHandlers(
    {
      if (.Platform$OS.type == "windows") {
        install.packages(pkgs, lib = local_lib, dependencies = TRUE, type = "win.binary")
      } else {
        install.packages(pkgs, lib = local_lib, dependencies = TRUE)
      }
    },
    warning = function(m) {
      w <<- c(w, conditionMessage(m))
      invokeRestart("muffleWarning")
    }
  )
  
  if (length(w)) {
    message("---- INSTALL WARNINGS (first 200) ----")
    print(head(unique(w), 200))
  }
  
  invisible(TRUE)
}

# ---- install missing ----
if (length(missing)) {
  message("Missing packages (", length(missing), "): ", paste(missing, collapse = ", "))
  install_with_log(missing)
}

# ---- final verification: presence check ----
installed2 <- tryCatch(rownames(installed.packages(lib.loc = local_lib)),
                       error = function(e) character(0))
missing2 <- setdiff(required_packages, installed2)

if (length(missing2)) {
  message("\nFAILED to install (", length(missing2), "): ", paste(missing2, collapse = ", "))
  quit(status = 2)
}

# ---- optional: load-test a few critical packages to catch DLL/runtime issues ----
load_test <- c("shiny", "bs4Dash", "DT", "plotly", "dplyr", "readr", "readxl", "lubridate", "qs2")
message("\nLoad test (quick): ", paste(load_test, collapse = ", "))

ok <- sapply(load_test, function(p) {
  tryCatch({
    suppressPackageStartupMessages(library(p, character.only = TRUE, lib.loc = local_lib))
    TRUE
  }, error = function(e) {
    message("FAILED to load ", p, ": ", conditionMessage(e))
    FALSE
  })
})

if (any(!ok)) {
  message("\nFAILED load test packages: ", paste(names(ok)[!ok], collapse = ", "))
  quit(status = 2)
}

message("\nDone. Installed packages in lib (sample):")
print(head(sort(installed2), 50))
