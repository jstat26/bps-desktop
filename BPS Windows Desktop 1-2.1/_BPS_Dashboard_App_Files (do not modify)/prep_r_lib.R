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
  try(options(download.file.method = "wininet"), silent = TRUE)
}

# Use your workflow env if provided, else fall back
repo1 <- Sys.getenv("R_REPO", unset = "https://packagemanager.posit.co/cran/latest")

# IMPORTANT: Put CRAN *second* as a fallback, but we will force binaries anyway on Windows
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
  
  # Capture warnings so they show in GH Actions logs
  w <- character(0)
  withCallingHandlers(
    {
      # On Windows we force binaries
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

# ---- install everything except qs first (qs gets special handling) ----
if (length(missing)) {
  message("Missing packages (", length(missing), "): ", paste(missing, collapse = ", "))
  missing_no_qs <- setdiff(missing, "qs")
  install_with_log(missing_no_qs)
}

# ---- install qs explicitly with retries + binary-only ----
install_qs_binary <- function() {
  if (requireNamespace("qs", quietly = TRUE, lib.loc = local_lib)) return(TRUE)
  
  for (i in 1:4) {
    message("Installing qs (binary-only) attempt ", i, "/4 ...")
    ok <- TRUE
    
    # capture warnings + errors
    w <- character(0)
    res <- tryCatch(
      withCallingHandlers(
        {
          install.packages("qs", lib = local_lib,
                           dependencies = TRUE,
                           type = if (.Platform$OS.type == "windows") "win.binary" else getOption("pkgType"))
          TRUE
        },
        warning = function(m) {
          w <<- c(w, conditionMessage(m))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        message("qs install ERROR: ", conditionMessage(e))
        ok <<- FALSE
        FALSE
      }
    )
    
    if (length(w)) {
      message("qs install WARNINGS (first 50):")
      print(head(w, 50))
    }
    
    # verify it exists
    inst <- tryCatch(rownames(installed.packages(lib.loc = local_lib)), error = function(e) character(0))
    if (!("qs" %in% inst)) {
      message("qs not present in installed.packages() after attempt ", i)
      ok <- FALSE
    }
    
    # verify it loads (DLL issues show here)
    if (ok) {
      ok_load <- tryCatch(
        {
          library(qs, lib.loc = local_lib, character.only = TRUE)
          TRUE
        },
        error = function(e) {
          message("qs installed but failed to load: ", conditionMessage(e))
          FALSE
        }
      )
      if (ok_load) {
        message("qs installed and loaded OK")
        return(TRUE)
      }
    }
    
    Sys.sleep(5)
  }
  
  stop("FAILED: qs could not be installed/loaded as a binary after 4 attempts.")
}

install_qs_binary()

# ---- final verification: presence check ----
installed2 <- tryCatch(rownames(installed.packages(lib.loc = local_lib)),
                       error = function(e) character(0))
missing2 <- setdiff(required_packages, installed2)

if (length(missing2)) {
  message("\nFAILED to install (", length(missing2), "): ", paste(missing2, collapse = ", "))
  quit(status = 2)
}

message("\nDone. Installed packages in lib (sample):")
print(head(sort(installed2), 50))
