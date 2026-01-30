#############################################
# BPS Dashboard - Launcher (portable-R friendly)
# - uses per-user library (no admin)
# - can seed from shipped r_lib (offline-friendly)
# - logs to %APPDATA%\BPS Dashboard\
#############################################

app_name <- "BPS Dashboard"
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# --------- logging ----------
log_dir <- tryCatch({
  if (.Platform$OS.type == "windows") {
    file.path(Sys.getenv("APPDATA"), app_name)
  } else {
    file.path(path.expand("~"), "Library", "Application Support", app_name)
  }
}, error = function(e) ".")

dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(log_dir, paste0("launch_", timestamp, ".log"))

log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = ""))
  cat(msg, "\n")
  cat(msg, "\n", file = log_file, append = TRUE)
}

# --------- helper: directory of this script ----------
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1) {
    script_path <- sub("^--file=", "", file_arg)
    return(normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE))
  }
  # fallback
  return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
}

script_dir <- get_script_dir()

# --------- bundle root + app dir ----------
app_root <- Sys.getenv("BPS_APP_DIR", unset = "")
if (!nzchar(app_root)) {
  # If someone runs this directly, assume bundle root is parent of this script folder
  app_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
} else {
  app_root <- normalizePath(app_root, winslash = "/", mustWork = FALSE)
}

app_dir <- file.path(app_root, "_BPS_Dashboard_App_Files (do not modify)")
app_r   <- file.path(app_dir, "app.R")

# --------- per-user library ----------
local_lib <- Sys.getenv("BPS_DASH_LIB", unset = "")
if (!nzchar(local_lib)) {
  if (.Platform$OS.type == "windows") {
    local_lib <- file.path(Sys.getenv("LOCALAPPDATA"), app_name, "r_lib")
  } else {
    local_lib <- normalizePath(file.path(app_root, "r_lib"), winslash = "/", mustWork = FALSE)
  }
}
local_lib <- normalizePath(local_lib, winslash = "/", mustWork = FALSE)
dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)

# Put local lib FIRST
.libPaths(c(local_lib, .libPaths()))
Sys.setenv(R_LIBS_USER = local_lib)

# --------- basic diagnostics ----------
log_msg("Starting launcher")
log_msg("script_dir: ", script_dir)
log_msg("app_root:   ", app_root)
log_msg("app_dir:    ", app_dir)
log_msg("app_r:      ", app_r)
log_msg("R version:  ", paste(R.version$major, R.version$minor, sep = "."))
log_msg("LibPaths:   ", paste(.libPaths(), collapse = " | "))

if (!file.exists(app_r)) {
  stop(
    "Could not find app.R at:\n  ", app_r, "\n\n",
    "Check that you extracted the full zip and did not rename folders.",
    call. = FALSE
  )
}

# --------- seed from shipped r_lib if user lib empty ----------
seed_from <- file.path(app_root, "r_lib")
needs_seed <- FALSE
if (dir.exists(seed_from)) {
  pkgs_in_user <- list.dirs(local_lib, full.names = FALSE, recursive = FALSE)
  if (length(pkgs_in_user) == 0) needs_seed <- TRUE
}

if (needs_seed) {
  log_msg("Seeding local_lib from shipped r_lib: ", seed_from)
  if (.Platform$OS.type == "windows") {
    # Use robocopy when available (faster + reliable)
    rc <- suppressWarnings(system2("robocopy", c(shQuote(seed_from), shQuote(local_lib), "/E", "/NFL", "/NDL", "/NJH", "/NJS", "/NP")))
    log_msg("robocopy exit code: ", rc)
  } else {
    # mac/unix fallback
    files <- list.files(seed_from, full.names = TRUE, recursive = TRUE)
    dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(files, file.path(local_lib, sub(paste0("^", seed_from, "/?"), "", files)), overwrite = TRUE)
    log_msg("seed file.copy ok count: ", sum(ok, na.rm = TRUE))
  }
}

# --------- CRAN / download behavior ----------
options(timeout = 900)
if (.Platform$OS.type == "windows") {
  options(download.file.method = "wininet")
  options(pkgType = "win.binary")
}

repo1 <- Sys.getenv("R_REPO", unset = "")
if (!nzchar(repo1)) repo1 <- "https://packagemanager.posit.co/cran/latest"

options(repos = c(
  CRAN  = repo1,
  CRAN2 = "https://cloud.r-project.org"
))

# --------- required packages (explicit list) ----------
required_packages <- c(
  "shiny", "bs4Dash", "shinydashboard", "shinyWidgets", "DT", "fresh",
  "shinymanager", "blastula", "sodium", "digest", "uuid",
  "data.table", "dplyr", "tidyr", "tibble", "tidyselect",
  "ggplot2", "readr", "stringr", "purrr", "forcats", "magrittr",
  "htmltools", "glue", "jsonlite",
  "plotly", "ggiraph", "ggtext", "patchwork", "viridis", "scales", "gtools",
  "lubridate", "later", "calendar",
  "qs", "readxl", "openxlsx",
  "fGarch", "truncnorm", "Matrix", "gridExtra", "shinyscreenshot",
  "Rcpp"
)

install_missing <- function(pkgs) {
  installed_local <- tryCatch(
    rownames(installed.packages(lib.loc = local_lib)),
    error = function(e) character(0)
  )
  missing <- setdiff(pkgs, installed_local)
  
  if (!length(missing)) {
    log_msg("All required packages already installed.")
    return(invisible(TRUE))
  }
  
  log_msg("Missing packages (", length(missing), "): ", paste(missing, collapse = ", "))
  
  # install in small chunks
  chunks <- split(missing, ceiling(seq_along(missing) / 8))
  
  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    log_msg("Installing chunk ", i, "/", length(chunks), ": ", paste(chunk, collapse = ", "))
    
    ok <- FALSE
    for (attempt in 1:3) {
      log_msg("  Attempt ", attempt, "...")
      while (sink.number(type = "output")  > 0) sink(NULL, type = "output")
      while (sink.number(type = "message") > 0) sink(NULL, type = "message")
      
      out_con <- file(log_file, open = "at", encoding = "UTF-8")
      msg_con <- file(log_file, open = "at", encoding = "UTF-8")
      
      ok <- tryCatch({
        sink(out_con, type = "output")
        sink(msg_con, type = "message")
        
        install.packages(
          chunk,
          lib = local_lib,
          dependencies = TRUE,
          quiet = TRUE,
          type = if (.Platform$OS.type == "windows") "binary" else getOption("pkgType")
        )
        
        TRUE
      }, error = function(e) {
        log_msg("  Install error: ", conditionMessage(e))
        FALSE
      }, finally = {
        try(sink(NULL, type = "message"), silent = TRUE)
        try(sink(NULL, type = "output"),  silent = TRUE)
        try(close(msg_con), silent = TRUE)
        try(close(out_con), silent = TRUE)
      })
      
      if (isTRUE(ok)) break
      Sys.sleep(2)
    }
    
    if (!isTRUE(ok)) {
      stop(
        "Package installation failed while installing: ", paste(chunk, collapse = ", "), "\n",
        "Log file: ", log_file,
        call. = FALSE
      )
    }
  }
  
  # final verify
  installed2 <- tryCatch(rownames(installed.packages(lib.loc = local_lib)), error = function(e) character(0))
  missing2 <- setdiff(pkgs, installed2)
  if (length(missing2)) {
    stop(
      "These packages could not be installed: ", paste(missing2, collapse = ", "), "\n",
      "Common causes: district blocks downloads / SSL inspection / proxy.\n",
      "Log file: ", log_file,
      call. = FALSE
    )
  }
  
  log_msg("Package install complete.")
  invisible(TRUE)
}

install_missing(required_packages)

# --------- launch the app ----------
log_msg("Launching app...")

message("--------------------------------------------------")
message("BPS Dashboard is starting...")
message("A web browser window or tab will open shortly.")
message("")
message("IMPORTANT:")
message("  • Leave this window open while using the dashboard.")
message("  • When you're finished, simply CLOSE the browser tab.")
message("The dashboard will shut down automatically.")
message("--------------------------------------------------")

tryCatch({
  shiny::runApp(appDir = app_dir, launch.browser = TRUE, host = "127.0.0.1", port = 0)
}, error = function(e) {
  log_msg("ERROR while running app: ", conditionMessage(e))
  stop(e)
})

message("--------------------------------------------------")
message("BPS Dashboard has shut down.")
message("You can now safely CLOSE this window.")
message("--------------------------------------------------")
