########################################### 
# BPS Analytics Dashboard with Email Reset 
########################################### 

# This is V1.0 created on 1.14.2026 

##########################################
# Single vs. Multi-Tribe Mode Activation
##########################################

# ===== Single-Tribe Mode (set and forget) =====
SINGLE_TRIBE <- FALSE
DEFAULT_TRIBE <- "Bps"

# ---- Do not touch these ----
# recode and guard: require DEFAULT_TRIBE when SINGLE_TRIBE = TRUE 
DEFAULT_TRIBE_banner<-ifelse(DEFAULT_TRIBE=="Kewa", "Santo Domingo", DEFAULT_TRIBE)
stopifnot(!SINGLE_TRIBE || nchar(DEFAULT_TRIBE) > 0)
if (isTRUE(SINGLE_TRIBE) && (is.na(DEFAULT_TRIBE) || !nzchar(trimws(DEFAULT_TRIBE)))) {
  stop("SINGLE_TRIBE = TRUE but DEFAULT_TRIBE is missing or blank. Set DEFAULT_TRIBE to a valid tribe name.", call. = FALSE)
}

############
# Packages 
############

list.of.packages <- c("shiny", "dplyr", "tidyr", "ggplot2", "scales", "plotly", "readxl", "tidyverse", 
                      "gtools", "fGarch", "stringr", "truncnorm", "openxlsx", "Matrix", "data.table", "grid", 
                      "gridExtra", "shinyscreenshot", "shinydashboard", "DT", "bs4Dash", "shinyWidgets", 
                      "shinymanager", "blastula", "sodium", "digest", "uuid", "glue", "scales", "calendar", 
                      "ggtext","ggiraph","qs2", "googledrive", "jsonlite", "fresh")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# Packages are installed by launch_app.R (district-proof launcher)
missing <- list.of.packages[!vapply(list.of.packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop(
    "Missing R packages: ", paste(missing, collapse = ", "),
    "\n\nPlease re-run the Windows launcher. If this persists, your district may be blocking CRAN downloads.",
    call. = FALSE
  )
}

invisible(lapply(list.of.packages, library, character.only = TRUE))

## --- launcher-safe CRAN settings (avoids install failures under Rscript) ---
repos <- getOption("repos")
if (is.null(repos) || isTRUE(repos["CRAN"] == "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}
if (.Platform$OS.type == "windows") {
  options(pkgType = "win.binary")
}

library(shinydashboard)
library(bs4Dash)
library(fresh)
library(shinyWidgets)
library(DT)
library(shinymanager)
library(shiny)
library(blastula)
library(sodium)
library(digest)
library(uuid)
library(later)
library(scales)
library(calendar)
library(lubridate)
library(ggtext)
library(ggiraph)

# Ensure qs is available (district machines often block the first install attempt)
# if (!requireNamespace("qs", quietly = TRUE)) {
#   repos <- getOption("repos")
#   if (is.null(repos) || isTRUE(repos["CRAN"] == "@CRAN@")) options(repos = c(CRAN = "https://cloud.r-project.org"))
#   if (.Platform$OS.type == "windows") options(pkgType = "win.binary")
#   suppressWarnings(try(install.packages("qs", dependencies = TRUE, quiet = TRUE), silent = TRUE))
# }
# if (!requireNamespace("qs", quietly = TRUE)) {
#   stop("Required package 'qs' is missing and could not be installed. Common causes: district blocks CRAN downloads / SSL inspection / proxy. Ask IT to allow https://cloud.r-project.org or ship an offline r_lib bundle.", call. = FALSE)
# }
# library(qs)

# New addition for OneDrive 
#library(Microsoft365R)
library(googledrive)
library(jsonlite)  # only needed if you use JSON; safe to include

# --------------------------------------------------
# OneDrive (personal / family account) helpers
# --------------------------------------------------

# get_onedrive <- function() {
#   Microsoft365R::get_personal_onedrive(
#     scopes = c("Files.Read", "offline_access")
#   )
# }
# 
# download_shared_folder_to_temp <- function(shared_url) {
#   
#   od <- get_onedrive()
#   
#   # Resolve the shared folder URL (works for 1drv.ms links)
#   folder <- od$get_item(shared_url)
#   
#   staging_dir <- file.path(
#     tempdir(),
#     paste0("bps_raw_", as.integer(Sys.time()))
#   )
#   dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
#   
#   items <- folder$list_items()
#   
#   for (it in items) {
#     if (!it$is_file()) next
#     it$download(file.path(staging_dir, it$properties$name))
#   }
#   
#   staging_dir
# }

# --------------------------------------------------
# Public link helper: download latest cache.qs
# (No Google auth required — file is "Anyone with link")
# --------------------------------------------------

# 1) Put your DIRECT download URL here (uc?export=download&id=...)
BPS_CACHE_QS_URL <- "https://drive.google.com/uc?export=download&id=1uKUqOqFwwxTte0VKeX2FR1fcdO_S6_vi"

download_cache_qs_public <- function(url = BPS_CACHE_QS_URL,
                                     dest = "./cache.qs") {
  # Download to a temp file first, then move into place (prevents partial/corrupt cache on bad wifi)
  tmp <- paste0(dest, ".tmp")
  
  # Be a bit more tolerant of slow connections
  old_timeout <- getOption("timeout")
  options(timeout = max(300, old_timeout %||% 60))
  on.exit(options(timeout = old_timeout), add = TRUE)
  
  utils::download.file(url = url, destfile = tmp, mode = "wb", quiet = TRUE)
  
  # Basic safety check
  if (!file.exists(tmp) || file.info(tmp)$size < 1000) {
    if (file.exists(tmp)) unlink(tmp)
    stop("Download succeeded but the file looks too small — check the sharing link.")
  }
  
  file.rename(tmp, dest)
  invisible(dest)
}

check_for_new_data <- function() {
  # returns TRUE if Drive file is newer than local cache
  
  drive_file <- drive_get(as_id(GDRIVE_FILE_ID))
  
  drive_modified <- drive_file$drive_resource[[1]]$modifiedTime
  drive_modified <- as.POSIXct(drive_modified, tz = "UTC")
  
  local_modified <- file.info(LOCAL_QS_PATH)$mtime
  
  if (is.na(local_modified)) return(TRUE)
  
  drive_modified > local_modified
}

############################
# User-defined functions 
############################

# ---- Tooltip helper (works with bs4Dash) for hover over descriptions ----
tooltip_span <- function(label, tip) {
  as.character(htmltools::tags$span(
    label,
    `data-toggle`    = "tooltip",     # Bootstrap 4 compat
    #`data-bs-toggle` = "tooltip",     # Bootstrap 5 compat (harmless here)
    `data-tip`       = tip,           # <-- store tip here, NOT in title
    `data-html` = "true",
    # no title attr here (prevents native tooltip)
    class            = "tip-target",
    style            = "cursor: help;"
  ))
}

# ---- Reusable helper for a titled tooltip label for hover over title descriptions ----
tt_title <- function(text, tip) {
  # uses your tooltip_span() so it’s wired into the On/Off system
  htmltools::HTML(tooltip_span(text, tip))
}

# ---- Reusable Non-Native race filter with Reset button (top-level) ----
nn_race_filter_ui <- function(id) {
  div(
    class = "nn-filter",
    tags$label("Race Filter (Non-Native):", class = "nn-filter-label"),
    shinyWidgets::pickerInput(
      inputId = id,
      label   = NULL,
      choices = character(0),   # server will populate
      selected= NULL,           # NULL = All (no filter)
      multiple= FALSE,
      options = list(
        `live-search` = TRUE,
        title         = "All (no filter)",
        size          = 8,
        `dropup-auto` = FALSE,
        style         = "btn-light"
      ),
      width = "260px"
    ),
    actionButton(
      inputId = paste0(id, "_reset"),
      label   = tagList(icon("undo"), span("Reset to All")),
      class   = "btn btn-sm btn-outline-secondary"
    )
  )
}

# function for converting data to a certain type to keep it clean 
to_utf8_df <- function(df) {
  if (!is.data.frame(df)) return(df)
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (nm in chr_cols) {
    v <- df[[nm]]
    was_na <- is.na(v)
    
    # first try: locale -> UTF-8
    v2 <- iconv(v, from = "", to = "UTF-8", sub = "\uFFFD")
    
    # fallback try for still-bad non-NA entries (common in CSVs from Windows)
    bad <- !was_na & is.na(v2)
    if (any(bad)) {
      v2[bad] <- iconv(v[bad], from = "latin1", to = "UTF-8", sub = "\uFFFD")
    }
    
    # keep original NAs as NA (do NOT turn into "")
    v2[was_na] <- NA_character_
    df[[nm]] <- v2
  }
  df
}

################################################
# Set up pieces needed for email password reset 
################################################

# Set environment variables (for blastula SMTP)
Sys.setenv(MY_GMAIL_ACCOUNT = "doctorj@healingharveststrategies.org")
Sys.setenv(SMTP_PASSWORD = "ajma dcpr pwtv tsmm")

email_from <- Sys.getenv("MY_GMAIL_ACCOUNT")
my_email_creds <- creds_envvar(
  user = Sys.getenv("MY_GMAIL_ACCOUNT"),
  pass_envvar = "SMTP_PASSWORD",
  provider = "gmail"
)

creds_path <- "credentials.csv"
read_creds <- function() read.csv(creds_path, stringsAsFactors=FALSE)
save_creds <- function(creds) write.csv(creds, creds_path, row.names=FALSE, quote=TRUE)

# --------------------------
# Password hashing helpers
# --------------------------
is_probably_hash <- function(x) {
  # sodium hashes usually start with "$7$"
  is.character(x) && length(x) == 1 && !is.na(x) && startsWith(x, "$7$")
}

hash_pw <- function(plain) {
  sodium::password_store(plain)
}

verify_pw <- function(plain, stored) {
  if (is_probably_hash(stored)) {
    #sodium::password_verify(stored, charToRaw(plain))
    sodium::password_verify(stored, plain)
  } else {
    # legacy plaintext support (temporary)
    identical(stored, plain)
  }
}

reset_tokens <- reactiveVal(data.frame(token=character(), user=character(), expires=numeric()))

##################
# Bring in data
##################

# Source in helper functions and cleaned data files - set to location created for Shiny app in Dockerfile
#setwd("./source_files") ; files.sources = list.files(); sapply(files.sources, source)
#setwd("/srv/shiny-server/bps-app/source_files") ; files.sources = list.files(); sapply(files.sources, source)
# cache <- qs::qread("./cache.qs"); list2env(cache, .GlobalEnv)

load_cache <- function(path = "./cache.qs") {
  #cache <- qs::qread(path)
  cache <- qs2::qs_read(path)
  list2env(cache, .GlobalEnv)
  invisible(TRUE)
}

# initial load on app start
load_cache("./cache.qs")
# cache_data(list(
#   students_dat  = students_dat,
#   students_dat2 = students_dat2,
#   tribe         = tribe,
#   lookup        = lookup,
#   courses_taken = courses_taken,
#   beh_wide      = beh_wide,
#   date_wide     = date_wide,
#   disc_wide     = disc_wide
# ))


# Upper bound for day-based sliders
MAX_DAYS <- tryCatch({
  md <- suppressWarnings(max(as.numeric(students_dat2$days_elapsed), na.rm = TRUE))
  if (is.finite(md)) as.integer(ceiling(md)) else 200L
}, error = function(e) 200L)

# Run on the data you use in the profile / lookups
students_dat2 <- to_utf8_df(students_dat2)
students_dat  <- to_utf8_df(students_dat)
lookup        <- to_utf8_df(lookup)
courses_taken <- to_utf8_df(courses_taken)
beh_wide      <- to_utf8_df(beh_wide)
date_wide     <- to_utf8_df(date_wide)
disc_wide     <- to_utf8_df(disc_wide)

################
# UI Content
################

# Main Dashboard Content - This is the content that shows inside dashboard body (ui)
dashboard_content <- function() {
  dashboardBody(
    tags$head(
      tags$style(HTML("
      #calendar_table_wrap table.dataTable { min-width: 1300px; }
      #calendar_table_wrap td, 
      #calendar_table_wrap th { white-space: nowrap; }
                      
      
      /* ensure tooltips float above cards/modals */
.tooltip { z-index: 2050 !important; }

/* let tooltips and ggiraph's floating div escape card bounds */
.plot-full, .plot-full .card-body,
#circularPlot, #circularPlot .girafe_container_std {
  overflow: visible !important;
}

/* keep a constant, small gap under the plot */
.cplot-wrap { margin-bottom: 12px !important; }

/* responsive plot height (view-height based, capped) */
#circularPlot { height: 100vh !important; }

/* make inner containers honor that height and remove padding tricks */
#circularPlot .html-widget,
#circularPlot .girafe_container_std { 
  padding-bottom: 0 !important; 
  height: 100% !important;
  width: 100% !important;
  padding: 0 !important;
  margin: 0 !important;
}

/* (sometimes ggiraph injects a wrapper with inline padding-bottom) */
#circularPlot .girafe_container_std > div { 
  padding-bottom: 0 !important; 
  height: 100% !important;
  width: 100% !important;
  padding: 0 !important;
  margin: 0 !important;
}

/* center the picker and keep it narrow */
.picker-row { display: flex; justify-content: center; }
.picker-row .picker-card {
  max-width: 520px; width: 100%;
  background: #fff; border-radius: 8px; 
  padding: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);
  text-align: center;
}

/* if you kept the old fixed-gap classes by accident, neutralize them */
.fixed-gap-row { margin: 0 !important; }
.fixed-gap     { height: 0 !important; }

  /* optional: a row with no bottom margin if bs4Dash adds any */
  .no-bottom-gap { margin-bottom: 0 !important; }
      
      /* ---- Compact profile card styles ---- */
.profile-card .card-body { padding: 12px 16px !important; }
.profile-title { margin: 4px 0 10px 0; }
.profile-watermark {
  position: absolute; top: 45%; left: 50%;
  transform: translate(-50%,-50%);
  height: 220px; opacity: .06; pointer-events: none;
}
.profile-grid {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 6px 24px; align-items: start;
}
.profile-list { list-style: none; margin: 0; padding: 0; font-size: 16px; line-height: 1.2; }
.profile-list li { margin: 3px 0; }
.profile-thresholds {
  border: 1px solid #c0392b; background: #f9f9f9;
  padding: 10px; border-radius: 5px; font-size: 16px;
  grid-column: 1 / -1;  /* span both columns */
}

  /* ensure dropdowns hug the right edge and stay compact */
  .main-header .dropdown-menu {
    right: 0 !important; left: auto !important;
    position: absolute !important;
    min-width: 240px; width: auto;
  }
      
        /* tighten up the hero and make it responsive */
        .hero-banner .d-flex {
          padding: 0 1rem;
          gap: 1.5rem;           /* ← add this line */
        }
        .hero-banner .hero-text {
          flex: 1 1 60%;
          min-width: 240px;
          padding-left: 1.5rem;
        }
        .hero-banner .hero-text h2 {
          margin-left: 0.5rem;      
        }
        .hero-banner .hero-picker {
          flex: 1 1 35%;
          min-width: 200px;
        }
        @media (max-width: 576px) {
          .hero-banner .hero-text,
          .hero-banner .hero-picker {
            flex: 1 1 100% !important;
            text-align: center;
            margin-bottom: 1rem;
          }
          .hero-banner .hero-text h2 { font-size: 24px; }
        }
        /* Make every value‐box at least 120px tall */
        .small-box {
          min-height: 120px !important;
        }
        /* allow subtitles to wrap if they’re too long */
        .small-box .inner p {
          white-space: normal !important;
          word-wrap: break-word !important;
        }
      "))
    ),
    # Javascript to scroll for full student profile card 
    tags$script(HTML("
        (function(){
      function debounce(fn, d){ var t; return function(){ clearTimeout(t); t=setTimeout(fn,d); }; }
      function eqHeights(){
        // target the two cards reliably
        var $cards = $('#wrap_profile_district .card:visible, #wrap_profile_tribes .card:visible');
        if($cards.length < 2) return;
        $cards.css('min-height','');                 // reset
        var max = 0;
        $cards.each(function(){
          var h = this.getBoundingClientRect().height;
          if(h > max) max = h;
        });
        if(max) $cards.css('min-height', Math.ceil(max) + 'px');
      }
      // expose for quick console checks, optional
      window._eqHeights = eqHeights;

      var run = debounce(eqHeights, 60);
      $(window).on('load resize', run);                 // initial + resize
      $(document).on('shiny:value shiny:idle', run);    // after outputs render
      $(document).on('shown.bs.tab collapsed.lte.pushmenu shown.lte.pushmenu', run); // tab/sidebar changes
      setTimeout(run, 150);                              // late reflow safety
    })();
    
    Shiny.addCustomMessageHandler('scrollToProfileCard', function(message) {
      function scrollWhenReady(retries) {
        var el = document.getElementById('profile_card_anchor');
        if (el) {
          el.scrollIntoView({ behavior: 'smooth', block: 'start' });
        } else if (retries > 0) {
          setTimeout(function() { scrollWhenReady(retries - 1); }, 100);
        }
      }
      scrollWhenReady(10);  // Try up to 10 times, every 100ms
    });
    ")),
    tags$head(
      tags$style(HTML("
     #incident_month_plot {
      cursor: pointer !important;
     }
    "))
    ),
    tabItems(
      tabItem(
        tabName = "admin",
        fluidRow(
          bs4Card(
            title = "Admin",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            DT::DTOutput("admin_table")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Add New User",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            # textInputs must have both an id AND a label
            textInput("new_user",  label = "Username:"   , value = ""),
            passwordInput("new_pw", label = "Password:"   , value = ""),
            textInput("new_email", label = "Email Address:", value = ""),
            checkboxInput("new_admin", label = "Admin?", value = FALSE),
            actionButton("add_user", "Add User", class   = "btn", style   = "background-color: orange; color: white; border: none;"),
            uiOutput("add_user_msg")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Delete User",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            div(id = "delete-picker-wrapper",
            pickerInput(
              inputId = "delete_user",
              label   = "Select user to delete:",
              choices = read_creds()$user,
              options = list(
                `live-search` = TRUE,
                title         = "Select user"
                )
              )
            ),
            actionButton(
              inputId = "delete_btn",
              label   = "Delete User",
              class   = "btn",
              style   = "background-color: orange; color: white; border: none;"
            ),
            uiOutput("delete_msg")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Data Refresh Status (To perform a data refresh, click 🔄 in top right hand corner)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            # textInput(
            #   inputId = "onedrive_sync_path",
            #   label   = "OneDrive synced folder (local path on this computer):",
            #   value   = "~/OneDrive - <Org Name>/<Folder Name>"
            # ),
            uiOutput("new_data_status"),
            # actionButton(
            #   inputId = "apply_new_cache",
            #   label   = "Refresh from Google Drive",
            #   icon    = icon("rotate")
            # ),
            br(), br(),
            verbatimTextOutput("data_refresh_status")
          )
        )
      ),
      # ---- Overview Tab ---- 
      tabItem(
        tabName = "overview",
        
        uiOutput("refresh_banner"),
        
        fluidRow(
          bs4ValueBox(
            value = HTML("<div style='font-size: 40px; font-weight: bold; margin-bottom: 0px;'>", comma(total_students), "</div>"),
            subtitle = HTML("<div style='font-size: 24px;'>Enrolled Students</div>"),
            icon = icon("users-line"),
            color = "danger",
            width = 3
          ),
          bs4ValueBox(
            value = HTML("<div style='font-size: 40px; font-weight: bold; margin-bottom: 0px;'>", comma(tribal_students), "</div>"),
            subtitle = HTML("<div style='font-size: 24px;'>Tribal Students</div>"),
            icon = icon("users-line"),
            color = "danger",
            width = 3
          ),
          bs4ValueBox(
            value = HTML("<div style='font-size: 40px; font-weight: bold; margin-bottom: 0px;'>", comma(dim(tribe)[1]), "</div>"),
            subtitle = HTML("<div style='font-size: 24px;'>Tribes</div>"),
            icon = icon("people-roof"),
            color = "danger",
            width = 3
          ),
          bs4ValueBox(
            value = HTML("<div style='font-size: 40px; font-weight: bold; margin-bottom: 0px;'>", paste0(prop_no_release * 100, "%"), "</div>"),
            subtitle = HTML("<div style='font-size: 24px;'>Did Not Sign A Release</div>"),
            icon = icon("file-signature"),
            color = "danger",
            width = 3
          )
        ),
        # ————————————————————————————
        #   Banner + Tribe Picker
        # ————————————————————————————
        fluidRow(
          if (!SINGLE_TRIBE) {
            bs4Card(
              width = 12, status = "danger", solidHeader = TRUE, collapsible = FALSE,
              class = "hero-banner",
              style = "background-color: #dc3545; color: white; padding: 1rem 0;",
              div(class = "d-flex flex-wrap justify-content-between align-items-center",
                  div(class = "hero-text",
                      tags$h2("Explore Tribes Within The District",
                              style = "color: white; font-weight: 800; margin: 0; font-size: 32px;"),
                      tags$p("Select one or more tribes from the dropdown menu to filter the charts below",
                             style = "color: white; font-size: 16px; margin: 0.5rem 0 0 0;")
                  ),
                  div(class = "hero-picker",
                      pickerInput(
                        inputId  = "tribe_select", label = NULL,
                        choices  = setdiff(unique(na.omit(students_dat$tribe_name)), "Not Applicable"),
                        selected = setdiff(unique(na.omit(students_dat$tribe_name)), "Not Applicable"),
                        multiple = TRUE,
                        options  = list(`actions-box`=TRUE, `live-search`=TRUE,
                                        `selected-text-format`="count > 2",
                                        `count-selected-text`="{0} Tribes selected",
                                        `none-selected-text`="No tribes selected",
                                        `style`="btn-light"),
                        width = "100%"
                      )
                  )
              )
            )
          } else {
            bs4Card(
              width = 12, status = "danger", solidHeader = TRUE,
              class = "hero-banner", style = "background-color:#dc3545;color:white;padding:1rem;",
              tags$h2(
                paste("Welcome to the Pueblo of ", DEFAULT_TRIBE_banner,  " Dashboard"),
                style = "color:white; font-weight:800; margin:0; font-size:32px;"
              )
            )
          }
        ),
  # ---- Spacer ----
  fluidRow(
    column(
      width = 12,
      div(style = "margin-top: 20px;")
    )
  ),
      fluidRow(
        column(7, div(id = "wrap_profile_district", uiOutput("profile_district"))),
        column(5, div(id = "wrap_profile_tribes",   uiOutput("profile_tribes")))
  ),
        fluidRow(
         column(width = 6, uiOutput("grades_box")),
         column(width = 6, uiOutput("schools_box"))
        )
      ),
      # ---- Attendance Tab ----
      tabItem(tabName = "attendance",
              {
                pct_fmt <- function(x) if (is.na(x)) "—" else sprintf("%.0f%%", x)
                if (SINGLE_TRIBE) {
                  tribe_df <- subset(
                    students_dat2,
                    trimws(as.character(tribe_name)) == DEFAULT_TRIBE
                  )
                  n <- nrow(tribe_df)
                  
                  # was: > 0.10 and round(..., 1)
                  pct_chronic <- if (n) round(100 * mean(tribe_df$adj_absenteeism_ratio >= 0.10, na.rm = TRUE), 2) else NA_real_
                  pct_truant  <- if (n) round(100 * mean(tribe_df$truancy_ratio            >= 0.10, na.rm = TRUE), 2) else NA_real_
                  
                } else {
                  df <- students_dat2
                  
                  # was: > 0.10 and round(..., 1)
                  pct_chronic <- round(100 * mean(df$adj_absenteeism_ratio >= 0.10, na.rm = TRUE), 2)
                  pct_truant  <- round(100 * mean(df$truancy_ratio            >= 0.10, na.rm = TRUE), 2)
                }
                fluidRow(
                  bs4ValueBox(
                    value = HTML(sprintf(
                      "<div style='font-size: 40px; font-weight: bold;'>%s</div>",
                      pct_fmt(pct_chronic)
                    )),
                    subtitle = HTML("<div style='font-size: 32px;'>Chronically Absent Students</div>"),
                    icon = icon("triangle-exclamation"),
                    color = "danger",
                    width = 6
                  ),
                  bs4ValueBox(
                    value = HTML(sprintf(
                      "<div style='font-size: 40px; font-weight: bold;'>%s</div>",
                      pct_fmt(pct_truant)
                    )),
                    subtitle = HTML("<div style='font-size: 32px;'>Truant Students</div>"),
                    icon = icon("triangle-exclamation"),
                    color = "danger",
                    width = 6
                  )
                )
              },
              fluidRow(
                bs4Card(
                  #title = "School Codes for Sun Chart",
                  title = tagList(
                    "School Codes for Sun Chart",
                    tags$small(
                      style = "display:block; font-style:italic; font-weight:400; color: rgba(255,255,255,0.9);",
                      ""
                    )
                  ),
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  DT::dataTableOutput("school_code_table")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  bs4Card(
                    width = 12, status = "danger", solidHeader = TRUE,
                    title = tagList(
                      "Sun Chart of Attendance",
                      tags$small(
                        tags$em("(Hover over the red school codes to see the full school name)"),
                        style = "display:block; font-weight:400; color: rgba(255,255,255,0.9);"
                      )
                    ),
                    class = "plot-full",
                    div(class = "cplot-wrap", ggiraph::girafeOutput("circularPlot", width = "100%"))
                  )
                )
              ),
              # fixed gap you control (constant across screen sizes)
              fluidRow(column(12, div(class = "after-circular-gap"))),
              # ---- Spacer ---- 
              fluidRow( column( width = 12, div(style = "margin-top: 20px;") ) ),
              
              # centered tribe picker (hidden in single-tribe mode)
              if (!SINGLE_TRIBE) {
                fluidRow(
                  column(
                    12,
                    div(class = "mx-auto",
                        style = "max-width: 520px; background: white; border-radius: 8px; padding: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); text-align: center;",
                        tags$label("Select Tribe:",
                                   style = "font-size: 18px; font-weight: bold; margin-bottom: 10px; display: block;"),
                        pickerInput(
                          inputId  = "tribe_track", label = NULL,
                          choices  = setdiff(unique(na.omit(students_dat2$tribe_name)), "Not Applicable"),
                          selected = setdiff(unique(na.omit(students_dat2$tribe_name)), "Not Applicable"),
                          multiple = FALSE,
                          options  = list(`actions-box`=TRUE, `live-search`=TRUE,
                                          `selected-text-format`="count > 2", `style`="btn-danger"),
                          width = "100%"
                        )
                    )
                  )
                )
              } else {
                fluidRow(
                  column(12,
                         div(class="mx-auto",
                             style="max-width:520px; background:white; border-radius:8px; padding:10px; box-shadow:0 2px 8px rgba(0,0,0,.1); text-align:center;",
                             tags$strong("Tribe: "),
                             span(DEFAULT_TRIBE)
                         )
                  )
                )
              },
              fluidRow(
                column(
                  width = 12,
                  div(style = "margin-top: 20px;")
                )
              ),
              fluidRow(
                box(
                  title = htmltools::HTML(
                    paste0(
                    tooltip_span(
                      "Periods Present",
                      paste0(
                        "How this metric is calculated and interpreted:<br><br>",
                        "• Periods Present = Percent of class periods attended by students, on average.<br><br>",
                        "• Values shown as percentages for readability.<br><br>",
                        "Tip: Toggle the ? menu (top-right corner of app) to turn hover descriptions On/Off."
                      )
                    ),
                    " <span class='hint-need-tips'>(Turn on the ? button in the top right for additional details)</span>",
                    " <span class='hint-hover'>(Hover over title in white text for details)</span>"
                    )
                  ),
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  nn_race_filter_ui("nn_race_present"),
                  plotlyOutput("present_comparison_plot", height = "500px")
                ),
                box(
                  title = htmltools::HTML(
                    paste0(
                    tooltip_span(
                      "Periods Absent",
                      paste0(
                        "How this metric is calculated and interpreted:<br><br>",
                        "• Periods Absent = Percent of class periods missed by students, on average.<br><br>",
                        "• Values shown as percentages for readability.<br><br>",
                        "Tip: Toggle the ? menu (top-right corner of app) to turn hover descriptions On/Off."
                      )
                    ),
                    " <span class='hint-need-tips'>(Turn on the ? button in the top right for additional details)</span>",
                    " <span class='hint-hover'>(Hover over title in white text for details)</span>"
                    )
                  ),
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  nn_race_filter_ui("nn_race_absent_prop"),
                  plotlyOutput("absent_prop_comparison_plot", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = htmltools::HTML(
                    paste0(
                    tooltip_span(
                      "Absence Type",
                      paste0(
                        "How this metric is calculated and interpreted:<br><br>",
                        "• Absence Type = total periods missed are divided up into two groups: 1.) those missed for excused reasons; and 2.) those missed for 
                        unexcused reasons. These two groups are then shown as percentages of all absences within the group.<br><br>",
                        "• Values shown as percentages for readability.<br><br>",
                        "Tip: Toggle the ? menu (top-right corner of app) to turn hover descriptions On/Off."
                      )
                    ),
                    " <span class='hint-need-tips'>(Turn on the ? button in the top right for additional details)</span>",
                    " <span class='hint-hover'>(Hover over title in white text for details)</span>"
                    )
                  ),
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  nn_race_filter_ui("nn_race_abs_type"),
                  plotlyOutput("absences_comparison_plot", height = "500px")
                ),
                box(
                  title = htmltools::HTML(
                    paste0(
                    tooltip_span(
                      "Truancy Rate",
                      paste0(
                        "How this metric is calculated and interpreted:<br><br>",
                        "• Truancy Rate = Number of unexcused period absences / ( Total number of periods - Total number of excused period absences ).<br><br>",
                        "• District Average bar: mean truancy rate across all students in the district.<br><br>",
                        "• Selected Tribe bar: mean truancy rate among students in the chosen tribe.<br><br>",
                        "• Values shown as percentages for readability.<br><br>",
                        "Tip: Toggle the ? menu (top-right corner of app) to turn hover descriptions On/Off."
                      )
                    ),
                    " <span class='hint-need-tips'>(Turn on the ? button in the top right for additional details)</span>",
                    " <span class='hint-hover'>(Hover over title in white text for details)</span>"
                    )
                  ),
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  nn_race_filter_ui("nn_race_truancy"),
                  plotlyOutput("truancy_comparison_plot", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = htmltools::HTML(
                    paste0(
                    tooltip_span(
                      "Unadjusted Absenteeism Rate",
                      paste0(
                        "How this metric is calculated and interpreted:<br><br>",
                        "• Unadjusted Absenteeism Rate = Total number of periods absent / Total number of periods.
                        Shows the percentage of class periods a student was absent out of all scheduled periods. Helps monitor general attendance trends.<br><br>",
                        "• District Average bar: mean absenteeism rate across all students in the district.<br><br>",
                        "• Selected Tribe bar: mean absenteeism rate among students in the chosen tribe.<br><br>",
                        "• Values shown as percentages for readability.<br><br>",
                        "Tip: Toggle the ? menu (top-right corner of app) to turn hover descriptions On/Off."
                      )
                    ),
                    " <span class='hint-need-tips'>(Turn on the ? button in the top right for additional details)</span>",
                    " <span class='hint-hover'>(Hover over title in white text for details)</span>"
                  )
                  ),
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  nn_race_filter_ui("nn_race_unadj_abs"),
                  plotlyOutput("absent_comparison_plot", height = "500px")
                ),
                box(
                  title = htmltools::HTML(
                    paste0(
                    tooltip_span(
                      "Adjusted Absenteeism Rate",
                      paste0(
                        "How this metric is calculated and interpreted:<br><br>",
                        "• Adjusted Absenteeism Rate = ( Total number of periods absent - Total number of periods with excused absences) / (Total number of periods - Total number of excused periods due to cultural days).
                        Refines the absenteeism rate by removing excused cultural days, providing a clearer view of potentially concerning absences.<br><br>",
                        "• District Average bar: mean adjusted absenteeism rate across all students in the district.<br><br>",
                        "• Selected Tribe bar: mean adjusted absenteeism rate among students in the chosen tribe.<br><br>",
                        "• Values shown as percentages for readability.<br><br>",
                        "Tip: Toggle the ? menu (top-right corner of app) to turn hover descriptions On/Off."
                      )
                    ),
                    " <span class='hint-need-tips'>(Turn on the ? button in the top right for additional details)</span>",
                    " <span class='hint-hover'>(Hover over title in white text for details)</span>"
                    )
                  ),
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  nn_race_filter_ui("nn_race_adj_abs"),
                  plotlyOutput("absent_comparison_plot_adj", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = htmltools::HTML(
                    paste0(
                      tooltip_span(
                        "Adj. Absenteeism Category",
                        paste0(
                          "How groups are calculated and interpreted:<br><br>",
                          "• Perfect = Students with an adjusted absenteeism rate of 0% (no absences).<br><br>",
                          "• Good = Students with an adjusted absenteeism rate between 0% and 2%.<br><br>",
                          "• Signal = Students with an adjusted absenteeism rate between 2% and 5%.<br><br>",
                          "• At-risk = Students with an adjusted absenteeism rate between 5% and 10%.<br><br>",
                          "• Chronic = Students with an adjusted absenteeism rate ABOVE 10%.<br><br>",
                          
                          "Interpretation: You can view the adjusted absenteeism rate that groups are based on as the proportion of absences a student has at the class period-level after removing excused cultural days. 
                          The higher the value, the worse the absenteeism.<br><br>",
                          "Tip: Toggle the ? menu (top-right corner of app) to turn hover descriptions On/Off."
                        )
                      ),
                      " <span class='hint-need-tips'>(Turn on the ? button in the top right for additional details)</span>",
                      " <span class='hint-hover'>(Hover over title in white text for details)</span>"
                    )
                  ),
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  nn_race_filter_ui("nn_race_abs_cat"),
                  plotlyOutput("absences_category_plot", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = htmltools::HTML(
                    paste0(
                      tooltip_span(
                        "Truancy Category",
                        paste0(
                          "How groups are calculated and interpreted:<br><br>",
                          "• Perfect = Students with a truancy rate of 0% (no unexcused absences).<br><br>",
                          "• Good = Students with a truancy rate between 0% and 2%.<br><br>",
                          "• Signal = Students with a truancy rate between 2% and 5%.<br><br>",
                          "• At-risk = Students with a truancy rate between 5% and 10%.<br><br>",
                          "• Truant = Students with a truancy rate ABOVE 10%.<br><br>",
                          
                          "Interpretation: You can view the truancy rate that groups are based on as the proportion of unexcused period-level absences a student has out of all their total class periods once excused absences are removed. 
                          The higher the value, the worse the truancy.<br><br>",
                          "Tip: Toggle the ? menu (top-right corner of app) to turn hover descriptions On/Off."
                        )
                      ),
                      " <span class='hint-need-tips'>(Turn on the ? button in the top right for additional details)</span>",
                      " <span class='hint-hover'>(Hover over title in white text for details)</span>"
                    )
                  ),
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  nn_race_filter_ui("nn_race_tru_cat"),
                  plotlyOutput("truancy_category_plot", height = "500px")
                )
              ),
              # ---- OPTIONAL: small spacer ----
              fluidRow(column(12, div(style = "height: 20px;"))),
              # ===== NEW: Attendance filters + charts/tables =====
              fluidRow(
                bs4Card(
                  title = htmltools::HTML(paste0(
                    tooltip_span("Attendance Filters", 
                                 "Use these to subset the students included in the charts/tables below.")
                  )),
                  status = "danger", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(3,
                           prettyRadioButtons(
                             inputId = "att_metric",
                             label   = "Metric",
                             choices = c("Proportion Present","Proportion Absent",
                                         "Adjusted Absenteeism","Unadjusted Absenteeism","Truancy Rate"),
                             selected = "Proportion Present",
                             inline = TRUE, status = "danger", bigger = TRUE
                           )
                    ),
                    column(3,
                           shinyWidgets::pickerInput(
                             inputId = "att_race", label = "Race (optional)",
                             choices = character(0), multiple = TRUE,
                             options = list(`actions-box`=TRUE, `live-search`=TRUE, title="All")
                           )
                    ),
                    column(2,
                           shinyWidgets::pickerInput(
                             inputId = "att_gender", label = "Gender (optional)",
                             choices = character(0), multiple = TRUE,
                             options = list(`actions-box`=TRUE, `live-search`=TRUE, title="All")
                           )
                    ),
                    column(2,
                           shinyWidgets::pickerInput(
                             inputId = "att_sped", label = "Special Ed (optional)",
                             choices = c("Yes","No"), multiple = TRUE,
                             options = list(`actions-box`=TRUE, title="All")
                           )
                    ),
                    column(2,
                           shinyWidgets::pickerInput(
                             inputId = "att_ell", label = "ELL (optional)",
                             choices = c("Yes","No"), multiple = TRUE,
                             options = list(`actions-box`=TRUE, title="All")
                           )
                    )
                  ),
                  div(class="text-right",
                      actionButton("att_reset", label = tagList(icon("undo"), "Reset Filters"),
                                   class="btn btn-sm btn-outline-secondary"))
                )
              ),
              fluidRow(
                box(
                  title = "Attendance by Grade", status = "danger", solidHeader = TRUE, width = 12,
                  plotlyOutput("att_grade_plot", height = "420px"),
                  tags$div(class="d-flex justify-content-between align-items-center mt-2",
                           tags$div(tags$small(em("Mean value per grade (percent)."))),
                           downloadButton("download_att_grade", "Download CSV", class="btn-danger btn-sm")
                  ),
                  DTOutput("att_grade_table")
                )
              ),
              fluidRow(
                box(
                  title = "Attendance by School", status = "danger", solidHeader = TRUE, width = 12,
                  plotlyOutput("att_school_plot", height = "520px"),
                  tags$div(class="d-flex justify-content-between align-items-center mt-2",
                           tags$div(tags$small(em("Mean value per school (percent)."))),
                           downloadButton("download_att_school", "Download CSV", class="btn-danger btn-sm")
                  ),
                  DTOutput("att_school_table")
                )
              )
      ),
      # ------ Disciplinary Calendar ------ 
  tabItem(
    tabName = "calendar",
    fluidRow(
      bs4Card(
        title = "Disciplinary Incidents Calendar",
        status = "danger",
        solidHeader = TRUE,
        width = 12,
        
        # === Full-width Month and Year selectors ===
        fluidRow(
          column(6,
                 selectInput("calendar_month", "Select Month",
                             choices = setNames(1:12, month.name),
                             selected = 8,
                             #selected = month(Sys.Date()), # selects current month 
                             width = "100%")
          ),
          column(6,
                 selectInput("calendar_year", "Select Year",
                             choices = 2020:year(Sys.Date()),
                             selected = 2025,
                             #selected = year(Sys.Date()), # selects current year 
                             width = "100%")
          )
        ),
        
        br(),
        plotOutput("incident_month_plot", height = "600px", click = "incident_month_plot_click"),
        br(),
        tags$p("Click a date above to view incidents for that day in the table below.",
               class = "text-center text-secondary",
               style = "margin-top: -10px; margin-bottom: 15px; font-style: italic;"),
        
        # === Full-width Date Range Input ===
        fluidRow(
          column(12,
                 dateRangeInput(
                   inputId = "calendar_date_range",
                   label = "Incident Table Date Range:",
                   start = min(date_wide %>% pivot_longer(starts_with("Incident_Date_")) %>% pull(value), na.rm = TRUE),
                   end = max(date_wide %>% pivot_longer(starts_with("Incident_Date_")) %>% pull(value), na.rm = TRUE),
                   width = "100%"
                 )
          )
        ),
        
        br(),
        uiOutput("calendar_range_title"),
        # --- Download CSV for the incidents table ---
        tags$div(
          class = "d-flex justify-content-end mb-2",
          downloadButton("download_calendar", "Download incidents (.csv)", class = "btn-danger btn-sm")
        ),
        
        #DTOutput("calendar_table"),
        div(
          id    = "calendar_table_wrap",
          style = "max-height: 420px; overflow: auto; width: 100%;",  # vertical + horizontal scroll
          DT::DTOutput("calendar_table", width = "100%")
        ),
        
        # Incident Line Trend 
        br(),
        tags$h5("Trend of Disciplinary Incidents Over Time", class = "text-muted", 
                style = "margin-top: 10px; text-align: center;"),
        # Filters
        # === Trend-only Date Range Input ===
        fluidRow(
          column(
            12,
            dateRangeInput(
              inputId = "trend_date_range",
              label   = "Trend Line Date Range:",
              start   = min(
                date_wide %>% 
                  tidyr::pivot_longer(starts_with("Incident_Date_")) %>% 
                  dplyr::pull(value),
                na.rm = TRUE
              ),
              end     = max(
                date_wide %>% 
                  tidyr::pivot_longer(starts_with("Incident_Date_")) %>% 
                  dplyr::pull(value),
                na.rm = TRUE
              ),
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(4,
                 selectInput("trend_grade", "Filter by Grade:",
                             choices = c("All", c("Pre-K 1", "Pre-K 2", "Kindergarten", as.character(1:12))),
                             selected = "All")
          ),
          column(4,
                 selectInput("trend_school", "Filter by School:",
                             choices = c("All", sort(unique(students_dat2$SCHOOLS.Name))),
                             selected = "All")
          ),
          if (!SINGLE_TRIBE) column(4, selectInput("trend_tribe", "Filter by Tribe:",
                             choices = c("All", sort(unique(setdiff(students_dat2$tribe_name, "Not Applicable")))),
                             selected = "All"))
        ),
        # --- Weekly vs Monthly toggle for the trend chart ---
        fluidRow(
          column(
            12,
            radioButtons(
              inputId  = "trend_granularity",
              label    = "Granularity:",
              choices  = c("Weekly", "Monthly"),
              selected = "Monthly",
              inline   = TRUE
            )
          )
        ),
        
        br(),
        plotOutput("discipline_trend_plot", height = "350px")
      ) 
    )   
  ),

# ---- Data Views Tab ----
tabItem(tabName = "dataviews",
        fluidRow(
          bs4Card(
            title = "Explore Attendance and Grades Data",
            status = "danger", solidHeader = TRUE, width = 12,
            tabsetPanel(
              id = "dv_tabset",
              # ---------------- Aggregates (no student rows) ----------------
              tabPanel(
                title = "Aggregates", icon = icon("layer-group"),
                fluidRow(
                  column(4,
                         prettyRadioButtons(
                           inputId = "dv_metric", label = "Metric",
                           choices = c("Proportion Present","Proportion Absent","Adjusted Absenteeism","Unadjusted Absenteeism","Truancy Rate"),
                           selected = "Proportion Absent", inline = FALSE, status = "info", bigger = TRUE
                         ),
                         sliderInput("dv_threshold", "Filter by metric (%):", min = 0, max = 100, value = c(0,100), step = 1)
                  ),
                  column(4,
                         shinyWidgets::pickerInput(
                           inputId = "dv_groups", label = "Group by:",
                           choices = c("School","Grade","Race/Ethnicity","Gender","SPED","ELL","Tribe"),
                           multiple = TRUE, options = list(`actions-box`=TRUE, `max-options`=7, `live-search`=FALSE, title="None")
                         ),
                         numericInput("dv_min_n", "Minimum N per group", value = 10, min = 1, max = 1000, step = 1)
                  ),
                  column(4,
                         downloadButton("download_dv_csv", "Download CSV"),
                         tags$small(class="text-muted",
                                    HTML("<br>Tip: Use the Attendance filters (left side) to subset before grouping.")
                         )
                  )
                ),
                br(),
                DTOutput("dv_table")
              ),
              # ---------------- Students (always student-level) ----------------
              tabPanel(
                title = "Students", icon = icon("user-graduate"),
                fluidRow(
                  # ---- Ranges for each measure ----
                  column(6,
                         sliderInput("dv_student_present", "Days Present range:", min = 0, max = MAX_DAYS, value = c(0, MAX_DAYS), step = 1),
                         sliderInput("dv_student_absent",  "Days Absent range:",  min = 0, max = MAX_DAYS, value = c(0, MAX_DAYS), step = 1),
                         sliderInput("dv_student_excused", "Days Excused range:", min = 0, max = MAX_DAYS, value = c(0, MAX_DAYS), step = 1)
                  ),
                  column(6,
                         sliderInput("dv_student_truant",  "Days Truant range:",  min = 0, max = MAX_DAYS, value = c(0, MAX_DAYS), step = 1),
                         sliderInput("dv_student_rate",    "Attendance Rate (%):",min = 0, max = 100, value = c(0, 100), step = 1),
                         shinyWidgets::pickerInput(
                           inputId = "dv_student_cols", label = "Show columns (optional)",
                           choices = c("School","Grade","Race/Ethnicity","Gender","SPED","ELL","Tribe"),
                           multiple = TRUE, options = list(`actions-box`=TRUE, `live-search`=FALSE, title="None")
                         ),
                         checkboxInput("dv_student_show_rate", "Include Attendance Rate column", value = TRUE)
                  )
                ),
                br(),
                downloadButton("download_dv_csv_student", "Download CSV"),
                br(), br(),
                DTOutput("dv_table_student")
              ),
              # ---------------- Courses (Course Explorer) ----------------
              tabPanel(
                title = "Courses", icon = icon("book-open"),
                fluidRow(
                  column(4,
                         shinyWidgets::pickerInput(
                           "cx_school", "School", choices = NULL, multiple = TRUE,
                           options = list(`actions-box`=TRUE, `live-search`=TRUE, title="All")
                         ),
                         shinyWidgets::pickerInput(
                           "cx_grade", "Grade", choices = NULL, multiple = TRUE,
                           options = list(`actions-box`=TRUE, title="All")
                         ),
                         shinyWidgets::pickerInput(
                           "cx_marking", "Marking Period", choices = NULL, multiple = TRUE,
                           options = list(`actions-box`=TRUE, title="All")
                         )
                  ),
                  column(4,
                         shinyWidgets::pickerInput(
                           "cx_eval", "Evaluation", choices = NULL, multiple = TRUE,
                           options = list(`actions-box`=TRUE, title="All")
                         ),
                         numericInput("cx_grade_max", "Flag as failing if Grade (0–100) ≤", value = 69, min = 0, max = 100, step = 1),
                         sliderInput("cx_letter_max", "…or Letter Grade ≤", min = 1, max = 4, value = 1, step = 1)
                  ),
                  column(4,
                         checkboxInput("cx_fail_only", "Show failing/at-risk only", value = TRUE),
                         shinyWidgets::switchInput("cx_show_all_cols", "Show all columns", value = FALSE, onLabel = "YES", offLabel = "NO"),
                         br(),
                         downloadButton("cx_download_selected", "Download Selected"),
                         actionButton("cx_email_btn", "Compose Email", icon = icon("envelope")),
                         actionButton("cx_note_btn",  "Compose Note",  icon = icon("sticky-note"))
                  )
                ),
                br(),
                uiOutput("cx_caption"),
                div(style = "overflow-x:auto;", DTOutput("cx_table", width = "100%"))
              )
            )
          )
        )
      ),
      # ---- Student Profiles ----
      tabItem(tabName = "profile",
              fluidRow(
                bs4Card(
                  title = "Student Lookup",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  icon = icon("search"),
                  collapsible = TRUE,
                  collapsed = FALSE,
                  textInput("search_last_name", "Search by Last Name", placeholder = "Optional (e.g., Johnson)"),
                  # NEW: Add tribe filter, if SINGLE TRIBE MODE is off  
                  if (!SINGLE_TRIBE) {
                    shinyWidgets::pickerInput(
                      inputId = "lookup_tribe_filter",
                      label = "Filter by Tribe (optional)",
                      choices = sort(unique(stats::na.omit(trimws(as.character(lookup$tribe_name))))),
                      multiple = TRUE,
                      options = list(`actions-box`=TRUE, `live-search`=TRUE, title="All")
                    )
                  },
                  # NEW: School filter (multi-select, live search; empty selection = All)
                  shinyWidgets::pickerInput(
                    inputId = "school_filter",
                    label   = "Filter by School (optional)",
                    choices = sort(unique(na.omit(lookup$school_name))),
                    selected = NULL,           # NULL/empty means "All schools"
                    multiple = TRUE,
                    options = list(
                      `actions-box`  = TRUE,
                      `live-search`  = TRUE,
                      title          = "All schools",
                      size           = 8,
                      `dropup-auto`  = FALSE,
                      style          = "btn-light"
                    ),
                    width = "100%"
                  ),
                  numericInput("gpa_filter", "Max GPA (shows students with GPA below this)", 
                               value = NA, min = 0, max = 4, step = 0.01),
                  numericInput("min_gpa_filter", "Min GPA (shows students with GPA above this)", 
                               value = NA, min = 0, max = 4, step = 0.01),
                  numericInput("absence_filter", "Minimum Absence Rate (shows students with absences above this)", 
                               value = NA, min = 0, max = 1, step = 0.01),
                  numericInput("disc_filter", "Minimum Disciplinary Incidents (shows students with number equal to or above this)", 
                               value = NA, min = 0, max = max(beh_wide$Number_behaviors), step = 1),
                  uiOutput("student_result_card")
                )
              ),
              tags$script(HTML("
          $(document).on('shiny:connected', function() {
          $('#gpa_filter').attr('placeholder', 'Enter GPA (optional)');
          $('#absence_filter').attr('placeholder', 'Optional (e.g., 0.10)');
          $('#disc_filter').attr('placeholder', 'Optional (e.g., 2)');
          });
        ")),
              fluidRow(
                bs4Card(
                  title = "Display Full Profile for a Student",
                  width = 12,
                  icon = icon("address-card"),
                  status = "info",
                  solidHeader = TRUE,
                  textInput("student_search", "Enter Student ID or Name:", placeholder = "e.g. 1800 or Jane Doe"),
                  div(
                    style = "text-align: center;",
                    tableOutput("student_profile")
                  )
                ),
                fluidRow(
                  uiOutput("profile_card"),
                  column(width=12, uiOutput("course_history_table")),
                  column(width=12, uiOutput("discipline_history_table")),
                  column(width = 12, uiOutput("student_contacts_table"))   # <--- NEW
                )
              )
      )
    )
  )
}

######
# UI
######

# Top level UI - theme and sidebar 
ui <- bs4DashPage(
  # Custom CSS goes here
  help = NULL,
  controlbar = NULL,
  footer     = bs4DashFooter(), 
  dark = NULL, 
  freshTheme = create_theme(
    bs4dash_vars(navbar_light_color = "red",
                 navbar_light_active_color = "grey",
                 navbar_light_hover_color = "red"),
    bs4dash_layout(main_bg = "#b1b0b0"),
    bs4dash_sidebar_light(bg = "#faf7f3")
  ),
  # Custom CSS for delete user highlighting 
  tags$head(
    # Initialize ALL bs5 tooltips on hover with zero delay
    tags$script(HTML("
        (function () {
      var tipsOn = false;
      function initTips($scope){ $scope = $scope || $(document);
        $scope.find('.tip-target').each(function(){
          var $el=$(this), tip=$el.attr('data-tip')||'';
          $el.removeAttr('title').attr('data-original-title', tip);
        });
        $scope.find('.tip-target').tooltip({
          container:'body', boundary:'window', trigger:'hover focus', html:true
        });
      }
      function disposeTips(){
        try{$('.tip-target').tooltip('dispose');}catch(e){}
        $('.tip-target').removeAttr('title data-original-title data-bs-title aria-describedby');
      }
      Shiny.addCustomMessageHandler('tipMode', function(message){
        tipsOn = !!(message && message.enabled);
        if (tipsOn){ initTips(); document.body.classList.add('tips-on'); }
        else { disposeTips(); document.body.classList.remove('tips-on'); }
      });
      $(document).on('shiny:value shiny:recalculated', function(){ if(tipsOn) initTips(); });
      $(document).on('mouseenter focus', '.tip-target', function(){
        if (tipsOn && !$(this).data('bs.tooltip')) { initTips($(this)); $(this).tooltip('show'); }
      });
    })();
  ")),
    tags$style(HTML("
      /* constant vertical space under the circular plot */
  .fixed-gap-row { overflow: hidden; }   /* prevents margin collapsing */
  .fixed-gap     { height: 28px; }       /* adjust to taste */
  
    .nn-filter { display:flex; justify-content:flex-end; align-items:center; gap:10px; margin-bottom:8px; }
    .nn-filter-label { font-weight:700; font-size:14px; color:#333; }

    
     /* Inline hint: visible when tips are off, hidden when on */
  .hint-need-tips { 
    color: #fff;             /* white text */
    font-size: 0.9em; 
    margin-left: .35rem; 
    white-space: nowrap;
    font-style: italic;      /* italicized */
  }
  body.tips-on .hint-need-tips { display: none !important; }
  
  /* New: visible only when tips are ON */
  .hint-hover { 
    display: none; 
    color: #fff;         
    font-size: 0.9em; 
    margin-left: .35rem; 
    white-space: nowrap;
    font-style: italic;
  }
  body.tips-on .hint-hover { display: inline !important; }
    
    .tooltip .tooltip-inner{
      max-width:320px; font-size:16px; line-height:1.35;
      padding:10px 12px; background:#000 !important; color:#fff !important;
      --bs-tooltip-bg:#000; --bs-tooltip-color:#fff;
    }
    .tooltip.bs-tooltip-top .tooltip-arrow::before    { border-top-color:#000 !important; }
    .tooltip.bs-tooltip-bottom .tooltip-arrow::before { border-bottom-color:#000 !important; }
    .tooltip.bs-tooltip-start .tooltip-arrow::before  { border-left-color:#000 !important; }
    .tooltip.bs-tooltip-end .tooltip-arrow::before    { border-right-color:#000 !important; }
    /* no underline on tip targets */
    [data-toggle='tooltip'], [data-bs-toggle='tooltip']{
      text-decoration:none !important; border-bottom:none !important; cursor:help;
    }
    
    /* Only change hover for the delete‐user picker */
    #delete-picker-wrapper .dropdown-menu li a:hover,
    #delete-picker-wrapper .dropdown-menu li.selected a,
    #delete-picker-wrapper .dropdown-menu li.active a {
      background-color: orange !important;
      color: white      !important;
    }
    
     /* when the sidebar is collapsed (mini) AND not hovered, hide the thresholds card */
    body.sidebar-mini.sidebar-collapse:not(.sidebar-expanded-on-hover)
      .custom-thresholds { display: none !important; }
    
    /* Tooltip */
    .tooltip-inner {
      max-width: 240px;
      background-color: #000 !important;
      color: #fff    !important;
      padding: 8px 10px;
      font-size: 14px;
      text-align: center;
    }
    .tooltip.bs-tooltip-top .tooltip-arrow::before {
      border-top-color: #000 !important;
    }
  "))
  ),
  header = bs4DashNavbar(
    rightUi = tagList(
      # HELP (tiny ? icon, right-aligned)
      tags$li(
        class = "nav-item dropdown",
        tags$a(
          href = "#", class = "nav-link", `data-toggle` = "dropdown",
          htmltools::tagList(icon("circle-question"))
        ),
        tags$div(
          class = "dropdown-menu dropdown-menu-right p-2",
          style = "min-width: 240px;",
          tags$div(class = "text-center mb-2 text-muted", "Hover Over Descriptions"),
          actionButton(
            "toggle_help", label = "Off",             # server flips to On/Off
            class = "btn btn-sm btn-outline-secondary w-100"
          )
        )
      ),
      
      # REFRESH (dropdown icon -> opens small menu with the refresh button)
      tags$li(
        class = "nav-item dropdown",
        tags$a(
          href = "#", class = "nav-link", `data-toggle` = "dropdown",
          htmltools::tagList(icon("rotate"))
        ),
        tags$div(
          class = "dropdown-menu dropdown-menu-right p-2",
          style = "min-width: 220px;",
          tags$div(class = "text-center mb-2 text-muted", "Refresh dashboard data"),
          actionButton(
            inputId = "apply_new_cache",
            label   = "Refresh from Google Drive",
            icon    = icon("rotate"),
            class   = "btn btn-sm btn-warning w-100"
          )
        )
      ),
      
      # USER (tiny user icon, right-aligned)
      tags$li(
        class = "nav-item dropdown",
        tags$a(
          href = "#", class = "nav-link", `data-toggle` = "dropdown",
          htmltools::tagList(icon("user"))
        ),
        tags$div(
          class = "dropdown-menu dropdown-menu-right p-2",
          style = "min-width: 240px;",
          tags$div(
            class = "text-center font-weight-bold mb-2",
            uiOutput("user_name_display")
          ),
          actionButton(
            "custom_logout", "Logout",
            class = "btn btn-danger btn-sm w-100"
          )
        )
      )
    )
  ),
  sidebar = bs4DashSidebar(
    # Header with logo and label
    tags$div(
      style = "padding: 15px; display: flex; align-items: center;",
      tags$img(src = "BPS-District-Logo.png", height = "70px", style = "margin-left: -20px; margin-right: 10px;"),
      tags$span("Menu", style = "font-weight: bold; font-size: 25px;")
    ),
    # Custom CSS for sidebar and sliders
    tags$head(
      tags$style(HTML("
      /* Menu item hover and active styles */
      .nav-sidebar .nav-item > .nav-link:hover {
        background-color: #dc3545 !important;
        color: white !important;
      }
      .nav-sidebar .nav-item > .nav-link.active {
        background-color: #dc3545 !important;
        color: white !important;
      }
      /* Slider track (bar), edge, and handle */
      .irs-bar,
      .irs-bar-edge,
      .irs-slider {
        background-color: orange !important;
        border: 1px solid orange !important;
      }

      /* Tooltip number (value above the slider handle) */
      .irs-single,
      .irs-from,
      .irs-to {
        background-color: orange !important;
        color: black !important;
        border: 2px solid orange !important;
      }
    "))
    ),
    minified = TRUE,
    expand_on_hover= TRUE,
    skin = "light",
    status = "red",
    width = 400, 
  # <- dynamic menu placeholder
  sidebarMenuOutput("sidebar_ui"),
  # only show on the Overview tab 
  conditionalPanel(
    condition = "input.tabs == 'overview'",
    div(class = "custom-thresholds",
        bs4Card(
          title = tagList(
            icon("cogs"),
            span("Thresholds", class = "thresholds-text")
          ),
          status      = "danger",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed   = FALSE,
          width       = 12,
          div(style = "font-size:14px;",
              tags$label("Set Absences (%)", style = "display:block; width:100%;"),
              div(
              numericInput("att_threshold_num", NULL, 
                           value = round(mean(students_dat2$proportion_absent, na.rm = TRUE) * 100, 0),
                           min = 0, max = 99, step = 1, width = "60%")),
              sliderInput(
                "att_threshold", "Absences (%)", # convert this back to proportion in calculations
                min   = 0, max = 99,
                value = round(mean(students_dat2$proportion_absent, na.rm=TRUE) * 100, 2),
                width = "100%"
              ),
              tags$label("Set Simple GPA (0–4)", style = "display:block; width:100%;"),
              div(
              numericInput("gpa_threshold_num", NULL, 
                           value = round(mean(students_dat2$GPA, na.rm=TRUE), 2),
                           min = 0, max = 3.99, step = 0.01, width = "60%")),
              sliderInput(
                "gpa_threshold", "Simple GPA (0–4)",
                min   = 0, max = 3.99,
                value = round(mean(students_dat2$GPA, na.rm=TRUE), 2),
                step  = 0.01,
                width = "100%"
              )
          )
        )
      )
    )
  ),
  body = uiOutput("main_content")
)

###########
# SERVER
###########
server <- function(input, output, session) {
  
  cache_version <- reactiveVal(0)
  cache_data <- reactiveVal(NULL)
  data_refreshed_flag <- reactiveVal(FALSE)
  new_data_available <- reactiveVal(FALSE)
  
  # ---- GLOBAL hard refresh trigger (SAFE) ----
  observeEvent(cache_version(), {
    load_cache("./cache.qs")
  }, ignoreInit = TRUE)
  
  output$refresh_banner <- renderUI({
    req(data_refreshed_flag())
    
    div(
      style = "
      background-color: #d4edda;
      color: #155724;
      padding: 12px 16px;
      margin-bottom: 15px;
      border-radius: 6px;
      font-weight: 600;
      border: 1px solid #c3e6cb;
    ",
      icon("check-circle"),
      span("  Data refreshed successfully.")
    )
  })
  
  output$new_data_status <- renderUI({
    if (new_data_available()) {
      div(
        style = "color: #28a745; font-weight: 600; margin-bottom: 8px;",
        icon("circle"),
        " New data available — click Refresh to update dashboard"
      )
    } else {
      div(
        style = "color: #6c757d; margin-bottom: 8px;",
        icon("check"),
        " Dashboard is up to date"
      )
    }
  })
  
  # When the user closes the browser tab / window or logs out,
  # stop the Shiny app completely so the launcher can finish.
  session$onSessionEnded(function() {
    message("BPS Dashboard session ended; stopping app.")
    stopApp()
  })
  
  user_r <- reactiveVal(NULL)
  
  # --------------------------------------------------
  # CHECK GOOGLE DRIVE FOR NEW DATA (METADATA ONLY)
  # --------------------------------------------------
  observe({
    invalidateLater(60000, session)  # every 60 seconds
    
    try({
      new_data_available(check_for_new_data())
    }, silent = TRUE)
  })
  
  # ---- Single-tribe shims (centralized selection) ----
  TRIBE  <- reactive({ if (SINGLE_TRIBE) DEFAULT_TRIBE else input$tribe_track })
  #TRIBES <- reactive({ if (SINGLE_TRIBE) DEFAULT_TRIBE else input$tribe_select })
  TRIBES <- reactive({
    sel <- input$tribe_select
    if (isTruthy(sel) && length(sel) > 0) sel else DEFAULT_TRIBE
  })
  selected_incident_date <- reactiveVal(NULL)
  has_shown_help_toast <- reactiveVal(FALSE)
  # -- is current user admin? --
  is_admin <- reactive({
    req(user_r())
    creds <- read_creds()
    row   <- creds$user == user_r()
    if (!"admin" %in% names(creds) || sum(row) != 1) return(FALSE)
    as.logical(creds$admin[row])
  })
  
  # Tooltip state: OFF by default
  tips_on <- reactiveVal(FALSE)
  # whenever user logs in, force off
  # observeEvent(user_r(), {
  #   req(user_r())
  #   tips_on(FALSE)
  #   session$sendCustomMessage('tipMode', list(enabled = FALSE))
  # }, ignoreInit = TRUE)
  # Schools present in-scope (single tribe -> that tribe's schools; else all)
  schools_in_scope <- reactive({
    if (SINGLE_TRIBE) {
      students_dat2 %>%
        dplyr::filter(trimws(as.character(tribe_name)) == TRIBE()) %>%
        dplyr::distinct(SCHOOLS.Name) %>%
        dplyr::pull(SCHOOLS.Name)
    } else {
      unique(stats::na.omit(students_dat2$SCHOOLS.Name))
    }
  })
  
  observeEvent(user_r(), {
    req(user_r())
    # 1) ensure tooltips are OFF on login
    tips_on(FALSE)
    session$sendCustomMessage('tipMode', list(enabled = FALSE))
    
    # 2) show a friendly toast just once per session
    if (!isTRUE(has_shown_help_toast())) {
      showNotification(
        HTML("💡 <b>Need more context?</b> Turn on the <b>?</b> help toggle (top-right) to see hover over descriptions for key text and metrics."),
        type = "message",
        duration = 60,
        closeButton = TRUE
      )
      has_shown_help_toast(TRUE)
    }
  }, ignoreInit = TRUE)
  
  # ensure initial sync of label and state
  observe({
    if (is.null(user_r())) return()
    updateActionButton(session, "toggle_help", label = if (isTRUE(tips_on())) "On" else "Off")
    session$sendCustomMessage('tipMode', list(enabled = isTRUE(tips_on())))
  })
  
  # toggle handler
  observeEvent(input$toggle_help, {
    new_state <- !isTRUE(tips_on())
    tips_on(new_state)
    updateActionButton(session, "toggle_help", label = if (new_state) "On" else "Off")
    session$sendCustomMessage('tipMode', list(enabled = new_state))
  })
  
  # Helper function for stripping too many options from plots only keeping downloads 
  # Make any ggplot use the tightest possible x/y box (no white moat)
  tight_equal <- function(p, pad = 0) {
    b <- ggplot_build(p)
    xs <- ys <- numeric(0)
    for (d in b$data) {
      xs <- c(xs, d$x, d$xmin, d$xmax, d$xend)
      ys <- c(ys, d$y, d$ymin, d$ymax, d$yend, d$r0, d$r)
    }
    xs <- xs[is.finite(xs)]; ys <- ys[is.finite(ys)]
    if (!length(xs) || !length(ys)) return(p)
    xr <- range(xs); yr <- range(ys)
    p +
      theme_void() +
      theme(plot.margin = margin(0,0,0,0), legend.position = "none") +
      scale_x_continuous(limits = xr, expand = c(0,0)) +
      scale_y_continuous(limits = yr, expand = c(0,0)) +
      coord_equal(xlim = xr, ylim = yr, clip = "on")
  }
  
  # Special case for coord_polar: lock the radial scale to the max radius
  tight_polar <- function(p, pad_mult = 0) {
    b <- ggplot_build(p)
    rmax <- suppressWarnings(max(unlist(lapply(
      b$data, function(d) c(d$y, d$ymax, d$yend, d$r, d$r1, d$r2)
    )), na.rm = TRUE))
    if (!is.finite(rmax)) rmax <- 1
    rmax <- rmax * (1 + pad_mult)
    p +
      theme_void() +
      theme(plot.margin = margin(0,0,0,0), legend.position = "none") +
      scale_y_continuous(limits = c(0, rmax), expand = c(0,0)) +
      coord_polar(clip = "on")
  }
  
  strip_modebar <- function(p) {
    p %>% config(
      displayModeBar = TRUE,
      modeBarButtons = list(list("toImage")),
      displaylogo = FALSE
    )
  }
  
  tribe_heading <- function(sel) {
    sel <- unique(stats::na.omit(sel))
    n   <- length(sel)
    if (SINGLE_TRIBE) return(TRIBES())
    if (n == 0) return("Selected Tribe(s)")
    if (n == 19) return("Selected Tribes — All Tribes")
    if (n == 1) return(paste0("Selected Tribe — ", sel))
    if (n <= 4) return(paste0("Selected Tribes — ", paste(sel, collapse = ", ")))
    paste0("Selected Tribes — ", paste(sel[1:3], collapse = ", "), " + ", n - 3, " more")
  }
  
  # Other helper functions for attendance tab plots 
  # Ensure tribal flag exists (1 = Native/tribal affiliated, 0 = Non-Native)
  if (!"tribal_student" %in% names(students_dat2)) {
    tribe_raw <- trimws(as.character(students_dat2$S_NM_STU_X.tribe))
    non_native_codes <- c(NA, "", "0", "00", "Not Applicable")
    students_dat2$tribal_student <- ifelse(tribe_raw %in% non_native_codes, 0L, 1L)
  }
  
  # --- robust race helpers ---
  norm_name <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  
  get_race_col <- function(df) {
    nms <- names(df)
    # exact fast path
    if ("STUDENTS.Ethnicity2" %in% nms) return("STUDENTS.Ethnicity2")
    # tolerant matching
    nn <- norm_name(nms)
    targets <- c("studentsethnicity2","studentethnicity2","ethnicity2","ethnicity","race")
    idx <- which(nn %in% targets)
    if (length(idx)) return(nms[idx[1]])
    NULL
  }
  
  race_choices_from <- function(df) {
    col <- get_race_col(df)
    if (is.null(col)) return(character(0))
    vals <- df[[col]]
    vals <- vals[!is.na(vals)]
    vals <- trimws(vals)
    vals <- vals[nzchar(vals)]
    vals <- vals[!vals %in% c("Not Applicable","Unknown","Other","Unspecified")]
    sort(unique(vals))        # keep original labels; no case transform
  }
  
  # NEWLY ADDED: 
  # ------- NEW: tolerant lookup + normalization for Gender, SPED, ELL -------
  get_gender_col <- function(df) {
    nms <- names(df); nn <- norm_name(nms)
    targets <- c("studentsgender2","gender2","gender")
    idx <- which(nn %in% targets)
    if (length(idx)) nms[idx[1]] else NULL
  }
  
  get_sped_col <- function(df) {
    nms <- names(df); nn <- norm_name(nms)
    targets <- c("sped","specialeducation","specialed","special_ed","iep","iepstatus","iep_status", "snmstuxsped38yn")
    idx <- which(nn %in% targets)
    if (length(idx)) nms[idx[1]] else NULL
  }
  
  get_ell_col <- function(df) {
    nms <- names(df); nn <- norm_name(nms)
    targets <- c("ell","englishlearner","ellstatus","ell_status","lep","limitedenglishproficiency", "snmstuxellyn")
    idx <- which(nn %in% targets)
    if (length(idx)) nms[idx[1]] else NULL
  }
  
  to_yesno <- function(x) {
    v <- trimws(tolower(as.character(x)))
    dplyr::case_when(
      v %in% c("1","y","yes","true","t") ~ "Yes",
      v %in% c("0","n","no","false","f") ~ "No",
      TRUE ~ NA_character_
    )
  }
  
  grade_label <- function(g) {
    v <- trimws(as.character(g))
    # normalize various Unicode dashes to ASCII hyphen
    v <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", v)
    gnum <- suppressWarnings(as.integer(v))
    dplyr::case_when(
      !is.na(gnum) & gnum == -2 ~ "Pre-K 1",
      !is.na(gnum) & gnum == -1 ~ "Pre-K 2",
      !is.na(gnum) & gnum ==  0 ~ "Kindergarten",
      !is.na(gnum) & gnum %in% 1:12 ~ as.character(gnum),
      TRUE ~ v
    )
  }
  
  # Canonical order: PK1, PK2, K, 1..12
  grade_order_codes <- c(-2, -1, 0, 1:12)
  
  # Parse messy grade inputs into an integer code
  #  -2 = Pre-K 1, -1 = Pre-K 2, 0 = Kindergarten, 1..12 = grades
  grade_code <- function(g) {
    # normalize to UTF-8 string
    v <- iconv(as.character(g), from = "", to = "UTF-8", sub = "")
    v <- tolower(trimws(v))
    # normalize odd spaces/dashes
    v <- gsub("\u00A0", " ", v)                   # NBSP -> space
    v <- gsub("[\u2010-\u2015\u2212]", "-", v)    # unicode dashes -> "-"
    # numeric? (covers -2,-1,0,1..12 when stored as numbers or numeric strings)
    gi <- suppressWarnings(as.integer(v))
    if (!is.na(gi)) return(gi)
    
    # text patterns
    if (grepl("^pre\\s*-?\\s*k\\s*1$", v) || grepl("^pk\\s*1$", v)) return(-2)
    if (grepl("^pre\\s*-?\\s*k\\s*2$", v) || grepl("^pk\\s*2$", v)) return(-1)
    if (v %in% c("k","kg","kindergarten","kinder","k-grade","k grade")) return(0)
    
    if (grepl("^grade\\s*(1[0-2]|[1-9])$", v)) return(as.integer(sub("^grade\\s*", "", v)))
    if (grepl("^(1[0-2]|[1-9])$", v))         return(as.integer(v))
    
    NA_integer_
  }
  
  # Turn the code back into a clean label for display
  grade_label_from_code <- function(code) dplyr::case_when(
    code == -2L ~ "Pre-K 1",
    code == -1L ~ "Pre-K 2",
    code ==  0L ~ "Kindergarten",
    code %in% 1:12 ~ as.character(code),
    TRUE ~ NA_character_
  )
  
  # --------- ENDS 
  
  
  build_three_group_df <- function(sel_df, n_df, nn_df, tribe_label, value_col, nn_lab) {
    labels <- c(nn_lab, "District Native", tribe_label)
    vals   <- c(
      safe_mean(nn_df[[value_col]]) * 100,
      safe_mean(n_df[[value_col]])  * 100,
      safe_mean(sel_df[[value_col]])* 100
    )
    out <- data.frame(
      Category = factor(labels, levels = labels),
      Value    = as.numeric(vals),
      stringsAsFactors = FALSE
    )
    out$Color <- ifelse(as.character(out$Category) == tribe_label, "#d62728",
                        ifelse(as.character(out$Category) == "District Native", "orange", "lightgrey"))
    out$Hover <- sprintf("<b>%s</b><br>%.2f%%", out$Category, out$Value)
    out
  }

  # IDs for the per-plot pickers (keep this list)
  nn_race_inputs <- c(
    "nn_race_present",
    "nn_race_absent_prop",
    "nn_race_truancy",
    "nn_race_adj_abs",
    "nn_race_unadj_abs",
    "nn_race_abs_type",
    "nn_race_abs_cat",
    "nn_race_tru_cat"
  )
  
  # ensure the reset buttons clear selection back to NULL (All)
  observe({
    for (id in nn_race_inputs) {
      local({
        the_id <- id
        observeEvent(input[[paste0(the_id, "_reset")]], {
          shinyWidgets::updatePickerInput(session, the_id, selected = character(0))  # ← not NULL
        }, ignoreInit = TRUE)
      })
    }
  })
  
  observeEvent(input$tabs, {
    req(input$tabs == "attendance")
    req(nrow(students_dat2) > 0)
    
    # Non-Native rows only
    nn <- dplyr::filter(students_dat2, tribal_student == 0)
    
    races <- race_choices_from(nn)  # uses your get_race_col()
    
    for (id in nn_race_inputs) {
      current  <- isolate(input[[id]])
      selected <- if (isTruthy(current) && current %in% races) current else character(0)  # ← guard + char(0)
      shinyWidgets::updatePickerInput(
        session, id,
        choices  = races,
        selected = selected,
        options  = list(
          title         = if (length(races)) "All (no filter)" else "No race values found",
          `live-search` = length(races) > 8,
          size          = 8,
          `dropup-auto` = FALSE,
          style         = "btn-light"
        )
      )
    }
  }, ignoreInit = FALSE)
  # Keep the School list scoped to the selected tribe (when in single-tribe mode)
  observeEvent(input$tabs, {
    req(input$tabs == "profile")
    
    schools <- if (SINGLE_TRIBE && "tribe_name" %in% names(lookup)) {
      sort(unique(na.omit(lookup$school_name[trimws(as.character(lookup$tribe_name)) == TRIBE()])))
    } else {
      sort(unique(na.omit(lookup$school_name)))
    }
    
    shinyWidgets::updatePickerInput(
      session, "school_filter",
      choices  = schools,
      selected = character(0),
      options  = list(
        `actions-box`  = TRUE,
        `live-search`  = TRUE,
        title          = if (length(schools)) "All schools" else "No schools available",
        size           = 8,
        `dropup-auto`  = FALSE,
        style          = "btn-light"
      )
    )
  }, ignoreInit = FALSE)
  # ---- tiny helpers used by the attendance plots ----
  safe_mean <- function(x) if (length(x) == 0 || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  safe_ymax <- function(vals, pad = 1.25, cap = 100) {
    vmax <- suppressWarnings(max(vals, na.rm = TRUE))
    if (!is.finite(vmax)) vmax <- 1
    min(vmax * pad, cap)
  }
  
  # District subset: Native/tribal-affiliated students
  district_native_df <- reactive({
    req(exists("students_dat2"))
    students_dat2 %>% dplyr::filter(tribal_student == 1)
  })
  
  # Non-Native subset, optionally filtered by a single race value from the picker
  nn_df_for <- function(sel = NULL) {
    df  <- dplyr::filter(students_dat2, tribal_student == 0)
    col <- get_race_col(df)
    if (isTruthy(sel) && !is.null(col)) {     # ← handles NULL and character(0)
      df <- df %>%
        dplyr::filter(!is.na(.data[[col]]) &
                        tolower(trimws(.data[[col]])) == tolower(trimws(sel)))
    }
    df
  }
  
  # Label for the Non-Native bar
  nn_label_for <- function(sel = NULL) {
    if (is.null(sel) || length(sel) == 0 || is.na(sel) || !nzchar(sel)) "Non-Native (All)"
    else paste0("Non-Native — ", sel)
  }
  
  # District baselines used by a few plots
  district_truancy            <- mean(students_dat2$truancy_ratio,           na.rm = TRUE)
  district_adj_absenteeism    <- mean(students_dat2$adj_absenteeism_ratio,   na.rm = TRUE)
  district_unadj_absenteeism  <- mean(students_dat2$absenteeism_ratio,       na.rm = TRUE)
  
  # ---- Incident events for discipline trend(s) ----
  incident_events <- reactive({
    req(exists("disc_wide"), exists("date_wide"), exists("students_dat2"))
    if (nrow(disc_wide) == 0 || nrow(date_wide) == 0) {
      # empty but well-formed result to keep downstream code safe
      return(tibble::tibble(
        dcid = character(),
        index = character(),
        description = character(),
        date = as.Date(character()),
        School = character(),
        Tribe = character(),
        tribal_student = integer(),
        Grade = character()
      ))
    }
    
    # 1) Unpivot descriptions, keep the trailing numeric index, make dcid comparable
    disc_long <- disc_wide %>%
      tidyr::pivot_longer(
        cols = tidyselect::starts_with("Discipline_"),
        names_to = "incident_col",
        values_to = "description",
        values_drop_na = TRUE
      ) %>%
      dplyr::mutate(
        index = sub(".*_(\\d+)$", "\\1", incident_col),
        dcid  = as.character(dcid)
      ) %>%
      dplyr::select(dcid, index, description)
    
    # 2) Unpivot dates with the same index rule; coerce to Date + comparable dcid
    date_long <- date_wide %>%
      tidyr::pivot_longer(
        cols = tidyselect::starts_with("Incident_Date_"),
        names_to = "date_col",
        values_to = "date",
        values_drop_na = TRUE
      ) %>%
      dplyr::mutate(
        index = sub(".*_(\\d+)$", "\\1", date_col),
        dcid  = as.character(dcid),
        date  = as.Date(date)
      ) %>%
      dplyr::select(dcid, index, date)
    
    # 3) Student attributes used for filtering/labels
    student_info <- students_dat2 %>%
      dplyr::transmute(
        dcid   = as.character(STUDENTS.dcid),
        School = SCHOOLS.Name,
        Tribe  = dplyr::case_when(
          is.na(tribe_name) ~ NA_character_,
          trimws(tribe_name) %in% c("Not Applicable", "0", "00", "") ~ "Non-Native American",
          TRUE ~ tribe_name
        ),
        tribal_student = {
          # keep your earlier flag if it exists; otherwise compute it
          if ("tribal_student" %in% names(students_dat2)) as.integer(tribal_student) else
            dplyr::if_else(
              is.na(tribe_name) | trimws(tribe_name) %in% c("Not Applicable","0","00",""),
              0L, 1L
            )
        },
        Grade_raw = suppressWarnings(as.numeric(Grade_level)),
        Grade = dplyr::case_when(
          Grade_raw == -2 ~ "Pre-K 1",
          Grade_raw == -1 ~ "Pre-K 2",
          Grade_raw ==  0 ~ "Kindergarten",
          TRUE            ~ as.character(Grade_raw)
        )
      )
    
    # 4) Join description + date + student info
    ev <- dplyr::inner_join(disc_long, date_long, by = c("dcid", "index")) %>%
      dplyr::left_join(student_info, by = "dcid") %>%
      dplyr::filter(!is.na(date))
    
    # 5) Respect single-tribe mode
    if (SINGLE_TRIBE) {
      ev <- dplyr::filter(ev, Tribe == TRIBE())
    }
    
    ev
  })
  
  
  # -- render dynamic sidebar menu --
  output$sidebar_ui <- renderMenu({
    bs4Dash::bs4SidebarMenu(
      id = "tabs",
      bs4Dash::menuItem("Overview",        tabName = "overview",   icon = icon("dashboard")),
      bs4Dash::menuItem("Attendance",      tabName = "attendance", icon = icon("calendar-check")),
      bs4Dash::menuItem("Data Explorer",    tabName = "dataviews", icon = icon("table")),
      bs4Dash::menuItem("Student Lookup", tabName = "profile",    icon = icon("users")),
      bs4Dash::menuItem("Incidents Calendar", tabName = "calendar", icon = icon("calendar-day")),
      # only show Admin View if admin
      if (is_admin()) {
        bs4Dash::menuItem("Admin", tabName = "admin", icon = icon("user-shield"))
      }
    )
  })
  # Update attendance numeric and slider input to be in sync 
  observeEvent(input$att_threshold, {
    updateNumericInput(session, "att_threshold_num", value = input$att_threshold)
  })
  
  observeEvent(input$att_threshold_num, {
    updateSliderInput(session, "att_threshold", value = input$att_threshold_num)
  })
  # Update gpa numeric and slider input to be in sync 
  observeEvent(input$gpa_threshold, {
    updateNumericInput(session, "gpa_threshold_num", value = input$gpa_threshold)
  })
  
  observeEvent(input$gpa_threshold_num, {
    updateSliderInput(session, "gpa_threshold", value = input$gpa_threshold_num)
  })
  # -- render Admin table --
  output$admin_table <- DT::renderDT({
    req(is_admin())
    creds <- read_creds()
    DT::datatable(
      creds[, c("user", "password", "email", "admin")],
      class    = "stripe hover",
      selection = "single",
      options = list(pageLength = 10, scrollX = TRUE)
                     # # this JS will fire after the table header is drawn:
                     # headerCallback = JS(
                     #   "function(thead, data, start, end, display){",
                     #   "  // set the entire header row red with white text",
                     #   "  $(thead).find('th').css({",
                     #   "    'background-color': 'orange',",
                     #   "    'color': 'white',",
                     #   "    'border-bottom': '1px solid #aaa'",
                     #   "  });",
                     #   "}"
                     # )
    )
  })
  observeEvent(input$add_user, {
    req(is_admin())  
    # basic validation
    req(input$new_user, input$new_pw, input$new_email)
    creds <- read_creds()
    
    if (input$new_user %in% creds$user) {
      output$add_user_msg <- renderUI(
        span("That username already exists.", style = "color: red;")
      )
      return()
    }
    
    # append new row
    new_row <- data.frame(
      user     = input$new_user,
      #password = input$new_pw,
      password = hash_pw(input$new_pw),
      email    = input$new_email,
      admin    = as.logical(input$new_admin),
      stringsAsFactors = FALSE
    )
    updated <- bind_rows(creds, new_row)
    
    # save back to disk
    save_creds(updated)
    
    updatePickerInput(
      session,
      "delete_user",
      choices = updated$user
    )
    
    # give feedback and refresh table
    output$add_user_msg <- renderUI(
      span("New user added!", style = "color: #CC5500;")
    )
    # re-render the table immediately
    output$admin_table <- DT::renderDT({
      DT::datatable(
        updated[, c("user","password","email","admin")],
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    
    # clear inputs
    updateTextInput(session,   "new_user",  value = "")
    updateTextInput(session,   "new_email", value = "")
    updateCheckboxInput(session,"new_admin", value = FALSE)
    updateTextInput(session,   "new_pw",    value = "")
  })
  observeEvent(input$delete_btn, {
    req(is_admin())
    user_to_delete <- input$delete_user
    # prevent admin nuking themselves accidentally
    if (user_to_delete == user_r()) {
      output$delete_msg <- renderUI(
        span("You can’t delete your own account while logged in.", style = "color: red;")
      )
      return()
    }
    creds <- read_creds()
    # check it still exists
    if (! user_to_delete %in% creds$user) {
      output$delete_msg <- renderUI(
        span("That user no longer exists.", style = "color: #CC5500;")
      )
      return()
    }
    # remove and save
    updated <- creds[ creds$user != user_to_delete, ]
    save_creds(updated)
    # update UI
    output$delete_msg <- renderUI(
      span(paste0("User “", user_to_delete, "” deleted."), style = "color: #CC5500;")
    )
    # refresh the table
    output$admin_table <- DT::renderDT({
      DT::datatable(
        updated[, c("user","password","email","admin")],
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
    # refresh the delete dropdown choices
    #updateSelectInput(session, "delete_user", choices = updated$user)
    updatePickerInput(
      session,
      "delete_user",
      choices = updated$user
    )
  })
  
  # rebuild_cache_and_reload <- function() {
  #   
  #   old_wd <- getwd()
  #   on.exit(setwd(old_wd), add = TRUE)
  #   
  #   # 1) Download raw files from OneDrive → TEMP ONLY
  #   staging_dir <- download_shared_folder_to_temp(
  #     "https://1drv.ms/f/c/2995c09075f4b77e/IgDC8kq0KT0NTIgsGUxEyGDBAUtD8KMf8YU4vG2paps0Zrk"
  #   )
  #   
  #   on.exit({
  #     Sys.unsetenv("BPS_RAW_DIR")
  #     if (dir.exists(staging_dir)) {
  #       unlink(staging_dir, recursive = TRUE, force = TRUE)
  #     }
  #   }, add = TRUE)
  #   
  #   # 2) Tell Analyses.R where raw data lives (TEMP)
  #   Sys.setenv(BPS_RAW_DIR = staging_dir)
  #   
  #   # 3) Run preprocessing (creates cache.qs)
  #   setwd(file.path(old_wd, "preprocess"))
  #   source("Analyses.R", local = FALSE)
  #   
  #   # 4) Reload cache
  #   load_cache(file.path(old_wd, "cache.qs"))
  #   
  #   invisible(TRUE)
  # }
  download_cache_and_reload <- function() {
    
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    
    # 1) Download latest cache.qs from Drive into the app folder
    #drive_download_cache_qs(dest = file.path(old_wd, "cache.qs"))
    download_cache_qs_public(dest = file.path(old_wd, "cache.qs"))
    
    
    # 2) Load it using your existing loader
    load_cache(file.path(old_wd, "cache.qs"))
    
    invisible(TRUE)
  }
  
  # -------------------------------
  # Data refresh: rebuild cache.qs
  # -------------------------------
  output$data_refresh_status <- renderText("")
  
  observeEvent(input$apply_new_cache, {
    #req(is_admin())
    req(!is.null(user_r()))
    
    output$data_refresh_status <- renderText("Refreshing data…")
    
    tryCatch({
      withProgress(message = "Refreshing dashboard data…", value = 0, {
        
        incProgress(0.10, detail = "Downloading latest cache.qs…")
        Sys.sleep(0.05)  # lets UI paint the progress box
        
        download_cache_and_reload()
        
        incProgress(0.90, detail = "Reloading dashboard…")
        Sys.sleep(0.05)
      })
      
      output$data_refresh_status <- renderText("Refresh complete ✅ ")
      
      # Trigger Shiny to rebuild outputs after cache load
      cache_version(cache_version() + 1)
      
      new_data_available(FALSE)
      
      # Mark refresh complete
      data_refreshed_flag(TRUE)
      
      # After the UI re-renders, force a tab "reset" by hopping away then back
      later::later(function() {
        updateTabItems(session, "tabs", "attendance")
        
        later::later(function() {
          updateTabItems(session, "tabs", "overview")
        }, 0.15)
        
      }, 0.35)
      
      # Auto-clear banner
      later::later(function() {
        data_refreshed_flag(FALSE)
      }, 10) 
      
      showNotification("Data refreshed. Dashboard updated.", type = "message")
      
      # ✅ FULL APP RELOAD (forces static UI to re-evaluate)
      # later::later(function() {
      #   session$reload()
      # }, 0.5)
      
      # Step 3 will add session reload here
      
    }, error = function(e) {
      output$data_refresh_status <- renderText(
        paste("Refresh failed:", conditionMessage(e))
      )
    })
  })
  
  output$main_content <- renderUI({
    cache_version()   # <-- ADD THIS LINE (forces UI rebuild after refresh)
    
    query <- parseQueryString(session$clientData$url_search)
    token <- query$token
    # Password reset via email
    if (!is.null(token) && token %in% reset_tokens()$token) {
      fluidRow(
        column(width=4, offset=4,
               div(class="panel panel-default", div(class="panel-body",
                                                    h3("Reset your password"),
                                                    textInput("reset_user", "Username"),
                                                    passwordInput("reset_pw1", "New password"),
                                                    passwordInput("reset_pw2", "Confirm new password"),
                                                    actionButton("submit_reset", "Reset Password"),
                                                    textOutput("reset_status")
               ))
        )
      )
    }
    else if (is.null(user_r())) {
      tags$div(
        class = "login-page",
        tags$head(
          tags$style(HTML("
            .login-page {
              min-height: 100vh;
              background-color: #f4f6f9;
              display: flex;
              align-items: center;
              justify-content: center;
            }
            .login-card {
              background: #fff;
              box-shadow: 0 4px 20px rgba(0,0,0,0.09);
              border-radius: 14px;
              padding: 32px 26px 22px 26px;
              width: 100%;
              max-width: 360px;
            }
            .login-logo-row {
              display: flex;
              align-items: center;
              justify-content: center;
              margin-bottom: 20px;
            }
            .login-title {
              font-size: 1.45rem;
              font-weight: 600;
              color: #222;
              margin-left: 14px;
              letter-spacing: 0.5px;
            }
          "))
        ),
        div(class = "login-card",
            div(class="login-logo-row",
                img(src = "bps_logo.png", height = "70px"),
                span("Please Enter Your Credentials", class="login-title")
            ),
            textInput("login_user", "Username"),
            passwordInput("login_pw", "Password"),
            actionButton("login_btn", "Login", width="100%"),
            div(
              style = "margin-top:12px; text-align:center;",
              actionLink("forgot_pw", "Forgot your password?")
            ),
            textOutput("login_status")
        )
      )
    }
    else {
      dashboard_content()
    }
  })
  output$user_name_display <- renderUI({
    req(user_r())
    div(
      style = "text-align: center; font-weight: bold;",
      paste("Logged in as:", user_r())
    )
  })
  observeEvent(input$login_btn, {
    creds <- read_creds()
    u <- trimws(input$login_user)
    p <- input$login_pw
    
    i <- which(creds$user == u)
    valid <- length(i) == 1 && verify_pw(p, creds$password[i])
    
    if (valid) {
      # Auto-upgrade legacy plaintext password -> hashed on successful login
      if (!is_probably_hash(creds$password[i])) {
        creds$password[i] <- hash_pw(p)
        save_creds(creds)
      }
      
      user_r(u)
      output$login_status <- renderText("")
      updateTabItems(session, "tabs", "attendance")
      updateTabItems(session, "tabs", "overview")
    } else {
      output$login_status <- renderText("Invalid credentials.")
    }
  })
  # observeEvent(input$custom_logout, {
  #   user_r(NULL)
  #   updateTextInput(session, "login_user", value="")
  #   updateTextInput(session, "login_pw", value="")
  #   output$login_status <- renderText("")
  # })
  observeEvent(input$custom_logout, {
    # turn off tooltips
    tips_on(FALSE)
    session$sendCustomMessage('tipMode', list(enabled = FALSE))
    
    # clear user + login inputs
    user_r(NULL)
    updateTextInput(session, "login_user", value = "")
    updateTextInput(session, "login_pw", value = "")
    output$login_status <- renderText("")
    
    # IMPORTANT: do NOT session$reload()
    # Let output$main_content re-render to the login page naturally
  }, ignoreInit = TRUE)
  observeEvent(input$forgot_pw, {
    showModal(modalDialog(
      title = "Reset password",
      textInput("reset_email", "Enter your email address:"),
      actionButton("send_reset", "Send reset link"),
      easyClose = TRUE
    ))
  })
  observeEvent(input$send_reset, {
    req(input$reset_email)
    creds <- read_creds()
    user_row <- creds[creds$email == input$reset_email, ]
    if (nrow(user_row)==1) {
      token <- UUIDgenerate()
      expires <- as.numeric(Sys.time()) + 15*60
      df <- rbind(reset_tokens(), data.frame(token=token, user=user_row$user, expires=expires))
      reset_tokens(df)
      app_url <- paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname,
                        ifelse(session$clientData$url_port == "", "", paste0(":", session$clientData$url_port)),
                        session$clientData$url_pathname)
      link <- paste0(app_url, "?token=", token)
      email <- compose_email(
        body = md(c("A password reset was requested for your account.",
                    "",
                    paste0("[Click here to reset your password](", link, ")"),
                    "",
                    "If you did not request this, ignore this email."))
      )
      smtp_send(
        email,
        from = email_from,
        to = user_row$email,
        subject = "Password Reset for Tribal Dashboard",
        credentials = my_email_creds
      )
      removeModal()
      showModal(modalDialog(
        "Reset link sent! Please check your email.",
        easyClose=TRUE
      ))
    } else {
      showModal(modalDialog(
        "Email not found.",
        easyClose=TRUE
      ))
    }
  })
  observeEvent(input$submit_reset, {
    query <- parseQueryString(session$clientData$url_search)
    token <- query$token
    tok_df <- reset_tokens()
    row <- which(tok_df$token==token)
    if (length(row)!=1 || Sys.time() > as.POSIXct(tok_df$expires[row], origin="1970-01-01")) {
      output$reset_status <- renderText("Reset link invalid or expired.")
      return()
    }
    creds <- read_creds()
    u <- input$reset_user
    if (nrow(creds[creds$user==u,]) != 1) {
      output$reset_status <- renderText("Username not found.")
      return()
    }
    pw1 <- input$reset_pw1
    pw2 <- input$reset_pw2
    if (pw1 != pw2 || nchar(pw1)<4) {
      output$reset_status <- renderText("Passwords do not match or too short.")
      return()
    }
    #creds$password[creds$user==u] <- pw1
    creds$password[creds$user==u] <- hash_pw(pw1)
    save_creds(creds)
    output$reset_status <- renderText("Password updated! You can now login.")
    tok_df <- tok_df[-row,]
    reset_tokens(tok_df)
  })
  # -------------- INSERT YOUR RENDER CODE FOR DASHBOARD OUTPUTS HERE --------------
  ######################
  ######################
  # YOUR SERVER CONTENT 
  ######################
  ######################
  observeEvent(list(user_r(), cache_version()), {
    req(user_r())
  #   # Handle a logout with a page reload (reloading page with secure_app brings you back to login page)
  #   observeEvent(input$custom_logout, {
  #       tips_on(FALSE)
  # session$sendCustomMessage('tipMode', list(enabled = FALSE))
  #     session$reload()
  #   })
    # Logic to handle show courses 
    show_courses <- reactiveVal(FALSE)
    observeEvent(input$show_courses_link, {
      show_courses(!show_courses())
    })
    show_discipline <- reactiveVal(FALSE)
    observeEvent(input$show_discipline_link, {
      show_discipline(!show_discipline())
    })
    # Filter data by tribe and allow output to be based on that 
    # filtered_data <- reactive({
    #   students_dat %>%
    #     filter(tribe_name %in% input$tribe_select)
    # })
    filtered_data <- reactive({
      students_dat %>% dplyr::filter(tribe_name %in% TRIBES())
    })
    
    # ---- OVERVIEW TAB PLOTS ----- 
    
    # Dynamic Grade‐Distribution box
    output$grades_box <- renderUI({
      #tribes <- input$tribe_select
      tribes <- TRIBES()
      title_text <- if (length(tribes) == 1) {
        paste0("Grade Distribution for ", tribes)
      } else {
        paste0("Grade Distribution for ", length(tribes), " Tribes")
      }
      box(
        title    = title_text,
        width    = 12,
        status   = "danger",
        solidHeader = TRUE,
        plotlyOutput("grades")
      )
    })
    
    # Dynamic School‐Breakdown box
    output$schools_box <- renderUI({
      #tribes <- input$tribe_select
      tribes <- TRIBES()
      title_text <- if (length(tribes) == 1) {
        paste0("School Breakdown for ", tribes)
      } else {
        paste0("School Breakdown for ", length(tribes), " Tribes")
      }
      box(
        title    = title_text,
        width    = 12,
        status   = "danger",
        solidHeader = TRUE,
        plotlyOutput("schools")
      )
    })
    
    # ---- District Profile ---- 
    # Filter based on thresholds 
    # min_att_district <- reactive({
    #   threshold <- input$att_threshold / 100
    #   subset(students_dat2, proportion_absent >= threshold) # absences more than threshold
    # })
    # min_gpa_district <- reactive({
    #   threshold <- input$gpa_threshold 
    #   subset(students_dat2, GPA >= threshold) # absences more than threshold
    # })
    # 
    # # Calculate inputs 
    # # GPA 
    # district_gpa<-round(mean(students_dat2$GPA, na.rm=TRUE),2)
    # # Weighted GPA
    # district_wgpa<-round(mean(students_dat2$Cumulative_GPA, na.rm=TRUE),2)
    # # Performance Category 
    # district_performance<-get_mode(students_dat2$Performance)
    # # Proportion of Students with Disciplinary Records 
    # district_disc<-round(nrow(students_dat2[students_dat2$Number_disciplines>0,])/nrow(students_dat2),2)
    # # Number of Periods Absent 
    # district_periods_absent<-round(mean(students_dat2$periods_absent, na.rm=TRUE), 0)
    # # Proportion Cultural Days = Proportion of Excused Cultural Days 
    # district_cultural<-round(mean(students_dat2$proportion_culture, na.rm=TRUE), 2)
    # # Proportion Present 
    district_prop_present<-round(mean(students_dat2$proportion_present, na.rm=TRUE), 2)
    # # Proportion of Absences = Proportion of Period-Level Absences 
    district_prop_absent<-round(mean(students_dat2$proportion_absent, na.rm=TRUE), 2)
    # # Unexcused Absences Out of Total Absences
    # district_prop_unexcused<-round(mean(students_dat2$proportion_unexcused_absence, na.rm=TRUE), 2)
    # # Truancy Ratio = unexcused absences/(total school days - excused absences)
    # district_truancy_ratio<-round(mean(students_dat2$truancy_ratio, na.rm=TRUE), 2)
    # # Adjusted Absenteeism Ratio = unexcused absences/(total school days - excused absences)
    # district_adj_absent_ratio<-sprintf("%.2f", round(mean(students_dat2$adj_absenteeism_ratio, na.rm=TRUE), 2))
    # # District Profile 
    # ---- District card: Native vs Non-Native side-by-side ----
    # Use the combined district dataset that includes BOTH groups and a tribal_student indicator.
    # If you named it differently than `students_all2`, just swap the object name below.
    district_base <- students_dat2
    
    # Ensure the tribal_student flag exists (1 = Native/tribal affiliated, 0 = Non-Native)
    # if (!"tribal_student" %in% names(district_base)) {
    #   district_base$tribal_student <- ifelse(district_base$S_NM_STU_X.tribe != "00", 1, 0)
    # }
    
    # Helper to compute all card metrics for a given subgroup
    calc_metrics <- function(df, att_thr, gpa_thr) {
      n <- nrow(df)
      safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
      pct <- function(x) if (is.na(x)) "—" else paste0(round(x * 100), "%")
      gpa_hi <- if (n > 0) sum(df$GPA >= gpa_thr, na.rm = TRUE) / n else NA_real_
      abs_hi <- if (n > 0) sum(df$proportion_absent >= att_thr, na.rm = TRUE) / n else NA_real_
      
      list(
        n                 = if (n > 0) length(unique(df$STUDENTS.dcid)) else 0,
        gpa               = round(safe_mean(df$GPA), 2),
        wgpa              = round(safe_mean(df$Cumulative_GPA), 2),
        performance       = get_mode(df$Performance),
        disc              = pct(if (n > 0) sum(df$Number_disciplines > 0, na.rm = TRUE) / n else NA_real_),
        prop_absent       = pct(safe_mean(df$proportion_absent)),
        prop_culture      = pct(safe_mean(df$proportion_culture)),
        prop_unexcused    = pct(safe_mean(df$proportion_unexcused_absence)),
        truancy_ratio    = pct(safe_mean(df$truancy_ratio)),
        adj_absent_ratio = pct(safe_mean(df$adj_absenteeism_ratio)),
        thr_abs           = pct(abs_hi),
        thr_gpa_hi        = pct(gpa_hi),
        thr_gpa_lo        = pct(if (is.na(gpa_hi)) NA_real_ else (1 - gpa_hi))
      )
    }
    
    # Render the side-by-side card
    output$profile_district <- renderUI({
      att_thr <- input$att_threshold / 100
      gpa_thr <- input$gpa_threshold
      
      native_df    <- subset(district_base, tribal_student == 1)
      nonnative_df <- subset(district_base, tribal_student == 0)
      
      M_native <- calc_metrics(native_df, att_thr, gpa_thr)
      M_non    <- calc_metrics(nonnative_df, att_thr, gpa_thr)
      
      tt <- list(
        n_non      = "Count of enrolled Non-Native American students included.",
        n_native   = "Count of enrolled Native American students included.",
        gpa    = "Unweighted 0–4 GPA averaged across students.",
        wgpa   = "Weighted GPA (e.g., honors/AP weighting) averaged across students.",
        perf   = "Most common overall performance category across students.",
        disc   = "Share of students with one or more recorded disciplinary incidents.",
        pabs   = "Shows the percentage of class periods a student was absent out of all scheduled periods. Helps monitor general attendance trends.",
        cult   = "Displays the percentage of absences that were excused due to cultural observances. Highlights culturally valid attendance exceptions.",
        unx    = "Percentage of total absences that were unexcused. A high rate may indicate truancy or disengagement.",
        tr     = "Rate of unexcused absences calculated relative to periods not excused for valid reasons. Helps assess potential truancy behavior with more precision.",
        adj    = "Rate of absenteeism refined by removing excused cultural days, providing a clearer view of potentially concerning absences.",
        thrA   = "Percent of students at or above the Absences (%) threshold (see sidebar).",
        thrGhi = "Percent of students at or above the Simple GPA threshold (see sidebar).",
        thrGlo = "Percent of students below the Simple GPA threshold (complement)."
      )
      
      fluidRow(
        column(
          width = 12,
          bs4Card(
            title       = "District Profile — Native vs Non-Native",
            status      = "danger",
            width       = 12,
            style       = "position: relative; overflow: visible",
            solidHeader = TRUE,
            collapsible = TRUE,
            icon        = icon("id-card"),
            HTML(glue::glue("
          <!-- Background image -->
          <div style='position: relative; width:100%; height:300px;'>
            <img src='indian_ed2.png'
                 style='position: absolute;
                        top: 50%;
                        left: 50%;
                        transform: translate(-50%, -50%);
                        height: 300px;
                        opacity: 0.07;
                        z-index: 0;'>
          </div>

          <!-- Overlay content -->
          <div style='position: relative; z-index: 1; margin-top: -316px; padding: 10px 5px 4px 5px;'>

            <div style='display:grid; grid-template-columns: 1fr 1fr; gap: 24px; align-items:start;'>
            
            <!-- Non-Native column -->
              <div>
                <h4 style='margin:4px 0 8px 0;'>Non-Native American</h3>
                <ul style='list-style: none; margin: 0; padding: 0; font-size: 16px;'>
                  <li><strong>{as.character(tooltip_span('Number of Students:',   tt$n_non))}</strong> {scales::comma(M_non$n)}</li>
                  <li><strong>{as.character(tooltip_span('Simple GPA:',           tt$gpa))}</strong>  {M_non$gpa}</li>
                  <li><strong>{as.character(tooltip_span('Weighted GPA:',         tt$wgpa))}</strong> {M_non$wgpa}</li>
                  <li><strong>{as.character(tooltip_span('Performance Category:', tt$perf))}</strong> {M_non$performance}</li>
                  <li><strong>{as.character(tooltip_span('Students w/ Disciplinary Incidents:', tt$disc))}</strong> {M_non$disc}</li>
                  <li><strong>{as.character(tooltip_span('Avg. Periods Absent:',  tt$pabs))}</strong> {M_non$prop_absent}</li>
                  <li><strong>{as.character(tooltip_span('Avg. Excused Cultural Days:', tt$cult))}</strong> {M_non$prop_culture}</li>
                  <li><strong>{as.character(tooltip_span('Avg. Unexcused Days:',  tt$unx))}</strong>  {M_non$prop_unexcused}</li>
                  <li><strong>{as.character(tooltip_span('Truancy Rate:',         tt$tr))}</strong>   {M_non$truancy_ratio}</li>
                  <li><strong>{as.character(tooltip_span('Adj. Absenteeism Rate:',tt$adj))}</strong>  {M_non$adj_absent_ratio}</li>
                </ul>

                <p style='text-decoration: underline; font-size:16px; font-weight:bold; color: black; margin:10px 0;'>
                  Percentages Based on Thresholds Sidebar
                </p>
                <div style='border: 1px solid #c0392b; background-color: #f9f9f9; padding: 10px; border-radius: 5px; font-size: 16px;'>
                  <ul style='list-style: none; margin: 0; padding: 0;'>
                    <li><strong>{as.character(tooltip_span('Above Absence Threshold:', tt$thrA))}</strong>
                      <span style='color: #ff8c00; font-weight: bold;'>{M_non$thr_abs}</span></li>
                    <li><strong>{as.character(tooltip_span('Above GPA Threshold:', tt$thrGhi))}</strong>
                      <span style='color: #ff8c00; font-weight: bold;'>{M_non$thr_gpa_hi}</span></li>
                    <li><strong>{as.character(tooltip_span('Below GPA Threshold:', tt$thrGlo))}</strong>
                      <span style='color: #ff8c00; font-weight: bold;'>{M_non$thr_gpa_lo}</span></li>
                  </ul>
                </div>
              </div>

              <!-- Native column -->
              <div>
                <h4 style='margin:4px 0 8px 0;'>Native American</h3>
                <ul style='list-style: none; margin: 0; padding: 0; font-size: 16px;'>
                  <li><strong>{as.character(tooltip_span('Number of Students:',   tt$n_native))}</strong> {scales::comma(M_native$n)}</li>
                  <li><strong>{as.character(tooltip_span('Simple GPA:',           tt$gpa))}</strong>  {M_native$gpa}</li>
                  <li><strong>{as.character(tooltip_span('Weighted GPA:',         tt$wgpa))}</strong> {M_native$wgpa}</li>
                  <li><strong>{as.character(tooltip_span('Performance Category:', tt$perf))}</strong> {M_native$performance}</li>
                  <li><strong>{as.character(tooltip_span('Students w/ Disciplinary Incidents:', tt$disc))}</strong> {M_native$disc}</li>
                  <li><strong>{as.character(tooltip_span('Avg. Periods Absent:',  tt$pabs))}</strong> {M_native$prop_absent}</li>
                  <li><strong>{as.character(tooltip_span('Avg. Excused Cultural Days:', tt$cult))}</strong> {M_native$prop_culture}</li>
                  <li><strong>{as.character(tooltip_span('Avg. Unexcused Days:',  tt$unx))}</strong>  {M_native$prop_unexcused}</li>
                  <li><strong>{as.character(tooltip_span('Truancy Rate:',         tt$tr))}</strong>   {M_native$truancy_ratio}</li>
                  <li><strong>{as.character(tooltip_span('Adj. Absenteeism Rate:',tt$adj))}</strong>  {M_native$adj_absent_ratio}</li>
                </ul>

                <p style='text-decoration: underline; font-size:16px; font-weight:bold; color: black; margin:10px 0;'>
                  Percentages Based on Thresholds Sidebar
                </p>
                <div style='border: 1px solid #c0392b; background-color: #f9f9f9; padding: 10px; border-radius: 5px; font-size: 16px;'>
                  <ul style='list-style: none; margin: 0; padding: 0;'>
                    <li><strong>{as.character(tooltip_span('Above Absence Threshold:', tt$thrA))}</strong>
                      <span style='color: #ff8c00; font-weight: bold;'>{M_native$thr_abs}</span></li>
                    <li><strong>{as.character(tooltip_span('Above GPA Threshold:', tt$thrGhi))}</strong>
                      <span style='color: #ff8c00; font-weight: bold;'>{M_native$thr_gpa_hi}</span></li>
                    <li><strong>{as.character(tooltip_span('Below GPA Threshold:', tt$thrGlo))}</strong>
                      <span style='color: #ff8c00; font-weight: bold;'>{M_native$thr_gpa_lo}</span></li>
                  </ul>
                </div>
              </div>


            </div> <!-- grid -->
          </div> <!-- overlay -->
        "))
          )
        )
      )
    })
    
    # ---- Tribal Profile -----
    # filtered_data2 <- reactive({
    #   students_dat2 %>%
    #     filter(tribe_name %in% input$tribe_select)
    # })
    # filtered_data2 <- reactive({
    #   students_dat2 %>% filter(tribe_name %in% input$tribe_select)
    # })
    filtered_data2 <- reactive({
      students_dat2 %>% dplyr::filter(tribe_name %in% TRIBES())
    })
    # Filter based on thresholds 
    min_att_district2 <- reactive({
      threshold <- input$att_threshold / 100
      subset(filtered_data2(), proportion_absent >= threshold) # absences more than threshold
    })
    min_gpa_district2 <- reactive({
      threshold <- input$gpa_threshold 
      subset(filtered_data2(), GPA >= threshold) # absences more than threshold
    })
    # Selected Tribe(s) Profile 
    output$profile_tribes <- renderUI({
      info<-filtered_data2()
      complement<-(1 - round(nrow(min_gpa_district2())/nrow(filtered_data2()),2))*100
      # Dynamic Title
      Title<-if(length(unique(na.omit(info$tribe_name)))==1){
        title=paste0("Profile for ", unique(na.omit(info$tribe_name)))
      }else{
        title=paste0("Profile for ", length(unique(na.omit(info$tribe_name))), " Tribes")
      }
      #header_text <- tribe_heading(input$tribe_select)
      header_text <- tribe_heading(TRIBES())
      # Hover over text 
      if(SINGLE_TRIBE){
        stt <- list(
          n      = paste0("Count of enrolled students in ", TRIBES(), "."),
          gpa    = "Unweighted 0–4 GPA averaged across students.",
          wgpa   = "Weighted GPA (e.g., honors/AP weighting) averaged across students.",
          perf   = "Most common overall performance category across students.",
          disc   = "Share of students with one or more recorded disciplinary incidents.",
          pabs   = "Shows the percentage of class periods a student was absent out of all scheduled periods. Helps monitor general attendance trends.",
          cult   = "Displays the percentage of absences that were excused due to cultural observances. Highlights culturally valid attendance exceptions.",
          unx    = "Percentage of total absences that were unexcused. A high rate may indicate truancy or disengagement.",
          tr     = "Rate of unexcused absences calculated relative to periods not excused for valid reasons. Helps assess potential truancy behavior with more precision.",
          adj    = "Rate of absenteeism refined by removing excused cultural days, providing a clearer view of potentially concerning absences.",
          thrA   = "Percent of students at or above the Absences (%) threshold (see sidebar).",
          thrGhi = "Percent of students at or above the Simple GPA threshold (see sidebar).",
          thrGlo = "Percent of students below the Simple GPA threshold (complement)."
        )
      }else{
      stt <- list(
        n      = "Count of enrolled students included in the selected tribe(s).",
        gpa    = "Unweighted 0–4 GPA averaged across students.",
        wgpa   = "Weighted GPA (e.g., honors/AP weighting) averaged across students.",
        perf   = "Most common overall performance category across students.",
        disc   = "Share of students with one or more recorded disciplinary incidents.",
        pabs   = "Shows the percentage of class periods a student was absent out of all scheduled periods. Helps monitor general attendance trends.",
        cult   = "Displays the percentage of absences that were excused due to cultural observances. Highlights culturally valid attendance exceptions.",
        unx    = "Percentage of total absences that were unexcused. A high rate may indicate truancy or disengagement.",
        tr     = "Rate of unexcused absences calculated relative to periods not excused for valid reasons. Helps assess potential truancy behavior with more precision.",
        adj    = "Rate of absenteeism refined by removing excused cultural days, providing a clearer view of potentially concerning absences.",
        thrA   = "Percent of students at or above the Absences (%) threshold (see sidebar).",
        thrGhi = "Percent of students at or above the Simple GPA threshold (see sidebar).",
        thrGlo = "Percent of students below the Simple GPA threshold (complement)."
      )}
      # GPA
      info_gpa<-round(mean(info$GPA, na.rm=TRUE),2)
      # Weighted GPA
      info_wgpa<-if(is.nan(mean(info$Cumulative_GPA, na.rm=TRUE))){
        "Not Available"
      }else{
        round(mean(info$Cumulative_GPA, na.rm=TRUE),2)
      }
      # Performance Category
      info_performance<-get_mode(info$Performance)
      # Proportion of Students with Disciplinary Records
      info_disc<-round(nrow(info[info$Number_disciplines>0,])/nrow(info),2)
      # Number of Periods Absent
      info_periods_absent<-round(mean(info$periods_present, na.rm=TRUE), 0)
      # Proportion Cultural Days = Proportion of Excused Cultural Days
      info_cultural<-round(mean(info$proportion_culture, na.rm=TRUE), 2)
      # Proportion of Absences = Proportion of Period-Level Absences
      info_prop_absent<-round(mean(info$proportion_absent, na.rm=TRUE), 2)
      # Unexcused Absences Out of Total Absences
      info_prop_unexcused<-round(mean(info$proportion_unexcused_absence, na.rm=TRUE), 2)
      # Truancy Ratio = unexcused absences/(total school days - excused absences)
      info_truancy_ratio <- paste0(round(100 * mean(info$truancy_ratio, na.rm = TRUE)), "%")
      # Adj. Absenteeism Ratio
      info_adj_absent_ratio <- paste0(round(100 * mean(info$adj_absenteeism_ratio, na.rm = TRUE)), "%")
      fluidRow(
        column(
          width = 12,
          bs4Card(
            title = Title,
            status = "danger",
            width = 12,
            style = "position: relative; overflow: visible; margin: 4px 12px;",
            solidHeader = TRUE,
            collapsible = TRUE,
            icon = icon("people-roof"),
            HTML(glue::glue("
          <div>
           <h3 style='margin:4px 0 8px 0;'>{htmltools::htmlEscape(header_text)}</h3>
          <ul style='list-style:none; padding-left:0; font-size:16px;'>
           <li><strong>{as.character(tooltip_span('Number of Students:', stt$n))}</strong> {comma(nrow(info))}</li>
          <li><strong>{as.character(tooltip_span('Tribe(s) Simple GPA:', stt$gpa))}</strong> {info_gpa}</li>
          <li><strong>{as.character(tooltip_span('Tribe(s) Weighted GPA:', stt$wgpa))}</strong> {info_wgpa}</li>
          <li><strong>{as.character(tooltip_span('Performance Category:', stt$perf))}</strong> {info_performance}</li>
          <li><strong>{as.character(tooltip_span('Students w/ Disciplinary Incidents:', stt$disc))}</strong> {paste0(info_disc*100,'%')}</li>
          <li><strong>{as.character(tooltip_span('Avg. Periods Absent:', stt$pabs))}</strong> {paste0(info_prop_absent*100,'%')}</li>
          <li><strong>{as.character(tooltip_span('Avg. Excused Cultural Days Taken:', stt$cult))}</strong> {paste0(info_cultural*100,'%')}</li>
          <li><strong>{as.character(tooltip_span('Avg. Unexcused Days Taken:', stt$unx))}</strong> {paste0(info_prop_unexcused*100,'%')}</li>
          <li><strong>{as.character(tooltip_span('Truancy Rate:', stt$tr))}</strong> {info_truancy_ratio}</li>
          <li><strong>{as.character(tooltip_span('Adj. Absenteeism Rate:', stt$adj))}</strong> {info_adj_absent_ratio}</li>
        </ul>
        <div style='height:2px'></div>
          <!-- Underlined title above threshold‐box -->
          <p style='text-decoration: underline; font-size:16px; font-weight:bold; color: black; margin-bottom:5px;'>
          Percentages Based on Thresholds Sidebar
          </p>
          <!-- Boxed section -->
          <div style='border: 1px solid #c0392b; background-color: #f9f9f9; padding: 10px; border-radius: 5px; margin-top: 10px; font-size:16px;'>
          <ul style='list-style:none; padding-left:0; margin:0;'>
          <li><strong>{as.character(tooltip_span('Above Absence Threshold:', stt$thrA))}</strong>
          <span style='color: #ff8c00; font-weight: bold;'>
          {paste0(round(nrow(min_att_district2())/nrow(filtered_data2()),2)*100, '%')}
          </span>
          </li>
          <li><strong>{as.character(tooltip_span('Above GPA Threshold:', stt$thrGhi))}</strong>
          <span style='color:  #ff8c00; font-weight: bold;'>
          {paste0(round(nrow(min_gpa_district2())/nrow(filtered_data2()),2)*100, '%')}
          </span>
          </li>
          <li><strong>{as.character(tooltip_span('Below GPA Threshold:', stt$thrGlo))}</strong>
          <span style='color:  #ff8c00; font-weight: bold;'>
          {paste0(complement, '%')}
          </span>
          </li>
        </ul>
        </div>
        </div>
      "))
          )
        )
      )
    })
    
    
    # ------ Grade Distribution ---------
    output$grades<-renderPlotly({
      req(nrow(filtered_data()) > 0)
      grade_counts <- table(filtered_data()$STUDENTS.Grade_Level)
      grade_df <- data.frame(
        Grade = as.character(names(grade_counts)),
        Count = as.numeric(grade_counts)
      )
      grade_df$Grade<- ifelse(grade_df$Grade=="-2", "Pre-K 1", 
                              ifelse(grade_df$Grade=="-1", "Pre-K 2", 
                                     ifelse(grade_df$Grade=="0", "Kindergarten", grade_df$Grade)))
      order_levels<-c("Pre-K 1", "Pre-K 2", "Kindergarten", "1",
                      "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
      grade_df$Grade<-factor(grade_df$Grade, levels=order_levels)
      # Custom colors for grades
      grade_colors <- c("lightgrey", "#d62728")
      colors <- rep(grade_colors, ceiling(length(grade_df$Grade)/2))[1:length(grade_df$Grade)]
      # Bar plot for grades
      plot_ly(
        data = grade_df,
        x = ~Grade,
        y = ~Count,
        type = 'bar',
        marker = list(color = ~colors,
                      line = list(color = "black", width = 0.5)),
        text = ~paste("Count:", Count),
        hoverinfo = 'text'
      ) %>%
        layout(
          # Student Grade Distribution 
          title = list(text = "", x = 0.75, 
                       font = list(family = "Arial", size = 20, color = "black")),
          xaxis = list(title = "Grade", tickfont = list(size = 14), tickangle=-45),
          yaxis = list(title = "Number of Students", tickfont = list(size = 14)),
          plot_bgcolor = "#f9f9f9",
          paper_bgcolor = "#f9f9f9",
          font = list(family = "Arial", size = 10),
          bargap = 0.2,
          margin = list(l = 60, r = 60, t = 30, b = 60)
        ) %>% strip_modebar()
    })
    
    # ------ School Distribution ---------
    output$schools<-renderPlotly({
      school_counts <- table(filtered_data()$SCHOOLS.Name)
      school_df <- data.frame(
        School = factor(names(school_counts), levels=mixedsort(levels(factor(names(school_counts))))),
        Count = as.numeric(school_counts)
      )
      # Bar chart for schools
      school_overall<-plot_ly(
        data = school_df,
        x = ~School,
        y = ~Count,
        type = 'bar',
        marker = list(color = ~colors,
                      line = list(color = "black", width = 0.75)),
        text = ~paste("Students:", Count),
        hoverinfo = 'text'
      ) %>%
        layout(
          # School Distribution
          title = list(text = "", x = 0.5, 
                       font = list(family = "Arial", size = 20, color = "black")),
          xaxis = list(title = "", tickfont = list(size = 12), tickangle=-45),
          yaxis = list(title = "Number of Students", tickfont = list(size = 12)),
          plot_bgcolor = "#f9f9f9",
          paper_bgcolor = "#f9f9f9",
          font = list(family = "Arial", size = 10),
          bargap = 0.2,
          margin = list(l = 60, r = 60, t = 30, b = 60)
        ) %>% strip_modebar()
    })
    
    
    # ---- STUDENT PROFILE TAB PLOTS ---- 

    # ---- Student Lookup ----
    student_lookup <- reactive({
      lname_input <- trimws(tolower(input$search_last_name))
      gpa_max     <- input$gpa_filter
      gpa_min     <- input$min_gpa_filter
      abs_min     <- input$absence_filter
      disc_min    <- input$disc_filter
      school_sel  <- input$school_filter 
      
      # No filters -> don't render the Results card yet
      if ((is.null(lname_input) || lname_input == "") &&
          (is.null(gpa_max)  || is.na(gpa_max))  &&
          (is.null(gpa_min)  || is.na(gpa_min))  &&
          (is.null(abs_min)  || is.na(abs_min))  &&
          (is.null(disc_min) || is.na(disc_min)) &&
          (is.null(school_sel) || length(school_sel) == 0) &&
          (is.null(input$lookup_tribe_filter) || length(input$lookup_tribe_filter) == 0)) {
        return(NULL)
      }
      
      df <- lookup
      
      # Enforce single-tribe scope
      if (SINGLE_TRIBE && "tribe_name" %in% names(df)) {
        df <- df[trimws(as.character(df$tribe_name)) == TRIBE(), , drop = FALSE]
      }
      # Add tribe filtering, if single tribe mode is off 
      if (!SINGLE_TRIBE && "tribe_name" %in% names(df)) {
        tsel <- input$lookup_tribe_filter
        if (!is.null(tsel) && length(tsel) > 0) {
          df <- df[!is.na(df$tribe_name) & trimws(as.character(df$tribe_name)) %in% tsel, , drop = FALSE]
        }
      }
      
      if (nzchar(lname_input)) {
        pat <- paste0("^", lname_input)
        df <- df[!is.na(df$last_name) & grepl(pat, tolower(df$last_name)), , drop = FALSE]
      }
      if (!is.null(gpa_max) && !is.na(gpa_max)) {
        df <- df[!is.na(df$GPA) & df$GPA <= gpa_max, , drop = FALSE]
      }
      if (!is.null(gpa_min) && !is.na(gpa_min)) {
        df <- df[!is.na(df$GPA) & df$GPA > gpa_min, , drop = FALSE]
      }
      if (!is.null(abs_min) && !is.na(abs_min)) {
        df <- df[!is.na(df$proportion_absent) & df$proportion_absent >= abs_min, , drop = FALSE]
      }
      if (!is.null(disc_min) && !is.na(disc_min)) {
        col_nm <- if ("number_disciplines" %in% names(df)) "number_disciplines" else "Number_disciplines"
        df <- df[!is.na(df[[col_nm]]) & df[[col_nm]] >= disc_min, , drop = FALSE]
      }
      if (!is.null(school_sel) && length(school_sel) > 0) {
        df <- df[!is.na(df$school_name) & df$school_name %in% school_sel, , drop = FALSE]
      }
      
      df
    })
    
    output$student_result_card <- renderUI({
      students_searched <- student_lookup()
      
      # If no input has been given yet
      if (is.null(students_searched)) {
        return(NULL)
      }
      
      has_results <- nrow(students_searched) > 0
      
      result_cards <- if (nrow(students_searched) == 0 & !has_results) {
        list(
          bs4Card(
            title = "No Record Found",
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            "No student found matching the criteria."
          )
        )
      } else {
        lapply(1:nrow(students_searched), function(i) {
          student <- students_searched[i, ]
          
          gpa_html <- if (!is.na(student$GPA) && student$GPA < 2.0) {
            paste0("<span style='color:red; font-weight:bold;'>", student$GPA, " (Low GPA)</span>")
          } else if (!is.na(student$GPA) && student$GPA >= 3.5) {
            paste0("<span style='color:green; font-weight:bold;'>", student$GPA, " (High GPA)</span>")
          } else {
            student$GPA
          }
          
          wgpa_html <- if (!is.na(student$cumulative_GPA) && student$cumulative_GPA < 2.0) {
            paste0("<span style='color:red; font-weight:bold;'>", student$cumulative_GPA, " (Low Weighted GPA)</span>")
          } else if (!is.na(student$cumulative_GPA) && student$cumulative_GPA >= 3.5) {
            paste0("<span style='color:green; font-weight:bold;'>", student$cumulative_GPA, " (High Weighted GPA)</span>")
          } else {
            student$cumulative_GPA
          }
          
          absent_html <- if (!is.na(student$proportion_absent) && student$proportion_absent >= 0.10) {
            paste0("<span style='color:red; font-weight:bold;'>", paste0(student$proportion_absent * 100, '%'), " (Chronically Absent)</span>")
          } else {
            paste0(student$proportion_absent * 100, '%')
          }
          
          bs4Card(
            title = paste("Student:", student$first_name, student$last_name),
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            icon = icon("user"),
            if(student$grade_level>8){ # line 992 was edited 
              HTML(glue::glue("
          <ul style='list-style: none; font-size: 16px; padding-left: 0;'>
            <li><strong>Student State ID:</strong> 
              {  as.character(
                 actionLink(inputId = paste0('select_student_', student$dcid),
                 label = student$dcid,
                 style = 'font-weight: bold; color: #dc3545; text-decoration: underline;')
              )
            }
            </li>
            <li><strong>Tribe:</strong> {display_tribe(student$tribe_name)}</li>
            <li><strong>School:</strong> {student$school_name}</li>
            <li><strong>Grade Level:</strong> {student$grade_level2}</li>
            <li><strong>Simple GPA:</strong> {gpa_html}</li>
            <li><strong>Weighted GPA:</strong> {wgpa_html}</li>
            <li><strong>Proportion Absent:</strong> {absent_html}</li>
            <li><strong>Disciplinary Incidents:</strong> {student$number_disciplines}</li>
          </ul>
        "))
            }
            else{
              HTML(glue::glue("
          <ul style='list-style: none; font-size: 16px; padding-left: 0;'>
            <li><strong>Student State ID:</strong> 
              {  as.character(
                 actionLink(inputId = paste0('select_student_', student$dcid),
                 label = student$dcid,
                 style = 'font-weight: bold; color: #dc3545; text-decoration: underline;')
              )
              }
            <li><strong>Tribe:</strong> {display_tribe(student$tribe_name)}</li>
            <li><strong>School:</strong> {student$school_name}</li>
            <li><strong>Grade Level:</strong> {student$grade_level2}</li>
            <li><strong>Simple GPA:</strong> {gpa_html}</li>
            <li><strong>Proportion Absent:</strong> {absent_html}</li>
            <li><strong>Disciplinary Incidents:</strong> {student$number_disciplines}</li>
          </ul>
        "))
            }
          )
        })
      }
      
      # Register click observers to populate the search input, i.e., click on Student ID search input is populated 
      # lapply(students_searched$dcid, function(dcid_val) {
      #   observeEvent(input[[paste0("select_student_", dcid_val)]], {
      #     updateTextInput(session, "student_search", value = as.character(dcid_val))
      #     session$sendCustomMessage(type = 'scrollToProfileCard', message = list()) # <--- added to scroll to profile card when ID is clicked
      #   })
      # })
      lapply(students_searched$dcid, function(dcid_val) {
        observeEvent(input[[paste0("select_student_", dcid_val)]], {
          updateTextInput(session, "student_search", value = as.character(dcid_val))
        }, ignoreInit = TRUE)
      })
      observeEvent(input$student_search, {
        session$sendCustomMessage(type = 'scrollToProfileCard', message = list())
      }, ignoreInit = TRUE)
      
      # Wrap all result cards in a collapsible parent card
      bs4Card(
        title = "Results",
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = !has_results,
        width = 12,
        #icon = icon("file"),
        #tagList(result_cards)
        tagList(
          div(style = "margin-bottom: 10px;", 
              downloadButton("download_search_results", "Download Search Results", class = "btn-danger")),
          result_cards
        )
      )
    })
    
    output$download_search_results <- downloadHandler(
      filename = function() {
        paste0("student_search_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        results <- student_lookup()
        if (is.null(results) || nrow(results) == 0) {
          write.csv(data.frame(Message = "No results found"), file, row.names = FALSE)
        } else {
          write.csv(results, file, row.names = FALSE)
        }
      }
    )
    
    
    # Course Information 
    output$course_history_table <- renderUI({
      req(input$show_courses %% 2 == 1)  # toggles visibility on odd clicks
      
      student <- selected_student()
      req(nrow(student) > 0)
      
      # Your course data (replace with actual data)
      course_data <- courses_taken %>%
        filter(dcid == student$STUDENTS.dcid | ID == student$STUDENTS.dcid)
      
      # If not nested, replace with a filter from another df:
      # course_data <- all_courses %>% filter(Student_ID == student$STUDENTS.dcid)
      
      if (is.null(course_data) || nrow(course_data) == 0) {
        return(
          tags$div(
            style = "background-color: white; padding: 10px; border-radius: 5px; font-size: 16px; font-weight: bold;",
            tags$p("No course history available.")
          )
        )
      }
      
      box(
        title = "Course History",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        DT::dataTableOutput("course_table")
      )
    })
    
    # output$course_table <- DT::renderDataTable({
    #   student <- selected_student()
    #   req(nrow(student) > 0)
    #   
    #   # Again, pull your actual course data here
    #   course_data <- courses_taken %>% filter(dcid == student$STUDENTS.dcid | ID == student$STUDENTS.dcid) %>%
    #     select(-dcid)# or from global df with filter
    #   
    #   req(!is.null(course_data), nrow(course_data) > 0)
    #   DT::datatable(course_data, options = list(pageLength = 10, scrollX = TRUE))
    # })
    output$course_table <- DT::renderDataTable({
      student <- selected_student()
      req(nrow(student) > 0)
      
      course_data <- courses_taken %>%
        dplyr::filter(dcid == student$STUDENTS.dcid | ID == student$STUDENTS.dcid) %>%
        dplyr::select(-dcid) %>%
        dplyr::rename(`Student State ID` = ID)
      
      req(nrow(course_data) > 0)
      
      DT::datatable(
        course_data,
        rownames = FALSE,
        class    = "stripe hover cell-border compact",
        options  = list(
          pageLength = 10,
          autoWidth  = TRUE,
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          headerCallback = DT::JS(
            "function(thead){ $(thead).find('th').css({'text-align':'center'}); }"
          )
        )
      )
    })
    
    # Disciplinary Information 
    output$discipline_history_table <- renderUI({
      req(input$show_discipline %% 2 == 1)  # toggles visibility on odd clicks
      
      student <- selected_student()
      req(nrow(student) > 0)
      
      # Your course data (replace with actual data)
      disciplinary_data <- beh_wide %>% filter(dcid == student$STUDENTS.dcid) # This assumes `course_history` is a nested data.frame in your `student`
      
      # If not nested, replace with a filter from another df:
      # course_data <- all_courses %>% filter(Student_ID == student$STUDENTS.dcid)
      
      if (is.null(disciplinary_data) || nrow(disciplinary_data) == 0) {
        return(
          tags$div(
            style = "background-color: white; padding: 10px; border-radius: 5px; font-size: 16px; font-weight: bold;",
            tags$p("Student has had no recorded disciplinary incidents.")
          )
        )
      }
      
      box(
        title = "Disciplinary History",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        DT::dataTableOutput("discipline_table")
      )
    })
    
    output$discipline_table <- DT::renderDataTable({
      student <- selected_student()
      req(nrow(student) > 0)
      
      discipline_data <- beh_wide %>%
        dplyr::filter(dcid == student$STUDENTS.dcid) %>%
        dplyr::select(-Number_behaviors) %>%
        tidyr::pivot_longer(
          cols = dplyr::starts_with("Behavior_"),
          names_to = "Incident_Number",
          values_to = "Incident_Description",
          values_drop_na = TRUE
        ) %>%
        dplyr::select(-Incident_Number)
      
      date_data <- date_wide %>%
        dplyr::filter(dcid == student$STUDENTS.dcid) %>%
        tidyr::pivot_longer(
          cols = dplyr::starts_with("Incident_Date_"),
          names_to = "Incident_Number",
          values_to = "Incident Date",
          values_drop_na = TRUE
        ) %>%
        dplyr::select(-Incident_Number)
      
      out <- merge(date_data, discipline_data, by = "dcid")
      out$Incident_Description <- gsub("^\\d+\\s*", "", out$Incident_Description)
      names(out) <- c("Student State ID", "Incident Date", "Incident Description")
      
      req(nrow(out) > 0)
      
      DT::datatable(
        out,
        rownames = FALSE,
        class    = "stripe hover cell-border compact",
        options  = list(
          pageLength = 10,
          autoWidth  = TRUE,
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          headerCallback = DT::JS(
            "function(thead){ $(thead).find('th').css({'text-align':'center'}); }"
          )
        )
      )
    })
    
    # Calendar Data Table Output 
    # SERVER
    # --- Build the incidents table data once and reuse it for render + download ---
    calendar_table_data <- reactive({
      req(input$calendar_date_range)
      
      # ====== BEGIN: original data-prep code that produced `final_df` ======
      # (This is the same logic you already had inside renderDataTable that creates `final_df`.)
      
      # 1) Unpivot discipline descriptions and dates; align by shared numeric index
      disc_long <- disc_wide %>%
        tidyr::pivot_longer(
          cols = tidyselect::starts_with("Discipline_"),
          names_to = "incident",
          values_to = "description",
          values_drop_na = TRUE
        ) %>%
        dplyr::mutate(index = sub(".*_(\\d+)$", "\\1", incident)) %>%  # trailing digits
        dplyr::select(dcid, index, description)
      
      date_long <- date_wide %>%
        tidyr::pivot_longer(
          cols = tidyselect::starts_with("Incident_Date_"),
          names_to = "incident",
          values_to = "date",
          values_drop_na = TRUE
        ) %>%
        dplyr::mutate(index = sub(".*_(\\d+)$", "\\1", incident)) %>%
        dplyr::select(dcid, index, date)
      
      # 2) Join descriptions to dates
      incident_data <- dplyr::inner_join(disc_long, date_long, by = c("dcid", "index")) %>%
        dplyr::mutate(date = as.Date(date))
      
      # --- Single-tribe filter (if applicable) ---
      if (SINGLE_TRIBE) {
        tribe_dcids <- students_dat2 %>%
          dplyr::filter(tribe_name %in% TRIBE()) %>%
          dplyr::pull(STUDENTS.dcid)
        incident_data <- incident_data %>% dplyr::filter(dcid %in% tribe_dcids)
      }
      
      # 3) Apply the selected date range (calendar_date_range)
      dr <- as.Date(input$calendar_date_range)
      incident_data <- incident_data %>%
        dplyr::filter(date >= dr[1], date <= dr[2])
      
      # 3b) (Optional) also apply the month/year pickers if you use them for the table
      # target_month <- as.numeric(input$calendar_month)
      # target_year  <- as.numeric(input$calendar_year)
      # incident_data <- incident_data %>%
      #   dplyr::filter(lubridate::month(date) == target_month,
      #                 lubridate::year(date)  == target_year)
      
      # 4) Add student info columns used by the table
      student_info <- students_dat2 %>%
        dplyr::transmute(
          dcid         = STUDENTS.dcid,
          `First Name` = STUDENTS.First_Name,
          `Last Name`  = STUDENTS.Last_Name,
          School       = SCHOOLS.Name,
          Grade        = Grade_level,
          Tribe        = dplyr::case_when(
            is.na(tribe_name) ~ NA_character_,
            trimws(tribe_name) %in% c("Not Applicable", "0", "00", "") ~ "Non-Native American",
            TRUE ~ tribe_name
          )
        )
      
      final_df <- incident_data %>%
        dplyr::left_join(student_info, by = "dcid") %>%
        tidyr::replace_na(list(School = "Unavailable",
                               Grade  = "Unavailable",
                               Tribe  = "Unavailable")) %>%
        dplyr::transmute(
          Date = date,
          `Student State ID` = dcid,
          `First Name`, `Last Name`,
          Tribe, School, Grade,
          Incident = description
        ) %>%
        dplyr::arrange(Date)
      
      if (SINGLE_TRIBE) {
        final_df <- final_df %>% dplyr::filter(Tribe == TRIBE())
      }
      
      # ====== END: original data-prep code that produced `final_df` ======
      
      final_df
    })
    
    # --- Render the DataTable from the reactive data ---
    output$calendar_table <- DT::renderDataTable({
      df <- calendar_table_data()
      
      DT::datatable(
        df,
        rownames = FALSE,
        class    = "stripe hover cell-border compact",
        options  = list(
          pageLength = 10,
          autoWidth  = TRUE,
          headerCallback = DT::JS(
            "function(thead){ $(thead).find('th').css({'text-align':'center','vertical-align':'middle'}); }"
          )
        ),
        escape = FALSE
      ) %>%
        DT::formatStyle(columns = names(df), `text-align` = "center") %>%
        DT::formatStyle(
          columns      = c("Tribe", "School", "Grade"),
          valueColumns = c("Tribe", "School", "Grade"),
          fontStyle    = DT::styleEqual("Unavailable", "italic"),
          color        = DT::styleEqual("Unavailable", "gray")
        )
    })
    
    # --- CSV download for exactly what you see in the table ---
    output$download_calendar <- downloadHandler(
      filename = function() paste0("incidents_table_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- calendar_table_data()
        readr::write_csv(df, file, na = "")
      }
    )
    
    # Creates dynamic title for incident table based on selected date range 
    output$calendar_range_title <- renderUI({
      req(input$calendar_date_range)
      start <- as.Date(input$calendar_date_range[1])
      end   <- as.Date(input$calendar_date_range[2])
      
      txt <- if (!is.na(end) && end != start) {
        sprintf("Incident Details for %s to %s",
                format(start, "%b %d, %Y"), format(end, "%b %d, %Y"))
      } else {
        sprintf("Incident Details for %s", format(start, "%b %d, %Y"))
      }
      
      tags$h5(
        txt,
        class = "text-muted",
        style = "margin-top: 10px; margin-bottom: 5px; text-align: center;"
      )
    })
    
    # Incident Calendar Output - allows selection of a date that then populates calendar table (listing incidents)
    output$incident_month_plot <- renderPlot({
      req(input$calendar_month, input$calendar_year)
      
      disc_long <- disc_wide %>%
        pivot_longer(cols = starts_with("Discipline_"), names_to = "incident", values_to = "description") %>%
        filter(!is.na(description)) %>%
        mutate(index = sub(".*_", "", incident))
      
      date_long <- date_wide %>%
        pivot_longer(cols = starts_with("Incident_Date_"), names_to = "incident", values_to = "date") %>%
        filter(!is.na(date)) %>%
        mutate(index = sub(".*Date_", "", incident))
      
      incident_data <- inner_join(disc_long, date_long, by = c("dcid", "index")) %>%
        mutate(date = as.Date(date))
      
      # --- Tribe filter: only in single-tribe mode ---
      if (SINGLE_TRIBE) {
        tribe_dcids <- students_dat2 %>%
          dplyr::filter(tribe_name %in% TRIBE()) %>%
          dplyr::pull(STUDENTS.dcid)
        incident_data <- incident_data %>% dplyr::filter(dcid %in% tribe_dcids)
      }
    
      target_month <- as.numeric(input$calendar_month)
      target_year <- as.numeric(input$calendar_year)
      
      incident_summary <- incident_data %>%
        filter(month(date) == target_month, year(date) == target_year) %>%
        count(date)
      
      total_incidents <- sum(incident_summary$n, na.rm = TRUE)
      start_date <- as.Date(sprintf("%04d-%02d-01", as.numeric(input$calendar_year), as.numeric(input$calendar_month)))
      end_date <- ceiling_date(start_date, "month") - 1
      
      req(start_date <= end_date)
      
      calendar_grid <- data.frame(date = seq(start_date, end_date, by = "1 day"))
      
      if (nrow(calendar_grid) == 0) {
        calendar_grid <- data.frame(
          date = as.Date(character(0)),
          day = numeric(0),
          weekday = numeric(0),
          week = numeric(0),
          n = numeric(0),
          is_selected = logical(0)
        )
      } else {
        calendar_grid <- calendar_grid %>%
          mutate(
            day = day(date),
            weekday = as.numeric(format(date, "%u")),
            week = (day(date) + as.numeric(format(start_date, "%u")) - 2) %/% 7 + 1
          ) %>%
          left_join(incident_summary, by = "date") %>%
          mutate(n = ifelse(is.na(n) | n == 0, NA, n))
        
        sel_date <- selected_incident_date()
        calendar_grid$is_selected <- if (!is.null(sel_date)) calendar_grid$date == sel_date else FALSE
      }
      
      max_n <- suppressWarnings(max(calendar_grid$n, na.rm = TRUE))
      has_data <- is.finite(max_n) && !is.na(max_n) && max_n > 0
      if (!has_data) max_n <- 1
      # Plot 
      p <- ggplot(calendar_grid, aes(x = weekday, y = -week)) +
        geom_tile(aes(fill = n), color = NA) +  # no border on default
        geom_tile(data = subset(calendar_grid, is_selected), 
                  aes(x = weekday, y = -week, fill = n), color = "grey20", linewidth = 1.2) +
        geom_text(aes(label = day), vjust = -0.5, size = 4, fontface = "bold") +
        geom_text(
          aes(
            label = ifelse(!is.na(n), paste0(n, " Incidents"), ""),
            color = ifelse(!is.na(n) & n >= 0.9 * max_n, "light", "dark")
          ),
          vjust = 1.8, size = 3.5, show.legend = FALSE
        ) +
        scale_color_manual(values = c("light" = "white", "dark" = "black")) +
        scale_x_continuous(breaks = 1:7, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
        labs(
          title = paste0(
            "Disciplinary Incidents in ", month.name[target_month], " ", target_year,
            " (Total: ", total_incidents, ")"
          ),
          x = "", y = ""
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          axis.text.x = element_text(size = 12, face = "bold"),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none"
        )
      
      if (has_data) {
        p <- p + scale_fill_gradientn(
          colors = c("#fff0f0", "#ffe5e5", "#fca3a3", "#e34242"),
          na.value = "white",
          limits = c(1, max_n),
          name = "# of Incidents"
        )
      } else {
        p <- p + scale_fill_manual(values = c("white"), na.value = "white")
      }
      
      p
    })
    # Observe click event so selection of calendar date updates calendar date filter 
    observeEvent(input$incident_month_plot_click, {
      click <- input$incident_month_plot_click
      req(click)
      
      # Rebuild the calendar grid to interpret the click
      start_date <- as.Date(sprintf("%04d-%02d-01", as.numeric(input$calendar_year), as.numeric(input$calendar_month)))
      end_date <- ceiling_date(start_date, "month") - 1
      
      calendar_grid <- data.frame(date = seq(start_date, end_date, by = "1 day")) %>%
        mutate(
          day = day(date),
          weekday = as.numeric(format(date, "%u")),
          week = (day(date) + as.numeric(format(start_date, "%u")) - 2) %/% 7 + 1
        )
      
      # Round x and y to find closest match
      selected <- calendar_grid %>%
        filter(round(weekday) == round(click$x), round(-week) == round(click$y))
      
      if (nrow(selected) == 1) {
        selected_incident_date(selected$date)
        updateDateRangeInput(session, "calendar_date_range", start = selected$date, end = selected$date)
      }
    })
    

    # ------ Student Profile Info --------
    # selected_student <- reactive({
    #   req(input$student_search)
    #   keyword <- tolower(trimws(input$student_search))
    #   keyword<-paste("^",keyword,"$", sep="") # search for it exactly
    #   students_dat2 %>% filter( grepl(keyword, tolower(as.character(STUDENTS.dcid))) | grepl(keyword, tolower(as.character(paste0(STUDENTS.First_Name, " ", STUDENTS.Last_Name)))) )
    # })
    # Normalizer for safe text comparisons
    norm_txt <- function(x) {
      x <- iconv(as.character(x), from = "", to = "UTF-8", sub = "")
      tolower(trimws(x))
    }
    # Map "Not Applicable"/empty/zeros to a nicer display label
    display_tribe <- function(x) {
      v <- trimws(as.character(x))
      ifelse(is.na(v) | v == "" | v %in% c("Not Applicable","0","00"),
             "Non-Native American", v)
    }
    # selected_student <- reactive({
    #   req(input$student_search)
    #   kw_raw <- trimws(input$student_search)
    #   if (!nzchar(kw_raw)) return(students_dat2[0, ])
    #   
    #   # If it's all digits, match by ID fields only (skip names entirely)
    #   if (grepl("^[0-9]+$", kw_raw)) {
    #     kw_id <- kw_raw
    #     return(
    #       students_dat2 %>%
    #         dplyr::filter(
    #           as.character(STUDENTS.dcid) == kw_id |
    #             as.character(STUDENTS.ID)   == kw_id
    #         )
    #     )
    #   }
    #   
    #   # Otherwise treat it as a full-name lookup (First + space + Last)
    #   kw <- norm_txt(kw_raw)
    #   students_dat2 %>%
    #     dplyr::mutate(.full_name = norm_txt(paste(STUDENTS.First_Name, STUDENTS.Last_Name))) %>%
    #     dplyr::filter(.full_name == kw) %>%
    #     dplyr::select(-.full_name)
    # })
    selected_student <- reactive({
      req(input$student_search)
      
      # 7B) Scope the search pool to the selected/single tribe
      # Requires TRIBE() from step 3 and SINGLE_TRIBE flag
      pool <- if (SINGLE_TRIBE) {
        students_dat2 %>% dplyr::filter(tribe_name %in% TRIBE())
      } else {
        students_dat2
      }
      
      kw_raw <- trimws(input$student_search)
      if (!nzchar(kw_raw)) return(pool[0, ])
      
      # If it's all digits, match by ID fields only (skip names entirely)
      if (grepl("^[0-9]+$", kw_raw)) {
        kw_id <- kw_raw
        return(
          pool %>%
            dplyr::filter(
              as.character(STUDENTS.dcid) == kw_id |
                as.character(STUDENTS.ID)   == kw_id
            )
        )
      }
      
      # Otherwise treat it as a full-name lookup (First + space + Last)
      kw <- norm_txt(kw_raw)
      pool %>%
        dplyr::mutate(.full_name = norm_txt(paste(STUDENTS.First_Name, STUDENTS.Last_Name))) %>%
        dplyr::filter(.full_name == kw) %>%
        dplyr::select(-.full_name)
    })
    output$profile_card <- renderUI({
      student <- selected_student()
      if (nrow(student) == 0) { return(NULL) }
      
      # Grade logic
      grades_df <- if (as.numeric(student$STUDENTS.Grade_Level) >= 7) {
        data.frame(
          A = student$Prop_A,
          B = student$Prop_B,
          C = student$Prop_C,
          D = student$Prop_D,
          F = student$Prop_F
        )
      } else {
        data.frame(
          Four = student$Prop_4,
          Three = student$Prop_3,
          Two = student$Prop_2,
          One = student$Prop_1
        )
      }
      
      grade_labels <- names(grades_df)
      grade_values <- as.numeric(grades_df[1, ])
      grade_row_html <- paste(
        "<li><strong>Grades:</strong><br><table style='width:100%; text-align:center;'>",
        "<tr style='font-weight:bold;'>", paste(sprintf("<td>%s</td>", grade_labels), collapse = ""), "</tr>",
        "<tr>", paste(sprintf("<td>%.1f%%</td>", grade_values * 100), collapse = ""), "</tr>",
        "</table></li>"
      )
      
      # GPA formatting
      gpa_threshold <- 2.0
      gpa_value <- student$GPA
      gpa_display <- if (!is.na(gpa_value) && gpa_value < gpa_threshold) {
        glue::glue("<li><strong>Simple GPA:</strong> <span style='color: red; font-weight: bold;'>{gpa_value}</span> <span style='color: red;'>(Low GPA)</span></li>")
      } else if (!is.na(gpa_value) && gpa_value >= 3.5) {
        glue::glue("<li><strong>Simple GPA:</strong> <span style='color: green; font-weight: bold;'>{gpa_value}</span> <span style='color: green;'>(High GPA)</span></li>")
      } else {
        glue::glue("<li><strong>Simple GPA:</strong> {gpa_value}</li>")
      }
      
      wgpa_value <- student$Cumulative_GPA
      wgpa_display <- if (!is.na(wgpa_value) && wgpa_value < gpa_threshold) {
        glue::glue("<li><strong>Weighted GPA:</strong> <span style='color: red; font-weight: bold;'>{wgpa_value}</span> <span style='color: red;'>(Low Weighted GPA)</span></li>")
      } else if (!is.na(wgpa_value) && wgpa_value >= 3.5) {
        glue::glue("<li><strong>Weighted GPA:</strong> <span style='color: green; font-weight: bold;'>{wgpa_value}</span> <span style='color: green;'>(High Weighted GPA)</span></li>")
      } else {
        glue::glue("<li><strong>Weighted GPA:</strong> {wgpa_value}</li>")
      }
      
      # Absenteeism formatting
      absent_value <- student$proportion_absent
      absent_display <- if (!is.na(absent_value) && absent_value >= 0.10) { # 10% or greater is considered chronically absent
        glue::glue("<li><strong>Periods Absent (%):</strong> <span style='color: red; font-weight: bold;'>{paste0(absent_value * 100, '%')}</span> <span style='color: red;'>(Chronically Absent)</span></li>")
      } else {
        glue::glue("<li><strong>Periods Absent (%):</strong> {paste0(absent_value * 100, '%')}</li>")
      }
      
      # HTML block
      fluidRow(
        column(
          width = 3,
          div(id = "profile_card_anchor",  # anchor for scrolling
            style = "text-align: center;",
            tags$img(
              src = "BPS IED logo.png",
              style = "max-width: 100%; height: auto; border-radius: 50%; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
              alt = "Student photo"
            ),
            tags$h5(student$Name)
          )
        ),
        column(
          width = 8,
          bs4Card(
            title = paste("Profile for Student State ID", student$STUDENTS.dcid),
            status = "info",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            icon = icon("id-card"),
            HTML(glue::glue("
          <ul style='list-style:none; padding-left:0; font-size:16px;'>
            <li><strong>Student State ID:</strong> {student$STUDENTS.dcid}</li>
            <li><strong>Student ID:</strong> {student$STUDENTS.ID}</li>
            <li><strong>First Name:</strong> {student$STUDENTS.First_Name}</li>
            <li><strong>Last Name:</strong> {student$STUDENTS.Last_Name}</li>
            <li><strong>Tribe:</strong> {display_tribe(student$tribe_name)}</li>
            <li><strong>School:</strong> {student$SCHOOLS.Name}</li>
            <li><strong>Grade:</strong> {student$Grade_level}</li>
            <li><strong>Gender:</strong> {student$STUDENTS.Gender2}</li>
            <li><strong>Ethnicity:</strong> {student$STUDENTS.Ethnicity2}</li>
            <p></p>
            <div style='border: 1px solid #c0392b; background-color: #f9f9f9; padding: 10px; border-radius: 5px; margin-top: 10px; font-size:16px;'>
              <ul style='list-style:none; padding-left:0; margin:0;'>
                {grade_row_html}
                <p></p>
                {gpa_display}
                {wgpa_display}
                <li><strong>Academic Performance:</strong> {student$Performance}</li>
                {absent_display}
                <li><strong>Adj. Absenteeism Rate:</strong> {student$adj_absenteeism_ratio}</li>
                <li><strong>Truancy Rate:</strong> {student$truancy_ratio}</li>
                <li><strong>Number of Disciplinary Incidents:</strong> {student$Number_disciplines}</li>
              </ul>
            </div>
            <p></p>
            <li><strong>Guardian First Name:</strong> {student$S_NM_STU_X.guardianfirstname}</li>
            <li><strong>Guardian Last Name:</strong> {student$S_NM_STU_X.guardianlastname}</li>
            <li><strong>Guardian Email:</strong> <a href='mailto:{student$STUDENTS.GuardianEmail}'>{student$STUDENTS.GuardianEmail}</a></li>
            <li><strong>Home Phone:</strong> <a href='tel:{student$STUDENTS.Home_Phone}'>{student$STUDENTS.Home_Phone}</a></li>
          </ul>
          { tags$p(
            actionLink('show_courses', label = 'View Full Course History', icon = icon('angle-down'), style = 'font-weight: bold;')
          ) }
          { tags$p(
            actionLink('show_discipline', label = 'View Disciplinary Incidents', icon = icon('angle-down'), style = 'font-weight: bold; color: #dc3545;')
          ) }
        ")),
            #tags$p(downloadButton("download_profile_with_courses", "Download Full Student Profile", class = "btn-info"))
            tags$p(
              downloadButton(
                outputId = "download_profile_with_courses",
                label = "Download Full Student Profile",
                class = "btn",
                style = "background-color: #17a2b8; color: white; font-weight: bold; border: none;"
              )
            )
          )
        )
      )
    })
    
    
    # Handle entire student profile download (Profile + Courses + Discipline + Contacts)
    output$download_profile_with_courses <- downloadHandler(
      filename = function() {
        student <- selected_student()
        paste0("student_profile_", student$STUDENTS.dcid, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        student <- selected_student()
        req(nrow(student) > 0)
        
        profile_df <- data.frame(
          DCID                          = coalesce(student$STUDENTS.dcid, NA),
          ID                            = coalesce(student$STUDENTS.ID, NA),
          Name                          = coalesce(student$Name, NA),
          First_Name                    = coalesce(student$STUDENTS.First_Name, NA),
          Last_Name                     = coalesce(student$STUDENTS.Last_Name, NA),
          Grade                         = coalesce(student$Grade_level, NA),
          School                        = coalesce(student$SCHOOLS.Name, NA),
          Gender                        = coalesce(student$STUDENTS.Gender2, NA),
          Ethnicity                     = coalesce(student$STUDENTS.Ethnicity2, NA),
          Tribe                         = coalesce(display_tribe(student$tribe_name), NA),
          GPA                           = coalesce(student$GPA, NA),
          Weighted_GPA                  = coalesce(student$Cumulative_GPA, NA),
          Performance                   = coalesce(student$Performance, NA),
          Absence_Proportion           = coalesce(student$proportion_absent, NA),
          Adj_Absenteeism              = coalesce(student$adj_absenteeism_ratio, NA),
          Truancy                      = coalesce(student$truancy_ratio, NA),
          Number_of_Disciplinary_Incidents = coalesce(student$Number_disciplines, NA),
          Guardian_First               = coalesce(student$S_NM_STU_X.guardianfirstname, NA),
          Guardian_Last                = coalesce(student$S_NM_STU_X.guardianlastname, NA),
          Guardian_Email               = coalesce(student$STUDENTS.GuardianEmail, NA),
          Home_Phone                   = coalesce(student$STUDENTS.Home_Phone, NA),
          stringsAsFactors             = FALSE
        )
        
        course_data <- courses_taken %>%
          dplyr::filter(dcid == student$STUDENTS.dcid | ID == student$STUDENTS.dcid)
        
        discipline_data <- beh_wide %>%
          dplyr::filter(dcid == student$STUDENTS.dcid)
        
        # -------------------------
        # Contacts data (if exists)
        # -------------------------
        contacts_data <- NULL
        if (exists("student_contacts")) {
          contacts_data <- student_contacts %>%
            dplyr::filter(STUDENTS.dcid == student$STUDENTS.dcid[1]) %>%
            dplyr::transmute(
              `Contact Name`  = Contact_Name,
              Relationship    = Relationship,
              Email           = Email,
              Phone           = Phone,
              `Lives With`    = Lives_With,
              `Emergency`     = Emergency_Contact,
              `Receives Mail` = Receives_Mail,
              `Pickup`        = School_Pickup
            )
        }
        
        # Write all to a "multi-section" CSV-style text file
        temp_file <- tempfile(fileext = ".csv")
        con <- file(temp_file, open = "wt")
        
        # 1) Profile
        writeLines("Student Profile Information", con)
        write.table(profile_df, con, sep = ",", row.names = FALSE)
        
        # 2) Courses
        writeLines("\nCourse History", con)
        if (nrow(course_data) > 0) {
          write.table(course_data, con, sep = ",", row.names = FALSE)
        } else {
          writeLines("No course history available.", con)
        }
        
        # 3) Discipline
        writeLines("\nDisciplinary History", con)
        if (nrow(discipline_data) > 0) {
          write.table(discipline_data, con, sep = ",", row.names = FALSE)
        } else {
          writeLines("No disciplinary incidents recorded.", con)
        }
        
        # 4) Contacts
        writeLines("\nStudent Contacts", con)
        if (!is.null(contacts_data) && nrow(contacts_data) > 0) {
          write.table(contacts_data, con, sep = ",", row.names = FALSE)
        } else {
          writeLines("No contacts available.", con)
        }
        
        close(con)
        file.copy(temp_file, file, overwrite = TRUE)
      }
    )
    
    
    
    # ---- ATTENDANCE TAB PLOTS ---- 
    
    # Reactive attendance data to plot attendance rates for the selected school vs district
    # tribe_tracker <- reactive({
    #   students_dat2 %>%
    #     filter(tribe_name %in% input$tribe_track) 
    # })
    tribe_tracker <- reactive({
      students_dat2 %>% dplyr::filter(tribe_name %in% TRIBE())
    })
    
    # Proportion Presence Plot 
    output$present_comparison_plot <- renderPlotly({
      sel     <- tribe_tracker()
      n_df    <- district_native_df()
      nn_sel  <- input$nn_race_present
      nn_df   <- nn_df_for(nn_sel)
      nn_lab  <- nn_label_for(nn_sel)
      
      labels <- c(nn_lab, "District Native", TRIBE()) # <-- changed here from input$tribe_track
      values <- c(
        safe_mean(nn_df$proportion_present) * 100,
        safe_mean(n_df$proportion_present)  * 100,
        safe_mean(sel$proportion_present)   * 100
      )
      rates <- setNames(values, labels)
      
      df <- data.frame(
        Category = factor(names(rates), levels = c(nn_lab, "District Native", TRIBE())), # <-- changed here from input$tribe_track
        Value    = as.numeric(rates),
        stringsAsFactors = FALSE
      )
      df$Color <- ifelse(as.character(df$Category) == TRIBE(), "#d62728", # <-- changed here from input$tribe_track
                         ifelse(as.character(df$Category) == "District Native", "orange", "lightgrey"))
      #ymax <- safe_ymax(df$Value, 1.25, 100)
      ymax <- safe_ymax(df$Value, pad = 1.10, cap = 110)
      
      # --- custom hover text (format however you like) ---
      df$Hover <- sprintf("<b>%s</b><br>Present: %.2f%%", df$Category, df$Value)
      plot_ly(
        df, x = ~Category, y = ~Value, type = "bar",
        text = ~paste0(round(Value, 2), "%"), textposition = "outside",
        hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>",
        marker = list(color = ~Color, line = list(color = "black", width = 0.8))
      ) %>%
        layout(
          title  = list(text = "Percentage of Class Periods Present", x = 0.5, font = list(size = 18)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "Percentage Present (%)", range = c(0, ymax)),
          showlegend = FALSE,
          bargap = 0.4,
          margin = list(t = 90)   # more space above the plotting area
        ) %>% strip_modebar()
    })
    
    # Proportion Absent Plot 
    output$absent_prop_comparison_plot <- renderPlotly({
      sel     <- tribe_tracker()
      n_df    <- district_native_df()
      nn_sel  <- input$nn_race_absent_prop
      nn_df   <- nn_df_for(nn_sel)
      nn_lab  <- nn_label_for(nn_sel)
      
      df   <- build_three_group_df(sel, n_df, nn_df, TRIBE(), "proportion_absent", nn_lab) # <-- changed here from input$tribe_track
      ymax <- safe_ymax(df$Value, 1.25, 100)
      
      plot_ly(
        df, x = ~Category, y = ~Value, type = "bar",
        text = ~paste0(round(Value, 2), "%"), textposition = "outside",
        hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>",
        marker = list(color = ~Color, line = list(color = "black", width = 0.8))
      ) %>%
        layout(
          title = list(text = "Percentage of Class Periods Absent", x = 0.5, font = list(size = 18)),
          xaxis = list(title = ""),
          yaxis = list(title = "Percentage Absent (%)", range = c(0, ymax)),
          showlegend = FALSE, margin = list(t = 40), bargap = 0.4
        ) %>% strip_modebar()
    })
  
    # ----- Truancy Tribe vs. District Plot (Interactive with Plotly) ------
    output$truancy_comparison_plot <- renderPlotly({
      sel     <- tribe_tracker()
      n_df    <- district_native_df()
      nn_sel  <- input$nn_race_truancy
      nn_df   <- nn_df_for(nn_sel)
      nn_lab  <- nn_label_for(nn_sel)
      
      df   <- build_three_group_df(sel, n_df, nn_df, TRIBE(), "truancy_ratio", nn_lab) # <-- changed here from input$tribe_track
      ymax <- safe_ymax(df$Value, 1.25, 100)
      
      plot_ly(
        df, x = ~Category, y = ~Value, type = "bar",
        text = ~paste0(round(Value, 2), "%"), textposition = "outside",
        hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>",
        marker = list(color = ~Color, line = list(color = "black", width = 0.8))
      ) %>%
        layout(
          title = list(text = "Truancy Rate Comparison", x = 0.5, font = list(size = 18)),
          xaxis = list(title = ""),
          yaxis = list(title = "Truancy Rate (%)", range = c(0, ymax)),
          showlegend = FALSE, margin = list(t = 40), bargap = 0.4
        ) %>% strip_modebar()
    })
    
    # ---- Adj. Absenteeism Rate Comparison (Interactive Plotly with Bold Borders) ----
    output$absent_comparison_plot_adj <- renderPlotly({
      sel     <- tribe_tracker()
      n_df    <- district_native_df()
      nn_sel  <- input$nn_race_adj_abs
      nn_df   <- nn_df_for(nn_sel)
      nn_lab  <- nn_label_for(nn_sel)
      
      df   <- build_three_group_df(sel, n_df, nn_df, TRIBE(), "adj_absenteeism_ratio", nn_lab) # <-- changed here from input$tribe_track
      ymax <- safe_ymax(df$Value, 1.25, 100)
      
      plot_ly(
        df, x = ~Category, y = ~Value, type = "bar",
        text = ~paste0(round(Value, 2), "%"), textposition = "outside",
        hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>",
        marker = list(color = ~Color, line = list(color = "black", width = 0.8))
      ) %>%
        layout(
          title = list(text = "Adjusted Absenteeism Rate Comparison", x = 0.5, font = list(size = 18)),
          xaxis = list(title = ""),
          yaxis = list(title = "Adj. Absenteeism Rate (%)", range = c(0, ymax)),
          showlegend = FALSE, margin = list(t = 40), bargap = 0.4
        ) %>% strip_modebar()
    })
    
    # ---- Unadjusted Absenteeism Rate Comparison (Interactive with Plotly + Bold Borders) ----
    output$absent_comparison_plot <- renderPlotly({
      sel     <- tribe_tracker()
      n_df    <- district_native_df()
      nn_sel  <- input$nn_race_unadj_abs
      nn_df   <- nn_df_for(nn_sel)
      nn_lab  <- nn_label_for(nn_sel)
      
      df   <- build_three_group_df(sel, n_df, nn_df, TRIBE(), "absenteeism_ratio", nn_lab) # <-- changed here from input$tribe_track
      ymax <- safe_ymax(df$Value, 1.25, 100)
      
      plot_ly(
        df, x = ~Category, y = ~Value, type = "bar",
        text = ~paste0(round(Value, 2), "%"), textposition = "outside",
        hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>",
        marker = list(color = ~Color, line = list(color = "black", width = 0.8))
      ) %>%
        layout(
          title = list(text = "Unadjusted Absenteeism Rate Comparison", x = 0.5, font = list(size = 18)),
          xaxis = list(title = ""),
          yaxis = list(title = "Absenteeism Rate (%)", range = c(0, ymax)),
          showlegend = FALSE, margin = list(t = 40), bargap = 0.4
        ) %>% strip_modebar()
    })
    
    output$absences_comparison_plot <- renderPlotly({
      sel     <- tribe_tracker()
      n_df    <- district_native_df()
      nn_sel  <- input$nn_race_abs_type
      nn_df   <- nn_df_for(nn_sel)
      nn_lab  <- nn_label_for(nn_sel)
      
      # Summarize counts for each group
      sum_two <- function(df) {
        tibble::tibble(
          Status = c("Excused","Unexcused"),
          Count  = c(sum(df$excused_absence,   na.rm = TRUE),
                     sum(df$unexcused_absence, na.rm = TRUE))
        ) %>% 
          dplyr::mutate(Total = sum(Count), Proportion = ifelse(Total > 0, 100*Count/Total, NA_real_)) %>%
          dplyr::select(-Total)
      }
      
      nn_tbl <- sum_two(nn_df) %>% dplyr::mutate(Category = nn_lab)
      dn_tbl <- sum_two(n_df)  %>% dplyr::mutate(Category = "District Native")
      tr_tbl <- sum_two(sel)   %>% dplyr::mutate(Category = TRIBE()) # <-- changed here from input$tribe_track
      
      comparison_df <- dplyr::bind_rows(nn_tbl, dn_tbl, tr_tbl) %>%
        dplyr::mutate(
          Category = factor(Category, levels = c(nn_lab, "District Native", TRIBE())), # <-- changed here from input$tribe_track
          Hover    = sprintf("<b>%s</b><br>%s: %.1f%%", Category, Status, Proportion)
        )
      
      plot_ly() %>%
        add_trace(
          data = comparison_df %>% dplyr::filter(Status == "Excused"),
          x = ~Category, y = ~Proportion, type = "bar", name = "Excused",
          marker = list(color = "orange",  line = list(color = "black", width = 1.2)),
          text = ~paste0(round(Proportion), "%"), textposition = "inside", textfont = list(color = "white", size = 14),
          hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>"
        ) %>%
        add_trace(
          data = comparison_df %>% dplyr::filter(Status == "Unexcused"),
          x = ~Category, y = ~Proportion, type = "bar", name = "Unexcused",
          marker = list(color = "#d62728", line = list(color = "black", width = 1.2)),
          text = ~paste0(round(Proportion), "%"), textposition = "inside", textfont = list(color = "white", size = 14),
          hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>"
        ) %>%
        layout(
          barmode = "stack",
          title   = list(text = "Unexcused vs. Excused Absences", x = 0.50, font = list(size = 18)),
          xaxis   = list(title = "", tickfont = list(size = 14)),
          yaxis   = list(title = "Percentage of Absences (%)", range = c(0, 100)),
          legend  = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1),
          margin  = list(t = 60),
          bargap  = 0.4
        ) %>% strip_modebar()
    })
    
    output$absences_category_plot <- renderPlotly({
      abs_labels <- c("Perfect","Good","Signal","At Risk","Chronic")
      sel   <- tribe_tracker()
      n_df  <- district_native_df()
      nn_sel <- input$nn_race_abs_cat
      nn_df  <- nn_df_for(nn_sel); nn_lab <- nn_label_for(nn_sel)
      
      pct_tbl <- function(df, labels, col) {
        p <- prop.table(table(factor(as.character(df[[col]]), levels = labels)))
        tibble::tibble(Category = labels, Percent = as.numeric(p) * 100)
      }
      
      nn_tb <- pct_tbl(nn_df, abs_labels, "adj_absenteeism_category") %>% dplyr::mutate(Group = nn_lab)
      dn_tb <- pct_tbl(n_df , abs_labels, "adj_absenteeism_category") %>% dplyr::mutate(Group = "District Native")
      tr_tb <- pct_tbl(sel , abs_labels, "adj_absenteeism_category") %>% dplyr::mutate(Group = TRIBE()) # <-- changed here from input$tribe_track
      
      combined <- dplyr::bind_rows(nn_tb, dn_tb, tr_tb) %>%
        dplyr::mutate(
          Category = factor(Category, levels = abs_labels),
          Hover = sprintf("<b>%s</b><br>%s: %.2f%%", Group, Category, Percent)
        )
      
      # Fixed colors
      col_nn  <- "lightgrey"  # Non-Native
      col_dn  <- "orange"     # District Native
      col_tr  <- "#d62728"    # Selected Tribe
      
      plot_ly() %>%
        add_trace(
          data = combined %>% dplyr::filter(Group == nn_lab),
          x = ~Category, y = ~Percent, type = "bar", name = nn_lab, legendgroup = "NN",
          marker = list(color = col_nn, line = list(color = "black", width = 0.8)),
          hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>"
        ) %>%
        add_trace(
          data = combined %>% dplyr::filter(Group == "District Native"),
          x = ~Category, y = ~Percent, type = "bar", name = "District Native", legendgroup = "DN",
          marker = list(color = col_dn, line = list(color = "black", width = 0.8)),
          hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>"
        ) %>%
        add_trace(
          data = combined %>% dplyr::filter(Group == TRIBE()), # <-- changed here from input$tribe_track
          x = ~Category, y = ~Percent, type = "bar", name = TRIBE(), legendgroup = "TRIBE", # <-- changed here from input$tribe_track
          marker = list(color = col_tr, line = list(color = "black", width = 0.8)),
          hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>"
        ) %>%
        layout(
          title   = list(text = "Adj. Absenteeism Categories — Tribe vs District Native vs Non-Native", x = 0.5, font = list(size = 18)),
          barmode = "group",
          xaxis   = list(title = list(text = "<b>Adj. Absenteeism Category</b>", standoff = 25), tickangle = -45),
          yaxis   = list(title = list(text = "<b>Percent of Students</b>"), range = c(0, 100)),
          showlegend = TRUE,
          margin  = list(l = 60, r = 60, t = 40, b = 60)
        ) %>% strip_modebar()
    })
    
    output$truancy_category_plot <- renderPlotly({
      tr_labels <- c("Perfect","Good","Signal","At Risk","Truant")
      sel   <- tribe_tracker()
      n_df  <- district_native_df()
      nn_sel <- input$nn_race_tru_cat
      nn_df  <- nn_df_for(nn_sel); nn_lab <- nn_label_for(nn_sel)
      
      pct_tbl <- function(df, labels, col) {
        p <- prop.table(table(factor(as.character(df[[col]]), levels = labels)))
        tibble::tibble(Category = labels, Percent = as.numeric(p) * 100)
      }
      
      nn_tb <- pct_tbl(nn_df, tr_labels, "truancy_category") %>% dplyr::mutate(Group = nn_lab)
      dn_tb <- pct_tbl(n_df , tr_labels, "truancy_category") %>% dplyr::mutate(Group = "District Native")
      tr_tb <- pct_tbl(sel , tr_labels, "truancy_category") %>% dplyr::mutate(Group = TRIBE()) # <-- changed here from input$tribe_track
      
      combined <- dplyr::bind_rows(nn_tb, dn_tb, tr_tb) %>%
        dplyr::mutate(
          Category = factor(Category, levels = tr_labels),
          Hover = sprintf("<b>%s</b><br>%s: %.2f%%", Group, Category, Percent)
        )
      
      # Fixed colors
      col_nn  <- "lightgrey"  # Non-Native
      col_dn  <- "orange"     # District Native
      col_tr  <- "#d62728"    # Selected Tribe
      
      plot_ly() %>%
        add_trace(
          data = combined %>% dplyr::filter(Group == nn_lab),
          x = ~Category, y = ~Percent, type = "bar", name = nn_lab, legendgroup = "NN",
          marker = list(color = col_nn, line = list(color = "black", width = 0.8)),
          hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>"
        ) %>%
        add_trace(
          data = combined %>% dplyr::filter(Group == "District Native"),
          x = ~Category, y = ~Percent, type = "bar", name = "District Native", legendgroup = "DN",
          marker = list(color = col_dn, line = list(color = "black", width = 0.8)),
          hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>"
        ) %>%
        add_trace(
          data = combined %>% dplyr::filter(Group == TRIBE()), # <-- changed here from input$tribe_track
          x = ~Category, y = ~Percent, type = "bar", name = TRIBE(), legendgroup = "TRIBE", # <-- changed here from input$tribe_track
          marker = list(color = col_tr, line = list(color = "black", width = 0.8)),
          hovertext = ~Hover, hovertemplate = "%{hovertext}<extra></extra>"
        ) %>%
        layout(
          title   = list(text = "Truancy Categories — Tribe vs District Native vs Non-Native", x = 0.5, font = list(size = 18)),
          barmode = "group",
          xaxis   = list(title = list(text = "<b>Truancy Category</b>", standoff = 25), tickangle = -45),
          yaxis   = list(title = list(text = "<b>Percent of Students</b>"), range = c(0, 100)),
          showlegend = TRUE,
          margin  = list(l = 60, r = 60, t = 40, b = 60)
        ) %>% strip_modebar()
    })
    
    
    # NEW CODE: Helpers
    # ---- Tribe-scoped base for Attendance ----
    get_selected_tribes <- function() {
      # Prefer your TRIBES() helper if it exists; otherwise fall back to input directly
      if (exists("TRIBES")) TRIBES() else input$tribe_select
    }
    get_default_tribe <- function() {
      if (exists("TRIBE")) TRIBE() else DEFAULT_TRIBE
    }
    
    # att_base <- reactive({
    #   df  <- students_dat2
    #   sel <- get_selected_tribes()
    #   if (isTruthy(sel) && length(sel) > 0) {
    #     df <- df %>% dplyr::filter(!is.na(tribe_name) &
    #                                  trimws(as.character(tribe_name)) %in% sel)
    #   } else if (isTRUE(SINGLE_TRIBE)) {
    #     df <- df %>% dplyr::filter(trimws(as.character(tribe_name)) == get_default_tribe())
    #   }
    #   df
    # })
    att_base <- reactive({
      df  <- students_dat2
      # Only scope by tribe when SINGLE_TRIBE is TRUE
      if (isTRUE(SINGLE_TRIBE)) {
        df <- df %>% dplyr::filter(trimws(as.character(tribe_name)) == get_default_tribe())
      }
      df
    })
    
    # Reusable: refresh Attendance filter choices from the tribe-scoped base
    refresh_att_filter_choices <- function() {
      df <- att_base()
      
      # Race choices (from scoped data)
      races <- race_choices_from(df)
      shinyWidgets::updatePickerInput(
        session, "att_race",
        choices = races, selected = NULL,
        options = list(`actions-box`=TRUE, `live-search`=TRUE, title="All")
      )
      
      # Gender choices (from scoped data)
      gcol <- get_gender_col(df)
      if (!is.null(gcol)) {
        vals <- df[[gcol]]
        vals <- sort(unique(stats::na.omit(trimws(as.character(vals)))))
        shinyWidgets::updatePickerInput(
          session, "att_gender",
          choices = vals, selected = NULL,
          options = list(`actions-box`=TRUE, `live-search`=TRUE, title="All")
        )
      } else {
        shinyWidgets::updatePickerInput(
          session, "att_gender",
          choices = character(0), selected = NULL,
          options = list(title="Not available in data")
        )
      }
    }
    
    # ===== NEW CODE: Attendance filter choice population & dataset =====
    # Populate choices when Attendance tab is active
    observeEvent(input$tabs, {
      req(input$tabs == "attendance")
      races   <- race_choices_from(students_dat2)
      gcol    <- get_gender_col(students_dat2)
      
      # race choices
      shinyWidgets::updatePickerInput(session, "att_race",
                                      choices = races, selected = NULL,
                                      options = list(`actions-box`=TRUE, `live-search`=TRUE, title="All")
      )
      
      # gender choices
      if (!is.null(gcol)) {
        vals <- students_dat2[[gcol]]
        vals <- sort(unique(stats::na.omit(trimws(as.character(vals)))))
        shinyWidgets::updatePickerInput(session, "att_gender",
                                        choices = vals, selected = NULL,
                                        options = list(`actions-box`=TRUE, `live-search`=TRUE, title="All")
        )
      } else {
        shinyWidgets::updatePickerInput(session, "att_gender",
                                        choices = character(0), selected = NULL,
                                        options = list(title="Not available in data")
        )
      }
    }, ignoreInit = TRUE)
    
    # sped choices  
    scol <- get_sped_col(students_dat2)
    if (!is.null(scol)) {
      vals <- to_yesno(students_dat2[[scol]])
      vals <- sort(unique(stats::na.omit(vals)))
      shinyWidgets::updatePickerInput(session, "att_sped",
                                      choices = vals, selected = NULL,
                                      options = list(`actions-box`=TRUE, `live-search`=TRUE, title="All")
      )
    } else {
      shinyWidgets::updatePickerInput(session, "att_sped",
                                      choices = character(0), selected = NULL,
                                      options = list(title="Not available in data")
      )
    }
    
    # ell choices  
    ecol <- get_ell_col(students_dat2)
    if (!is.null(ecol)) {
      vals <- to_yesno(students_dat2[[ecol]])
      vals <- sort(unique(stats::na.omit(vals)))
      shinyWidgets::updatePickerInput(session, "att_ell",
                                      choices = vals, selected = NULL,
                                      options = list(`actions-box`=TRUE, `live-search`=TRUE, title="All")
      )
    } else {
      shinyWidgets::updatePickerInput(session, "att_ell",
                                      choices = character(0), selected = NULL,
                                      options = list(title="Not available in data")
      )
    }
    
    observeEvent(input$att_reset, {
      shinyWidgets::updatePickerInput(session, "att_race",   selected = NULL)
      shinyWidgets::updatePickerInput(session, "att_gender", selected = NULL)
      shinyWidgets::updatePickerInput(session, "att_sped",   selected = NULL)
      shinyWidgets::updatePickerInput(session, "att_ell",    selected = NULL)
      updatePrettyRadioButtons(session, "att_metric", selected = "Proportion Present")
    })
    
    # Map metric -> column
    att_metric_col <- reactive({
      switch(input$att_metric,
             "Proportion Present"    = "proportion_present",
             "Proportion Absent"     = "proportion_absent",
             "Adjusted Absenteeism"  = "adj_absenteeism_ratio",
             "Unadjusted Absenteeism"= "absenteeism_ratio",
             "Truancy Rate"          = "truancy_ratio",
             "proportion_present")
    })
    
    # Filtered dataset for charts/tables
    att_filtered <- reactive({
      #df <- students_dat2
      df <- att_base()
      
      # Coerce the metric columns to numeric if they exist
      metric_cols <- c(
        "proportion_present","proportion_absent",
        "adj_absenteeism_ratio","absenteeism_ratio","truancy_ratio"
      )
      have <- intersect(metric_cols, names(df))
      if (length(have)) {
        df <- df %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(have),
                                      ~ suppressWarnings(as.numeric(.))))
      }
      
      # Race
      rcol <- get_race_col(df)
      if (!is.null(rcol) && length(input$att_race)) {
        df <- df %>% dplyr::filter(!is.na(.data[[rcol]]) &
                                     trimws(as.character(.data[[rcol]])) %in% input$att_race)
      }
      
      # Gender
      gcol <- get_gender_col(df)
      if (!is.null(gcol) && length(input$att_gender)) {
        df <- df %>% dplyr::filter(!is.na(.data[[gcol]]) &
                                     trimws(as.character(.data[[gcol]])) %in% input$att_gender)
      }
      
      # SPED
      scol <- get_sped_col(df)
      if (!is.null(scol) && length(input$att_sped)) {
        df <- df %>% dplyr::mutate(.sped = to_yesno(.data[[scol]])) %>%
          dplyr::filter(.sped %in% input$att_sped)
      }
      
      # ELL
      ecol <- get_ell_col(df)
      if (!is.null(ecol) && length(input$att_ell)) {
        df <- df %>% dplyr::mutate(.ell = to_yesno(.data[[ecol]])) %>%
          dplyr::filter(.ell %in% input$att_ell)
      }
      
      df
    })
  
    ########################################
    # NEW CODE FOR ATTENDANCE DATA VIEW
    #########################################
    # ===== Data Views (Attendance table explorer) =====

    # Clean up column names metric variables for viewing 
    dv_metric_col <- reactive({
      switch(input$dv_metric,
             "Proportion Present"     = "proportion_present",
             "Proportion Absent"      = "proportion_absent",
             "Adjusted Absenteeism"   = "adj_absenteeism_ratio",
             "Unadjusted Absenteeism" = "absenteeism_ratio",
             "Truancy Rate"           = "truancy_ratio",
             "proportion_absent")
    })
    
    # Map UI group labels -> prepared label columns
    # Clean up variable names for grouping variables 
    dv_group_map <- reactive({
      df <- att_base()
      rcol <- get_race_col(df)
      gcol <- get_gender_col(df)
      scol <- get_sped_col(df)
      ecol <- get_ell_col(df)
      list(
        "School"         = "SCHOOLS.Name",
        "Grade"          = "Grade_level",
        "Race/Ethnicity" = rcol,
        "Gender"         = gcol,
        "SPED"           = scol,
        "ELL"            = ecol,
        "Tribe"          = "tribe_name"
      )
    })
    
    # Build the working data set (already respects Single-Tribe mode and Attendance filters)
    # ===== Data Views: robust base (always returns a tibble) =====
    dv_base <- reactive({
      # Try pulling the filtered attendance base; fall back to students_dat2 if needed
      df <- try(att_filtered(), silent = TRUE)
      if (inherits(df, "try-error") || is.null(df)) {
        df <- att_base()
      }
      
      # Must be a data frame; coerce robustly
      if (!is.data.frame(df)) {
        df <- tibble::tibble(.x = df)  # if it was a vector, coerce to tibble
      }
      df <- tibble::as_tibble(df)
      
      # Metric column
      mc <- dv_metric_col()
      shiny::validate(need(mc %in% names(df), paste0("Data Views: metric column '", mc, "' not found.")))
      df <- dplyr::mutate(df, Metric = suppressWarnings(as.numeric(.data[[mc]]) * 100))
      
      # ---- Helpers to pick first existing column name from candidates ----
      pick_col <- function(cands) {
        hits <- cands[cands %in% names(df)]
        if (length(hits)) hits[1] else NULL
      }
      
      # Grade label (if Grade_level exists)
      gl <- pick_col("Grade_level")
      if (!is.null(gl)) df[["Grade"]] <- grade_label(df[[gl]])
      
      # Race/Gender/SPED/ELL using your tolerant detectors
      rcol <- get_race_col(df)
      if (!is.null(rcol)) df[["Race/Ethnicity"]] <- trimws(as.character(df[[rcol]]))
      
      gcol <- get_gender_col(df)
      if (!is.null(gcol)) df[["Gender"]] <- trimws(as.character(df[[gcol]]))
      
      scol <- get_sped_col(df)
      if (!is.null(scol)) df[["SPED"]] <- to_yesno(df[[scol]])
      
      ecol <- get_ell_col(df)
      if (!is.null(ecol)) df[["ELL"]] <- to_yesno(df[[ecol]])
      
      # Tribe & School labels if present
      if ("tribe_name" %in% names(df))   df[["Tribe"]]  <- trimws(as.character(df$tribe_name))
      if ("SCHOOLS.Name" %in% names(df)) df[["School"]] <- trimws(as.character(df$SCHOOLS.Name))
      
      # Student ID & Name (robust)
      sid <- pick_col(c("STUDENTS.dcid", "dcid"))
      if (!is.null(sid)) {
        df[["Student ID"]] <- as.character(df[[sid]])
      } else {
        df[["Student ID"]] <- as.character(seq_len(nrow(df)))
      }
      
      fn <- pick_col(c("STUDENTS.First_Name","First_Name","first_name","FIRST_NAME"))
      ln <- pick_col(c("STUDENTS.Last_Name","Last_Name","last_name","LAST_NAME"))
      if (!is.null(fn) && !is.null(ln)) {
        df[["Student Name"]] <- trimws(paste0(as.character(df[[fn]]), " ", as.character(df[[ln]])))
      } else if ("STUDENTS.Student_Name" %in% names(df)) {
        df[["Student Name"]] <- as.character(df[["STUDENTS.Student_Name"]])
      } else {
        df[["Student Name"]] <- NA_character_
      }
      
      df
    })
    
    dv_summary <- reactive({
      df <- dv_base()
      
      # Ensure data frame & Metric
      shiny::validate(need(is.data.frame(df), "Data Views: internal data is not a data.frame"))
      if (!"Metric" %in% names(df)) return(tibble::tibble())
      
      # Make sure Metric is numeric (0–100 scale coming from dv_base)
      df$Metric <- suppressWarnings(as.numeric(df$Metric))
      
      # Threshold range (robust default)
      rng <- input$dv_threshold
      if (is.null(rng) || length(rng) != 2 || any(!is.finite(rng))) rng <- c(0, 100)
      
      # Apply threshold (on numeric)
      df <- dplyr::filter(df, is.finite(Metric) & Metric >= rng[1] & Metric <= rng[2])
      
      # Groups
      groups <- if (is.null(input$dv_groups)) character(0) else input$dv_groups
      valid_groups <- intersect(groups, c("Student","School","Grade","Race/Ethnicity","Gender","SPED","ELL","Tribe"))
      
      # Summarise (keep numeric here)
      if (length(valid_groups) == 0) {
        df <- df %>%
          dplyr::summarise(
            N = dplyr::n(),
            Metric = mean(Metric, na.rm = TRUE),
            .groups = "drop"
          )
      } else if ("Student" %in% valid_groups) {
        # Always allow N=1 for student-level views
        other_groups <- setdiff(valid_groups, "Student")
        df <- df %>%
          dplyr::group_by(dplyr::across(all_of(other_groups)), `Student ID`, `Student Name`) %>%
          dplyr::summarise(
            N = dplyr::n(),
            Metric = mean(Metric, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        df <- df %>%
          dplyr::group_by(dplyr::across(all_of(valid_groups))) %>%
          dplyr::summarise(
            N = dplyr::n(),
            Metric = mean(Metric, na.rm = TRUE),
            .groups = "drop"
          )
      }
      
      # Enforce Minimum N (but never >1 for Student case)
      mn_ui <- if (is.null(input$dv_min_n)) 0 else input$dv_min_n
      mn <- if ("Student" %in% valid_groups) max(1L, as.integer(mn_ui)) else as.integer(mn_ui)
      if (mn > 0) df <- dplyr::filter(df, N >= mn)
      
      # Sort by numeric Metric, then format as % with 2 decimals
      df <- dplyr::arrange(df, dplyr::desc(Metric))
      if (nrow(df)) df$Metric <- sprintf("%.2f%%", df$Metric)
      
      df
    })
    
    observeEvent(input$dv_groups, {
      groups <- if (is.null(input$dv_groups)) character(0) else input$dv_groups
      if ("Student" %in% groups) {
        updateNumericInput(session, "dv_min_n", value = 1)
      }
    })

    output$dv_table <- DT::renderDT({
      df <- dv_summary()
      DT::datatable(
        df,
        rownames = FALSE,
        extensions = c("Buttons","FixedHeader","Scroller"),
        options = list(
          dom = "Bfrtip",
          buttons = list("copy","csv","excel"),
          pageLength = 25,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE
        ),
        class = "stripe hover row-border order-column"
      )
    })
    
    # ---- Aggregates CSV download ----
    output$download_dv_csv <- downloadHandler(
      filename = function() paste0("data_view_aggregates_", Sys.Date(), ".csv"),
      content  = function(file) {
        readr::write_csv(dv_summary(), file, na = "")
      }
    )

    # ===== Student Data View (always student-level with min/max filters) =====
    dv_student_tbl <- reactive({
      df <- att_filtered()
      shiny::validate(need(is.data.frame(df), "Student view: data unavailable"))
      
      # Helper: first existing column
      pick_first <- function(nms) { for (nm in nms) if (nm %in% names(df)) return(nm); return(NULL) }
      
      # IDs & labels
      sid    <- pick_first(c("STUDENTS.dcid","dcid","student_id","REQUIRED.02-student_id"))
      fn     <- pick_first(c("STUDENTS.First_Name","First_Name","first_name","FIRST_NAME"))
      ln     <- pick_first(c("STUDENTS.Last_Name","Last_Name","last_name","LAST_NAME"))
      school <- pick_first(c("SCHOOLS.Name","school_name"))
      grade  <- pick_first(c("Grade_level","grade_level"))
      rcol   <- get_race_col(df)
      gcol   <- get_gender_col(df)
      scol   <- get_sped_col(df)
      ecol   <- get_ell_col(df)
      tcol   <- if ("tribe_name" %in% names(df)) "tribe_name" else NULL
      
      # Day & period fields
      d_abs  <- pick_first(c("days_absent","periods_absent"))
      d_exc  <- pick_first(c("days_excused","excused_absence"))
      d_tru  <- pick_first(c("days_truant","unexcused_absence"))
      d_pre  <- pick_first(c("days_present","periods_present"))
      d_elapsed <- pick_first(c("days_elapsed","total_days"))  # denominator for day-based rate
      rate_period <- pick_first(c("proportion_present"))       # fallback if needed
      
      # Numeric vectors
      days_present <- if (!is.null(d_pre) && grepl("^days_", d_pre)) as.numeric(df[[d_pre]]) else NA_real_
      days_absent  <- if (!is.null(d_abs) && grepl("^days_", d_abs)) as.numeric(df[[d_abs]]) else NA_real_
      days_elapsed <- if (!is.null(d_elapsed)) as.numeric(df[[d_elapsed]]) else NA_real_
      
      # If days_present missing but we have elapsed & days_absent, approximate present = elapsed - absent
      if (all(is.na(days_present)) && any(is.finite(days_elapsed)) && any(is.finite(days_absent))) {
        days_present <- days_elapsed - days_absent
      }
      
      # Days-based attendance rate (preferred)
      att_rate_num <- ifelse(is.finite(days_elapsed) & days_elapsed > 0 & is.finite(days_present),
                             100 * (days_present / days_elapsed),
                             NA_real_)
      
      # Fallback to period-based if day-based wasn’t computable
      if (all(is.na(att_rate_num)) && !is.null(rate_period)) {
        att_rate_num <- 100 * as.numeric(df[[rate_period]])
      }
      
      out <- tibble::tibble(
        `Student ID`     = as.character(df[[sid]]),
        `Student Name`   = paste0(trimws(as.character(df[[fn]])), " ", trimws(as.character(df[[ln]]))),
        School           = if (!is.null(school)) trimws(as.character(df[[school]])) else NA_character_,
        Grade            = if (!is.null(grade))  df[[grade]] else NA,
        `Race/Ethnicity` = if (!is.null(rcol))   trimws(as.character(df[[rcol]])) else NA_character_,
        Gender           = if (!is.null(gcol))   trimws(as.character(df[[gcol]])) else NA_character_,
        SPED             = if (!is.null(scol))   trimws(as.character(df[[scol]])) else NA_character_,
        ELL              = if (!is.null(ecol))   trimws(as.character(df[[ecol]])) else NA_character_,
        Tribe            = if (!is.null(tcol))   trimws(as.character(df[[tcol]])) else NA_character_,
        `Days Present`   = days_present,
        `Days Absent`    = if (!is.null(d_abs) && grepl("^days_", d_abs)) as.numeric(df[[d_abs]]) else NA_real_,
        `Days Excused`   = if (!is.null(d_exc) && grepl("^days_", d_exc)) as.numeric(df[[d_exc]]) else NA_real_,
        `Days Truant`    = if (!is.null(d_tru) && grepl("^days_", d_tru)) as.numeric(df[[d_tru]]) else NA_real_,
        `Attendance Rate`= att_rate_num  # numeric (0–100) for filtering/sorting
      )
      
      # --- Apply min/max filters (each independently) ---
      rng <- function(x, r, default_max) {
        if (!is.null(r) && length(r) == 2L && all(is.finite(r))) {
          is.finite(x) & x >= r[1] & x <= r[2]
        } else {
          rep(TRUE, length(x))
        }
      }
      out <- out[
        rng(out[["Days Present"]],    input$dv_student_present, NA) &
          rng(out[["Days Absent"]],     input$dv_student_absent,  NA) &
          rng(out[["Days Excused"]],    input$dv_student_excused, NA) &
          rng(out[["Days Truant"]],     input$dv_student_truant,  NA) &
          rng(out[["Attendance Rate"]], input$dv_student_rate,    NA),
        , drop = FALSE]
      
      # Keep optional cols
      keep_extra <- intersect(if (is.null(input$dv_student_cols)) character(0) else input$dv_student_cols,
                              c("School","Grade","Race/Ethnicity","Gender","SPED","ELL","Tribe"))
      base_cols  <- c("Student ID","Student Name")
      keep <- unique(c(base_cols, keep_extra, "Days Present","Days Absent","Days Excused","Days Truant"))
      if (isTRUE(input$dv_student_show_rate)) keep <- unique(c(keep, "Attendance Rate"))
      out <- out[, keep, drop = FALSE]
      
      # Sort to surface issues first (Absent/Truant high, then low Present)
      ord_abs <- if ("Days Absent" %in% names(out)) -out[["Days Absent"]] else 0
      ord_tru <- if ("Days Truant" %in% names(out)) -out[["Days Truant"]] else 0
      ord_pre <- if ("Days Present" %in% names(out))  out[["Days Present"]] else 0
      out <- out[order(ord_abs, ord_tru, ord_pre), , drop = FALSE]
      
      # Format Attendance Rate as percentage with exactly 2 decimals (display)
      if ("Attendance Rate" %in% names(out)) {
        out[["Attendance Rate"]] <- ifelse(
          is.finite(out[["Attendance Rate"]]),
          sprintf("%.2f%%", out[["Attendance Rate"]]),
          NA_character_
        )
      }
      
      out
    })
    
    output$dv_table_student <- DT::renderDT({
      DT::datatable(
        dv_student_tbl(),
        rownames  = FALSE,
        extensions = c("Buttons","FixedHeader","Scroller"),
        options = list(
          dom         = "Bfrtip",
          buttons     = list("copy","csv","excel"),
          pageLength  = 25,
          deferRender = TRUE,
          scrollY     = 500,
          scroller    = TRUE,
          scrollX     = TRUE,    # <-- important: horizontal scroll keeps header aligned
          autoWidth   = TRUE,
          fixedHeader = TRUE     # <-- keep header pinned vertically too (optional but nice)
        ),
        class = "nowrap stripe hover row-border order-column"  # nowrap helps alignment
      )
    })
    
    output$download_dv_csv_student <- downloadHandler(
      filename = function() paste0("data_view_students_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(dv_student_tbl(), file, na = "")
    )
    # ------- END 
    
    ###############################
    # NEW COURSE EXPLORATION CODE 
    ###############################
    # ---- Robust column picker for course_explorer ----
    cx_pick <- function(df, candidates) {
      for (nm in candidates) if (nm %in% names(df)) return(nm)
      NULL
    }
    
    cx_base <- reactive({
      shiny::validate(need(exists("course_explorer", inherits = TRUE),
                    "course_explorer not found in cache"))
      df <- get("course_explorer", inherits = TRUE)
      if (exists("to_utf8_df", inherits = TRUE)) df <- to_utf8_df(df)
      
      # --- Single-tribe scoping (minimal fix) ---
      if (isTRUE(SINGLE_TRIBE)) {
        # reuse the helper you already defined
        tcol <- cx_pick(df, c("tribe_name","Tribe","tribe","TRIBE","Tribe Name","Tribe_Name"))
        if (!is.null(tcol)) {
          df <- df[!is.na(df[[tcol]]) &
                     trimws(as.character(df[[tcol]])) == TRIBE(), , drop = FALSE]
        }
      }
      
      df
    })
    
    # Populate filter choices
    observe({
      df <- cx_base()
      school   <- cx_pick(df, c("School","School Name","SCHOOLS.Name"))
      grade    <- cx_pick(df, c("Grade","Grade Level","grade_level"))
      marking  <- cx_pick(df, c("Marking Period","REQUIRED.04-mp_code","mp_code","Term"))
      evalcol  <- cx_pick(df, c("Evaluation","evaluation","Status"))
      
      if (!is.null(school))  shinyWidgets::updatePickerInput(session, "cx_school",
                                                             choices = sort(unique(na.omit(trimws(as.character(df[[school]]))))))
      
      if (!is.null(grade))   shinyWidgets::updatePickerInput(session, "cx_grade",
                                                             choices = sort(unique(na.omit(df[[grade]]))))
      
      if (!is.null(marking)) shinyWidgets::updatePickerInput(session, "cx_marking",
                                                             choices = sort(unique(na.omit(trimws(as.character(df[[marking]]))))))
      
      if (!is.null(evalcol)) shinyWidgets::updatePickerInput(session, "cx_eval",
                                                             choices = sort(unique(na.omit(trimws(as.character(df[[evalcol]]))))))
    })
    
    # Convert letter grades (A+, A, …, F) to numeric points for a threshold slider (0–4.3)
    .grade_points <- local({
      gp <- c(
        "A+"=4.3,"A"=4.0,"A-"=3.7,
        "B+"=3.3,"B"=3.0,"B-"=2.7,
        "C+"=2.3,"C"=2.0,"C-"=1.7,
        "D+"=1.3,"D"=1.0,"D-"=0.7,
        "E" =0.0,"F"=0.0
      )
      function(x) {
        x <- trimws(toupper(as.character(x)))
        unname(ifelse(x %in% names(gp), gp[x], NA_real_))
      }
    })
    
    cx_filtered <- reactive({
      df <- cx_base()
      
      # Columns
      dcid     <- cx_pick(df, c("DCID","dcid","STUDENTS.dcid","Student ID","student_id"))
      first    <- cx_pick(df, c("First Name","FIRST_NAME","STUDENTS.First_Name"))
      last     <- cx_pick(df, c("Last Name","LAST_NAME","STUDENTS.Last_Name"))
      school   <- cx_pick(df, c("School","School Name","SCHOOLS.Name"))
      grade    <- cx_pick(df, c("Grade","Grade Level","grade_level"))
      marking  <- cx_pick(df, c("Marking Period","REQUIRED.04-mp_code","mp_code","Term"))
      course   <- cx_pick(df, c("Course Name","REQUIRED.03-course_name","course_name"))
      ltr      <- cx_pick(df, c("Letter Grade","letter_grade","letter"))
      num      <- cx_pick(df, c("Grade (0–100)","Grade (0-100)","grade_0_100","numeric_grade"))
      evalcol  <- cx_pick(df, c("Evaluation","evaluation","Status"))
      gemail   <- cx_pick(df, c("Guardian Email","guardian_email","Parent Email","parent_email"))
      phone    <- cx_pick(df, c("Home Phone","home_phone","Phone","phone"))
      
      # Filters
      if (!is.null(school)   && length(input$cx_school))  df <- dplyr::filter(df, .data[[school]]   %in% input$cx_school)
      if (!is.null(grade)    && length(input$cx_grade))   df <- dplyr::filter(df, .data[[grade]]    %in% input$cx_grade)
      if (!is.null(marking)  && length(input$cx_marking)) df <- dplyr::filter(df, .data[[marking]]  %in% input$cx_marking)
      if (!is.null(evalcol)  && length(input$cx_eval))    df <- dplyr::filter(df, .data[[evalcol]]  %in% input$cx_eval)
      
      # Failing rule
      failing <- rep(FALSE, nrow(df))
      if (!is.null(evalcol)) failing <- failing | grepl("FAILING", toupper(as.character(df[[evalcol]])), fixed = TRUE)
      .grade_points <- function(x){
        gp <- c("A+"=4.3,"A"=4.0,"A-"=3.7,"B+"=3.3,"B"=3.0,"B-"=2.7,"C+"=2.3,"C"=2.0,"C-"=1.7,
                "D+"=1.3,"D"=1.0,"D-"=0.7,"E"=0.0,"F"=0.0)
        x <- trimws(toupper(as.character(x))); unname(ifelse(x %in% names(gp), gp[x], NA_real_))
      }
      if (!is.null(num)) failing <- failing | (suppressWarnings(as.numeric(df[[num]])) <= input$cx_grade_max)
      if (!is.null(ltr)) failing <- failing | (.grade_points(df[[ltr]]) <= input$cx_letter_max)
      df$`Failing Based on Filter?` <- failing
      if (isTRUE(input$cx_fail_only)) df <- dplyr::filter(df, `Failing Based on Filter?`)
      
      # --- Build "Student Name" and drop raw First/Last ---
      if (!("Student Name" %in% names(df))) {
        fn <- if (!is.null(first)) trimws(as.character(df[[first]])) else ""
        ln <- if (!is.null(last))  trimws(as.character(df[[last]]))  else ""
        df$`Student Name` <- trimws(ifelse(nchar(ln) & nchar(fn), paste0(ln, ", ", fn),
                                           ifelse(nchar(ln), ln, fn)))
      }
      
      # Order failing first, then lowest numeric grade
      if (!is.null(num)) df <- dplyr::arrange(df, dplyr::desc(`Failing Based on Filter?`), suppressWarnings(as.numeric(.data[[num]])))
      
      # Curated default view
      out <- df
      if (!isTRUE(input$cx_show_all_cols)) {
        #keep <- c(dcid, "Student Name", school, grade, marking, course, ltr, num, evalcol, gemail, phone, "Fail?")
        keep <- c(dcid, "Student Name", course, ltr, num, evalcol, gemail, "Failing Based on Filter?")
        keep <- unique(keep[!is.na(keep) & keep %in% names(df)])
        if (length(keep)) out <- df[, keep, drop = FALSE]
      }
      
      # Always hide First/Last from display (avoid duplication)
      drop_cols <- na.omit(c(first, last))
      drop_cols <- drop_cols[drop_cols %in% names(out)]
      if (length(drop_cols)) out <- out[, setdiff(names(out), drop_cols), drop = FALSE]
      
      # Put Student Name right after DCID (if present)
      if ("Student Name" %in% names(out)) {
        if (!is.null(dcid) && dcid %in% names(out)) {
          ord <- c(dcid, "Student Name", setdiff(names(out), c(dcid, "Student Name")))
          out <- out[, ord, drop = FALSE]
        } else {
          ord <- c("Student Name", setdiff(names(out), "Student Name"))
          out <- out[, ord, drop = FALSE]
        }
      }
      
      out
    })
    
    output$cx_caption <- renderUI({
      df  <- cx_filtered()
      tot <- nrow(cx_base())
      htmltools::div(class="text-muted small",
                     sprintf("%s rows (of %s) | %s columns",
                             format(nrow(df), big.mark=","), format(tot, big.mark=","), ncol(df))
      )
    })
    
    output$cx_table <- DT::renderDT({
      DT::datatable(
        cx_filtered(),
        selection = "multiple",
        rownames  = FALSE,
        extensions = c("Buttons","FixedHeader","Scroller"),
        options = list(
          dom         = "Bfrtip",
          buttons     = list("copy","csv","excel"),
          pageLength  = 25,
          deferRender = TRUE,
          scrollY     = 500,
          scroller    = TRUE,
          scrollX     = TRUE,
          autoWidth   = TRUE,
          fixedHeader = TRUE
        ),
        class = "nowrap stripe hover row-border order-column"
      )
    })
    
    # Download selected rows (or all if none)
    output$cx_download_selected <- downloadHandler(
      filename = function() sprintf("course_explorer_selected_%s.csv", Sys.Date()),
      content  = function(file) {
        df <- cx_filtered()
        idx <- input$cx_table_rows_selected
        if (length(idx)) df <- df[idx, , drop = FALSE]
        readr::write_csv(df, file, na = "")
      }
    )
    
    # Email composer (Guardian Email -> fallback to any email-like column)
    observeEvent(input$cx_email_btn, {
      df  <- cx_filtered()
      idx <- input$cx_table_rows_selected
      
      if (!length(idx)) {
        showModal(modalDialog(
          title = "Select rows first",
          "Please select one or more rows in the Courses table to email those guardians.",
          easyClose = TRUE
        ))
        return()
      }
      
      df_sel <- df[idx, , drop = FALSE]
      
      # ---- Find the Guardian Email column in the *visible* table ----
      cols <- names(df_sel)
      cols_l <- tolower(cols)
      norm   <- function(x) tolower(gsub("[^a-z]", "", x))
      
      # candidates by several heuristics
      cand_idx <- unique(c(
        which(cols_l %in% c("guardian email", "guardian_email", "parent email", "parent_email")),
        which(grepl("guardian.*email|parent.*email", cols_l, perl = TRUE)),
        which(norm(cols) %in% c("guardianemail","parentemail","guardianemailaddress","parentemailaddress"))
      ))
      
      if (!length(cand_idx)) {
        showModal(modalDialog(
          title = "Guardian Email column not found",
          "The visible table does not include a recognizable Guardian Email column.",
          easyClose = TRUE
        ))
        return()
      }
      
      # Prefer columns containing the word 'guardian' over 'parent'
      cand_names <- cols[cand_idx]
      prefer_ord <- order(!grepl("guardian", tolower(cand_names)))  # guardian first
      gemail_col <- cand_names[prefer_ord][1]
      
      # ---- Extract emails from the selected cell values (robust; no first-letter loss) ----
      extract_emails <- function(v) {
        v <- as.character(v)
        # remove zero-width / BOM and mailto:
        v <- gsub("[\u200B-\u200D\uFEFF]", "", v, perl = TRUE)
        v <- gsub("(?i)\\bmailto:\\s*", "", v, perl = TRUE)
        # find emails anywhere in the string
        m <- gregexpr("(?i)[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}", v, perl = TRUE)
        out <- unlist(regmatches(v, m), use.names = FALSE)
        out <- tolower(unique(out[nzchar(out)]))
        out
      }
      
      recipients <- extract_emails(df_sel[[gemail_col]])
      
      if (!length(recipients)) {
        showModal(modalDialog(
          title = "No guardian emails found",
          "The selected rows contain no valid addresses in the Guardian Email column.",
          easyClose = TRUE
        ))
        return()
      }
      
      mailto <- paste0("mailto:", paste(utils::head(recipients, 100), collapse = ";"))
      showModal(modalDialog(
        title = "Compose Guardian Email",
        htmltools::div(class = "text-muted small",
                       sprintf("%d selected row(s) → %d unique guardian email(s) via column “%s”.",
                               nrow(df_sel), length(recipients), gemail_col)
        ),
        htmltools::tags$pre(paste(recipients, collapse = ";")),
        htmltools::a(href = mailto, target = "_blank", class = "btn btn-primary", "Open email client"),
        footer = tagList(modalButton("Close")),
        size = "l", easyClose = TRUE
      ))
    })
    
    
    # Note composer (unchanged)
    observeEvent(input$cx_note_btn, {
      df <- cx_filtered()
      idx <- input$cx_table_rows_selected
      if (!length(idx)) idx <- seq_len(min(nrow(df), 200))
      showModal(modalDialog(
        title = "Compose Note",
        textAreaInput("cx_note_text", "Note", width = "100%", height = "200px",
                      value = "Student is currently failing the following course(s). Plan:\n- \n- "),
        footer = tagList(modalButton("Close")),
        size = "l", easyClose = TRUE
      ))
    })
    # -------- END 
    
    # ------ ATTENDANCE BY GRADE ------
    att_by_grade <- reactive({
      mc <- att_metric_col()
      req(mc %in% names(att_filtered()))
      
      att_filtered() %>%
        dplyr::mutate(
          # normalize, then label
          Grade = grade_label(Grade_level),
          # numeric sort key that preserves Pre-K/K order
          Grade_ord = dplyr::case_when(
            Grade == "Pre-K 1"    ~ -2L,
            Grade == "Pre-K 2"    ~ -1L,
            Grade == "Kindergarten" ~ 0L,
            TRUE ~ suppressWarnings(as.integer(Grade))
          )
        ) %>%
        dplyr::group_by(Grade) %>%
        dplyr::summarise(
          N = dplyr::n(),
          Value = 100 * safe_mean(.data[[mc]]),
          .groups = "drop"
        ) %>%
        # reattach order key (safe join on Grade)
        dplyr::left_join(
          y = dplyr::distinct(
            att_filtered() %>%
              dplyr::mutate(Grade = grade_label(Grade_level),
                            Grade_ord = dplyr::case_when(
                              Grade == "Pre-K 1" ~ -2L,
                              Grade == "Pre-K 2" ~ -1L,
                              Grade == "Kindergarten" ~ 0L,
                              TRUE ~ suppressWarnings(as.integer(Grade))
                            )) %>%
              dplyr::select(Grade, Grade_ord)
          ),
          by = "Grade"
        ) %>%
        # if you prefer to drop any truly unknown/malformed grades, uncomment:
        # dplyr::filter(!is.na(Grade), !is.na(Grade_ord)) %>%
        dplyr::arrange(Grade_ord, Grade) %>%
        dplyr::select(Grade, N, Value)
    })
    
    # NEW CODE CONT. 
    output$att_grade_plot <- renderPlotly({
      df <- att_by_grade(); req(nrow(df) > 0)
      
      # Preserve PreK1 / PreK2, then TK, K, 1-12, etc.
      norm_grade <- function(x) {
        x0 <- trimws(as.character(x))
        xl <- stringr::str_to_lower(x0)
        xl <- stringr::str_replace_all(xl, "[\\s-]+", "")  # "Pre-K 1" -> "prek1"
        
        dplyr::case_when(
          xl %in% c("prek1","pk1")               ~ "PreK1",
          xl %in% c("prek2","pk2")               ~ "PreK2",
          xl %in% c("prek","pk")                 ~ "PreK",
          xl == "tk"                             ~ "TK",
          xl %in% c("k","kindergarten")          ~ "K",
          stringr::str_detect(xl, "^\\d+(st|nd|rd|th)?$") ~ as.character(readr::parse_number(xl)),
          stringr::str_detect(xl, "ungraded|ug") ~ "Ungraded",
          TRUE                                   ~ x0
        )
      }
      
      grade_levels <- c("PreK1","PreK2","K", as.character(1:12), "Ungraded","Other")
      
      df <- df |>
        dplyr::mutate(
          Grade = norm_grade(Grade),
          Grade = factor(Grade, levels = grade_levels, ordered = TRUE)
        ) |>
        dplyr::arrange(Grade)
      
      plot_ly(
        df, x = ~Grade, y = ~Value, type = "bar",
        text = ~sprintf("%.1f%%", Value), textposition = "outside",
        hovertemplate = "<b>%{x}</b><br>%{y:.2f}%<extra></extra>",
        marker = list(color = "#d62728", line = list(color="black", width=0.8))
      ) %>%
        layout(
          title = list(
            text = paste0("Metric: ", input$att_metric, " — by Grade"),
            x = 0.5, font = list(size = 18)
          ),
          xaxis = list(title = ""),
          yaxis = list(title = "Percent",
                       range = c(0, max(100, ceiling(max(df$Value, na.rm=TRUE)*1.15)))),
          showlegend = FALSE, margin = list(t=50, b=80)
        ) %>%
        strip_modebar()
    })
    
    output$att_grade_table <- DT::renderDT({
      df <- att_by_grade()
      if (!nrow(df)) {
        return(DT::datatable(data.frame(Message = "No data"), options = list(dom = 't')))
      }
      
      # Same normalization used in the plot:
      norm_grade <- function(x) {
        x0 <- trimws(as.character(x))
        xl <- stringr::str_to_lower(x0)
        xl <- stringr::str_replace_all(xl, "[\\s-]+", "")
        dplyr::case_when(
          xl %in% c("prek1","pk1")               ~ "PreK1",
          xl %in% c("prek2","pk2")               ~ "PreK2",
          xl %in% c("prek","pk")                 ~ "PreK",
          xl == "tk"                             ~ "TK",
          xl %in% c("k","kindergarten")          ~ "K",
          stringr::str_detect(xl, "^\\d+(st|nd|rd|th)?$") ~ as.character(readr::parse_number(xl)),
          stringr::str_detect(xl, "ungraded|ug") ~ "Ungraded",
          TRUE                                   ~ x0
        )
      }
      
      grade_levels <- c("PreK1","PreK2","PreK","TK","K", as.character(1:12), "Ungraded","Other")
      
      df <- df %>%
        dplyr::mutate(
          Grade = norm_grade(Grade),
          Grade = factor(Grade, levels = grade_levels, ordered = TRUE),
          .order = as.integer(Grade)
        ) %>%
        dplyr::arrange(Grade)
      
      # Format Value for display: 2 decimals, and '--' when missing
      df$Value <- ifelse(is.na(df$Value), "--", sprintf("%.2f", df$Value))
      
      ord_col <- which(names(df) == ".order") - 1L
      
      DT::datatable(
        df,
        rownames = FALSE,
        class = "stripe hover cell-border compact",
        options = list(
          pageLength = 15,
          autoWidth = TRUE,
          order = list(list(ord_col, "asc")),
          columnDefs = list(list(visible = FALSE, targets = ord_col))
        )
      )
    })
    # output$att_grade_table <- DT::renderDT({
    #   df <- att_by_grade()
    #   if (!nrow(df)) return(DT::datatable(data.frame(Message = "No data"), options = list(dom = 't')))
    #   
    #   # Same normalization used in the plot:
    #   norm_grade <- function(x) {
    #     x0 <- trimws(as.character(x))
    #     xl <- stringr::str_to_lower(x0)
    #     xl <- stringr::str_replace_all(xl, "[\\s-]+", "")  # "Pre-K 1" -> "prek1"
    #     dplyr::case_when(
    #       xl %in% c("prek1","pk1")               ~ "PreK1",
    #       xl %in% c("prek2","pk2")               ~ "PreK2",
    #       xl %in% c("prek","pk")                 ~ "PreK",
    #       xl == "tk"                             ~ "TK",
    #       xl %in% c("k","kindergarten")          ~ "K",
    #       stringr::str_detect(xl, "^\\d+(st|nd|rd|th)?$") ~ as.character(readr::parse_number(xl)),
    #       stringr::str_detect(xl, "ungraded|ug") ~ "Ungraded",
    #       TRUE ~ x0
    #     )
    #   }
    #   
    #   grade_levels <- c("PreK1","PreK2","PreK","TK","K", as.character(1:12), "Ungraded","Other")
    #   
    #   df <- df %>%
    #     dplyr::mutate(
    #       Grade = norm_grade(Grade),
    #       Grade = factor(Grade, levels = grade_levels, ordered = TRUE),
    #       Value = round(Value, 2),
    #       .order = as.integer(Grade)   # hidden sort key that follows factor order
    #     ) %>%
    #     dplyr::arrange(Grade)
    #   
    #   ord_col <- which(names(df) == ".order") - 1L  # 0-based index for DataTables
    #   
    #   DT::datatable(
    #     df,
    #     rownames = FALSE,
    #     class = "stripe hover cell-border compact",
    #     options = list(
    #       pageLength = 15,
    #       autoWidth = TRUE,
    #       order = list(list(ord_col, "asc")),
    #       columnDefs = list(list(visible = FALSE, targets = ord_col))  # hide .order
    #     )
    #   ) %>%
    #     DT::formatRound("Value", 2)
    # })
    
    output$download_att_grade <- downloadHandler(
      filename = function() paste0("attendance_by_grade_", Sys.Date(), ".csv"),
      content = function(file) {
        readr::write_csv(att_by_grade(), file, na = "")
      }
    )
    
    # ------- BY SCHOOL -------
    att_by_school <- reactive({
      mc <- att_metric_col()
      req(mc %in% names(att_filtered()))
      att_filtered() %>%
        dplyr::group_by(School = SCHOOLS.Name) %>%
        dplyr::summarise(
          N = dplyr::n(),
          Value = 100 * safe_mean(.data[[mc]]) 
        ) %>% dplyr::ungroup() %>%
        dplyr::filter(!is.na(School) & nzchar(School)) %>%
        dplyr::arrange(School)
    })
    
    output$att_school_plot <- renderPlotly({
      df <- att_by_school(); req(nrow(df) > 0)
      # order schools by value descending for readability
      df$School <- factor(df$School, levels = df$School[order(df$Value, decreasing = TRUE)])
      
      plot_ly(
        df, x = ~School, y = ~Value, type = "bar",
        text = ~sprintf("%.1f%%", Value), textposition = "outside",
        hovertemplate = "<b>%{x}</b><br>%{y:.2f}%<extra></extra>",
        marker = list(color = "orange", line = list(color="black", width=0.8))
      ) %>%
        layout(
          title = list(
            text = paste0("Metric: ", input$att_metric, " — by School"),
            x = 0.5, font = list(size = 18)
          ),
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Percent", range = c(0, max(100, ceiling(max(df$Value, na.rm=TRUE)*1.15)))),
          showlegend = FALSE, margin = list(t=50, b=120)
        ) %>% strip_modebar()
    })
    
    output$att_school_table <- DT::renderDT({
      df <- att_by_school()
      if (!nrow(df)) {
        return(DT::datatable(data.frame(Message="No data"), options=list(dom='t')))
      }
      
      # Show 2 decimals and '--' for missing
      df <- df %>%
        dplyr::mutate(
          Value = ifelse(is.na(Value), "--", sprintf("%.2f", Value))
        )
      
      DT::datatable(
        df,
        rownames = FALSE,
        class = "stripe hover cell-border compact",
        options = list(pageLength = 15, autoWidth = TRUE)
      )
    })
    # output$att_school_table <- DT::renderDT({
    #   df <- att_by_school()
    #   if (!nrow(df)) return(DT::datatable(data.frame(Message="No data"), options=list(dom='t')))
    #   df %>%
    #     dplyr::mutate(Value = round(Value, 2)) %>%
    #     DT::datatable(
    #       rownames = FALSE, class = "stripe hover cell-border compact",
    #       options = list(pageLength = 15, autoWidth = TRUE)
    #     )
    # })
    
    output$download_att_school <- downloadHandler(
      filename = function() paste0("attendance_by_school_", Sys.Date(), ".csv"),
      content = function(file) {
        readr::write_csv(att_by_school(), file, na = "")
      }
    )
    # ------ END 
    
    # --- Helper (optional): map grade picker to values in data ---
    map_grade_choice <- function(choice) {
      if (identical(choice, "All")) return(NULL)
      if (identical(choice, "Pre-K 1"))      return(c(-2, "Pre-K 1"))
      if (identical(choice, "Pre-K 2"))      return(c(-1, "Pre-K 2"))
      if (identical(choice, "Kindergarten")) return(c(0,  "Kindergarten"))
      # 1..12 as number or string
      c(suppressWarnings(as.numeric(choice)), choice)
    }
    
    # Incident plot where main line reacts to filters 
    output$discipline_trend_plot <- renderPlot({
      req(input$trend_date_range)
      dr <- as.Date(input$trend_date_range)
      
      # --- Build long incident table (same approach as calendar/table) ---
      disc_long <- disc_wide %>%
        tidyr::pivot_longer(cols = starts_with("Discipline_"),
                            names_to = "incident", values_to = "description") %>%
        dplyr::filter(!is.na(description)) %>%
        dplyr::mutate(index = sub(".*_", "", incident))
      
      date_long <- date_wide %>%
        tidyr::pivot_longer(cols = starts_with("Incident_Date_"),
                            names_to = "incident", values_to = "date") %>%
        dplyr::filter(!is.na(date)) %>%
        dplyr::mutate(index = sub(".*Incident_Date_", "", incident))
      
      inc0 <- dplyr::inner_join(disc_long, date_long, by = c("dcid", "index")) %>%
        dplyr::mutate(date = as.Date(date)) %>%
        dplyr::filter(date >= dr[1], date <= dr[2])
      
      # --- Attach student info (school, grade, tribe, native flag) ---
      stu <- students_dat2 %>%
        dplyr::select(
          dcid = STUDENTS.dcid,
          School = SCHOOLS.Name,
          Grade  = Grade_level,
          Tribe  = tribe_name,
          tribal_student
        )
      
      inc <- dplyr::left_join(inc0, stu, by = "dcid")
      
      # --- Apply UI filters (grade/school/tribe) to the INCIDENTS used for the selected tribe series ---
      grade_to_label <- function(g) {
        gch <- as.character(g)
        dplyr::recode(gch,
                      "-2" = "Pre-K 1",
                      "-1" = "Pre-K 2",
                      "0"  = "Kindergarten",
                      .default = gch
        )
      }
      
      inc_sel <- inc
      if (!identical(input$trend_grade, "All")) {
        inc_sel <- inc_sel %>%
          dplyr::mutate(GradeLabel = grade_to_label(Grade)) %>%
          dplyr::filter(GradeLabel == input$trend_grade)
      }
      
      if (!identical(input$trend_school, "All")) {
        inc_sel <- inc_sel %>% dplyr::filter(School == input$trend_school)
      }
      
      #sel_tribe <- input$trend_tribe
      sel_tribe <- if(SINGLE_TRIBE) {
        TRIBE()
      } else {
        input$trend_tribe
      }
      
      # --- Weekly vs Monthly granularity ---
      monthly  <- identical(input$trend_granularity, "Monthly")
      collapse <- function(d) {
        if (monthly) {
          lubridate::floor_date(d, "month")
        } else {
          lubridate::floor_date(d, "week", week_start = 1)  # ISO week start Monday
        }
      }
      
      all_bins <- tibble::tibble(
        date = if (monthly) {
          seq(lubridate::floor_date(dr[1], "month"),
              lubridate::floor_date(dr[2], "month"), by = "month")
        } else {
          seq(lubridate::floor_date(dr[1], "week", week_start = 1),
              lubridate::floor_date(dr[2], "week", week_start = 1), by = "week")
        }
      )
      
      # --- Decide x-axis breaks & labels dynamically ---
      if (monthly) {
        x_breaks     <- seq(lubridate::floor_date(dr[1], "month"),
                            lubridate::floor_date(dr[2], "month"), by = "1 month")
        x_labels_fun <- scales::label_date("%b %Y")
        x_guide      <- ggplot2::guide_axis()
        x_text_theme <- ggplot2::theme()
      } else {
        n_bins <- nrow(all_bins)  # weekly bins
        
        if (n_bins <= 16) {
          target_lbls <- 10L
          step_weeks  <- max(1L, ceiling(n_bins / target_lbls))
          x_breaks    <- seq(lubridate::floor_date(dr[1], "week", week_start = 1),
                             lubridate::floor_date(dr[2], "week", week_start = 1),
                             by = paste(step_weeks, "weeks"))
          x_labels_fun <- function(d) paste0("Week of ", format(d, "%b %d"))
          x_guide       <- ggplot2::guide_axis(
            n.dodge = if (step_weeks >= 3) 2 else 1,
            check.overlap = TRUE
          )
          x_text_theme  <- ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          )
        } else {
          x_breaks     <- seq(lubridate::floor_date(dr[1], "month"),
                              lubridate::floor_date(dr[2], "month"), by = "1 month")
          x_labels_fun <- scales::label_date("%b %Y")
          x_guide      <- ggplot2::guide_axis()
          x_text_theme <- ggplot2::theme()
        }
      }
      
      
      # --- Reference series: All Native (District) ---
      s_native <- inc %>%
        dplyr::filter(tribal_student == 1) %>%
        dplyr::count(date = collapse(date), name = "n") %>%
        dplyr::right_join(all_bins, by = "date") %>%
        dplyr::mutate(n = tidyr::replace_na(n, 0L),
                      series = "All Native (District)")
      
      # --- Reference series: Non-Native ---
      s_nonnative <- inc %>%
        dplyr::filter(tribal_student == 0) %>%
        dplyr::count(date = collapse(date), name = "n") %>%
        dplyr::right_join(all_bins, by = "date") %>%
        dplyr::mutate(n = tidyr::replace_na(n, 0L),
                      series = "Non-Native")
      
      plot_df <- dplyr::bind_rows(s_native, s_nonnative)
      
      # --- Selected Tribe series (only if a specific tribe is chosen) ---
      if (!identical(sel_tribe, "All")) {
        s_sel <- inc_sel %>%
          dplyr::filter(Tribe == sel_tribe) %>%
          dplyr::count(date = collapse(date), name = "n") %>%
          dplyr::right_join(all_bins, by = "date") %>%
          dplyr::mutate(n = tidyr::replace_na(n, 0L),
                        series = paste0("Selected Tribe — ", sel_tribe))
        plot_df <- dplyr::bind_rows(plot_df, s_sel)
      }
      
      # --- Legend order & colors ---
      lvl <- c("All Native (District)", "Non-Native",
               if (!identical(sel_tribe, "All")) paste0("Selected Tribe — ", sel_tribe))
      plot_df$series <- factor(plot_df$series, levels = lvl)
      
      cols <- c("All Native (District)" = "orange",
                "Non-Native"            = "grey50")
      if (!identical(sel_tribe, "All")) {
        cols[paste0("Selected Tribe — ", sel_tribe)] <- "#d62728"
      }
      
      # --- Plot (with point labels + adaptive x-axis) ---
      ggplot2::ggplot(plot_df, ggplot2::aes(date, n, color = series)) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::geom_text(
          ggplot2::aes(label = n),
          vjust = -0.6, size = 3, color = "black",
          show.legend = FALSE
        ) +
        ggplot2::scale_color_manual(values = cols, name = NULL) +
        # ggplot2::scale_x_date(date_breaks = date_breaks_str,
        #                       date_labels = date_labels_str,
        #                       guide = x_guide) +
        ggplot2::scale_x_date(breaks = x_breaks, labels = x_labels_fun, guide = x_guide) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.15))) +
        ggplot2::labs(
          x = NULL, y = "Incidents",
          title = "Trend of Disciplinary Incidents Over Time"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          legend.position = "top",
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
        ) +
        x_text_theme   # <-- keep this at the very end
    })

    # ---- School Map ---- 
    output$school_code_table <- DT::renderDataTable({
      datatable(
        school_map,
        rownames = FALSE,
        options = list(
          dom = 't',
          pageLength = nrow(school_map),
          ordering = FALSE,
          columnDefs = list(list(className = 'dt-left', targets = "_all"))
        )
      )
    })
    
    # # ---- UPDATED Circular Absent vs. Present Bar plot (stacked 100%, interactive) ----
    # output$circularPlot <- ggiraph::renderGirafe({
    #   # Ensure required objects exist & have rows (precomputed by Analyses.R)
    #   req(exists("data", inherits = TRUE),
    #       exists("grid_data", inherits = TRUE),
    #       exists("label_data", inherits = TRUE),
    #       exists("base_data", inherits = TRUE))
    #   req(nrow(data) > 0, nrow(grid_data) > 0, nrow(label_data) > 0, nrow(base_data) > 0)
    #   
    #   # --- Clean & normalize: ensure two parts per id that sum to 1 ---
    #   plot_df <- data |>
    #     dplyr::mutate(
    #       id          = as.integer(id),
    #       observation = as.character(observation),
    #       value       = suppressWarnings(as.numeric(value))
    #     ) |>
    #     dplyr::filter(is.finite(value)) |>
    #     dplyr::mutate(value = pmax(0, pmin(1, value))) |>
    #     dplyr::group_by(id) |>
    #     dplyr::mutate(
    #       total = sum(value, na.rm = TRUE),
    #       # If not already complementary, scale so value1 + value2 = 1
    #       value = dplyr::if_else(total > 0, value / total,
    #                              dplyr::if_else(observation == "value2", 1, 0))
    #     ) |>
    #     dplyr::ungroup()
    #   
    #   # --- Build stacked rects so wedges sum to 100% under coord_polar ---
    #   # Draw Present (value1) first (base), then Absent (value2) on top.
    #   bars <- plot_df |>
    #     dplyr::mutate(
    #       # Present first (base), Absent second (on top)
    #       draw_order = dplyr::case_when(
    #         observation == "value2" ~ 1L,  # % Present (bottom)
    #         observation == "value1" ~ 2L,  # % Absent (top)
    #         TRUE ~ 3L
    #       )
    #     ) |>
    #     dplyr::arrange(id, draw_order) |>
    #     dplyr::group_by(id) |>
    #     dplyr::mutate(
    #       frac = value,
    #       y0   = dplyr::lag(cumsum(frac), default = 0) * 100,
    #       y1   = cumsum(frac) * 100
    #     ) |>
    #     dplyr::ungroup() |>
    #     dplyr::mutate(
    #       xmid        = id,
    #       xmin        = xmid - 0.45,
    #       xmax        = xmid + 0.45,
    #       ymin        = y0,
    #       ymax        = y1,
    #       tooltip_txt = paste0(
    #         dplyr::if_else(observation == "value1", "% Absent",
    #                        dplyr::if_else(observation == "value2", "% Present", observation)
    #         ),
    #         ": ", sprintf("%.1f%%", frac * 100)
    #       ),
    #       data_id_txt = paste0(id, "_", observation)
    #     )
    #   
    #   # --- Plot ---
    #   p <- ggplot2::ggplot() +
    #     ggiraph::geom_rect_interactive(
    #       data = bars,
    #       mapping = ggplot2::aes(
    #         xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    #         fill    = observation,
    #         tooltip = tooltip_txt,
    #         data_id = data_id_txt
    #       ),
    #       alpha = 0.8
    #     ) +
    #     ggplot2::scale_fill_manual(
    #       values = c("value1" = "red", "value2" = "orange"),
    #       labels = c("value1" = "% Absent", "value2" = "% Present"),
    #       name   = "Attendance",
    #       na.translate = FALSE
    #     ) +
    #     # grid rings at 0 / 50 / 100
    #     ggplot2::geom_segment(data = grid_data, ggplot2::aes(x = end, y = 0,   xend = start, yend = 0),
    #                           colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
    #     ggplot2::geom_segment(data = grid_data, ggplot2::aes(x = end, y = 50,  xend = start, yend = 50),
    #                           colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
    #     ggplot2::geom_segment(data = grid_data, ggplot2::aes(x = end, y = 100, xend = start, yend = 100),
    #                           colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
    #     ggplot2::annotate(
    #       "text",
    #       x = max(label_data$id, na.rm = TRUE) + 1,
    #       y = c(0, 50, 100),
    #       label = c("0%", "50%", "100%"),
    #       color = "black", size = 4, hjust = 0, fontface = "bold"
    #     ) +
    #     ggplot2::ylim(-30, 135) +
    #     ggplot2::theme_void() +
    #     ggplot2::theme(
    #       legend.position   = c(0.09, 0.88),
    #       legend.background = ggplot2::element_rect(fill = "white", color = NA),
    #       legend.title      = ggplot2::element_text(face = "bold", size = 10),
    #       legend.text       = ggplot2::element_text(face = "bold", size = 9),
    #       plot.margin       = grid::unit(rep(-1, 4), "cm")
    #     ) +
    #     ggplot2::coord_polar(start = 0) +
    #     # OUTER full school names
    #     ggplot2::geom_text(
    #       data = label_data,
    #       mapping = ggplot2::aes(x = id, y = 125, label = school_clean, angle = angle, hjust = hjust),
    #       inherit.aes = FALSE, color = "black", fontface = "bold", size = 2.7, alpha = 0.9
    #     ) +
    #     # INNER red school codes (hover shows full name)
    #     ggiraph::geom_text_interactive(
    #       data = base_data,
    #       mapping = ggplot2::aes(
    #         x = title, y = 112,
    #         label   = paste0("Sch ", group),
    #         tooltip = as.character(Name),
    #         data_id = as.character(title)
    #       ),
    #       inherit.aes = FALSE,
    #       angle = -0.5, hjust = 0.5,
    #       color = "red", fontface = "bold", size = 2.8
    #     )
    #   
    #   ggiraph::girafe(
    #     ggobj = p,
    #     width_svg  = 9,
    #     height_svg = 9,
    #     options = list(
    #       ggiraph::opts_tooltip(css = "background:#000;color:#fff;padding:6px 8px;border-radius:4px;font-size:12px;"),
    #       ggiraph::opts_hover(css   = "fill:#d62728;font-weight:700;"),
    #       ggiraph::opts_sizing(rescale = TRUE),
    #       ggiraph::opts_toolbar(saveaspng = FALSE),
    #       ggiraph::opts_zoom(min = 1, max = 1),
    #       ggiraph::opts_selection(type = "none")
    #     )
    #   )
    # })
    
    # ---------------------------
    # Student contact information
    # ---------------------------
    
    output$student_contacts_table <- renderUI({
      # Only show if we have contacts data at all
      req(exists("student_contacts"))
      
      student <- selected_student()
      req(nrow(student) > 0)
      
      # Match by DCID
      this_dcid <- student$STUDENTS.dcid[1]
      
      contacts_df <- student_contacts %>%
        dplyr::filter(STUDENTS.dcid == this_dcid)
      
      if (nrow(contacts_df) == 0) {
        return(
          tags$div(
            style = "background-color: white; padding: 10px; border-radius: 5px; font-size: 16px; font-weight: bold;",
            tags$p("No contacts found for this student.")
          )
        )
      }
      
      # Keep a clean subset of columns for the table
      contacts_table <- contacts_df %>%
        dplyr::transmute(
          `Contact Name`  = Contact_Name,
          Relationship    = Relationship,
          Email           = Email,
          Phone           = Phone,
          `Lives With`    = Lives_With,
          `Emergency`     = Emergency_Contact,
          `Receives Mail` = Receives_Mail,
          `Pickup`        = School_Pickup
        )
      
      bs4Card(
        title       = "Student Contacts",
        status      = "info",
        solidHeader = TRUE,
        width       = 12,
        collapsible = TRUE,
        DT::dataTableOutput("student_contacts_dt")
      )
    })
    
    output$student_contacts_dt <- DT::renderDataTable({
      req(exists("student_contacts"))
      student <- selected_student()
      req(nrow(student) > 0)
      
      this_dcid <- student$STUDENTS.dcid[1]
      
      contacts_df <- student_contacts %>%
        dplyr::filter(STUDENTS.dcid == this_dcid)
      
      if (nrow(contacts_df) == 0) return(NULL)
      
      contacts_df %>%
        dplyr::transmute(
          `Contact Name`  = Contact_Name,
          Relationship    = Relationship,
          Email           = Email,
          Phone           = Phone,
          `Lives With`    = Lives_With,
          `Emergency`     = Emergency_Contact,
          `Receives Mail` = Receives_Mail,
          `Pickup`        = School_Pickup
        ) %>%
        DT::datatable(
          rownames = FALSE,
          class    = "stripe hover cell-border compact",
          options  = list(
            pageLength = 5,
            dom        = "tip",
            autoWidth  = TRUE
          )
        )
    })
    
    # ---- UPDATED Circular Absent vs. Present Bar plot (stacked 100%, interactive) ----
    output$circularPlot <- ggiraph::renderGirafe({
      # Ensure required objects exist & have rows (precomputed by Analyses.R)
      req(exists("data", inherits = TRUE),
          exists("grid_data", inherits = TRUE),
          exists("label_data", inherits = TRUE),
          exists("base_data", inherits = TRUE))
      req(nrow(data) > 0, nrow(grid_data) > 0, nrow(label_data) > 0, nrow(base_data) > 0)
      
      # --- Clean & normalize: ensure two parts per id that sum to 1 ---
      plot_df <- data |>
        dplyr::mutate(
          id          = as.integer(id),
          observation = as.character(observation),
          value       = suppressWarnings(as.numeric(value))
        ) |>
        dplyr::filter(is.finite(value)) |>
        dplyr::mutate(value = pmax(0, pmin(1, value))) |>
        dplyr::group_by(id) |>
        dplyr::mutate(
          total = sum(value, na.rm = TRUE),
          # If not already complementary, scale so value1 + value2 = 1
          value = dplyr::if_else(total > 0, value / total,
                                 dplyr::if_else(observation == "value2", 1, 0))
        ) |>
        dplyr::ungroup()
      
      # --- Build stacked rects so wedges sum to 100% under coord_polar ---
      # Draw Absent (value1) first (base), then Present (value2) on top.
      bars <- plot_df |>
        dplyr::mutate(
          draw_order = dplyr::case_when(
            observation == "value1" ~ 1L,  # Absent at the base
            observation == "value2" ~ 2L,  # Present on top
            TRUE ~ 3L
          )
        ) |>
        dplyr::arrange(id, draw_order) |>
        dplyr::group_by(id) |>
        dplyr::mutate(
          frac = value,
          y0   = dplyr::lag(cumsum(frac), default = 0) * 100,
          y1   = cumsum(frac) * 100
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          xmid        = id,
          xmin        = xmid - 0.45,
          xmax        = xmid + 0.45,
          ymin        = y0,
          ymax        = y1,
          tooltip_txt = paste0(
            dplyr::if_else(observation == "value1", "% Absent",
                           dplyr::if_else(observation == "value2", "% Present", observation)
            ),
            ": ", sprintf("%.1f%%", frac * 100)
          ),
          data_id_txt = paste0(id, "_", observation)
        )
      
      # --- Plot ---
      p <- ggplot2::ggplot() +
        ggiraph::geom_rect_interactive(
          data = bars,
          mapping = ggplot2::aes(
            xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
            fill    = observation,
            tooltip = tooltip_txt,
            data_id = data_id_txt
          ),
          alpha = 0.8
        ) +
        ggplot2::scale_fill_manual(
          values = c("value1" = "red", "value2" = "orange"),
          labels = c("value1" = "% Absent", "value2" = "% Present"),
          name   = "Attendance",
          na.translate = FALSE
        ) +
        # grid rings at 0 / 50 / 100
        ggplot2::geom_segment(data = grid_data, ggplot2::aes(x = end, y = 0,   xend = start, yend = 0),
                              colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
        ggplot2::geom_segment(data = grid_data, ggplot2::aes(x = end, y = 50,  xend = start, yend = 50),
                              colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
        ggplot2::geom_segment(data = grid_data, ggplot2::aes(x = end, y = 100, xend = start, yend = 100),
                              colour = "black", linewidth = 0.3, inherit.aes = FALSE) +
        ggplot2::annotate(
          "text",
          x = max(label_data$id, na.rm = TRUE) + 1,
          y = c(0, 50, 100),
          label = c("0%", "50%", "100%"),
          color = "black", size = 4, hjust = 0, fontface = "bold"
        ) +
        ggplot2::ylim(-30, 135) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position   = c(0.09, 0.88),
          legend.background = ggplot2::element_rect(fill = "white", color = NA),
          legend.title      = ggplot2::element_text(face = "bold", size = 10),
          legend.text       = ggplot2::element_text(face = "bold", size = 9),
          plot.margin       = grid::unit(rep(-1, 4), "cm")
        ) +
        ggplot2::coord_polar(start = 0) +
        # OUTER full school names
        ggplot2::geom_text(
          data = label_data,
          mapping = ggplot2::aes(x = id, y = 125, label = school_clean, angle = angle, hjust = hjust),
          inherit.aes = FALSE, color = "black", fontface = "bold", size = 2.7, alpha = 0.9
        ) +
        # INNER red school codes (hover shows full name)
        ggiraph::geom_text_interactive(
          data = base_data,
          mapping = ggplot2::aes(
            x = title, y = 112,
            label   = paste0("Sch ", group),
            tooltip = as.character(Name),
            data_id = as.character(title)
          ),
          inherit.aes = FALSE,
          angle = -0.5, hjust = 0.5,
          color = "red", fontface = "bold", size = 2.8
        )
      
      ggiraph::girafe(
        ggobj = p,
        width_svg  = 9,
        height_svg = 9,
        options = list(
          ggiraph::opts_tooltip(css = "background:#000;color:#fff;padding:6px 8px;border-radius:4px;font-size:12px;"),
          ggiraph::opts_hover(css   = "fill:#d62728;font-weight:700;"),
          ggiraph::opts_sizing(rescale = TRUE),
          ggiraph::opts_toolbar(saveaspng = FALSE),
          ggiraph::opts_zoom(min = 1, max = 1),
          ggiraph::opts_selection(type = "none")
        )
      )
    })
    
  })
  
  # -- LOGIN
  observeEvent(input$login_btn, {
    creds <- read_creds()
    u <- trimws(input$login_user)
    p <- input$login_pw
    valid <- nrow(creds[creds$user==u & creds$password==p, ]) == 1
    if (valid) {
      user_r(u)
      output$login_status <- renderText("")
    } else {
      output$login_status <- renderText("Invalid credentials.")
    }
  })
  
  # -- LOGOUT
  observeEvent(input$logout, { user_r(NULL); updateTextInput(session, "login_user", value=""); updateTextInput(session, "login_pw", value="") })
}

shinyApp(ui, server)
