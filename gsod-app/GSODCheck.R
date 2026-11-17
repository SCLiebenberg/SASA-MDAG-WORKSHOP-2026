# GSOD Data Quality Explorer (workshop)
#
# Downloads NOAA GSOD daily data for one or more stations and shows three
# kinds of data problems:
#   1. Missing values: completeness and runs of consecutive missing days
#      (Missing_score1 and Missing_score2).
#   2. Physical consistency: values outside plausible limits and relations
#      between variables that cannot hold (MIN > MAX, DEWP > TEMP, ...).
#   3. Temporal checks: spikes, abrupt steps and flatlines (repeated values).
#
# Run with shiny::runApp("app.R") or the Run App button in RStudio.

# Package check ---------------------------------------------------------------

pkgs <- c("shiny", "bslib", "bsicons", "GSODR", "leaflet", "DT",
          "dplyr", "plotly")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Install the missing packages, then run the app again:\n  install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))", call. = FALSE)
}
if (utils::packageVersion("bslib") < "0.7.0") {
  stop('This app needs bslib 0.7.0 or newer: install.packages("bslib")', call. = FALSE)
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(GSODR)
  library(leaflet)
  library(DT)
  library(dplyr)
  library(plotly)
})

options(shiny.maxRequestSize = 50 * 1024^2)

# Base R only has %||% from version 4.4
if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x)) y else x

# Settings --------------------------------------------------------------------

# Missing_score2. 
grade_breaks <- c(-Inf, 20, 50, 80, Inf)
grade_labels <- c("Poor", "Fair", "Good", "Excellent")
grade_cols   <- c(Poor = "#C0392B", Fair = "#E67E22",
                  Good = "#F4D03F", Excellent = "#1E8449")

max_stations <- 20

climate_vars <- c(
  "Mean temperature (TEMP)"    = "TEMP",
  "Maximum temperature (MAX)"  = "MAX",
  "Minimum temperature (MIN)"  = "MIN",
  "Dew point (DEWP)"           = "DEWP",
  "Precipitation (PRCP)"       = "PRCP",
  "Mean wind speed (WDSP)"     = "WDSP",
  "Max sustained wind (MXSPD)" = "MXSPD",
  "Sea level pressure (SLP)"   = "SLP",
  "Visibility (VISIB)"         = "VISIB"
)
all_vars <- c("TEMP", "MAX", "MIN", "DEWP", "PRCP", "WDSP", "MXSPD", "GUST",
              "SLP", "VISIB")

# Converts GSOD to metric units
var_units <- c(TEMP = "\u00b0C", MAX = "\u00b0C", MIN = "\u00b0C", DEWP = "\u00b0C",
               PRCP = "mm", WDSP = "m/s", MXSPD = "m/s", GUST = "m/s",
               SLP = "hPa", VISIB = "km")

# Limits for the range check.
range_limits <- list(
  TEMP  = c(-80, 60),   MAX   = c(-80, 60),  MIN = c(-80, 60),
  DEWP  = c(-80, 40),   PRCP  = c(0, 500),
  WDSP  = c(0, 50),     MXSPD = c(0, 75),    GUST = c(0, 100),
  SLP   = c(870, 1090), VISIB = c(0, 161)
)

# Default day-to-day change that counts as a spike or step (0 = check off).
step_defaults <- c(TEMP = 10, MAX = 15, MIN = 15, DEWP = 15, PRCP = 0,
                   WDSP = 10, MXSPD = 15, SLP = 20, VISIB = 0)

rule_levels <- c("Out of range", "MIN > MAX", "TEMP outside [MIN, MAX]",
                 "DEWP > TEMP", "MXSPD < WDSP", "GUST < MXSPD",
                 "Spike", "Step", "Flatline")
rule_cols <- c("Out of range" = "#1B2631", "MIN > MAX" = "#2471A3",
               "TEMP outside [MIN, MAX]" = "#5DADE2", "DEWP > TEMP" = "#148F77",
               "MXSPD < WDSP" = "#76D7C4", "GUST < MXSPD" = "#A9CCE3",
               "Spike" = "#E74C3C", "Step" = "#F39C12", "Flatline" = "#8E44AD")

gap_breaks <- c(0, 1, 7, 30, 90, Inf)
gap_labels <- c("1 day", "2 to 7 days", "8 to 30 days", "31 to 90 days",
                "Over 90 days")
gap_cols   <- setNames(c("#F9E79F", "#F5B041", "#E67E22", "#BA4A00", "#6E2C00"),
                       gap_labels)

# Functions ---------------------------------------------------------------------

# GSODR date formatting
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  out <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  alt <- is.na(out)
  out[alt] <- suppressWarnings(as.Date(x[alt], format = "%Y%m%d"))
  out
}

prepare_stations <- function(isd) {
  isd <- as.data.frame(isd)
  nm  <- names(isd)
  if (!"STNID" %in% nm) isd$STNID <- paste0(isd$USAF, "-", isd$WBAN)
  if (!"NAME" %in% nm)  isd$NAME  <- isd[["STATION NAME"]]
  country <- if ("COUNTRY_NAME" %in% nm) isd$COUNTRY_NAME
             else if ("CTRY" %in% nm) isd$CTRY
             else NA_character_

  data.frame(
    STNID   = isd$STNID,
    NAME    = isd$NAME,
    COUNTRY = country,
    LAT     = suppressWarnings(as.numeric(isd$LAT)),
    LON     = suppressWarnings(as.numeric(isd$LON)),
    BEGIN   = to_date(isd$BEGIN),
    END     = to_date(isd$END),
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(LAT), !is.na(LON), abs(LAT) <= 90, abs(LON) <= 180,
           !(LAT == 0 & LON == 0)) %>%
    distinct(STNID, .keep_all = TRUE) %>%
    mutate(COUNTRY = ifelse(is.na(COUNTRY) | COUNTRY == "", "Unknown", COUNTRY),
           LABEL   = sprintf("%s, %s (%s)", NAME, COUNTRY, STNID))
}

# GSODR ships the station list as a data file (isd_history.rda), not a function.
# Fall back to NOAA's own CSV if file is missing.
load_isd_history <- function() {
  rda <- system.file("extdata", "isd_history.rda", package = "GSODR")
  if (nzchar(rda)) {
    env <- new.env()
    load(rda, envir = env)
    return(env$isd_history)
  }
  read.csv("https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv",
           colClasses = c(USAF = "character", WBAN = "character"),
           check.names = FALSE, stringsAsFactors = FALSE)
}

stations_all <- tryCatch(
  prepare_stations(load_isd_history()),
  error = function(e) stop("Could not load the NOAA station list: ",
                           conditionMessage(e), call. = FALSE)
)

# The list is only as recent as the installed GSODR, so a station
# counts as "still operating" if it reported within 60 days of the list's end.
list_latest_end <- max(stations_all$END, na.rm = TRUE)

# Uploaded raw GSOD files use sentinel codes for missing values.
clean_sentinels <- function(df) {
  codes <- list(TEMP = 9999.9, DEWP = 9999.9, SLP = 9999.9, MAX = 9999.9,
                MIN = 9999.9, VISIB = 999.9, WDSP = 999.9, MXSPD = 999.9,
                GUST = 999.9, PRCP = 99.99)
  for (v in intersect(names(codes), names(df))) {
    x <- suppressWarnings(as.numeric(df[[v]]))
    x[!is.na(x) & abs(x - codes[[v]]) < 1e-6] <- NA
    df[[v]] <- x
  }
  df
}

# One row per calendar day in the period; days absent from GSOD become NA.
daily_frame <- function(gsod, dates) {
  out <- data.frame(Date = dates)
  ok  <- nrow(gsod) > 0 && "YEARMODA" %in% names(gsod)
  idx <- if (ok) match(dates, to_date(gsod$YEARMODA)) else rep(NA_integer_, length(dates))
  for (v in all_vars) {
    out[[v]] <- if (ok && v %in% names(gsod)) {
      suppressWarnings(as.numeric(gsod[[v]]))[idx]
    } else {
      NA_real_
    }
  }
  out
}

first_value <- function(gsod, col) {
  if (col %in% names(gsod) && nrow(gsod) > 0) gsod[[col]][1] else NA
}

# Missing-value score -----------------------------------------------------------
# Missing_score1 = 100 * (1 - missing days / days in period)
# Missing_score2 = Missing_score1 / mean length of runs of consecutive missing
#                  days (equals Missing_score1 when there are no gaps).
# A day is missing if GSOD has no row for it OR the variable is NA that day.
score_missing <- function(x, dates, id) {
  present   <- !is.na(x)
  runs      <- rle(!present)
  run_end   <- cumsum(runs$lengths)
  run_start <- run_end - runs$lengths + 1
  is_gap    <- runs$values
  gap_len   <- runs$lengths[is_gap]

  gaps <- data.frame(
    STNID  = rep(id, length(gap_len)),
    start  = dates[run_start[is_gap]],
    end    = dates[run_end[is_gap]],
    length = gap_len
  )

  n_days    <- length(dates)
  n_missing <- sum(!present)
  score1    <- 100 * (1 - n_missing / n_days)
  mean_gap  <- if (length(gap_len)) mean(gap_len) else 0

  row <- data.frame(
    no_days        = n_days,
    no_missing     = n_missing,
    no_gaps        = length(gap_len),
    mean_gap       = mean_gap,
    max_gap        = if (length(gap_len)) max(gap_len) else 0L,
    Missing_score1 = score1,
    Missing_score2 = if (length(gap_len)) score1 / mean_gap else score1
  )
  list(row = row, gaps = gaps)
}

# Outlier checks ----------------------------------------------------------------

empty_flags <- function() {
  data.frame(date = as.Date(character()), end = as.Date(character()),
             check = character(), rule = character(), vars = character(),
             detail = character(), stringsAsFactors = FALSE)
}

make_flags <- function(d, mask, check, rule, vars, detail) {
  mask[is.na(mask)] <- FALSE
  if (!any(mask)) return(empty_flags())
  data.frame(date = d$Date[mask], end = d$Date[mask], check = check,
             rule = rule, vars = vars, detail = detail[mask],
             stringsAsFactors = FALSE)
}

# Station-level checks on all variables; independent of the chosen variable.
# tol is in degrees C for the temperature relations.
check_physical <- function(d, tol = 0.5, wind_tol = 0.1) {
  out <- list()
  for (v in names(range_limits)) {
    lim <- range_limits[[v]]
    x   <- d[[v]]
    out[[length(out) + 1]] <- make_flags(
      d, x < lim[1] | x > lim[2], "Physical", "Out of range", v,
      sprintf("%s = %.1f %s, outside [%g, %g]", v, x, var_units[v], lim[1], lim[2]))
  }
  out[[length(out) + 1]] <- make_flags(
    d, d$MIN > d$MAX, "Physical", "MIN > MAX", "MIN,MAX",
    sprintf("MIN %.1f > MAX %.1f", d$MIN, d$MAX))
  out[[length(out) + 1]] <- make_flags(
    d, d$TEMP > d$MAX + tol | d$TEMP < d$MIN - tol, "Physical",
    "TEMP outside [MIN, MAX]", "TEMP,MAX,MIN",
    sprintf("TEMP %.1f, MIN %.1f, MAX %.1f", d$TEMP, d$MIN, d$MAX))
  out[[length(out) + 1]] <- make_flags(
    d, d$DEWP > d$TEMP + tol, "Physical", "DEWP > TEMP", "DEWP,TEMP",
    sprintf("DEWP %.1f > TEMP %.1f", d$DEWP, d$TEMP))
  out[[length(out) + 1]] <- make_flags(
    d, d$MXSPD < d$WDSP - wind_tol, "Physical", "MXSPD < WDSP", "MXSPD,WDSP",
    sprintf("MXSPD %.1f < WDSP %.1f", d$MXSPD, d$WDSP))
  out[[length(out) + 1]] <- make_flags(
    d, d$GUST < d$MXSPD - wind_tol, "Physical", "GUST < MXSPD", "GUST,MXSPD",
    sprintf("GUST %.1f < MXSPD %.1f", d$GUST, d$MXSPD))
  bind_rows(empty_flags(), out)
}

# Checks on the chosen variable only.
#   Spike: differs from both neighbours by more than step_thr, in the same
#          direction (up and back down, or down and back up).
#   Step:  a single day-to-day change larger than step_thr that is not part
#          of a spike.
#   Flatline: the same value on flat_k or more consecutive days.
check_temporal <- function(d, v, step_thr = 10, flat_k = 5, ignore_zero = TRUE) {
  x   <- d[[v]]
  n   <- length(x)
  out <- list(empty_flags())
  if (n < 2) return(out[[1]])

  if (isTRUE(step_thr > 0)) {
    prev <- c(NA, x[-n])
    nxt  <- c(x[-1], NA)
    dp   <- x - prev
    dn   <- x - nxt
    spike <- abs(dp) > step_thr & abs(dn) > step_thr & sign(dp) == sign(dn)
    spike[is.na(spike)] <- FALSE
    step  <- abs(dp) > step_thr
    step[is.na(step)] <- FALSE
    step  <- step & !spike & !c(FALSE, spike[-n])
    out[[length(out) + 1]] <- make_flags(
      d, spike, "Temporal", "Spike", v,
      sprintf("%s %.1f; day before %.1f, day after %.1f", v, x, prev, nxt))
    out[[length(out) + 1]] <- make_flags(
      d, step, "Temporal", "Step", v,
      sprintf("%s changed by %+.1f %s (from %.1f to %.1f)",
              v, dp, var_units[v], prev, x))
  }

  if (isTRUE(flat_k >= 2)) {
    r    <- rle(x)                       # NA values break runs
    ends <- cumsum(r$lengths)
    strt <- ends - r$lengths + 1
    keep <- !is.na(r$values) & r$lengths >= flat_k
    if (ignore_zero) keep <- keep & r$values != 0
    if (any(keep)) {
      out[[length(out) + 1]] <- data.frame(
        date = d$Date[strt[keep]], end = d$Date[ends[keep]],
        check = "Temporal", rule = "Flatline", vars = v,
        detail = sprintf("%s = %.1f on %d consecutive days", v,
                         r$values[keep], r$lengths[keep]),
        stringsAsFactors = FALSE)
    }
  }
  bind_rows(out)
}

# UI ----------------------------------------------------------------------------

last_year <- as.integer(format(Sys.Date(), "%Y")) - 1

learn_tab <- div(
  class = "p-2", style = "max-width: 900px; overflow-y: auto;",
  h4("What this app shows"),
  p("GSOD (Global Surface Summary of the Day) contains daily summaries that NOAA's ",
    "National Centers for Environmental Information derive from hourly synoptic ",
    "reports at thousands of stations worldwide. The GSODR package downloads the ",
    "files, converts them to metric units and replaces GSOD's missing-value codes ",
    "with NA. The app looks at three kinds of problems in these series."),

  h4("1. Missing values"),
  p("A GSOD file has no row at all for a day without reports, so missing days are ",
    "found by comparing the data with the full calendar of the chosen period. A day ",
    "is also missing for a variable when its row exists but that variable is NA."),
  tags$ul(
    tags$li(HTML("<b>Missing_score1</b> = 100 &times; (1 &minus; <i>m</i>/<i>n</i>), ",
                 "where <i>m</i> is the number of missing days and <i>n</i> the number ",
                 "of days in the period.")),
    tags$li(HTML("<b>Missing_score2</b> = Missing_score1 / <i>g</i>, where <i>g</i> is ",
                 "the mean length of the runs of consecutive missing days. With no ",
                 "missing days, Missing_score2 = Missing_score1."))
  ),
  p(HTML("Example for one year (<i>n</i> = 365) with <i>m</i> = 10 missing days. ",
         "Ten isolated days give Missing_score1 = 97.3 and Missing_score2 = 97.3. ",
         "One block of ten days gives the same Missing_score1 but Missing_score2 = 9.7. ",
         "Isolated gaps are easy to infill; a long outage is not, and the second ",
         "score reflects this.")),
  p(HTML(sprintf("Grades on Missing_score2: Excellent &ge; %g, Good %g to %g, Fair %g to %g, Poor &lt; %g.",
                 grade_breaks[4], grade_breaks[3], grade_breaks[4],
                 grade_breaks[2], grade_breaks[3], grade_breaks[2]))),
  p("Where to look: Overview (scores), Missing values (timeline and gap lengths), ",
    "Quality map."),

  h4("2. Physical consistency"),
  p("These checks run on every variable of each station, whichever variable is ",
    "selected."),
  tags$ul(
    tags$li(HTML("<b>Out of range</b>: a value outside broad plausible limits ",
                 "(for example TEMP outside &minus;80 to 60 &deg;C, SLP outside 870 to ",
                 "1090 hPa, negative precipitation or wind speed).")),
    tags$li(HTML("<b>MIN &gt; MAX</b>: the daily minimum exceeds the daily maximum.")),
    tags$li(HTML("<b>TEMP outside [MIN, MAX]</b>: the daily mean lies outside the ",
                 "daily extremes by more than the tolerance. GSOD sometimes derives MAX ",
                 "and MIN from the hourly reports (marked with * in the attributes), and ",
                 "their observation window need not match the calendar day used for ",
                 "TEMP, so small violations can be artefacts of the processing rather ",
                 "than bad readings. This is why the tolerance exists.")),
    tags$li(HTML("<b>DEWP &gt; TEMP</b>: dew point above air temperature, which ",
                 "implies relative humidity above 100%.")),
    tags$li(HTML("<b>MXSPD &lt; WDSP</b> and <b>GUST &lt; MXSPD</b>: the maximum ",
                 "sustained wind below the mean wind, or a gust below the maximum ",
                 "sustained wind."))
  ),

  h4("3. Temporal checks"),
  p("These run on the selected variable, using consecutive calendar days only."),
  tags$ul(
    tags$li(HTML("<b>Spike</b>: the value differs from both the previous and the ",
                 "next day by more than the threshold, in the same direction. A single ",
                 "wrong reading (a decimal slip or a sign error) usually looks like this.")),
    tags$li(HTML("<b>Step</b>: one day-to-day change larger than the threshold that ",
                 "does not return. Real weather (a cold front) can produce these, so a ",
                 "step is a prompt to look, not proof of an error.")),
    tags$li(HTML("<b>Flatline</b>: the same value on at least the chosen number of ",
                 "consecutive days, which points to a stuck sensor or values copied ",
                 "forward. Runs of zero precipitation are ignored by default because ",
                 "dry spells are normal."))
  ),
  p("Thresholds are in the sidebar under Check settings. Changing them re-runs the ",
    "checks immediately without downloading the data again."),

  h4("Points worth raising"),
  tags$ul(
    tags$li(HTML("Precipitation: GSOD marks some zero totals with the attribute I, ",
                 "meaning the station reported no precipitation data that day. A zero ",
                 "can therefore mean either a dry day or no report.")),
    tags$li("Visibility and wind speed are often reported in coarse steps, so ",
            "flatlines in VISIB or WDSP are common and not always errors."),
    tags$li("A flag marks a value for inspection. Deciding whether it is an error ",
            "needs context: nearby stations, station history, the weather that day.")
  )
)

ui <- page_sidebar(
  title = tags$span(bs_icon("cloud-sun"), " GSOD Data Quality Explorer"),
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1F4E79"),
  fillable = TRUE,

  sidebar = sidebar(
    width = 340,
    radioButtons("source", "Data source",
                 c("Download from NOAA" = "noaa", "Upload GSOD CSVs" = "upload"),
                 inline = TRUE),
    dateRangeInput("daterange", "Period",
                   start = as.Date(sprintf("%d-01-01", last_year)),
                   end   = as.Date(sprintf("%d-12-31", last_year)),
                   max   = Sys.Date()),
    selectInput("variable", "Variable", climate_vars, selected = "TEMP"),

    conditionalPanel(
      "input.source == 'noaa'",
      selectizeInput("country", "Filter map by country", choices = NULL,
                     multiple = TRUE,
                     options = list(placeholder = "All countries")),
      checkboxInput("active_only",
                    "Only stations operating across the whole period", TRUE),
      selectizeInput("stations", "Stations (search here or click the map)",
                     choices = NULL, multiple = TRUE,
                     options = list(maxItems = max_stations,
                                    placeholder = "Name, country or ID")),
      actionLink("clear", "Clear selection", icon = icon("xmark"))
    ),
    conditionalPanel(
      "input.source == 'upload'",
      fileInput("upload", NULL, accept = ".csv", multiple = TRUE,
                buttonLabel = "Browse", placeholder = "Needs STNID and YEARMODA")
    ),

    input_task_button("run", "Download and check", icon = bs_icon("play-fill")),

    accordion(
      open = FALSE,
      accordion_panel(
        "Check settings", icon = bs_icon("sliders"),
        numericInput("step_thr", "Spike/step threshold (0 = off)",
                     value = step_defaults[["TEMP"]], min = 0, step = 1),
        numericInput("flat_k", "Flatline: minimum run of equal values (days)",
                     value = 5, min = 2, step = 1),
        checkboxInput("ignore_zero", "Ignore runs of zero (dry spells, calm days)", TRUE),
        numericInput("tol", "Tolerance for TEMP/DEWP vs MIN/MAX (\u00b0C)",
                     value = 0.5, min = 0, step = 0.1),
        helpText("Changes apply immediately; the data are not downloaded again.")
      )
    ),
    tags$small(class = "text-muted",
               "Data: NOAA NCEI GSOD, downloaded with the GSODR package.")
  ),

  navset_card_underline(
    id = "tabs",
    nav_panel("Select stations", icon = bs_icon("geo-alt"),
              leafletOutput("station_map", height = "100%")),
    nav_panel("Overview", icon = bs_icon("speedometer2"),
              uiOutput("value_boxes"),
              layout_columns(
                col_widths = c(6, 6),
                card(full_screen = TRUE,
                     card_header("Missing values: adjusted score (Missing_score2)"),
                     plotlyOutput("score_plot")),
                card(full_screen = TRUE,
                     card_header("Flagged values by check"),
                     plotlyOutput("flag_plot"))
              )),
    nav_panel("Station explorer", icon = bs_icon("graph-up"),
              div(class = "d-flex flex-wrap gap-4 align-items-end",
                  selectInput("explore_station", "Station", choices = NULL,
                              width = "380px"),
                  uiOutput("explorer_note")),
              plotlyOutput("explorer_plot", height = "100%")),
    nav_menu("Missing values", icon = bs_icon("calendar-x"),
             nav_panel("Gap timeline", icon = bs_icon("calendar-range"),
                       plotlyOutput("gap_timeline", height = "100%")),
             nav_panel("Gap lengths", icon = bs_icon("bar-chart-steps"),
                       plotlyOutput("gap_dist", height = "100%"))),
    nav_panel("Flags", icon = bs_icon("flag"),
              DTOutput("flag_table")),
    nav_panel("Quality map", icon = bs_icon("map"),
              leafletOutput("quality_map", height = "100%")),
    nav_panel("Table & downloads", icon = bs_icon("table"),
              uiOutput("download_buttons"),
              DTOutput("score_table")),
    nav_panel("About", icon = bs_icon("mortarboard"), learn_tab)
  )
)

# Server ------------------------------------------------------------------------

# Downloads are cached for the entirety of the R process, so re-running with the
# same stations and years (or switching variables) does not hit NOAA again.
dl_cache <- new.env()

server <- function(input, output, session) {

  updateSelectizeInput(session, "country",
                       choices = sort(unique(stations_all$COUNTRY)), server = TRUE)

  observeEvent(input$variable, {
    updateNumericInput(session, "step_thr", value = step_defaults[[input$variable]])
    updateCheckboxInput(session, "ignore_zero",
                        value = input$variable %in% c("PRCP", "WDSP", "MXSPD"))
  })

  visible_stations <- reactive({
    s <- stations_all
    if (length(input$country)) s <- s[s$COUNTRY %in% input$country, ]
    if (isTRUE(input$active_only)) {
      need_end <- min(input$daterange[2], list_latest_end - 60)
      s <- s[!is.na(s$BEGIN) & !is.na(s$END) &
               s$BEGIN <= input$daterange[1] & s$END >= need_end, ]
    }
    s
  })

  # Keep already-selected stations available even if the filter hides them
  station_choices <- function(keep = character()) {
    s <- bind_rows(visible_stations(), stations_all[stations_all$STNID %in% keep, ]) %>%
      distinct(STNID, .keep_all = TRUE)
    setNames(s$STNID, s$LABEL)
  }

  set_selection <- function(ids) {
    updateSelectizeInput(session, "stations", choices = station_choices(ids),
                         selected = ids, server = TRUE)
  }

  observe({
    set_selection(isolate(input$stations) %||% character())
  })

  # Station map -----------------------------------------------------------------
  output$station_map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE, worldCopyJump = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") %>%
      addTiles(group = "Streets") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(baseGroups = c("Light", "Streets", "Satellite")) %>%
      setView(lng = 20, lat = 10, zoom = 2)
  })

  # Proxy calls sent before the map exists are lost, so wait for it
  map_ready <- reactiveVal(FALSE)
  observeEvent(input$station_map_bounds, map_ready(TRUE), once = TRUE)

  observe({
    req(map_ready())
    s <- visible_stations()
    proxy <- leafletProxy("station_map") %>% clearGroup("stations")
    if (nrow(s)) {
      proxy %>% addCircleMarkers(
        data = s, lng = ~LON, lat = ~LAT, layerId = ~STNID, group = "stations",
        radius = 5, stroke = FALSE, fillColor = "#2E86C1", fillOpacity = 0.75,
        label = ~LABEL,
        clusterOptions = markerClusterOptions(disableClusteringAtZoom = 7)
      )
    }
  })

  observe({
    req(map_ready())
    sel <- stations_all[stations_all$STNID %in% input$stations, ]
    proxy <- leafletProxy("station_map") %>% clearGroup("selected")
    if (nrow(sel)) {
      proxy %>% addCircleMarkers(
        data = sel, lng = ~LON, lat = ~LAT, layerId = ~paste0("sel_", STNID),
        group = "selected", radius = 9, color = "#922B21", weight = 3,
        fillColor = "#E74C3C", fillOpacity = 0.9, label = ~LABEL
      )
    }
  })

  observeEvent(input$station_map_marker_click, {
    id <- sub("^sel_", "", input$station_map_marker_click$id %||% "")
    if (!nzchar(id)) return()
    cur <- input$stations %||% character()
    new <- if (id %in% cur) setdiff(cur, id) else c(cur, id)
    if (length(new) > max_stations) {
      showNotification(sprintf("At most %d stations.", max_stations), type = "warning")
      return()
    }
    set_selection(new)
  })

  observeEvent(input$clear, set_selection(character()))

  # Step 1: download (only when the button is pressed) --------------------------
  raw_data <- eventReactive(input$run, {
    rng <- input$daterange
    validate(need(!any(is.na(rng)) && rng[1] <= rng[2],
                  "The start date must precede the end date."))
    dates <- seq(rng[1], rng[2], by = "day")

    if (input$source == "upload") {
      validate(need(!is.null(input$upload), "Upload at least one GSOD CSV file."))
      data_list <- list()
      for (i in seq_len(nrow(input$upload))) {
        df <- read.csv(input$upload$datapath[i], stringsAsFactors = FALSE)
        if (!all(c("STNID", "YEARMODA") %in% names(df))) {
          showNotification(sprintf("%s skipped: needs STNID and YEARMODA.",
                                   input$upload$name[i]), type = "warning")
          next
        }
        df <- clean_sentinels(df)
        for (part in split(df, as.character(df$STNID))) {
          id <- as.character(part$STNID[1])
          data_list[[id]] <- if (is.null(data_list[[id]])) part
                             else bind_rows(data_list[[id]], part)
        }
      }
    } else {
      ids <- input$stations
      validate(need(length(ids) > 0,
                    "Select at least one station on the map or in the search box."))
      years <- seq(as.integer(format(rng[1], "%Y")), as.integer(format(rng[2], "%Y")))
      data_list <- list()
      withProgress(message = "Downloading GSOD data", value = 0, {
        for (id in ids) {
          incProgress(1 / length(ids), detail = id)
          key <- paste(id, min(years), max(years))
          if (is.null(dl_cache[[key]])) {
            dl_cache[[key]] <- tryCatch(
              as.data.frame(get_GSOD(years = years, station = id)),
              error = function(e) {
                showNotification(sprintf("%s skipped: %s", id, conditionMessage(e)),
                                 type = "warning", duration = 8)
                NULL
              })
          }
          if (!is.null(dl_cache[[key]])) data_list[[id]] <- dl_cache[[key]]
        }
      })
    }

    validate(need(length(data_list) > 0, "No data could be loaded."))

    list(
      gsod  = data_list,
      daily = lapply(data_list, daily_frame, dates = dates),
      name  = vapply(data_list, function(g) as.character(first_value(g, "NAME")), ""),
      lat   = vapply(data_list, function(g) suppressWarnings(as.numeric(first_value(g, "LATITUDE"))), 0),
      lon   = vapply(data_list, function(g) suppressWarnings(as.numeric(first_value(g, "LONGITUDE"))), 0),
      range = rng,
      dates = dates
    )
  })

  observeEvent(raw_data(), nav_select("tabs", "Overview"))

  # Step 2: score and check (re-runs when variable or settings change) ----------
  settings <- debounce(reactive(list(
    step   = input$step_thr %||% 0,
    flat   = input$flat_k %||% 5,
    zero   = isTRUE(input$ignore_zero),
    tol    = input$tol %||% 0.5
  )), 600)

  results <- reactive({
    raw <- raw_data()
    s   <- settings()
    v   <- input$variable
    ids <- names(raw$daily)

    parts <- lapply(ids, function(id) {
      d  <- raw$daily[[id]]
      m  <- score_missing(d[[v]], d$Date, id)
      fl <- bind_rows(check_physical(d, tol = s$tol),
                      check_temporal(d, v, step_thr = s$step, flat_k = s$flat,
                                     ignore_zero = s$zero))
      fl$STNID <- rep(id, nrow(fl))
      list(summary = cbind(data.frame(STNID = id, NAME = raw$name[[id]],
                                      LAT_file = raw$lat[[id]],
                                      LON_file = raw$lon[[id]],
                                      variable = v, from = raw$range[1],
                                      to = raw$range[2],
                                      stringsAsFactors = FALSE),
                           m$row),
           gaps = m$gaps, flags = fl)
    })

    flags <- bind_rows(lapply(parts, `[[`, "flags"))
    counts <- flags %>%
      group_by(STNID) %>%
      summarise(n_physical  = sum(check == "Physical"),
                n_spike     = sum(rule == "Spike"),
                n_step      = sum(rule == "Step"),
                n_flatline  = sum(rule == "Flatline"),
                flat_days   = sum(as.integer(end - date + 1)[rule == "Flatline"]),
                .groups = "drop")

    summary <- bind_rows(lapply(parts, `[[`, "summary")) %>%
      left_join(stations_all %>% select(STNID, COUNTRY, LAT, LON, ISD_NAME = NAME),
                by = "STNID") %>%
      left_join(counts, by = "STNID") %>%
      mutate(across(c(n_physical, n_spike, n_step, n_flatline, flat_days),
                    ~ coalesce(as.integer(.x), 0L)),
             n_flags = n_physical + n_spike + n_step + n_flatline,
             NAME  = coalesce(NAME, ISD_NAME, STNID),
             LAT   = coalesce(LAT, LAT_file),
             LON   = coalesce(LON, LON_file),
             lab   = paste0(NAME, " (", STNID, ")"),
             grade = cut(Missing_score2, grade_breaks, grade_labels, right = FALSE)) %>%
      select(-ISD_NAME, -LAT_file, -LON_file) %>%
      arrange(desc(Missing_score2))

    gaps <- bind_rows(lapply(parts, `[[`, "gaps")) %>%
      left_join(summary %>% select(STNID, lab), by = "STNID")

    flags <- flags %>%
      left_join(summary %>% select(STNID, lab), by = "STNID") %>%
      mutate(rule = factor(rule, levels = rule_levels)) %>%
      arrange(lab, date)

    list(summary = summary, gaps = gaps, flags = flags, daily = raw$daily,
         variable = v, range = raw$range)
  })

  observeEvent(results(), {
    s   <- results()$summary
    cur <- isolate(input$explore_station)
    updateSelectInput(session, "explore_station",
                      choices = setNames(s$STNID, s$lab),
                      selected = if (isTRUE(cur %in% s$STNID)) cur else s$STNID[1])
  })

  var_label <- function(v) sprintf("%s (%s)", v, var_units[[v]])

  # Overview --------------------------------------------------------------------
  output$value_boxes <- renderUI({
    if (!isTruthy(input$run)) {
      return(div(class = "text-muted p-3",
                 "Choose stations and a period, then press Download and check."))
    }
    r <- results()
    s <- r$summary
    worst <- s[which.max(s$max_gap), ]
    layout_column_wrap(
      width = 1/4, fill = FALSE,
      value_box("Stations", nrow(s), showcase = bs_icon("broadcast-pin"),
                theme = "primary",
                p(names(climate_vars)[climate_vars == r$variable])),
      value_box("Median completeness", sprintf("%.1f%%", median(s$Missing_score1)),
                showcase = bs_icon("pie-chart"), theme = "teal",
                p(sprintf("%s to %s", r$range[1], r$range[2]))),
      value_box("Longest gap", paste(format(worst$max_gap), "days"),
                showcase = bs_icon("calendar-x"), theme = "danger", p(worst$NAME)),
      value_box("Flagged values", sum(s$n_flags), showcase = bs_icon("flag"),
                theme = "warning",
                p(sprintf("%d physical, %d temporal", sum(s$n_physical),
                          sum(s$n_flags) - sum(s$n_physical))))
    )
  })

  output$score_plot <- renderPlotly({
    s <- results()$summary %>%
      mutate(hover = sprintf(
        "%s<br>Completeness: %.1f%%<br>Gaps: %d (mean %.1f days, longest %s)<br>Adjusted score: %.1f",
        lab, Missing_score1, no_gaps, mean_gap, format(max_gap), Missing_score2))
    plot_ly(s, x = ~Missing_score2, y = ~lab, color = ~grade, colors = grade_cols,
            type = "bar", orientation = "h", hovertext = ~hover, hoverinfo = "text") %>%
      layout(xaxis = list(title = "Missing_score2", range = c(0, 100)),
             yaxis = list(title = "", categoryorder = "array",
                          categoryarray = rev(s$lab)),
             legend = list(orientation = "h", y = -0.15)) %>%
      config(displaylogo = FALSE)
  })

  output$flag_plot <- renderPlotly({
    r <- results()
    validate(need(nrow(r$flags) > 0, "No values flagged with the current settings."))
    cnt <- r$flags %>% count(lab, rule) %>% mutate(rule = droplevels(rule))
    plot_ly(cnt, x = ~n, y = ~lab, color = ~rule,
            colors = rule_cols[levels(cnt$rule)], type = "bar", orientation = "h",
            hovertext = ~sprintf("%s<br>%s: %d", lab, rule, n), hoverinfo = "text") %>%
      layout(barmode = "stack",
             xaxis = list(title = "Number of flags"),
             yaxis = list(title = "", categoryorder = "array",
                          categoryarray = rev(r$summary$lab)),
             legend = list(orientation = "h", y = -0.15)) %>%
      config(displaylogo = FALSE)
  })

  # Station explorer --------------------------------------------------------------
  output$explorer_note <- renderUI({
    r  <- results()
    id <- input$explore_station
    req(id %in% r$summary$STNID)
    s <- r$summary[r$summary$STNID == id, ]
    div(class = "pb-3 text-muted",
        sprintf("Completeness %.1f%% | %d gaps, longest %s days | %d flags. ",
                s$Missing_score1, s$no_gaps, format(s$max_gap), s$n_flags),
        "Grey bands are missing days; markers are flagged values.")
  })

  output$explorer_plot <- renderPlotly({
    r  <- results()
    id <- input$explore_station
    req(id %in% names(r$daily))
    v <- r$variable
    d <- r$daily[[id]]

    # Context series make the physical checks visible on the chart
    ctx <- switch(v,
                  TEMP = c("MAX", "MIN"), MAX = c("TEMP", "MIN"), MIN = c("TEMP", "MAX"),
                  DEWP = "TEMP", WDSP = c("MXSPD", "GUST"), MXSPD = c("WDSP", "GUST"),
                  character())
    ctx <- ctx[vapply(ctx, function(cv) any(!is.na(d[[cv]])), logical(1))]
    ctx_cols <- c(TEMP = "#566573", MAX = "#E59866", MIN = "#7FB3D5",
                  WDSP = "#566573", MXSPD = "#E59866", GUST = "#BB8FCE")

    g <- r$gaps[r$gaps$STNID == id, ]
    shapes <- lapply(seq_len(nrow(g)), function(i) list(
      type = "rect", xref = "x", yref = "paper", layer = "below",
      x0 = g$start[i], x1 = g$end[i] + 1, y0 = 0, y1 = 1,
      fillcolor = "rgba(127, 140, 141, 0.25)", line = list(width = 0)))

    p <- plot_ly()
    for (cv in ctx) {
      p <- p %>% add_lines(x = d$Date, y = d[[cv]], name = cv,
                           line = list(width = 1, color = ctx_cols[[cv]]),
                           opacity = 0.7, hoverinfo = "x+y+name")
    }
    p <- p %>% add_lines(x = d$Date, y = d[[v]], name = v,
                         line = list(width = 1.8, color = "#1F4E79"),
                         hoverinfo = "x+y+name")

    # Flags that involve the plotted variable, drawn at its value that day
    f <- r$flags[r$flags$STNID == id &
                   vapply(strsplit(r$flags$vars, ","), function(z) v %in% z, logical(1)), ]
    if (nrow(f)) {
      f <- bind_rows(lapply(seq_len(nrow(f)), function(i) data.frame(
        Date = seq(f$date[i], f$end[i], by = "day"),
        rule = as.character(f$rule[i]), detail = f$detail[i],
        stringsAsFactors = FALSE)))
      f$y <- d[[v]][match(f$Date, d$Date)]
      for (ru in intersect(rule_levels, unique(f$rule))) {
        fr <- f[f$rule == ru, ]
        p <- p %>% add_markers(
          x = fr$Date, y = fr$y, name = ru,
          marker = list(size = 9, color = rule_cols[[ru]],
                        line = list(color = "white", width = 1)),
          hovertext = sprintf("%s<br>%s<br>%s", fr$Date, ru, fr$detail),
          hoverinfo = "text")
      }
    }

    p %>%
      layout(shapes = shapes,
             xaxis = list(title = "", rangeslider = list(visible = TRUE)),
             yaxis = list(title = var_label(v)),
             legend = list(orientation = "h", y = 1.08),
             hovermode = "closest") %>%
      config(displaylogo = FALSE)
  })

  # Missing values: gap timeline ----------------------------------------------------
  output$gap_timeline <- renderPlotly({
    r <- results()
    s <- r$summary
    g <- r$gaps
    validate(need(nrow(g) > 0, "No gaps: every station is complete for this period."))

    # Horizontal bars on a date axis: base = start date, length in milliseconds
    day_ms <- 86400000
    bg <- data.frame(lab = s$lab, start = as.character(r$range[1]),
                     dur = as.numeric(r$range[2] - r$range[1] + 1) * day_ms)
    g <- g %>%
      mutate(size  = factor(cut(length, gap_breaks, gap_labels), levels = gap_labels),
             base  = as.character(start),
             dur   = length * day_ms,
             hover = sprintf("%s<br>%s to %s<br>%d day(s)", lab, start, end, length))

    p <- plot_ly() %>%
      add_bars(data = bg, y = ~lab, x = ~dur, base = ~start, orientation = "h",
               name = "Data present", marker = list(color = "#D5F5E3"),
               hoverinfo = "skip")
    for (b in gap_labels) {
      gb <- g[g$size == b, ]
      if (nrow(gb)) {
        p <- p %>% add_bars(data = gb, y = ~lab, x = ~dur, base = ~base,
                            orientation = "h", name = b,
                            marker = list(color = gap_cols[[b]]),
                            hovertext = ~hover, hoverinfo = "text")
      }
    }
    p %>%
      layout(barmode = "overlay", bargap = 0.35,
             title = list(text = sprintf("%s: green = data present, coloured blocks = runs of missing days",
                                         r$variable),
                          x = 0, font = list(size = 14)),
             xaxis = list(type = "date", title = ""),
             yaxis = list(title = "", categoryorder = "array",
                          categoryarray = rev(s$lab)),
             legend = list(orientation = "h", y = -0.12, title = list(text = "Gap length  "))) %>%
      config(displaylogo = FALSE)
  })

  # Missing values: gap length distribution -------------------------------------------
  output$gap_dist <- renderPlotly({
    r <- results()
    g <- r$gaps
    validate(need(nrow(g) > 0, "No gaps: every station is complete for this period."))
    g <- g %>% mutate(hover = sprintf("%s<br>%s to %s<br>%d day(s)", lab, start, end, length))
    plot_ly(g, x = ~length, y = ~lab, type = "box", orientation = "h",
            boxpoints = "all", jitter = 0.4, pointpos = 0,
            marker = list(color = "#922B21", opacity = 0.6),
            line = list(color = "#566573"), fillcolor = "rgba(0,0,0,0)",
            hovertext = ~hover, hoverinfo = "text") %>%
      layout(xaxis = list(type = "log", title = "Gap length (days, log scale)"),
             yaxis = list(title = "", categoryorder = "array",
                          categoryarray = rev(r$summary$lab)),
             showlegend = FALSE) %>%
      config(displaylogo = FALSE)
  })

  # Flags table -------------------------------------------------------------------
  output$flag_table <- renderDT({
    f <- results()$flags
    validate(need(nrow(f) > 0, "No values flagged with the current settings."))
    f <- f %>% transmute(Station = lab, From = date, To = end,
                         Days = as.integer(end - date + 1), Check = check,
                         Rule = as.character(rule), Detail = detail)
    datatable(f, rownames = FALSE, filter = "top", extensions = "Buttons",
              options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"),
                             pageLength = 20, scrollX = TRUE)) %>%
      formatStyle("Rule", color = styleEqual(names(rule_cols), unname(rule_cols)),
                  fontWeight = "bold")
  }, server = FALSE)

  # Quality map ---------------------------------------------------------------------
  output$quality_map <- renderLeaflet({
    s <- results()$summary %>% filter(!is.na(LAT), !is.na(LON))
    validate(need(nrow(s) > 0, "No coordinates available for the scored stations."))
    pal <- colorFactor(unname(grade_cols), levels = grade_labels)
    leaflet(s) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircleMarkers(
        lng = ~LON, lat = ~LAT, radius = ~6 + Missing_score1 / 10,
        color = "white", weight = 1.5, fillColor = ~pal(grade), fillOpacity = 0.9,
        label = ~NAME,
        popup = ~sprintf(paste0("<b>%s</b><br>%s<br>Completeness: %.1f%%<br>",
                                "Gaps: %d (mean %.1f, longest %s days)<br>",
                                "Flags: %d physical, %d temporal<br>",
                                "<b>Adjusted score: %.1f (%s)</b>"),
                         NAME, STNID, Missing_score1, no_gaps, mean_gap,
                         format(max_gap), n_physical, n_flags - n_physical,
                         Missing_score2, as.character(grade))
      ) %>%
      addLegend("bottomright", pal = pal, values = ~grade, title = "Grade", opacity = 1)
  })

  # Downloads -------------------------------------------------------------------------
  output$download_buttons <- renderUI({
    req(raw_data())
    div(class = "d-flex flex-wrap gap-2 pb-3",
        downloadButton("dl_gsod", "GSOD data as downloaded (CSV)",
                       class = "btn-sm btn-outline-primary"),
        downloadButton("dl_daily", "Daily series with missing days as NA (CSV)",
                       class = "btn-sm btn-outline-primary"))
  })

  period_tag <- function(raw) paste(format(raw$range, "%Y%m%d"), collapse = "_")

  # All GSODR columns, one row per reported day, limited to the chosen period
  output$dl_gsod <- downloadHandler(
    filename = function() paste0("GSOD_", period_tag(raw_data()), ".csv"),
    content = function(file) {
      raw <- raw_data()
      out <- tryCatch(
        bind_rows(raw$gsod),
        # Uploaded files may disagree on column types; fall back to text
        error = function(e) bind_rows(lapply(raw$gsod, function(df) {
          df[] <- lapply(df, as.character)
          df
        })))
      day <- to_date(out$YEARMODA)
      out <- out[!is.na(day) & day >= raw$range[1] & day <= raw$range[2], ]
      write.csv(out, file, row.names = FALSE, na = "")
    })

  # One row per station and calendar day, so the missing days are visible
  output$dl_daily <- downloadHandler(
    filename = function() paste0("GSOD_daily_", period_tag(raw_data()), ".csv"),
    content = function(file) {
      raw <- raw_data()
      out <- bind_rows(lapply(names(raw$daily), function(id) {
        cbind(STNID = id, NAME = raw$name[[id]], raw$daily[[id]],
              stringsAsFactors = FALSE)
      }))
      write.csv(out, file, row.names = FALSE, na = "")
    })

  # Summary table ---------------------------------------------------------------------
  output$score_table <- renderDT({
    s <- results()$summary %>%
      transmute(Station = NAME, ID = STNID, Country = COUNTRY, Variable = variable,
                From = from, To = to, Days = no_days, `Missing days` = no_missing,
                Gaps = no_gaps, `Mean gap` = round(mean_gap, 2),
                `Longest gap` = max_gap,
                Missing_score1 = round(Missing_score1, 2),
                Missing_score2 = round(Missing_score2, 2),
                Grade = as.character(grade),
                `Physical flags` = n_physical, Spikes = n_spike, Steps = n_step,
                Flatlines = n_flatline, `Flatline days` = flat_days)
    datatable(s, rownames = FALSE, extensions = "Buttons",
              options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"),
                             pageLength = 15, scrollX = TRUE)) %>%
      formatStyle("Missing_score2",
                  background = styleColorBar(c(0, 100), "#A9DFBF"),
                  backgroundSize = "98% 80%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Grade", color = styleEqual(grade_labels, unname(grade_cols)),
                  fontWeight = "bold")
  }, server = FALSE)
}

shinyApp(ui, server)
