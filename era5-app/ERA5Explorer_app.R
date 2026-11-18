# ERA5Explorer_app.R
# ERA5 Explorer (workshop edition)
#
# Builds a Copernicus Climate Data Store (CDS) request for "ERA5 hourly data on
# single levels", submits it with the ecmwfr package, reads the NetCDF that
# comes back and lets you map, plot and export the data as CSV.
#
# Tabs: 1. Request  2. Data  3. Explore  4. About
#
#
# Run with shiny::runApp("ERA5Explorer_app.R") or the Run App button in RStudio.

# Package check -----------------------------------------------------------------

pkgs <- c("shiny", "bslib", "bsicons", "ecmwfr", "ncdf4", "leaflet", "plotly",
          "DT", "httr")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Install the missing packages, then run the app again:\n  install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))", call. = FALSE)
}
if (utils::packageVersion("bslib") < "0.6.0") {
  stop('This app needs bslib 0.6.0 or newer: install.packages("bslib")', call. = FALSE)
}
if (utils::packageVersion("ecmwfr") < "2.0.0") {
  stop('This app needs ecmwfr 2.0.0 or newer (new CDS API): install.packages("ecmwfr")',
       call. = FALSE)
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(leaflet)
  library(plotly)
  library(DT)
})

options(shiny.maxRequestSize = 500 * 1024^2)

# Base R only has %||% from version 4.4
if (!exists("%||%")) `%||%` <- function(x, y) if (is.null(x)) y else x

# Settings ----------------------------------------------------------------------

cds_dataset      <- "reanalysis-era5-single-levels"
cds_home_url     <- "https://cds.climate.copernicus.eu"
cds_profile_url  <- "https://cds.climate.copernicus.eu/profile"
cds_dataset_url  <- "https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=download"
cds_requests_url <- "https://cds.climate.copernicus.eu/requests?tab=all"
# Endpoint used by ECMWF's own Python client to verify a personal access token
cds_verify_url   <- "https://cds.climate.copernicus.eu/api/profiles/v1/account/verification/pat"

grid_res     <- 0.25   # ERA5 native grid spacing in degrees
poll_seconds <- 5      # how often to ask the CDS about a queued request
era5_lag     <- 6      # ERA5 (preliminary ERA5T) runs about 5 days behind real time

# Soft limits for the size check. The CDS rejects requests that are too large,
# and its limits have changed over time (NetCDF requests were reduced in 2025).
warn_fields <- 10000    # fields = variables x dates x hours
warn_rows   <- 1e6      # CSV rows = grid points x dates x hours
max_map_cells <- 5000   # cells drawn on the Explore map

example_candidates <- c("era5_example.nc", "era5_example.zip",
                        file.path("data", "era5_example.nc"),
                        file.path("data", "era5_example.zip"))

era5_vars <- data.frame(
  group = c(rep("Temperature and humidity", 5), rep("Wind", 5),
            rep("Pressure", 2), rep("Precipitation, radiation and cloud", 4)),
  name = c("2m_temperature", "2m_dewpoint_temperature", "skin_temperature",
           "maximum_2m_temperature_since_previous_post_processing",
           "minimum_2m_temperature_since_previous_post_processing",
           "10m_u_component_of_wind", "10m_v_component_of_wind",
           "10m_wind_gust_since_previous_post_processing",
           "100m_u_component_of_wind", "100m_v_component_of_wind",
           "mean_sea_level_pressure", "surface_pressure",
           "total_precipitation", "evaporation",
           "surface_solar_radiation_downwards", "total_cloud_cover"),
  label = c("2m temperature", "2m dewpoint temperature", "Skin temperature",
            "Maximum 2m temperature (previous hour)",
            "Minimum 2m temperature (previous hour)",
            "10m u-component of wind", "10m v-component of wind",
            "10m wind gust (previous hour)",
            "100m u-component of wind", "100m v-component of wind",
            "Mean sea level pressure", "Surface pressure",
            "Total precipitation", "Evaporation",
            "Surface solar radiation downwards", "Total cloud cover"),
  stringsAsFactors = FALSE
)
var_groups <- unique(era5_vars$group)

# Approximate boxes (North, West, South, East). 
area_presets <- list(
  "Potchefstroom"  = c(-26.25, 26.75, -27.25, 27.75),
  "Gauteng"        = c(-25.00, 27.00, -27.00, 29.00),
  "Western Cape"   = c(-30.50, 17.75, -35.00, 24.25),
  "South Africa"   = c(-22.00, 16.50, -35.00, 33.00)
)
default_area <- area_presets[["Potchefstroom"]]

this_year <- as.integer(format(Sys.Date() - era5_lag, "%Y"))
all_hours <- sprintf("%02d:00", 0:23)

# Functions: area and request -----------------------------------------------------

# Each ERA5 value belongs to a grid point and is drawn as the 0.25 degree cell
# centred on it. Snapping every edge to the nearest grid point keeps any cell
# that contains the edge, and gives the CDS coordinates that sit exactly on
# the grid.
snap_area <- function(a) {
  s <- round(a / grid_res) * grid_res
  c(N = s[[1]], W = s[[2]], S = s[[3]], E = s[[4]])
}

area_problem <- function(a) {
  if (length(a) != 4 || any(is.na(a))) return("Enter all four coordinates.")
  if (a[1] > 90 || a[3] < -90) return("Latitudes must lie between -90 and 90.")
  if (a[2] < -180 || a[4] > 180) return("Longitudes must lie between -180 and 180.")
  # Equal values are allowed: N = S and W = E selects a single grid point
  if (a[1] < a[3]) return("North must not be smaller than South.")
  if (a[4] < a[2]) return("East must not be smaller than West.")
  NULL
}

grid_dims <- function(a) {
  c(lat = unname(round((a[1] - a[3]) / grid_res) + 1),
    lon = unname(round((a[4] - a[2]) / grid_res) + 1))
}

# Calendar dates the request covers (the CDS skips impossible dates such as 31 Feb)
request_dates <- function(years, months, days) {
  if (!length(years) || !length(months) || !length(days)) return(as.Date(character()))
  g <- expand.grid(y = years, m = months, d = days, stringsAsFactors = FALSE)
  dt <- as.Date(sprintf("%s-%s-%s", g$y, g$m, g$d), format = "%Y-%m-%d")
  sort(dt[!is.na(dt)])
}

build_request <- function(vars, years, months, days, times, area,
                          target = "era5_request.nc") {
  list(
    dataset_short_name = cds_dataset,
    product_type       = "reanalysis",
    variable           = vars,
    year               = as.character(years),
    month              = months,
    day                = days,
    time               = times,
    data_format        = "netcdf",
    download_format    = "unarchived",
    area               = unname(area),      # North, West, South, East
    target             = target
  )
}

# R vector as code, wrapped at about 75 characters
fmt_vec <- function(x, indent = 2) {
  items <- if (is.numeric(x)) format(x, trim = TRUE) else paste0('"', x, '"')
  if (length(items) == 1) return(items)
  pad <- strrep(" ", indent + 2)
  lines <- character()
  cur <- "c("
  for (i in seq_along(items)) {
    piece <- paste0(items[i], if (i < length(items)) ", " else ")")
    if (nchar(cur) + nchar(piece) > 75 - indent) {
      lines <- c(lines, sub(" $", "", cur))
      cur <- paste0(pad, piece)
    } else {
      cur <- paste0(cur, piece)
    }
  }
  paste(c(lines, cur), collapse = "\n")
}

request_code <- function(r) {
  field <- function(name, value, last = FALSE) {
    sprintf("  %s = %s%s", name, fmt_vec(value, indent = nchar(name) + 5),
            if (last) "" else ",")
  }
  paste(c(
    "library(ecmwfr)",
    "",
    "# Run once per computer; never keep the token in a shared script:",
    "# wf_set_key(key = \"YOUR-PERSONAL-ACCESS-TOKEN\")",
    "",
    "request <- list(",
    field("dataset_short_name", r$dataset_short_name),
    field("product_type", r$product_type),
    field("variable", r$variable),
    field("year", r$year),
    field("month", r$month),
    field("day", r$day),
    field("time", r$time),
    field("data_format", r$data_format),
    field("download_format", r$download_format),
    paste0(field("area", r$area), "  # North, West, South, East"),
    field("target", r$target, last = TRUE),
    ")",
    "",
    "# Waits in the CDS queue, then downloads to the working directory.",
    "# A mix of instantaneous and accumulated variables may arrive as a .zip",
    "# holding one NetCDF file per type.",
    "file <- wf_request(request, path = \".\")",
    "",
    "# Read one variable with ncdf4",
    "library(ncdf4)",
    "nc <- nc_open(file)   # unzip first if the file is a .zip",
    sprintf("print(nc)        # lists variables such as t2m (K) or tp (m)"),
    "nc_close(nc)"
  ), collapse = "\n")
}

# Functions: CDS token ------------------------------------------------------------

get_key <- function() {
  env <- Sys.getenv("ecmwfr_PAT")
  if (nzchar(env)) return(env)
  tryCatch(ecmwfr::wf_get_key(), error = function(e) "")
}

# ok = TRUE (accepted), FALSE (rejected) or NA (could not check)
verify_token <- function(key) {
  r <- tryCatch(
    httr::POST(cds_verify_url, httr::add_headers(`PRIVATE-TOKEN` = key),
               httr::timeout(10)),
    error = function(e) e)
  if (inherits(r, "error")) {
    return(list(ok = NA, msg = paste("Could not reach the CDS to check the token:",
                                     conditionMessage(r))))
  }
  code <- httr::status_code(r)
  if (code < 300) return(list(ok = TRUE, msg = "Token accepted by the CDS."))
  if (code %in% c(401, 403)) return(list(ok = FALSE, msg = "The CDS rejected this token."))
  list(ok = NA, msg = sprintf("Unexpected reply from the CDS (HTTP %d).", code))
}

save_token <- function(key, remember) {
  Sys.setenv(ecmwfr_PAT = key)          # ecmwfr reads this before the keyring
  if (!remember) return("Token stored for this R session only.")
  res <- tryCatch({ ecmwfr::wf_set_key(key = key); TRUE }, error = function(e) e)
  if (isTRUE(res)) return("Token saved in this computer's keyring.")
  paste("Token stored for this R session only; the keyring gave an error:",
        conditionMessage(res))
}

# Functions: reading ERA5 NetCDF ----------------------------------------------------

is_zip <- function(path) {
  sig <- tryCatch(readBin(path, "raw", 4), error = function(e) raw())
  length(sig) == 4 && identical(sig, as.raw(c(0x50, 0x4b, 0x03, 0x04)))
}

parse_nc_time <- function(vals, units) {
  m <- regmatches(units, regexec("^\\s*(\\w+)\\s+since\\s+(.+?)\\s*$", units))[[1]]
  if (length(m) < 3) stop("Cannot read time units: ", units)
  mult <- switch(tolower(m[2]), seconds = , second = , s = 1, minutes = , minute = 60,
                 hours = , hour = , h = 3600, days = , day = , d = 86400,
                 stop("Unknown time unit: ", m[2]))
  origin <- as.POSIXct(substr(sub("T", " ", m[3]), 1, 19), tz = "UTC",
                       tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"))
  origin + as.numeric(vals) * mult
}

pick_dim <- function(dims, candidates) {
  hit <- intersect(candidates, dims)
  if (!length(hit)) NA_character_ else hit[1]
}

# Reads every variable on a (longitude, latitude, time) grid from one file.
# Handles the new CDS layout (valid_time, latitude, longitude) and the legacy
# one (time, plus an optional expver dimension mixing ERA5 and ERA5T).
read_one_nc <- function(f) {
  nc <- ncdf4::nc_open(f)
  on.exit(ncdf4::nc_close(nc))
  dims  <- names(nc$dim)
  lon_d <- pick_dim(dims, c("longitude", "lon"))
  lat_d <- pick_dim(dims, c("latitude", "lat"))
  tim_d <- pick_dim(dims, c("valid_time", "time"))
  if (anyNA(c(lon_d, lat_d, tim_d))) {
    stop(basename(f), ": expected longitude, latitude and time dimensions, found ",
         paste(dims, collapse = ", "))
  }
  lon   <- as.numeric(nc$dim[[lon_d]]$vals)
  lat   <- as.numeric(nc$dim[[lat_d]]$vals)
  times <- parse_nc_time(nc$dim[[tim_d]]$vals, nc$dim[[tim_d]]$units)
  key3  <- c(lon_d, lat_d, tim_d)

  values <- list()
  meta   <- list()
  for (v in names(nc$var)) {
    vd <- vapply(nc$var[[v]]$dim, function(d) d$name, "")
    if (!all(key3 %in% vd) || nc$var[[v]]$prec == "char") next
    arr  <- ncdf4::ncvar_get(nc, v, collapse_degen = FALSE)
    dim(arr) <- nc$var[[v]]$varsize
    extra <- setdiff(seq_along(vd), match(key3, vd))
    arr  <- aperm(arr, c(match(key3, vd), extra))
    n3   <- length(lon) * length(lat) * length(times)
    m    <- matrix(arr, nrow = n3)
    x    <- m[, 1]
    if (ncol(m) > 1) for (j in 2:ncol(m)) x[is.na(x)] <- m[is.na(x), j]
    values[[v]] <- x
    att <- function(a) {
      r <- ncdf4::ncatt_get(nc, v, a)
      if (isTRUE(r$hasatt)) as.character(r$value) else ""
    }
    meta[[v]] <- data.frame(name = v, long_name = att("long_name"),
                            units = att("units"), stringsAsFactors = FALSE)
  }
  if (!length(values)) stop(basename(f), ": no gridded variables found")

  g <- expand.grid(longitude = lon, latitude = lat, ti = seq_along(times))
  df <- data.frame(time = times[g$ti], latitude = g$latitude,
                   longitude = g$longitude, values, check.names = FALSE)
  list(data = df, meta = do.call(rbind, meta))
}

# One file, or a zip holding several (the CDS splits instantaneous and
# accumulated variables into separate NetCDF files).
read_era5 <- function(path) {
  files <- path
  if (is_zip(path)) {
    ex <- tempfile("era5_unzip_")
    utils::unzip(path, exdir = ex)
    files <- list.files(ex, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
    if (!length(files)) stop("The zip file holds no NetCDF (.nc) files.")
  }
  parts <- lapply(files, read_one_nc)
  data <- Reduce(function(a, b) merge(a, b, by = c("time", "latitude", "longitude"),
                                      all = TRUE),
                 lapply(parts, `[[`, "data"))
  data <- data[order(data$time, -data$latitude, data$longitude), ]
  rownames(data) <- NULL
  meta <- do.call(rbind, lapply(parts, `[[`, "meta"))
  meta <- meta[!duplicated(meta$name), ]
  list(data = data, meta = meta, files = basename(files))
}

# Unit conversion and derived variables -----------------------------------------

water_vars <- c("tp", "e", "ro", "sf", "cp", "lsp", "pev", "sro", "ssro")

# Variables that describe the hour ending at the time stamp (hourly totals, and
# maxima/minima since the previous post-processing). For daily values they are
# assigned to the hour they cover, so the 00:00 value counts for the day before.
period_vars <- c(water_vars, "ssrd", "ssr", "strd", "str", "tisr",
                 "mx2t", "mn2t", "i10fg")

# Time zones for display; South Africa has no daylight saving, so SAST = UTC+2
tz_choices <- c(UTC = "UTC", SAST = "Africa/Johannesburg")
cloud_vars <- c("tcc", "lcc", "mcc", "hcc")

convert_era5 <- function(df, meta) {
  for (i in seq_len(nrow(meta))) {
    v <- meta$name[i]
    u <- meta$units[i]
    new <- NULL
    if (u == "K") {
      df[[v]] <- df[[v]] - 273.15; new <- "\u00b0C"
    } else if (u == "Pa") {
      df[[v]] <- df[[v]] / 100; new <- "hPa"
    } else if (u == "m" && v %in% water_vars) {
      df[[v]] <- df[[v]] * 1000; new <- "mm"
    } else if (u %in% c("J m**-2", "J m-2", "J m^-2")) {
      df[[v]] <- df[[v]] / 3600; new <- "W m-2 (hourly mean)"
    } else if (v %in% cloud_vars && u %in% c("(0 - 1)", "(0-1)", "1", "0-1", "")) {
      df[[v]] <- df[[v]] * 100; new <- "%"
    }
    if (!is.null(new)) meta$units[i] <- new
  }

  add <- function(name, long_name, units, x) {
    df[[name]] <<- x
    meta <<- rbind(meta, data.frame(name = name, long_name = long_name,
                                    units = units, stringsAsFactors = FALSE))
  }
  for (h in c("10", "100")) {
    u <- paste0("u", h); v <- paste0("v", h)
    if (all(c(u, v) %in% names(df))) {
      add(paste0("ws", h), sprintf("%sm wind speed (derived)", h), "m s**-1",
          sqrt(df[[u]]^2 + df[[v]]^2))
      add(paste0("wd", h), sprintf("%sm wind direction, from (derived)", h), "degrees",
          (270 - atan2(df[[v]], df[[u]]) * 180 / pi) %% 360)
    }
  }
  if (all(c("t2m", "d2m") %in% names(df)) &&
      all(meta$units[match(c("t2m", "d2m"), meta$name)] == "\u00b0C")) {
    # Magnus formula (Alduchov and Eskridge, 1996)
    es <- function(t) exp(17.625 * t / (243.04 + t))
    add("rh2m", "2m relative humidity (derived)", "%", 100 * es(df$d2m) / es(df$t2m))
  }
  list(data = df, meta = meta)
}

var_choices <- function(meta) {
  setNames(meta$name, sprintf("%s: %s (%s)", meta$name, meta$long_name, meta$units))
}

pal_for <- function(v, units) {
  if (units %in% c("K", "\u00b0C")) return(list(name = "RdYlBu", rev = TRUE))
  if (v %in% c(water_vars, "rh2m")) return(list(name = "Blues", rev = FALSE))
  list(name = "viridis", rev = FALSE)
}

# UI ------------------------------------------------------------------------------

check_grid <- function(id, choices, selected, min_width = 78) {
  div(class = "check-grid", style = sprintf("--cell: %dpx;", min_width),
      checkboxGroupInput(id, NULL, choices = choices, selected = selected,
                         width = "100%"))
}
select_links <- function(...) {
  div(class = "small mb-2 d-flex flex-wrap gap-3", ...)
}

css <- "
.check-grid .shiny-options-group { display: grid;
  grid-template-columns: repeat(auto-fill, minmax(var(--cell), 1fr)); column-gap: .5rem; }
.check-grid .checkbox { margin: 0; }
.var-group .shiny-options-group { display: grid;
  grid-template-columns: repeat(auto-fill, minmax(230px, 1fr)); column-gap: 1rem; }
.var-group .checkbox { margin: 0; }
.size-table td { padding: 2px 10px 2px 0; }
.rcode pre { white-space: pre; font-size: .85rem; max-height: 60vh; overflow: auto; }
"

request_panel <- nav_panel(
  "1. Request", icon = bs_icon("cloud-download"),
  layout_columns(
    col_widths = c(5, 7), fill = FALSE,
    card(
      height = 780,
      card_header("Sub-region (bounding box)"),
      card_body(
      fillable = FALSE,
      div(leafletOutput("bbox_map", height = "420px")),
      div(class = "small text-muted mt-2",
          "Each ERA5 value belongs to a grid point (dot) and is shown as the ",
          "0.25\u00b0 square centred on it. Click the cells at two opposite corners ",
          "(click the same cell twice for a single cell), choose a preset or type ",
          "the coordinates. Zoom in to see the cell edges."),
      tags$h6(class = "mt-3 mb-1", "Area sent to the CDS (outermost grid points)"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        numericInput("north", "North", default_area[1], min = -90, max = 90, step = grid_res),
        numericInput("west",  "West",  default_area[2], min = -180, max = 180, step = grid_res),
        numericInput("south", "South", default_area[3], min = -90, max = 90, step = grid_res),
        numericInput("east",  "East",  default_area[4], min = -180, max = 180, step = grid_res)
      ),
      uiOutput("area_summary"),
      selectInput("preset", "Preset area",
                  c("Choose a preset" = "", names(area_presets)))
      )
    ),
    card(
      height = 780,
      card_header("ERA5 hourly data on single levels"),
      accordion(
        multiple = TRUE, open = c("Variable", "Year", "Month", "Day", "Time"),
        accordion_panel(
          "Product type",
          radioButtons("product_type", NULL, c("Reanalysis" = "reanalysis")),
          div(class = "small text-muted",
              "Ensemble products are left out to keep requests small.")),
        accordion_panel(
          "Variable",
          lapply(seq_along(var_groups), function(i) {
            g <- era5_vars[era5_vars$group == var_groups[i], ]
            div(class = "var-group",
                tags$h6(class = "mt-2", var_groups[i]),
                checkboxGroupInput(paste0("var_", i), NULL, width = "100%",
                                   choices = setNames(g$name, g$label),
                                   selected = intersect(g$name, c("2m_temperature",
                                                                  "total_precipitation"))))
          })),
        accordion_panel(
          "Year",
          selectizeInput("years", NULL, choices = rev(1940:this_year),
                         selected = this_year - 1, multiple = TRUE,
                         options = list(plugins = list("remove_button")))),
        accordion_panel(
          "Month",
          select_links(actionLink("months_all", "Select all"),
                       actionLink("months_none", "Clear")),
          check_grid("months", setNames(sprintf("%02d", 1:12), month.name), "01",
                     min_width = 110)),
        accordion_panel(
          "Day",
          select_links(actionLink("days_all", "Select all"),
                       actionLink("days_none", "Clear")),
          check_grid("days", sprintf("%02d", 1:31), sprintf("%02d", 1:31))),
        accordion_panel(
          "Time",
          select_links(actionLink("times_all", "Select all"),
                       actionLink("times_6h", "00/06/12/18"),
                       actionLink("times_12", "12:00 only"),
                       actionLink("times_none", "Clear")),
          check_grid("times", all_hours, all_hours),
          div(class = "small text-muted",
              "Requests are always in UTC, as the CDS requires: 00:00 UTC is 02:00 ",
              "SAST. The Data tab can show the results in SAST.")),
        accordion_panel(
          "Data format",
          div(class = "small",
              "NetCDF4, unarchived. The app reads NetCDF; the CDS may still send a ",
              ".zip when instantaneous and accumulated variables are mixed, and the ",
              "app unpacks it."))
      )
    )
  ),
  layout_columns(
    col_widths = c(7, 5), fill = FALSE,
    card(card_header("Request size"), uiOutput("size_box")),
    card(
      card_header("Submit"),
      div(class = "d-flex flex-wrap gap-2 mb-3",
          actionButton("show_code", tagList(bs_icon("code-slash"), "Show R code")),
          actionButton("submit", tagList(bs_icon("send"), "Submit request"),
                       class = "btn-primary")),
      uiOutput("job_status")
    )
  )
)

data_panel <- nav_panel(
  "2. Data", icon = bs_icon("table"),
  layout_sidebar(
    sidebar = sidebar(
      width = 320, title = "Data source",
      uiOutput("data_source_info"),
      actionButton("use_example", tagList(bs_icon("box-seam"), "Use example data"),
                   class = "btn-outline-secondary"),
      fileInput("nc_upload", "Or load a NetCDF/zip file from the CDS",
                accept = c(".nc", ".zip"), buttonLabel = "Browse"),
      hr(),
      radioButtons("tz", "Show times in",
                   c("UTC (as delivered)" = "UTC", "SAST (UTC+2)" = "SAST")),
      div(class = "small text-muted mb-3",
          "Affects the preview, the CSV, the map, the time series and the days used ",
          "for daily values. Requests stay in UTC."),
      checkboxInput("convert", "Convert units and add derived variables", TRUE),
      div(class = "small text-muted",
          "K to \u00b0C, Pa to hPa, m to mm (precipitation, evaporation), J m-2 to ",
          "W m-2, cloud cover to %. Adds wind speed and direction from u and v, and ",
          "relative humidity from temperature and dew point."),
      hr(),
      uiOutput("download_ui")
    ),
    uiOutput("data_boxes"),
    card(card_header("Variables in the file"), tableOutput("var_table"), fill = FALSE),
    card(full_screen = TRUE, card_header(textOutput("preview_title", inline = TRUE)),
         DTOutput("preview"))
  )
)

explore_panel <- nav_panel(
  "3. Explore", icon = bs_icon("map"),
  div(class = "d-flex flex-wrap gap-4 align-items-end",
      selectInput("ex_var", "Variable", choices = NULL, width = "420px"),
      checkboxInput("fixed_scale", "Same colour scale for every time step", TRUE)),
  layout_columns(
    col_widths = c(6, 6),
    card(full_screen = TRUE,
         card_header("Map of one time step (click a cell for its time series)"),
         uiOutput("time_slider_ui", fill = FALSE),
         leafletOutput("slice_map", height = "100%")),
    card(full_screen = TRUE,
         card_header("Time series"),
         layout_columns(
           col_widths = c(6, 6), fill = FALSE,
           selectInput("ts_cell", "Grid cell", choices = NULL),
           selectInput("ts_agg", "Aggregation",
                       c("As delivered (hourly)" = "none", "Daily mean" = "mean",
                         "Daily minimum" = "min", "Daily maximum" = "max",
                         "Daily total" = "sum"))),
         checkboxInput("ts_boxmean", "Add the mean over the whole box", FALSE),
         div(class = "small text-muted mb-1",
             "Daily values leave out incomplete days at the start and end of the ",
             "period. Hourly totals count for the hour they cover."),
         plotlyOutput("ts_plot", height = "100%"))
  )
)

about_panel <- nav_panel(
  "About", icon = bs_icon("info-circle"),
  div(
    class = "p-2", style = "max-width: 900px; overflow-y: auto;",
    h4("ERA5 in brief"),
    p("ERA5 is ECMWF's fifth-generation global reanalysis. It combines a weather ",
      "model with historical observations to give hourly estimates on a regular ",
      "0.25\u00b0 grid (about 28 km at the equator) from 1940 to a few days ago. ",
      "The most recent months come from a preliminary version, ERA5T, which can ",
      "still be revised."),
    p("Each value belongs to a grid point. The app draws it as the 0.25\u00b0 square ",
      "centred on that point, which is how gridded data are usually mapped; read it ",
      "as representative of that area rather than of one exact location."),
    h4("Getting access"),
    tags$ol(
      tags$li(HTML(sprintf('Register at <a href="%s" target="_blank">the Climate Data Store</a>.',
                           cds_home_url))),
      tags$li(HTML(sprintf('Copy the personal access token from your <a href="%s" target="_blank">profile page</a> into this app (the key icon at the top right).',
                           cds_profile_url))),
      tags$li(HTML(sprintf('Accept the licence at the bottom of the <a href="%s" target="_blank">ERA5 download page</a>. Requests fail until this is done.',
                           cds_dataset_url))))
    ,
    h4("Things that catch people out"),
    tags$ul(
      tags$li("ERA5 times are UTC. GSOD also summarises each day from 00:00 to 23:59 ",
              "UTC, so ERA5 daily means built from UTC days line up with GSOD's daily ",
              "TEMP. GSOD's MAX, MIN and PRCP can come from reports whose 24-hour ",
              "window includes part of the previous day, and the timing varies by ",
              "country, so those match less exactly."),
      tags$li("A South African calendar day (SAST, UTC+2) runs from 22:00 UTC the ",
              "previous day to 22:00 UTC. Choose SAST on the Data tab to see times and ",
              "daily values in local time."),
      tags$li("Accumulated variables (precipitation, radiation, evaporation) in the ",
              "hourly data are totals over the hour ending at the time stamp, so the ",
              "00:00 value is the last hour of the previous day. The app assigns these ",
              "values to the hour they cover when it builds daily totals. Precipitation ",
              "is in metres of water; the conversion gives mm."),
      tags$li("Evaporation is negative when water evaporates (ECMWF sign convention)."),
      tags$li("Temperatures arrive in kelvin and pressures in pascal."),
      tags$li("Requests queue at the CDS. Small boxes and short periods return in ",
              "minutes; large ones can wait much longer or be rejected as too large. ",
              "Splitting a long period into one request per year or month is the ",
              "usual remedy."),
      tags$li("A mix of instantaneous and accumulated variables arrives as a zip ",
              "with one NetCDF file per type.")
    )
  )
)

ui <- page_navbar(
  title = tags$span(bs_icon("globe-americas"), " ERA5 Explorer"),
  id = "tabs",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1F4E79"),
  fillable = c("2. Data", "3. Explore"),
  header = tags$head(tags$style(HTML(css))),
  request_panel,
  data_panel,
  explore_panel,
  about_panel,
  nav_spacer(),
  nav_item(uiOutput("token_badge"))
)

# Server ----------------------------------------------------------------------------

dl_dir <- file.path(tempdir(), "era5_downloads")
dir.create(dl_dir, showWarnings = FALSE)

server <- function(input, output, session) {

  # Token -----------------------------------------------------------------------------
  token <- reactiveVal(list(status = "missing", msg = ""))

  token_modal <- function(msg = NULL) {
    modalDialog(
      title = "Copernicus CDS personal access token",
      p("Downloading ERA5 needs a free Climate Data Store account and its personal ",
        "access token. Without one you can still load the example data or a file ",
        "downloaded from the CDS website."),
      tags$ol(
        tags$li(HTML(sprintf('Register or log in at <a href="%s" target="_blank">cds.climate.copernicus.eu</a>.',
                             cds_home_url))),
        tags$li(HTML(sprintf('Copy the API token from your <a href="%s" target="_blank">profile page</a>.',
                             cds_profile_url))),
        tags$li(HTML(sprintf('Accept the licence at the bottom of the <a href="%s" target="_blank">ERA5 download page</a> (once per account).',
                             cds_dataset_url)))),
      if (!is.null(msg)) div(class = "alert alert-warning", msg),
      passwordInput("token_in", "Personal access token", width = "100%",
                    placeholder = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"),
      checkboxInput("token_remember", "Remember on this computer (system keyring)", TRUE),
      footer = tagList(
        actionButton("token_skip", "Continue without a token"),
        actionButton("token_save", "Check and save", class = "btn-primary")),
      easyClose = FALSE
    )
  }

  # Startup check: is a token stored, and does the CDS accept it?
  local({
    key <- get_key()
    if (!nzchar(key)) {
      token(list(status = "missing", msg = "No CDS token found."))
      showModal(token_modal())
    } else {
      v <- verify_token(key)
      st <- if (isTRUE(v$ok)) "ok" else if (identical(v$ok, FALSE)) "invalid" else "unverified"
      token(list(status = st, msg = v$msg))
      if (st == "invalid") showModal(token_modal("The stored token was rejected by the CDS. Paste a new one."))
    }
  })

  observeEvent(input$token_open, showModal(token_modal(token()$msg)))
  observeEvent(input$token_skip, removeModal())

  observeEvent(input$token_save, {
    key <- trimws(input$token_in %||% "")
    if (!nzchar(key)) {
      showModal(token_modal("Paste a token first."))
      return()
    }
    if (grepl("^[0-9]+:", key)) {
      showModal(token_modal(paste(
        "This looks like a key from the old CDS (UID:KEY). The current CDS uses a",
        "personal access token without the number and colon; copy it again from",
        "your profile page.")))
      return()
    }
    v <- verify_token(key)
    if (identical(v$ok, FALSE)) {
      showModal(token_modal(v$msg))
      return()
    }
    msg <- save_token(key, isTRUE(input$token_remember))
    token(list(status = if (isTRUE(v$ok)) "ok" else "unverified",
                msg = paste(v$msg, msg)))
    removeModal()
    showNotification(paste(v$msg, msg), type = if (isTRUE(v$ok)) "message" else "warning",
                     duration = 8)
  })

  output$token_badge <- renderUI({
    t <- token()
    cls <- switch(t$status, ok = "bg-success", unverified = "bg-warning text-dark",
                  "bg-danger")
    txt <- switch(t$status, ok = "CDS token OK", unverified = "CDS token not verified",
                  invalid = "CDS token rejected", "No CDS token")
    actionLink("token_open", span(class = paste("badge", cls), bs_icon("key"), txt),
               title = t$msg)
  })

  # Area ------------------------------------------------------------------------------
  area_raw <- debounce(reactive(c(input$north, input$west, input$south, input$east)), 500)

  area <- reactive({
    a <- area_raw()
    if (!is.null(area_problem(a))) return(NULL)
    snap_area(a)
  })

  output$area_summary <- renderUI({
    a <- area()
    if (is.null(a)) {
      return(div(class = "small text-danger mb-2", area_problem(area_raw())))
    }
    gd <- grid_dims(a)
    div(class = "small mb-2",
        strong(sprintf("%d x %d = %d %s.", gd[["lat"]], gd[["lon"]], prod(gd),
                       if (prod(gd) == 1) "grid point (cell)" else "grid points (cells)")),
        " Points on the edges are included: North -33 and South -33.25 give two ",
        "rows. For one cell, use the same value for North and South, and for West ",
        "and East.")
  })

  set_area <- function(a) {
    updateNumericInput(session, "north", value = a[1])
    updateNumericInput(session, "west",  value = a[2])
    updateNumericInput(session, "south", value = a[3])
    updateNumericInput(session, "east",  value = a[4])
  }

  observeEvent(input$preset, {
    req(nzchar(input$preset))
    set_area(area_presets[[input$preset]])
  })

  output$bbox_map <- renderLeaflet({
    a <- isolate(area()) %||% snap_area(default_area)
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") %>%
      addTiles(group = "Streets") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(baseGroups = c("Light", "Streets", "Satellite")) %>%
      fitBounds(a[["W"]] - 2, a[["S"]] - 2, a[["E"]] + 2, a[["N"]] + 2)
  })

  map_ready <- reactiveVal(FALSE)
  observeEvent(input$bbox_map_bounds, map_ready(TRUE), once = TRUE)

  observe({
    req(map_ready())
    a <- area()
    proxy <- leafletProxy("bbox_map") %>% clearGroup("box") %>% clearGroup("cells") %>%
      clearGroup("grid")
    req(a)
    h <- grid_res / 2
    if (prod(grid_dims(a)) <= 2500) {
      pts <- expand.grid(lon = seq(a[["W"]], a[["E"]], by = grid_res),
                         lat = seq(a[["S"]], a[["N"]], by = grid_res))
      proxy %>%
        addRectangles(data = pts, lng1 = ~lon - h, lat1 = ~lat - h,
                      lng2 = ~lon + h, lat2 = ~lat + h, group = "cells",
                      color = "#1F4E79", weight = 0.7, opacity = 0.7,
                      fillColor = "#5DADE2", fillOpacity = 0.18,
                      options = pathOptions(interactive = FALSE)) %>%
        addCircleMarkers(data = pts, lng = ~lon, lat = ~lat, group = "grid",
                         radius = 2, stroke = FALSE, fillColor = "#1F4E79",
                         fillOpacity = 0.8, options = pathOptions(interactive = FALSE))
    }
    # Outline of the area the selected cells cover
    proxy %>% addRectangles(a[["W"]] - h, a[["S"]] - h, a[["E"]] + h, a[["N"]] + h,
                            group = "box", color = "#C0392B", weight = 2.5,
                            fill = FALSE, options = pathOptions(interactive = FALSE))
  })

  # Faint cell edges across the visible map once zoomed in far enough
  map_bounds <- debounce(reactive(input$bbox_map_bounds), 300)
  observe({
    b <- map_bounds()
    req(b)
    proxy <- leafletProxy("bbox_map") %>% clearGroup("gridlines")
    if (b$east - b$west > 15 || b$north - b$south > 15) return()
    h <- grid_res / 2
    lons <- seq(floor(b$west / grid_res) * grid_res - h,
                ceiling(b$east / grid_res) * grid_res + h, by = grid_res)
    lats <- seq(floor(b$south / grid_res) * grid_res - h,
                ceiling(b$north / grid_res) * grid_res + h, by = grid_res)
    # One polyline layer; NA separates the individual lines
    lng <- c(c(rbind(lons, lons, NA)), rep(c(min(lons), max(lons), NA), length(lats)))
    lat <- c(rep(c(min(lats), max(lats), NA), length(lons)), c(rbind(lats, lats, NA)))
    proxy %>% addPolylines(lng = lng, lat = lat, group = "gridlines", color = "#7F8C8D",
                           weight = 0.6, opacity = 0.6,
                           options = pathOptions(interactive = FALSE))
  })

  # Two clicks on the map define opposite corners
  corner <- reactiveVal(NULL)
  observeEvent(input$bbox_map_click, {
    cl <- input$bbox_map_click
    first <- corner()
    if (is.null(first)) {
      corner(c(cl$lat, cl$lng))
      g <- round(c(cl$lat, cl$lng) / grid_res) * grid_res
      h <- grid_res / 2
      leafletProxy("bbox_map") %>% clearGroup("corner") %>%
        addRectangles(g[2] - h, g[1] - h, g[2] + h, g[1] + h, group = "corner",
                      color = "#C0392B", weight = 2, fillColor = "#C0392B",
                      fillOpacity = 0.4, options = pathOptions(interactive = FALSE))
      showNotification("First corner set: click the opposite corner.", id = "corner_note",
                       duration = 4)
    } else {
      a <- snap_area(c(max(first[1], cl$lat), min(first[2], cl$lng),
                       min(first[1], cl$lat), max(first[2], cl$lng)))
      corner(NULL)
      leafletProxy("bbox_map") %>% clearGroup("corner")
      set_area(unname(a))
    }
  })

  # Form Functions ----------------------------------------------------------------------
  observeEvent(input$months_all,  updateCheckboxGroupInput(session, "months", selected = sprintf("%02d", 1:12)))
  observeEvent(input$months_none, updateCheckboxGroupInput(session, "months", selected = character()))
  observeEvent(input$days_all,    updateCheckboxGroupInput(session, "days", selected = sprintf("%02d", 1:31)))
  observeEvent(input$days_none,   updateCheckboxGroupInput(session, "days", selected = character()))
  observeEvent(input$times_all,   updateCheckboxGroupInput(session, "times", selected = all_hours))
  observeEvent(input$times_6h,    updateCheckboxGroupInput(session, "times", selected = c("00:00", "06:00", "12:00", "18:00")))
  observeEvent(input$times_12,    updateCheckboxGroupInput(session, "times", selected = "12:00"))
  observeEvent(input$times_none,  updateCheckboxGroupInput(session, "times", selected = character()))

  selected_vars <- reactive({
    unlist(lapply(seq_along(var_groups), function(i) input[[paste0("var_", i)]]))
  })

  # Size estimate ---------------------------------------------------------------------
  estimate <- reactive({
    a      <- area()
    vars   <- selected_vars()
    dates  <- request_dates(input$years, input$months, input$days)
    latest <- Sys.Date() - era5_lag
    problems <- c(
      if (is.null(a)) area_problem(area_raw()),
      if (!length(vars)) "Select at least one variable.",
      if (!length(input$years)) "Select at least one year.",
      if (!length(input$months)) "Select at least one month.",
      if (!length(input$days)) "Select at least one day.",
      if (!length(input$times)) "Select at least one time.",
      if (length(dates) && !any(dates <= latest))
        sprintf("ERA5 is only available up to about %s.", format(latest, "%d %B %Y"))
    )
    dates  <- dates[dates <= latest]
    gd     <- if (is.null(a)) c(lat = 0, lon = 0) else grid_dims(a)
    fields <- length(vars) * length(dates) * length(input$times)
    rows   <- prod(gd) * length(dates) * length(input$times)
    level  <- if (fields > 5 * warn_fields || rows > 5 * warn_rows) "danger"
              else if (fields > warn_fields || rows > warn_rows) "warning" else "success"
    list(area = a, vars = vars, dates = dates, gd = gd, fields = fields,
         rows = rows, values = rows * length(vars), level = level, problems = problems)
  })

  current_request <- reactive({
    e <- estimate()
    req(!length(e$problems))
    build_request(e$vars, sort(input$years), sort(input$months), sort(input$days),
                  sort(input$times), e$area,
                  target = sprintf("era5_%s.nc", format(Sys.time(), "%Y%m%d_%H%M%S")))
  })

  output$size_box <- renderUI({
    e <- estimate()
    if (length(e$problems)) {
      return(div(class = "alert alert-warning mb-0",
                 tags$ul(class = "mb-0", lapply(e$problems, tags$li))))
    }
    fmt <- function(x) format(x, big.mark = ",", scientific = FALSE)
    verdict <- switch(e$level,
      success = "Small request: should return quickly.",
      warning = "Large request: expect a longer wait in the CDS queue.",
      danger  = paste("Very large request: the CDS may reject it, and the CSV would be",
                      "unwieldy. Consider a smaller box, fewer hours or splitting by year."))
    a <- e$area
    tagList(
      tags$table(class = "size-table",
        tags$tr(tags$td("Outer grid points (sent to the CDS)"),
                tags$td(sprintf("N %.2f, W %.2f, S %.2f, E %.2f",
                                a[["N"]], a[["W"]], a[["S"]], a[["E"]]))),
        tags$tr(tags$td("Area covered by the cells"),
                tags$td(sprintf("N %.3f, W %.3f, S %.3f, E %.3f",
                                a[["N"]] + grid_res / 2, a[["W"]] - grid_res / 2,
                                a[["S"]] - grid_res / 2, a[["E"]] + grid_res / 2))),
        tags$tr(tags$td("Grid points"),
                tags$td(sprintf("%d x %d = %s", e$gd[["lat"]], e$gd[["lon"]],
                                fmt(prod(e$gd))))),
        tags$tr(tags$td("Fields (variables x dates x hours)"),
                tags$td(sprintf("%d x %d x %d = %s", length(e$vars), length(e$dates),
                                length(input$times), fmt(e$fields)))),
        tags$tr(tags$td("Values"), tags$td(fmt(e$values))),
        tags$tr(tags$td("CSV rows (grid points x time steps)"), tags$td(fmt(e$rows)))
      ),
      div(class = paste0("alert alert-", e$level, " mt-2 mb-0 py-2"), verdict)
    )
  })

  output$code_text <- renderText(request_code(current_request()))

  observeEvent(input$show_code, {
    e <- estimate()
    if (length(e$problems)) {
      showNotification("Complete the request first.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "R code for this request", size = "l", easyClose = TRUE,
      p("Run this outside the app to get the same data. The token is stored once ",
        "with wf_set_key() and never written into the script."),
      div(class = "rcode", verbatimTextOutput("code_text")),
      footer = tagList(downloadButton("code_dl", "Save as .R"), modalButton("Close"))
    ))
  })

  output$code_dl <- downloadHandler(
    filename = function() "era5_request.R",
    content = function(file) writeLines(request_code(current_request()), file)
  )

  # Submitting and polling ------------------------------------------------------------
  job <- reactiveVal(NULL)
  job_state <- reactiveValues(status = "idle", started = NULL, msg = NULL, fails = 0)

  licence_modal <- function(msg) {
    modalDialog(
      title = "Could not submit the request",
      div(class = "alert alert-danger", msg),
      p("Common causes:"),
      tags$ul(
        tags$li(HTML(sprintf('The ERA5 licence has not been accepted: accept it at the bottom of the <a href="%s" target="_blank">ERA5 download page</a>.',
                             cds_dataset_url))),
        tags$li("No internet connection, or a proxy blocks the CDS. ecmwfr then reports ",
                "that the dataset \"is not found\" because it cannot read the CDS catalogue."),
        tags$li("The token is wrong or expired: click the key badge at the top right."),
        tags$li("The request is too large: reduce the box, the hours or the period.")),
      easyClose = TRUE)
  }

  submit_request <- function() {
    r <- current_request()
    job_state$status <- "submitting"
    j <- tryCatch(
      withCallingHandlers(
        ecmwfr::wf_request(request = r, transfer = FALSE, path = dl_dir,
                           retry = poll_seconds, verbose = FALSE),
        # Harmless R6 notice from ecmwfr 2.0.3 and earlier (fixed in 2.0.4)
        warning = function(w) {
          if (grepl("cloneable", conditionMessage(w))) invokeRestart("muffleWarning")
        }),
      error = function(e) e)
    if (inherits(j, "error")) {
      msg <- conditionMessage(j)
      job_state$status <- "error"
      job_state$msg <- paste("Could not submit:", msg)
      showModal(licence_modal(msg))
      return()
    }
    job(j)
    job_state$status  <- j$get_status() %||% "accepted"
    job_state$started <- Sys.time()
    job_state$msg     <- NULL
    job_state$fails   <- 0
  }

  observeEvent(input$submit, {
    if (token()$status %in% c("missing", "invalid")) {
      showModal(token_modal("A CDS token is needed to submit a request."))
      return()
    }
    if (job_state$status %in% c("accepted", "running", "submitting", "downloading")) {
      showNotification("A request is already in progress.", type = "warning")
      return()
    }
    e <- estimate()
    if (length(e$problems)) {
      showNotification("Complete the request first.", type = "warning")
      return()
    }
    if (e$level == "danger") {
      showModal(modalDialog(
        title = "Very large request",
        p("This request may be rejected by the CDS or take a long time. Submit anyway?"),
        footer = tagList(modalButton("Cancel"),
                         actionButton("submit_anyway", "Submit", class = "btn-danger"))))
      return()
    }
    submit_request()
  })

  observeEvent(input$submit_anyway, {
    removeModal()
    submit_request()
  })

  observeEvent(input$cancel_job, {
    j <- job()
    req(j)
    tryCatch(j$delete(), error = function(e) NULL)
    job_state$status <- "cancelled"
    job_state$msg <- "Request cancelled and removed from the CDS queue."
  })

  # Ask the CDS for the status every few seconds while the request is queued
  observe({
    st <- job_state$status
    j  <- job()
    req(!is.null(j), st %in% c("accepted", "running"))
    invalidateLater(poll_seconds * 1000)
    isolate({
      res <- tryCatch({
        j$update_status(fail_is_error = FALSE)
        j$get_status()
      }, error = function(e) e)

      if (inherits(res, "error")) {
        job_state$fails <- job_state$fails + 1
        if (job_state$fails >= 3) {
          job_state$status <- "error"
          job_state$msg <- paste("Lost contact with the CDS:", conditionMessage(res),
                                 "The request may still finish; check your CDS requests page.")
        }
        return()
      }
      job_state$fails <- 0

      if (identical(res, "successful")) {
        job_state$status <- "downloading"
        ok <- tryCatch({ j$download(fail_is_error = FALSE); TRUE },
                       error = function(e) { job_state$msg <- conditionMessage(e); FALSE })
        f <- j$get_file()
        if (ok && !is.na(f) && file.exists(f)) {
          tryCatch(j$delete(), error = function(e) NULL)
          job_state$status <- "done"
          job_state$msg <- sprintf("Downloaded %s (%.1f MB).", basename(f),
                                   file.size(f) / 1024^2)
          load_into_app(f, "CDS request")
        } else {
          job_state$status <- "error"
          job_state$msg <- paste("The request finished but the download failed.",
                                 job_state$msg %||% "")
        }
      } else if (identical(res, "failed")) {
        job_state$status <- "failed"
        job_state$msg <- paste("The CDS reports that the request failed. Open your",
                               "requests page for the reason; common causes are a licence",
                               "that has not been accepted or a request that is too large.")
      } else {
        job_state$status <- res %||% "accepted"
      }
    })
  })

  output$job_status <- renderUI({
    st <- job_state$status
    if (st %in% c("accepted", "running")) invalidateLater(1000)
    elapsed <- if (is.null(job_state$started)) "" else {
      s <- as.integer(difftime(Sys.time(), job_state$started, units = "secs"))
      sprintf(" (%d min %02d s)", s %/% 60, s %% 60)
    }
    info <- switch(st,
      idle        = list("secondary", "No request submitted yet."),
      submitting  = list("info", "Submitting to the CDS..."),
      accepted    = list("info", paste0("Queued at the CDS", elapsed, ".")),
      running     = list("info", paste0("The CDS is processing the request", elapsed, ".")),
      downloading = list("info", "Downloading the file..."),
      done        = list("success", "Done. The data are on the Data tab."),
      cancelled   = list("secondary", "Cancelled."),
      failed      = list("danger", "Failed."),
      error       = list("danger", "Error."),
      list("info", paste0("Status: ", st, elapsed, ".")))
    tagList(
      div(class = paste0("alert alert-", info[[1]], " py-2 mb-2"), info[[2]],
          if (!is.null(job_state$msg)) div(class = "small mt-1", job_state$msg)),
      if (st %in% c("accepted", "running"))
        div(class = "small text-muted mb-2",
            "If the queue is slow, load the example data on the Data tab and come back later."),
      div(class = "d-flex flex-wrap gap-2",
          if (st %in% c("accepted", "running"))
            actionButton("cancel_job", tagList(bs_icon("x-circle"), "Cancel request"),
                         class = "btn-sm btn-outline-danger"),
          tags$a(href = cds_requests_url, target = "_blank",
                 class = "btn btn-sm btn-outline-secondary",
                 bs_icon("box-arrow-up-right"), " Your CDS requests"))
    )
  })

  # Loading data ----------------------------------------------------------------------
  dataset <- reactiveVal(NULL)

  load_into_app <- function(path, source, display_name = basename(path)) {
    res <- tryCatch(read_era5(path), error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(paste("Could not read the file:", conditionMessage(res)),
                       type = "error", duration = 10)
      return(invisible(FALSE))
    }
    res$path <- path
    res$source <- source
    res$name <- display_name
    dataset(res)
    nav_select("tabs", "2. Data")
    showNotification(sprintf("Loaded %s: %s rows.", display_name,
                             format(nrow(res$data), big.mark = ",")), type = "message")
    invisible(TRUE)
  }

  observeEvent(input$use_example, {
    f <- example_candidates[file.exists(example_candidates)]
    if (!length(f)) {
      showNotification(paste("No example file found next to the app. Run",
                             "make_era5_example.R once to create era5_example.nc."),
                       type = "warning", duration = 10)
      return()
    }
    load_into_app(normalizePath(f[1]), "Example file")
  })

  observeEvent(input$nc_upload, {
    load_into_app(input$nc_upload$datapath, "Uploaded file", input$nc_upload$name)
  })

  processed <- reactive({
    d <- dataset()
    req(d)
    if (isTRUE(input$convert)) convert_era5(d$data, d$meta)
    else list(data = d$data, meta = d$meta)
  })

  tz_name  <- reactive(tz_choices[[input$tz %||% "UTC"]])
  tz_label <- reactive(input$tz %||% "UTC")
  fmt_time <- function(t, f = "%Y-%m-%d %H:%M") format(t, f, tz = tz_name())

  output$preview_title <- renderText(
    sprintf("Preview (first 1,000 rows, times in %s)", tz_label()))

  output$data_source_info <- renderUI({
    d <- dataset()
    if (is.null(d)) {
      return(div(class = "small text-muted mb-2",
                 "No data loaded. Submit a request, use the example or load a file."))
    }
    div(class = "small mb-2",
        strong(d$source), br(), d$name,
        if (length(d$files) > 1)
          tagList(br(), sprintf("(zip with %d files: %s)", length(d$files),
                                paste(d$files, collapse = ", "))))
  })

  output$data_boxes <- renderUI({
    p <- processed()
    df <- p$data
    times <- sort(unique(df$time))
    ncell <- nrow(unique(df[c("latitude", "longitude")]))
    layout_column_wrap(
      width = 1/4, fill = FALSE,
      value_box("Grid cells", format(ncell, big.mark = ","),
                showcase = bs_icon("grid-3x3"), theme = "primary"),
      value_box("Time steps", format(length(times), big.mark = ","),
                showcase = bs_icon("clock"), theme = "teal",
                p(sprintf("%s to %s %s", fmt_time(min(times)), fmt_time(max(times)),
                          tz_label()))),
      value_box("Variables", nrow(p$meta), showcase = bs_icon("layers"),
                theme = "secondary"),
      value_box("Rows", format(nrow(df), big.mark = ","), showcase = bs_icon("table"),
                theme = "secondary",
                p(sprintf("%s missing values", format(sum(is.na(df[p$meta$name])),
                                                       big.mark = ","))))
    )
  })

  output$var_table <- renderTable({
    p <- processed()
    df <- p$data
    stat <- function(x, f) if (all(is.na(x))) "NA" else sprintf("%.2f", f(x, na.rm = TRUE))
    data.frame(
      Name = p$meta$name, Description = p$meta$long_name, Units = p$meta$units,
      Min  = vapply(p$meta$name, function(v) stat(df[[v]], min), ""),
      Mean = vapply(p$meta$name, function(v) stat(df[[v]], mean), ""),
      Max  = vapply(p$meta$name, function(v) stat(df[[v]], max), ""),
      check.names = FALSE)
  }, striped = TRUE, spacing = "s")

  output$preview <- renderDT({
    df <- utils::head(processed()$data, 1000)
    df$time <- fmt_time(df$time)
    names(df)[names(df) == "time"] <- paste0("time_", tolower(tz_label()))
    num <- vapply(df, is.numeric, logical(1))
    df[num] <- lapply(df[num], round, 3)
    datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$download_ui <- renderUI({
    req(dataset())
    tagList(
      tags$label(class = "form-label", "Download"),
      downloadButton("dl_csv", "CSV (as shown)", class = "btn-sm btn-outline-primary mb-2 w-100"),
      downloadButton("dl_nc", "Original file from the CDS",
                     class = "btn-sm btn-outline-secondary w-100"),
      div(class = "small text-muted mt-2",
          "The CSV has one row per grid cell and time step, one column per ",
          "variable, in the units shown in the variable table. The time column is ",
          "named time_utc or time_sast to match the choice above.")
    )
  })

  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(dataset()$name),
             if (isTRUE(input$convert)) "_converted",
             if (tz_label() == "SAST") "_SAST", ".csv")
    },
    content = function(file) {
      df <- processed()$data
      df$time <- fmt_time(df$time, "%Y-%m-%d %H:%M:%S")
      names(df)[names(df) == "time"] <- paste0("time_", tolower(tz_label()))
      num <- vapply(df, is.numeric, logical(1))
      df[num] <- lapply(df[num], round, 4)
      utils::write.csv(df, file, row.names = FALSE, na = "")
    }
  )

  output$dl_nc <- downloadHandler(
    filename = function() dataset()$name,
    content = function(file) file.copy(dataset()$path, file)
  )

  # Explore ---------------------------------------------------------------------------
  observeEvent(processed(), {
    p <- processed()
    ch <- var_choices(p$meta)
    cur <- isolate(input$ex_var)
    first <- intersect(c("t2m", unname(ch)), unname(ch))[1]
    updateSelectInput(session, "ex_var", choices = ch,
                      selected = if (isTRUE(cur %in% ch)) cur else first)
    cells <- unique(p$data[c("latitude", "longitude")])
    ids <- sprintf("%.2f_%.2f", cells$latitude, cells$longitude)
    labs <- sprintf("Lat %.2f, Lon %.2f", cells$latitude, cells$longitude)
    cur_cell <- isolate(input$ts_cell)
    updateSelectInput(session, "ts_cell", choices = setNames(ids, labs),
                      selected = if (isTRUE(cur_cell %in% ids)) cur_cell else ids[ceiling(length(ids) / 2)])
  })

  times_all <- reactive(sort(unique(processed()$data$time)))

  output$time_slider_ui <- renderUI({
    tt <- times_all()
    req(length(tt) > 0)
    if (length(tt) == 1) {
      return(div(class = "small text-muted mb-2",
                 sprintf("Single time step: %s %s", fmt_time(tt), tz_label())))
    }
    tagList(
      div(class = "small", textOutput("time_label", inline = TRUE)),
      sliderInput("t_idx", NULL, min = 1, max = length(tt), value = 1, step = 1,
                  ticks = FALSE, width = "100%",
                  animate = animationOptions(interval = 900)))
  })

  current_time <- reactive({
    tt <- times_all()
    i <- input$t_idx %||% 1
    tt[min(max(1, i), length(tt))]
  })

  output$time_label <- renderText({
    sprintf("Time step %d of %d: %s %s (press play to animate)",
            match(current_time(), times_all()), length(times_all()),
            fmt_time(current_time()), tz_label())
  })

  output$slice_map <- renderLeaflet({
    df <- processed()$data
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      fitBounds(min(df$longitude) - grid_res, min(df$latitude) - grid_res,
                max(df$longitude) + grid_res, max(df$latitude) + grid_res)
  })

  outputOptions(output, "slice_map", suspendWhenHidden = FALSE)

  observeEvent(input$tabs, {
    req(input$tabs == "3. Explore", dataset())
    df <- processed()$data
    leafletProxy("slice_map") %>%
      fitBounds(min(df$longitude) - grid_res, min(df$latitude) - grid_res,
                max(df$longitude) + grid_res, max(df$latitude) + grid_res)
  })

  observe({
    p <- processed()
    v <- input$ex_var
    req(v %in% names(p$data))
    t <- current_time()
    sl <- p$data[p$data$time == t, c("latitude", "longitude", v)]
    names(sl)[3] <- "value"
    if (nrow(sl) > max_map_cells) {
      showNotification(sprintf("Showing the first %d of %d cells.", max_map_cells, nrow(sl)),
                       type = "warning", id = "cells_cap")
      sl <- sl[seq_len(max_map_cells), ]
    }
    units <- p$meta$units[p$meta$name == v]
    pl <- pal_for(v, units)
    domain <- if (isTRUE(input$fixed_scale)) range(p$data[[v]], na.rm = TRUE)
              else range(sl$value, na.rm = TRUE)
    if (!all(is.finite(domain))) domain <- c(0, 1)
    if (diff(domain) == 0) domain <- domain + c(-0.5, 0.5)
    pal <- colorNumeric(pl$name, domain = domain, reverse = pl$rev, na.color = "transparent")
    h <- grid_res / 2
    sl$id <- sprintf("%.2f_%.2f", sl$latitude, sl$longitude)
    leafletProxy("slice_map") %>%
      clearGroup("cells") %>% clearControls() %>%
      addRectangles(data = sl, lng1 = ~longitude - h, lat1 = ~latitude - h,
                    lng2 = ~longitude + h, lat2 = ~latitude + h, layerId = ~id,
                    group = "cells", stroke = TRUE, color = "white", weight = 0.4,
                    fillColor = ~pal(value), fillOpacity = 0.85,
                    label = ~sprintf("Lat %.2f, Lon %.2f: %.2f %s", latitude, longitude,
                                     value, units)) %>%
      addLegend("bottomright", pal = pal, values = domain,
                title = sprintf("%s (%s)<br>%s %s", v, units, fmt_time(t),
                                tz_label()),
                opacity = 1)
  })

  observe({
    id <- input$ts_cell
    req(id)
    ll <- as.numeric(strsplit(id, "_")[[1]])
    h <- grid_res / 2
    leafletProxy("slice_map") %>% clearGroup("sel") %>%
      addRectangles(ll[2] - h, ll[1] - h, ll[2] + h, ll[1] + h, group = "sel",
                    color = "#111111", weight = 2.5, fill = FALSE)
  })

  observeEvent(input$slice_map_shape_click, {
    id <- input$slice_map_shape_click$id
    if (!is.null(id)) updateSelectInput(session, "ts_cell", selected = id)
  })

  output$ts_plot <- renderPlotly({
    p <- processed()
    v <- input$ex_var
    id <- input$ts_cell
    req(v %in% names(p$data), id)
    ll <- as.numeric(strsplit(id, "_")[[1]])
    df <- p$data
    units <- p$meta$units[p$meta$name == v]

    # Values for the hour ending at the stamp belong to the hour before it
    shift <- if (v %in% period_vars) 3600 else 0
    agg <- function(time, x) {
      if (input$ts_agg == "none") {
        return(data.frame(time = fmt_time(time, "%Y-%m-%d %H:%M:%S"), value = x))
      }
      day <- as.Date(time - shift, tz = tz_name())
      f <- switch(input$ts_agg, mean = mean, min = min, max = max, sum = sum)
      a <- tapply(x, day, function(z) if (all(is.na(z))) NA_real_ else f(z, na.rm = TRUE))
      n <- tapply(x, day, function(z) sum(!is.na(z)))
      # Leave out partial days at the ends of the period (fewer hours than the rest)
      full <- n >= max(n)
      data.frame(time = names(a)[full], value = as.numeric(a)[full])
    }

    sel <- abs(df$latitude - ll[1]) < 1e-6 & abs(df$longitude - ll[2]) < 1e-6
    s1 <- agg(df$time[sel], df[[v]][sel])
    plt <- plot_ly() %>%
      add_lines(data = s1, x = ~time, y = ~value, name = sprintf("Lat %.2f, Lon %.2f", ll[1], ll[2]),
                line = list(color = "#1F4E79", width = 1.6))
    if (isTRUE(input$ts_boxmean)) {
      tt <- sort(unique(df$time))
      bm <- vapply(split(df[[v]], factor(match(df$time, tt), levels = seq_along(tt))),
                   function(z) if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE), 0)
      s2 <- agg(tt, bm)
      plt <- plt %>% add_lines(data = s2, x = ~time, y = ~value, name = "Box mean",
                               line = list(color = "#E67E22", width = 1.4, dash = "dot"))
    }
    plt %>%
      layout(xaxis = list(type = "date",
                          title = if (input$ts_agg == "none") sprintf("Time (%s)", tz_label())
                                  else sprintf("Date (%s days)", tz_label())),
             yaxis = list(title = sprintf("%s (%s)", v, units)),
             legend = list(orientation = "h", y = 1.08), hovermode = "x unified") %>%
      config(displaylogo = FALSE)
  })
}

shinyApp(ui, server)
