# 04_example_data.R
# Data for the visualisation and modelling example.
#
#   * ERA5 hourly data on single levels for November 2023 over South Africa
#     (2m temperature, surface pressure, total precipitation, UV radiation),
#     downloaded once from the CDS website: nov2023.zip, placed in data/raw/
#   * GSOD daily data for November 2023 for the South African stations inside
#     the ERA5 area (NOAA, no queue)
#
# Run from the workshop folder (the one that holds code/ and data/).

if (basename(getwd()) == "code") setwd("..")

library(GSODR)

raw_dir <- file.path("data", "raw")        # downloads only; never edit these files
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

# ERA5: unzip the CDS download ---------------------------------------------------------
# Mixing instantaneous variables (temperature, pressure) with accumulated ones
# (precipitation, radiation) makes the CDS deliver a zip with one NetCDF file per type.
era5_dir <- file.path(raw_dir, "era5_nov2023")
if (!dir.exists(era5_dir)) unzip(file.path(raw_dir, "nov2023.zip"), exdir = era5_dir)
list.files(era5_dir)    # data_stream-oper_stepType-instant.nc and ...-accum.nc

# GSOD: South African stations inside the ERA5 area --------------------------------------
area <- c(N = -24.25, W = 14.25, S = -35.5, E = 33)

load(system.file("extdata", "isd_history.rda", package = "GSODR"))
isd <- as.data.frame(isd_history)
to_date <- function(x) as.Date(as.character(x), tryFormats = c("%Y%m%d", "%Y-%m-%d"))

stns <- isd[isd$CTRY == "SF" & !is.na(isd$LAT) & !is.na(isd$LON) &
              isd$LAT <= area["N"] & isd$LAT >= area["S"] &
              isd$LON >= area["W"] & isd$LON <= area["E"] &
              to_date(isd$BEGIN) <= as.Date("2023-11-01") &
              to_date(isd$END) >= as.Date("2023-11-30"), ]
nrow(stns)
head(stns[c("STNID", "NAME", "LAT", "LON", "BEGIN", "END")])

gsod_file <- file.path(raw_dir, "gsod_sa_2023.csv")
if (!file.exists(gsod_file)) {
  # One station at a time, so a station without a 2023 file is skipped, not fatal
  parts <- lapply(stns$STNID, function(id) {
    tryCatch(as.data.frame(get_GSOD(years = 2023, station = id)),
             error = function(e) { message(id, ": ", conditionMessage(e)); NULL })
  })
  gsod <- do.call(rbind, parts)
  write.csv(gsod, gsod_file, row.names = FALSE, na = "")
}

writeLines(c(paste("GSOD downloaded:", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
             paste("GSODR", packageVersion("GSODR")),
             "ERA5: CDS request dbea5d83-b20c-446b-8a13-c1ffcf5de099 (website, 2026-11-18)"),
           file.path(raw_dir, "download_log.txt"))
