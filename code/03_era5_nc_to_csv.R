# 03_era5_nc_to_csv.R
# Turn the ERA5 NetCDF from the CDS (or the zip holding several NetCDF files)
# into one CSV: one row per grid point and hour, one column per variable.
# This is what the ERA5 app does when you press the CSV button.

# install.packages("ncdf4")
library(ncdf4)

file <- "era5_potch_2025_01.zip"     # or .nc, as returned by 02_download_era5.R

# 1. Unzip if needed -----------------------------------------------------------------
if (grepl("\\.zip$", file)) {
  unzip(file, exdir = "era5_unzipped")
  nc_files <- list.files("era5_unzipped", pattern = "\\.nc$", full.names = TRUE)
} else {
  nc_files <- file
}
nc_files

# 2. Read one NetCDF file into a table ---------------------------------------------
nc_to_table <- function(f) {
  nc <- nc_open(f)
  on.exit(nc_close(nc))

  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  # New CDS files call the time dimension valid_time: seconds since 1970-01-01, UTC
  tim <- as.POSIXct(ncvar_get(nc, "valid_time"), origin = "1970-01-01", tz = "UTC")

  # One row per combination; longitude varies fastest, then latitude, then time.
  # That is the same order as the values in each variable's array.
  out <- expand.grid(longitude = lon, latitude = lat, time = tim)

  # Every variable on the (longitude, latitude, time) grid
  for (v in names(nc$var)) {
    dims <- sapply(nc$var[[v]]$dim, function(d) d$name)
    if (all(c("longitude", "latitude", "valid_time") %in% dims)) {
      out[[v]] <- as.vector(ncvar_get(nc, v, collapse_degen = FALSE))
    }
  }
  out
}

parts <- lapply(nc_files, nc_to_table)
era5  <- Reduce(function(a, b) merge(a, b, by = c("time", "latitude", "longitude")), parts)
head(era5)

# 3. Units ----------------------------------------------------------------------------
# ERA5 arrives in SI units: temperatures in kelvin, precipitation in metres
# (the total for the hour ending at the time stamp), pressure in pascal.
era5$t2m <- era5$t2m - 273.15          # K to degrees C
era5$d2m <- era5$d2m - 273.15
era5$tp  <- era5$tp * 1000             # m to mm
era5$ws10 <- sqrt(era5$u10^2 + era5$v10^2)   # wind speed from its components (m/s)

# 4. Save -----------------------------------------------------------------------------
era5 <- era5[order(era5$time, -era5$latitude, era5$longitude), ]
era5$time <- format(era5$time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
names(era5)[names(era5) == "time"] <- "time_utc"
write.csv(era5, "era5_potch_2025_01.csv", row.names = FALSE)
