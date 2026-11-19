# 01_download_gsod.R
# Download GSOD daily data for weather stations near a point with GSODR,
# and save it as CSV. This is the core of what the GSOD app does.

# install.packages("GSODR")
library(GSODR)

# 1. Station list --------------------------------------------------------------
# GSODR sends NOAA's station list (isd-history) as a data file.
load(system.file("extdata", "isd_history.rda", package = "GSODR"))
head(isd_history)    # STNID, NAME, LAT, LON, BEGIN, END, COUNTRY_NAME, ...

# Stations within 50 km of Potchefstroom 
lat0 <- -26.71
lon0 <- 27.10
rad  <- pi / 180
dist_km <- 6371 * acos(pmin(1,
  sin(lat0 * rad) * sin(isd_history$LAT * rad) +
  cos(lat0 * rad) * cos(isd_history$LAT * rad) * cos((isd_history$LON - lon0) * rad)))

near <- isd_history[!is.na(dist_km) & dist_km <= 50, ]
near$dist_km <- round(dist_km[!is.na(dist_km) & dist_km <= 50], 1)
near[order(near$dist_km), c("STNID", "NAME", "LAT", "LON", "BEGIN", "END", "dist_km")]

# 2. Download ----------------------------------------------------------------------
# One file per station per year is fetched from NOAA. GSODR converts the
# values to metric units (degrees C, mm, m/s, hPa, km) and replaces the
# GSOD missing-value codes (9999.9 and similar) with NA.
stations <- near$STNID
gsod <- get_GSOD(years = 2024, station = stations)

str(gsod)
table(gsod$NAME)

# 3. Missing days ------------------------------------------------------------------
# A day without reports has no row at all, so compare with the full calendar.
all_days <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
for (id in unique(gsod$STNID)) {
  have <- gsod$YEARMODA[gsod$STNID == id]
  cat(id, ":", sum(!all_days %in% have), "of", length(all_days), "days missing\n")
}

# 4. Save ----------------------------------------------------------------------------
write.csv(gsod, "GSOD_2024.csv", row.names = FALSE, na = "")
