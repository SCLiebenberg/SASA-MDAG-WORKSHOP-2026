# 02_download_era5.R
# Request ERA5 hourly data on single levels from the Copernicus Climate Data
# Store (CDS) with ecmwfr. This is the request the ERA5 app builds and submits.
#
# Before the first run:
#   1. Register at https://cds.climate.copernicus.eu
#   2. Accept the ERA5 licence at the bottom of the dataset's download page
#   3. Store your personal access token (from your CDS profile page) once:

# install.packages("ecmwfr")
library(ecmwfr)

# wf_set_key(key = "YOUR-PERSONAL-ACCESS-TOKEN")   # once per computer; keep it out of shared scripts

# 1. The request -------------------------------------------------------------------
# The same fields as the CDS download form. Times are UTC.
# area = North, West, South, East in degrees. The values are grid-point
# coordinates on the 0.25 degree ERA5 grid, and points on the edges are included:
# this box gives 5 x 5 = 25 grid points.
request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type       = "reanalysis",
  variable           = c("2m_temperature", "2m_dewpoint_temperature",
                         "10m_u_component_of_wind", "10m_v_component_of_wind",
                         "total_precipitation"),
  year               = "2025",
  month              = "01",
  day                = sprintf("%02d", 1:31),
  time               = sprintf("%02d:00", 0:23),
  data_format        = "netcdf",
  download_format    = "unarchived",
  area               = c(-26.25, 26.75, -27.25, 27.75),
  target             = "era5_potch_2025_01.nc"
)

# 2. Submit and download -----------------------------------------------------------
# The request waits in the CDS queue, then the file is saved in `path`.
# Mixing instantaneous variables (temperature, wind) with accumulated ones
# (precipitation) makes the CDS return a zip with one NetCDF file per type;
# ecmwfr then saves it with a .zip extension.
file <- wf_request(request, path = ".")
file
