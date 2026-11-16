# Install the R packages needed for the
# "Sourcing Multivariate Climate Data" workshop (SASA MDAG).
# Run this once before the workshop. It only installs packages you do not have.

# TODO: confirm this list against the final GSOD and ERA5 Shiny apps
pkgs <- c(
  "shiny",     # both Shiny apps
  "ecmwfr",    # ERA5 requests to the Copernicus CDS
  "keyring",   # secure storage of the CDS API token
  "GSODR",     # GSOD station data  (TODO: check the GSOD app uses this)
  "leaflet",   # interactive station map
  "terra",     # gridded (raster) ERA5 data
  "sf",        # spatial vector data
  "ncdf4",     # reading NetCDF files
  "dplyr",
  "tidyr",
  "ggplot2"
)

missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing) > 0) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# Report
cat("\nSetup check\n-----------\n")
cat(sprintf("%-10s %s\n", "R", R.version.string))
for (p in pkgs) {
  ok <- requireNamespace(p, quietly = TRUE)
  cat(sprintf("%-10s %s\n", p, if (ok) "OK" else "FAILED"))
}
if (packageVersion("ecmwfr") < "2.0.0") {
  cat("\nYour ecmwfr version is too old for the current CDS. Run install.packages(\"ecmwfr\").\n")
}
