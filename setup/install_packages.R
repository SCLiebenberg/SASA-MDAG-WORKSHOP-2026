# install_packages.R
# Install the R packages for the workshop "Sourcing Multivariate Climate Data" (SASA MDAG).
#
# Run this script one time before the workshop.
# It installs a package only when it is missing or older than the minimum version.
# At the end, it prints a check table. Every line must show "OK".

# Packages, minimum versions (NA = any version) and where they are used --------------------------
pkgs <- data.frame(
  package = c(
    # Data access
    "GSODR",    "ecmwfr",
    # Climate data processing and figures
    "ncdf4",    "terra",    "ggplot2",  "mgcv",
    # Shiny apps
    "shiny",    "bslib",    "bsicons",  "leaflet",  "plotly",   "DT",
    "dplyr",    "httr"
  ),
  min_version = c(
    NA,         "2.0.0",
    NA,         NA,         NA,         NA,
    NA,         "0.7.0",    NA,         NA,         NA,         NA,
    NA,         NA
  ),
  used_in = c(
    "scripts 01, 04; GSOD app",
    "script 02; ERA5 app (2.0.0 or newer for the new CDS)",
    "scripts 03, 05; ERA5 app",
    "script 05",
    "scripts 05, 06",
    "script 06 (included with R)",
    "both apps",
    "both apps (0.7.0 or newer)",
    "both apps",
    "both apps (maps)",
    "both apps (interactive graphs)",
    "both apps (tables)",
    "GSOD app",
    "ERA5 app"
  ),
  stringsAsFactors = FALSE
)

repos <- "https://cloud.r-project.org"

# Find the packages to install ----------------------------------------------------------------------
needs_install <- function(p, min_v) {
  if (!requireNamespace(p, quietly = TRUE)) return(TRUE)
  !is.na(min_v) && utils::packageVersion(p) < min_v
}
todo <- pkgs$package[mapply(needs_install, pkgs$package, pkgs$min_version)]

if (length(todo) > 0) {
  message("Installing: ", paste(todo, collapse = ", "))
  install.packages(todo, repos = repos)
} else {
  message("All packages are already installed.")
}

# Check -----------------------------------------------------------------------------------------------
status <- vapply(seq_len(nrow(pkgs)), function(i) {
  p <- pkgs$package[i]
  if (!requireNamespace(p, quietly = TRUE)) return("FAILED: not installed")
  v <- as.character(utils::packageVersion(p))
  if (!is.na(pkgs$min_version[i]) && utils::packageVersion(p) < pkgs$min_version[i])
    return(sprintf("TOO OLD: %s (need %s)", v, pkgs$min_version[i]))
  sprintf("OK      %s", v)
}, character(1))

cat("\nSetup check\n-----------\n")
cat(sprintf("%-9s %s\n", "R", R.version.string))
cat(sprintf("%-9s %-24s %s\n", pkgs$package, status, pkgs$used_in), sep = "")

if (getRversion() < "4.2.0" && .Platform$OS.type == "windows") {
  cat("\nNote: R 4.2 or newer is recommended on Windows, so that the degree sign shows correctly.\n")
}
if (any(!startsWith(status, "OK"))) {
  cat("\nSome packages failed. Run this script again.",
      "If a package still fails, install it alone with install.packages(\"<name>\")",
      "and read the error message.\n")
} else {
  cat("\nAll packages are ready.\n")
  cat("Before the ERA5 part: make a CDS account, accept the dataset licence and save your token",
      "(see code/02_download_era5.R).\n")
}
