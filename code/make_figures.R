# make_figures.R
# Recreates the figures in figures/ that the slides use. Run from the workshop
# folder (the one that holds code/, data/ and figures/), after 04_example_data.R
# has downloaded the data.

# Work from the workshop folder, also when this script is started from code/
if (basename(getwd()) == "code") setwd("..")

source(file.path("code", "05_visualise.R"))   # gsod_missing, era5_tmax_map, era5_diurnal_terra,
                                               # ts_ and scatter_station_vs_era5,
                                               # era5_precip_map
source(file.path("code", "06_gam_model.R"))   # gam_smooths, cdd_station_vs_era5
