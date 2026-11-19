# 05_visualise.R
# Visualising the station and ERA5 data with terra and ggplot2.
# Input: the files prepared by 04_example_data.R.
# Output: figures/*.pdf for the slides, and data/processed/station_era5_nov2023.csv
# for the model in 06_gam_model.R.

if (basename(getwd()) == "code") setwd("..")

library(terra)
library(ncdf4)
library(ggplot2)

raw_dir  <- file.path("data", "raw")
era5_dir <- file.path(raw_dir, "era5_nov2023")
out_dir  <- file.path("data", "processed")
fig_dir  <- "figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, showWarnings = FALSE)

nwu <- c(purple = "#6C3D91", teal = "#00889C", grey = "#78848E")
theme_set(theme_minimal(base_size = 11))

# 1. ERA5 hourly fields with terra ----------------------------------------------------------
f_inst  <- list.files(era5_dir, pattern = "instant\\.nc$", full.names = TRUE)
f_accum <- list.files(era5_dir, pattern = "accum\\.nc$", full.names = TRUE)

# Hourly time stamps (UTC) from the NetCDF time dimension. (Some terra and GDAL
# versions do not recognise the "valid_time" dimension of new CDS files, so
# time(r) would be NA.)
nc_times <- function(f) {
  nc <- nc_open(f)
  on.exit(nc_close(nc))
  as.POSIXct(nc$dim$valid_time$vals, origin = "1970-01-01", tz = "UTC")
}

t2m <- rast(f_inst, subds = "t2m") - 273.15        # K to degrees C
time(t2m) <- nc_times(f_inst)
tp  <- rast(f_accum, subds = "tp") * 1000          # m to mm, total for the hour ending at the stamp
time(tp) <- nc_times(f_accum)
t2m                                                # 46 x 76 cells, 720 hourly layers

# Daily values on UTC days (the day GSOD uses)
day  <- format(time(t2m), "%Y-%m-%d")
tmax <- tapp(t2m, index = day, fun = max)
tmean <- tapp(t2m, index = day, fun = mean)
time(tmax) <- time(tmean) <- as.Date(sort(unique(day)))

# Precipitation: the 00:00 value is the last hour of the previous day
pday <- format(time(tp) - 3600, "%Y-%m-%d")
full <- names(which(table(pday) == 24))            # drop 31 Oct and 30 Nov (incomplete)
prcp <- tapp(tp[[pday %in% full]], index = pday[pday %in% full], fun = sum)
time(prcp) <- as.Date(full)

# 2. Station data -----------------------------------------------------------------------------
gsod <- read.csv(file.path(raw_dir, "gsod_sa_2023.csv"), colClasses = c(STNID = "character"))
gsod$YEARMODA <- as.Date(gsod$YEARMODA)
gsod <- gsod[format(gsod$YEARMODA, "%Y-%m") == "2023-11", ]

stn_df <- unique(gsod[c("STNID", "NAME", "LATITUDE", "LONGITUDE", "ELEVATION")])
names(stn_df) <- c("STNID", "NAME", "lat", "lon", "elev")
stn_df <- stn_df[!duplicated(stn_df$STNID), ]      # one row per station
# Some names occur twice (e.g. two VEREENIGING stations), so label with the ID too
stn_df$label <- paste0(stn_df$NAME, " (", stn_df$STNID, ")")
nrow(stn_df)

# Missing days: a day without reports has no row at all in GSOD
days <- seq(as.Date("2023-11-01"), as.Date("2023-11-30"), by = "day")
cal  <- expand.grid(STNID = stn_df$STNID, date = days, stringsAsFactors = FALSE)
have <- paste(gsod$STNID, gsod$YEARMODA)[!is.na(gsod$MAX)]
cal$status <- ifelse(paste(cal$STNID, cal$date) %in% have, "Observed", "Missing")
cal$label  <- factor(stn_df$label[match(cal$STNID, stn_df$STNID)],
                     levels = stn_df$label[order(stn_df$lat)])   # south at the bottom
table(cal$status)

p_miss <- ggplot(cal, aes(date, label, fill = status)) +
  geom_tile() +
  scale_fill_manual(values = c(Observed = unname(nwu["teal"]), Missing = unname(nwu["purple"]))) +
  labs(x = NULL, y = sprintf("%d stations, south to north", nrow(stn_df)), fill = NULL,
       title = "GSOD daily maximum temperature, November 2023") +
  theme(legend.position = "bottom", panel.grid = element_blank(),
        axis.text.y = element_blank())
ggsave(file.path(fig_dir, "gsod_missing.pdf"), p_miss, width = 6, height = 6.5)

# 3. Maps -----------------------------------------------------------------------------------------
# ggplot2: November mean of daily maxima, with the stations
tmax_month <- mean(tmax)
df <- as.data.frame(tmax_month, xy = TRUE)
names(df)[3] <- "tmax"

p_map <- ggplot(df, aes(x, y, fill = tmax)) +
  geom_raster() +
  scale_fill_viridis_c(option = "inferno", name = "Tmax (\u00b0C)") +
  geom_point(data = stn_df, aes(lon, lat), inherit.aes = FALSE,
             shape = 21, fill = "white", size = 1.5) +
  coord_equal(expand = FALSE) +
  labs(x = NULL, y = NULL, title = "ERA5 mean of daily maxima, November 2023")
p_map
ggsave(file.path(fig_dir, "era5_tmax_map.pdf"), p_map, width = 6, height = 4.2)

# terra: the daily cycle on one day, 00, 06, 12 and 18 UTC (02, 08, 14, 20 SAST)
one_day <- t2m[[format(time(t2m), "%Y-%m-%d") == "2023-11-15" &
                format(time(t2m), "%H") %in% c("00", "06", "12", "18")]]
names(one_day) <- format(time(one_day), "%d %b %H:%M UTC")
pdf(file.path(fig_dir, "era5_diurnal_terra.pdf"), width = 9, height = 5.5)
plot(one_day, col = hcl.colors(100, "Inferno"), range = range(minmax(one_day)),
     main = paste(names(one_day), "(\u00b0C)"),
     fun = function() points(stn_df$lon, stn_df$lat, pch = 20, cex = 0.5))
dev.off()

# terra: total precipitation for the month
prcp_month <- sum(prcp)
pdf(file.path(fig_dir, "era5_precip_map.pdf"), width = 6, height = 4)
plot(prcp_month, col = hcl.colors(100, "Blues 3", rev = TRUE),
     main = "ERA5 total precipitation (mm), 1 to 29 November 2023")
points(stn_df$lon, stn_df$lat, pch = 21, bg = "white", cex = 0.7)
dev.off()

# 4. ERA5 at the stations -----------------------------------------------------------------
pts <- vect(stn_df, geom = c("lon", "lat"), crs = "EPSG:4326", keepgeom = TRUE)

at_stations <- function(r, method = "simple") {
  e <- as.matrix(extract(r, pts, method = method, ID = FALSE))
  data.frame(STNID = rep(stn_df$STNID, times = ncol(e)),
             date  = rep(time(r), each = nrow(e)),
             value = as.vector(e))
}
key <- function(d) paste(d$STNID, d$date)

dat <- expand.grid(STNID = stn_df$STNID, date = days, stringsAsFactors = FALSE)
dat <- cbind(dat, stn_df[match(dat$STNID, stn_df$STNID), c("NAME", "lat", "lon", "elev")])
rownames(dat) <- NULL
g <- gsod[match(key(dat), key(data.frame(STNID = gsod$STNID, date = gsod$YEARMODA))), ]
dat$tmax_stn  <- g$MAX
dat$tmean_stn <- g$TEMP
dat$prcp_stn  <- g$PRCP

e <- at_stations(tmax);                       dat$tmax_era5     <- e$value[match(key(dat), key(e))]
e <- at_stations(tmax, "bilinear");           dat$tmax_era5_bil <- e$value[match(key(dat), key(e))]
e <- at_stations(tmean);                      dat$tmean_era5    <- e$value[match(key(dat), key(e))]
e <- at_stations(prcp);                       dat$prcp_era5     <- e$value[match(key(dat), key(e))]

# ERA5 covers land and sea, so only stations outside the ERA5 grid would get NA here
dat <- dat[order(dat$STNID, dat$date), ]
write.csv(dat, file.path(out_dir, "station_era5_nov2023.csv"), row.names = FALSE, na = "")

# 5. Station versus ERA5 ---------------------------------------------------------------------
dat$diff <- dat$tmax_era5 - dat$tmax_stn
round(c(mean_bias = mean(dat$diff, na.rm = TRUE),
        rmse = sqrt(mean(dat$diff^2, na.rm = TRUE)),
        cor  = cor(dat$tmax_stn, dat$tmax_era5, use = "complete.obs"),
        cor_bias_elevation = cor(dat$diff, dat$elev, use = "complete.obs")), 2)

# One station: the hourly ERA5 series, its daily maximum and the station maximum.
# The example station is the one closest to Potchefstroom with a complete month.
n_obs <- tapply(!is.na(dat$tmax_stn), dat$STNID, sum)
cand  <- stn_df[stn_df$STNID %in% names(n_obs)[n_obs >= 25], ]
ex    <- cand[which.min((cand$lat + 26.71)^2 + (cand$lon - 27.10)^2), ]
ex

hourly <- data.frame(time = time(t2m),
                     t2m = as.vector(as.matrix(extract(t2m, vect(ex, geom = c("lon", "lat"),
                                                                 crs = "EPSG:4326"), ID = FALSE))))
d_ex <- dat[dat$STNID == ex$STNID, ]
d_ex$time <- as.POSIXct(paste(d_ex$date, "12:00"), tz = "UTC")

p_ts <- ggplot() +
  geom_line(data = hourly, aes(time, t2m, colour = "ERA5 hourly"), linewidth = 0.25) +
  geom_line(data = d_ex, aes(time, tmax_era5, colour = "ERA5 daily max"), linewidth = 0.6) +
  geom_point(data = d_ex, aes(time, tmax_stn, colour = "Station daily max"), size = 1.4) +
  scale_colour_manual(values = c(`ERA5 hourly` = "grey75", `ERA5 daily max` = unname(nwu["teal"]),
                                 `Station daily max` = unname(nwu["purple"]))) +
  labs(x = NULL, y = "Temperature (\u00b0C)", colour = NULL,
       title = sprintf("%s, November 2023 (UTC)", ex$NAME)) +
  theme(legend.position = "bottom")
p_ts
ggsave(file.path(fig_dir, "ts_station_vs_era5.pdf"), p_ts, width = 6, height = 3.4)

# All stations and days: is the gap larger at high altitude?
p_sc <- ggplot(dat[!is.na(dat$tmax_stn) & !is.na(dat$tmax_era5), ],
               aes(tmax_stn, tmax_era5, colour = elev)) +
  geom_point(alpha = 0.5, size = 0.8) +
  geom_abline(linetype = "dashed") +
  scale_colour_viridis_c(name = "Elevation (m)") +
  coord_equal() +
  labs(x = "Station Tmax (\u00b0C)", y = "ERA5 Tmax (\u00b0C)",
       title = "Daily maxima, November 2023 (dashed: 1:1)")
p_sc
ggsave(file.path(fig_dir, "scatter_station_vs_era5.pdf"), p_sc, width = 5, height = 4.4)
