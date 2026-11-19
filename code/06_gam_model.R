# 06_gam_model.R
# A simple modelling example: can ERA5 stand in for the stations?
#
# One month of data, many stations. For station i on day t,
#
#   T_stn(i, t) = b0 + f1(T_ERA5(i, t)) + f2(lon_i, lat_i) + f3(elev_i) + e(i, t),
#   e(i, t) = rho e(i, t - 1) + u(i, t)
#
#   f1: calibration of the ERA5 value
#   f2: spatial pattern of the bias (coast, interior, terrain)
#   f3: elevation, since a 28 km grid cell smooths out hills and valleys
#
# Input: data/processed/station_era5_nov2023.csv from 05_visualise.R.

if (basename(getwd()) == "code") setwd("..")

library(mgcv)
library(ggplot2)

fig_dir <- "figures"
nwu <- c(purple = "#6C3D91", teal = "#00889C", grey = "#78848E")

dat <- read.csv(file.path("data", "processed", "station_era5_nov2023.csv"),
                colClasses = c(STNID = "character"))
dat$date <- as.Date(dat$date)

# 1. Fit -------------------------------------------------------------------------------------------
fit_dat <- dat[!is.na(dat$tmax_stn) & !is.na(dat$tmax_era5) & !is.na(dat$elev), ]
fit_dat <- fit_dat[order(fit_dat$STNID, fit_dat$date), ]
# The AR(1) process restarts at each station and after every gap
fit_dat$start <- c(TRUE, fit_dat$STNID[-1] != fit_dat$STNID[-nrow(fit_dat)] |
                         diff(fit_dat$date) != 1)

f <- tmax_stn ~ s(tmax_era5) + s(lon, lat, k = 30) + s(elev, k = 5)

m0  <- gam(f, data = fit_dat, method = "REML")
r0  <- residuals(m0)
rho <- cor(r0[-1][!fit_dat$start[-1]], r0[-nrow(fit_dat)][!fit_dat$start[-1]])  # lag 1, within stations

m1 <- bam(f, data = fit_dat, rho = rho, AR.start = fit_dat$start)
summary(m1)
gam.check(m1)
# A low k-index here mostly reflects that days of the same station resemble each
# other (local effects the smooths cannot see); doubling k changes little.
round(c(stations = length(unique(fit_dat$STNID)), days = nrow(fit_dat), rho = rho,
        deviance_explained = summary(m1)$dev.expl), 3)

pdf(file.path(fig_dir, "gam_smooths.pdf"), width = 10, height = 3.4)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
plot(m1, select = 1, shade = TRUE, seWithMean = TRUE, scale = 0,
     xlab = "ERA5 Tmax (\u00b0C)", ylab = "f1", main = "Calibration")
abline(h = 0, lty = 3)
plot(m1, select = 2, scheme = 2, too.far = 0.1, xlab = "Longitude", ylab = "Latitude",
     main = "Spatial bias f2 (\u00b0C)")
points(unique(fit_dat[c("lon", "lat")]), pch = 20, cex = 0.5)
plot(m1, select = 3, shade = TRUE, seWithMean = TRUE, scale = 0,
     xlab = "Elevation (m)", ylab = "f3", main = "Elevation")
abline(h = 0, lty = 3)
dev.off()

# 2. Validation: leave stations out ----------------------------------------------------------------
# Can the model predict at a place without a station? Hold out whole stations,
# five groups in turn, and compare with using ERA5 as it is.
set.seed(1)
ids   <- unique(fit_dat$STNID)
group <- setNames(sample(rep(1:5, length.out = length(ids))), ids)

cv <- do.call(rbind, lapply(1:5, function(k) {
  test  <- group[fit_dat$STNID] == k
  train <- fit_dat[!test, ]
  # inside a function, refer to the column by name: bam() looks AR.start up in the data
  m <- bam(f, data = train, rho = rho, AR.start = start)
  o <- fit_dat$tmax_stn[test]
  data.frame(fold = k, stations = sum(group == k),
             rmse_era5 = sqrt(mean((fit_dat$tmax_era5[test] - o)^2)),
             rmse_gam  = sqrt(mean((predict(m, fit_dat[test, ]) - o)^2)))
}))
cv
colMeans(cv[, c("rmse_era5", "rmse_gam")])

# 3. From temperatures to a financial quantity ---------------------------------------------------
# November cooling degree days per station, CDD = sum(max(0, Tmean - 18)), from the
# station, from ERA5 as it is, and from ERA5 calibrated with the same model for the
# daily mean. Only days with a station value count, so all three cover the same days.
mdat <- dat[!is.na(dat$tmean_stn) & !is.na(dat$tmean_era5) & !is.na(dat$elev), ]
mdat <- mdat[order(mdat$STNID, mdat$date), ]
mdat$start <- c(TRUE, mdat$STNID[-1] != mdat$STNID[-nrow(mdat)] | diff(mdat$date) != 1)
fm <- tmean_stn ~ s(tmean_era5) + s(lon, lat, k = 30) + s(elev, k = 5)
mm <- bam(fm, data = mdat, rho = rho, AR.start = mdat$start)
mdat$tmean_gam <- predict(mm, mdat)

cdd <- function(x) sum(pmax(0, x - 18))
by_stn <- data.frame(
  STNID     = sort(unique(mdat$STNID)),
  station   = as.numeric(tapply(mdat$tmean_stn,  mdat$STNID, cdd)),
  era5      = as.numeric(tapply(mdat$tmean_era5, mdat$STNID, cdd)),
  era5_gam  = as.numeric(tapply(mdat$tmean_gam,  mdat$STNID, cdd)),
  days      = as.integer(table(mdat$STNID)))
by_stn <- by_stn[by_stn$days >= 25, ]             # stations with (nearly) the whole month

# Basis: mean absolute difference between the station index and each ERA5 version
round(c(era5 = mean(abs(by_stn$era5 - by_stn$station)),
        era5_gam = mean(abs(by_stn$era5_gam - by_stn$station))), 1)

cdd_long <- rbind(data.frame(station = by_stn$station, era5 = by_stn$era5, version = "ERA5 as it is"),
                  data.frame(station = by_stn$station, era5 = by_stn$era5_gam, version = "ERA5 + GAM"))
p_cdd <- ggplot(cdd_long, aes(station, era5, colour = version)) +
  geom_abline(linetype = "dashed") +
  geom_point(size = 1.6, alpha = 0.8) +
  scale_colour_manual(values = c(`ERA5 as it is` = unname(nwu["teal"]),
                                 `ERA5 + GAM` = unname(nwu["purple"]))) +
  coord_equal() +
  labs(x = "Station CDD (\u00b0C days)", y = "ERA5 CDD (\u00b0C days)", colour = NULL,
       title = "November 2023 cooling degree days, base 18 \u00b0C") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
p_cdd
ggsave(file.path(fig_dir, "cdd_station_vs_era5.pdf"), p_cdd, width = 4.8, height = 4.8)
