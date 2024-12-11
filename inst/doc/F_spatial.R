## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(tectonicr)
library(ggplot2) # load ggplot library

## ----load_data----------------------------------------------------------------
data("san_andreas")

data("cpm_models")
por <- cpm_models |>
  subset(model == "NNR-MORVEL56") |>
  equivalent_rotation("na", "pa")

plate_boundary <- subset(plates, plates$pair == "na-pa")

## -----------------------------------------------------------------------------
circular_summary(san_andreas$azi, w = 1 / san_andreas$unc)

## ----interpolation,message=FALSE,warning=FALSE--------------------------------
spatial_stats_R <- PoR_stress2grid_stats(san_andreas, PoR = por, gridsize = 1, R_range = 100)
head(spatial_stats_R)

## ----interpolation2,message=FALSE,warning=FALSE-------------------------------
spatial_stats <- PoR_stress2grid_stats(san_andreas, PoR = por, gridsize = 1, R_range = seq(50, 350, 100), kappa = 10)

## ----plot, warning=FALSE, message=FALSE---------------------------------------
trajectories <- eulerpole_loxodromes(x = por, n = 40, cw = FALSE)

ggplot(spatial_stats) +
  geom_sf(data = plate_boundary, color = "red") +
  geom_sf(data = trajectories, lty = 2) +
  geom_spoke(data = san_andreas, aes(lon, lat, angle = deg2rad(90 - azi)), radius = .5, color = "grey30", position = "center_spoke") +
  geom_spoke(aes(lon, lat, angle = deg2rad(90 - mean), alpha = sd, color = mdr), radius = 1, position = "center_spoke", lwd = 1) +
  coord_sf(xlim = range(san_andreas$lon), ylim = range(san_andreas$lat)) +
  scale_alpha(name = "Standard deviation", range = c(1, .25)) +
  scale_color_viridis_c(
    "Wavelength\n(R-normalized mean distance)",
    limits = c(0, 1),
    breaks = seq(0, 1, .25)
  ) +
  facet_wrap(~R)

## -----------------------------------------------------------------------------
spatial_stats_comp <- spatial_stats |>
  compact_grid2(var)

## -----------------------------------------------------------------------------
ggplot(spatial_stats_comp) +
  geom_sf(data = plate_boundary, color = "red") +
  geom_sf(data = trajectories, lty = 2) +
  geom_spoke(data = san_andreas, aes(lon, lat, angle = deg2rad(90 - azi)), radius = .5, color = "grey30", position = "center_spoke") +
  geom_spoke(aes(lon, lat, angle = deg2rad(90 - median), alpha = `95%CI`, color = skewness), radius = 1, position = "center_spoke", lwd = 1) +
  coord_sf(xlim = range(san_andreas$lon), ylim = range(san_andreas$lat)) +
  scale_alpha(name = "95% CI", range = c(1, .25)) +
  scale_color_viridis_c(
    "Skewness"
  )

## -----------------------------------------------------------------------------
ggplot(spatial_stats_comp) +
  geom_sf(data = plate_boundary, color = "red") +
  geom_sf(data = trajectories, lty = 2) +
  geom_spoke(data = san_andreas, aes(lon, lat, angle = deg2rad(90 - azi)), radius = .5, color = "grey30", position = "center_spoke") +
  geom_spoke(aes(lon, lat, angle = deg2rad(90 - mode), alpha = `95%CI`, color = abs(kurtosis)), radius = 1, position = "center_spoke", lwd = 1) +
  coord_sf(xlim = range(san_andreas$lon), ylim = range(san_andreas$lat)) +
  scale_alpha(name = "95% CI", range = c(1, .25)) +
  scale_color_viridis_c(
    "|Kurtosis|"
  )

## ----variance, warning=FALSE, message=FALSE-----------------------------------
ggplot(spatial_stats_comp) +
  ggforce::geom_voronoi_tile(
    aes(lon, lat, fill = var),
    max.radius = .7, normalize = FALSE
  ) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  geom_sf(data = plate_boundary, color = "red") +
  geom_sf(data = trajectories, lty = 2) +
  geom_spoke(
    aes(lon, lat, angle = deg2rad(90 - mean)),
    radius = .5, position = "center_spoke", lwd = .2, colour = "white"
  ) +
  coord_sf(xlim = range(san_andreas$lon), ylim = range(san_andreas$lat))

## ----skew, warning=FALSE, message=FALSE---------------------------------------
ggplot(spatial_stats_comp) +
  ggforce::geom_voronoi_tile(
    aes(lon, lat, fill = skewness),
    max.radius = .7, normalize = FALSE
  ) +
  scale_fill_gradient2("|Skewness|", low = "#001260", mid = "#EBE5E0", high = "#590007") +
  geom_sf(data = plate_boundary, color = "red") +
  geom_sf(data = trajectories, lty = 2) +
  geom_spoke(
    aes(lon, lat, angle = deg2rad(90 - median), alpha = var),
    radius = .5, position = "center_spoke", lwd = .2, colour = "white"
  ) +
  scale_alpha("variance", range = c(1, 0)) +
  coord_sf(xlim = range(san_andreas$lon), ylim = range(san_andreas$lat))

## ----kurtosis, warning=FALSE, message=FALSE-----------------------------------
ggplot(spatial_stats_comp) +
  ggforce::geom_voronoi_tile(
    aes(lon, lat, fill = abs(kurtosis)),
    max.radius = .7, normalize = FALSE
  ) +
  scale_fill_viridis_c("|Kurtosis|", transform = "sqrt") +
  geom_sf(data = plate_boundary, color = "red") +
  geom_sf(data = trajectories, lty = 2) +
  geom_spoke(
    aes(lon, lat, angle = deg2rad(90 - mode), alpha = var),
    radius = .5, position = "center_spoke", lwd = .2, colour = "white"
  ) +
  scale_alpha("variance", range = c(1, 0)) +
  coord_sf(xlim = range(san_andreas$lon), ylim = range(san_andreas$lat))

## ----kernel_disp--------------------------------------------------------------
san_andreas_por <- san_andreas
san_andreas_por$azi <- PoR_shmax(san_andreas, por, "right")$azi.PoR # transform to PoR azimuth
san_andreas_por$prd <- 135 # test direction
san_andreas_kdisp <- kernel_dispersion(san_andreas_por, gridsize = 1, R_range = seq(50, 350, 100))
san_andreas_kdisp <- compact_grid(san_andreas_kdisp, "dispersion")

ggplot(san_andreas_kdisp) +
  ggforce::geom_voronoi_tile(
    aes(lon, lat, fill = stat),
    max.radius = .7, normalize = FALSE
  ) +
  scale_fill_viridis_c("Dispersion", limits = c(0, 1)) +
  geom_sf(data = trajectories, lty = 2) +
  geom_spoke(
    data = san_andreas,
    aes(lon, lat, angle = deg2rad(90 - azi), alpha = unc),
    radius = .5, position = "center_spoke", lwd = .2, colour = "white"
  ) +
  scale_alpha("Standard deviation", range = c(1, .25)) +
  coord_sf(xlim = range(san_andreas$lon), ylim = range(san_andreas$lat))

