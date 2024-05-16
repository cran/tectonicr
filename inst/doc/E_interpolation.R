## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tectonicr)
library(ggplot2) # load ggplot library

## ----load_data----------------------------------------------------------------
data("san_andreas")

data("cpm_models")
por <- cpm_models |>
  subset(model == "NNR-MORVEL56") |>
  equivalent_rotation("na", "pa")

## ----interpolation------------------------------------------------------------
mean_SH <- stress2grid(san_andreas, gridsize = 1, R_range = seq(50, 350, 100))

## ----plot, warning=FALSE, message=FALSE---------------------------------------
trajectories <- eulerpole_loxodromes(x = por, n = 40, cw = FALSE)

ggplot(mean_SH) +
  geom_sf(data = trajectories, lty = 2) +
  geom_spoke(data = san_andreas, aes(lon, lat, angle = deg2rad(90 - azi)), radius = .5, color = "grey30", position = "center_spoke") +
  geom_spoke(aes(lon, lat, angle = deg2rad(90 - azi), alpha = sd, color = mdr), radius = 1, position = "center_spoke", lwd = 1) +
  coord_sf(xlim = range(san_andreas$lon), ylim = range(san_andreas$lat)) +
  scale_alpha(name = "Standard deviation", range = c(1, .25)) +
  scale_color_viridis_c(
    "Wavelength\n(R-normalized mean distance)",
    limits = c(0, 1),
    breaks = seq(0, 1, .25)
  ) +
  facet_wrap(~R)

## ----interpolation_PoR--------------------------------------------------------
mean_SH_PoR <- PoR_stress2grid(san_andreas, PoR = por, gridsize = 1, R_range = seq(50, 350, 100))

## ----plot2, warning=FALSE, message=FALSE--------------------------------------
ggplot(mean_SH_PoR) +
  geom_sf(data = trajectories, lty = 2) +
  geom_spoke(data = san_andreas, aes(lon, lat, angle = deg2rad(90 - azi)), radius = .5, color = "grey30", position = "center_spoke") +
  geom_spoke(aes(lon, lat, angle = deg2rad(90 - azi), alpha = sd, color = mdr), radius = 1, position = "center_spoke", lwd = 1) +
  coord_sf(xlim = range(san_andreas$lon), ylim = range(san_andreas$lat)) +
  scale_alpha(name = "Standard deviation", range = c(1, .25)) +
  scale_color_viridis_c(
    "Wavelength\n(R-normalized mean distance)",
    limits = c(0, 1),
    breaks = seq(0, 1, .25)
  ) +
  facet_wrap(~R)

## ----compact------------------------------------------------------------------
mean_SH_PoR_reduced <- mean_SH_PoR |>
  compact_grid() |>
  dplyr::mutate(cdist = circular_distance(azi.PoR, 135))

## ----voronoi, warning=FALSE, message=FALSE------------------------------------
ggplot(mean_SH_PoR_reduced) +
  ggforce::geom_voronoi_tile(
    aes(lon, lat, fill = cdist),
    max.radius = .7, normalize = FALSE
  ) +
  scale_fill_viridis_c("Angular distance", limits = c(0, 1)) +
  geom_sf(data = trajectories, lty = 2) +
  geom_spoke(
    aes(lon, lat, angle = deg2rad(90 - azi), alpha = sd),
    radius = .5, position = "center_spoke", lwd = .2, colour = "white"
  ) +
  scale_alpha("Standard deviation", range = c(1, .25)) +
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

