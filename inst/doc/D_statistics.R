## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=TRUE---------------------------------------------------------
library(tectonicr)
library(ggplot2) # load ggplot library

## ----mean, echo=TRUE----------------------------------------------------------
data("san_andreas")
circular_mean(san_andreas$azi)
circular_median(san_andreas$azi)

## ----weighted, echo=TRUE------------------------------------------------------
circular_mean(san_andreas$azi, 1 / san_andreas$unc)
circular_median(san_andreas$azi, 1 / san_andreas$unc)

## ----weighted_spread, echo=TRUE-----------------------------------------------
circular_sd(san_andreas$azi, 1 / san_andreas$unc) # standard deviation
circular_IQR(san_andreas$azi, 1 / san_andreas$unc) # interquartile range

## ----por, echo=TRUE-----------------------------------------------------------
data("cpm_models")
por <- equivalent_rotation(subset(cpm_models, model == "NNR-MORVEL56"), "na", "pa")
san_andreas.por <- PoR_shmax(san_andreas, por, type = "right")

## ----por_stats, echo=TRUE-----------------------------------------------------
circular_mean(san_andreas.por$azi.PoR, 1 / san_andreas$unc)
circular_sd(san_andreas.por$azi.PoR, 1 / san_andreas$unc)

circular_median(san_andreas.por$azi.PoR, 1 / san_andreas$unc)
circular_IQR(san_andreas.por$azi.PoR, 1 / san_andreas$unc)

## ----rose1, echo=TRUE---------------------------------------------------------
rose(san_andreas$azi, weights = 1 / san_andreas$unc, col = "grey", main = "North pole")

## ----rose2, echo=TRUE---------------------------------------------------------
rose(san_andreas.por$azi, weights = 1 / san_andreas$unc, col = "grey", main = "PoR")

## ----random, echo=TRUE--------------------------------------------------------
rayleigh_test(san_andreas.por$azi.PoR)

## ----confidence, echo=TRUE----------------------------------------------------
confidence_interval(san_andreas.por$azi.PoR, conf.level = 0.95, w = 1 / san_andreas$unc)

## ----dispersion, echo=TRUE----------------------------------------------------
circular_dispersion(san_andreas.por$azi.PoR, y = 135, w = 1 / san_andreas$unc)

## ----dispersion_MLE, echo=TRUE------------------------------------------------
circular_dispersion_boot(san_andreas.por$azi.PoR, y = 135, w = 1 / san_andreas$unc, R = 1000)

## ----rayleigh2, echo=TRUE-----------------------------------------------------
weighted_rayleigh(san_andreas.por$azi.PoR, prd = 135, unc = san_andreas$unc)

## ----interpolation, echo=TRUE-------------------------------------------------
mean_SH <- stress2grid(san_andreas, gridsize = 1, R_range = seq(50, 350, 100))

## ----plot, echo=TRUE, warning=FALSE, message=FALSE, eval=FALSE----------------
#  trajectories <- eulerpole_loxodromes(x = por, n = 40, cw = FALSE)
#  ggplot(mean_SH) +
#    borders(fill = "grey80") +
#    geom_sf(data = trajectories, lty = 2) +
#    geom_spoke(data = san_andreas, aes(lon, lat, angle = deg2rad(90 - azi)), radius = .5, color = "grey30", position = "center_spoke") +
#    geom_spoke(aes(lon, lat, angle = deg2rad(90 - azi), alpha = sd, color = mdr), radius = 1, position = "center_spoke", size = 1) +
#    coord_sf(xlim = range(san_andreas$lon), ylim = range(san_andreas$lat)) +
#    scale_alpha(name = "Standard deviation", range = c(1, .25)) +
#    scale_color_continuous(
#      type = "viridis",
#      limits = c(0, 1),
#      name = "Wavelength\n(R-normalized mean distance)",
#      breaks = seq(0, 1, .25)
#    ) +
#    facet_wrap(~R)

## ----interpolation_PoR, eval=FALSE--------------------------------------------
#  mean_SH_PoR <- PoR_stress2grid(san_andreas, PoR = por, gridsize = 1, R_range = seq(50, 350, 100))

## ----plot2, echo=TRUE, warning=FALSE, message=FALSE, eval=FALSE---------------
#  ggplot(mean_SH_PoR) +
#    borders(fill = "grey80") +
#    geom_sf(data = trajectories, lty = 2) +
#    geom_spoke(data = san_andreas, aes(lon, lat, angle = deg2rad(90 - azi)), radius = .5, color = "grey30", position = "center_spoke") +
#    geom_spoke(aes(lon, lat, angle = deg2rad(90 - azi), alpha = sd, color = mdr), radius = 1, position = "center_spoke", size = 1) +
#    coord_sf(xlim = range(san_andreas$lon), ylim = range(san_andreas$lat)) +
#    scale_alpha(name = "Standard deviation", range = c(1, .25)) +
#    scale_color_continuous(
#      type = "viridis",
#      limits = c(0, 1),
#      name = "Wavelength\n(R-normalized mean distance)",
#      breaks = seq(0, 1, .25)
#    ) +
#    facet_wrap(~R)

