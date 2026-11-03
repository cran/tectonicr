## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
set.seed(20250411)

## ----setup, echo=TRUE, message=FALSE------------------------------------------
library(tectonicr)
library(ggplot2) # load ggplot library

## ----directional_vs_axial, echo=TRUE------------------------------------------
angles <- rvm(10, mean = 35, kappa = 20)

par(mfrow = c(1, 2), xpd = NA)
circular_plot(main = "Directional")
rose_line(angles, axial = FALSE, col = "#B63679FF")

circular_plot(main = "Axial")
rose_line(angles, axial = TRUE, col = "#B63679FF")

## ----mean, echo=TRUE----------------------------------------------------------
data("san_andreas")
circular_mean(san_andreas$azi)
circular_median(san_andreas$azi)

## ----mean_directional, echo=TRUE----------------------------------------------
circular_mean(san_andreas$azi, axial = FALSE)
circular_median(san_andreas$azi, axial = FALSE)

## ----weighted, echo=TRUE------------------------------------------------------
w <- weighting(san_andreas$unc)

circular_mean(san_andreas$azi, w)
circular_median(san_andreas$azi, w)

## ----weighted_spread, echo=TRUE-----------------------------------------------
circular_sd(san_andreas$azi, w) # standard deviation
circular_IQR(san_andreas$azi, w) # interquartile range

## ----por, echo=TRUE-----------------------------------------------------------
data("cpm_models")
por <- cpm_models[["NNR-MORVEL56"]] |>
  equivalent_rotation("na", "pa")
san_andreas.por <- PoR_shmax(san_andreas, por, type = "right")

## ----por_stats, echo=TRUE-----------------------------------------------------
circular_mean(san_andreas.por$azi.PoR, w)
circular_sd(san_andreas.por$azi.PoR, w)

circular_median(san_andreas.por$azi.PoR, w)
circular_IQR(san_andreas.por$azi.PoR, w)

## ----summary_stats, echo=TRUE-------------------------------------------------
circular_summary(san_andreas.por$azi.PoR, w, mode = TRUE)

## ----rose1, echo=TRUE---------------------------------------------------------
rose(san_andreas$azi,
  weights = w, main = "North pole",
  dots = TRUE, stack = TRUE, dot_cex = 0.5, dot_pch = 21
)

# add the density curve as a corona plot:
plot_density(san_andreas$azi, kappa = 20, col = "#51127CFF", scale = 1.1, shrink = 2, xpd = NA)

## ----rose2, echo=TRUE---------------------------------------------------------
# rose diagram
rose(san_andreas.por$azi,
  weights = w, main = "Pole of Rotation", origin.text = "PoR",
  dots = TRUE, stack = TRUE, dot_cex = 0.5, dot_pch = 21
)
# add density corona
plot_density(san_andreas.por$azi, kappa = 20, col = "#51127CFF", scale = 1.1, shrink = 2, xpd = NA)

# add predicted direction as streight line
rose_line(135, radius = 1.1, col = "#FB8861FF")

## ----qqplot, echo=TRUE--------------------------------------------------------
circular_qqplot(san_andreas.por$azi.PoR)

## ----random, echo=TRUE--------------------------------------------------------
rayleigh_test(san_andreas.por$azi.PoR)

## ----confidence, echo=TRUE----------------------------------------------------
confidence_interval(san_andreas.por$azi.PoR, conf.level = 0.95, w = w)

## ----dispersion, echo=TRUE----------------------------------------------------
circular_dispersion(san_andreas.por$azi.PoR, y = 135, w = w)

## ----dispersion_MLE, echo=TRUE------------------------------------------------
circular_dispersion_boot(san_andreas.por$azi.PoR, y = 135, w = w, R = 1000)

## ----rayleigh2, echo=TRUE-----------------------------------------------------
weighted_rayleigh(san_andreas.por$azi.PoR, mu = 135, w = w)

## ----otensor------------------------------------------------------------------
ot_eigen2d(san_andreas.por$azi.PoR, w)

