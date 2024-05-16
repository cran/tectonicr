## ----include = FALSE----------------------------------------------------------
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
por <- subset(cpm_models, model == "NNR-MORVEL56") |>
  equivalent_rotation("na", "pa")
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
rose_line(135, radius = 1.1, col = "#009E73") # show the predicted direction

## ----random, echo=TRUE--------------------------------------------------------
rayleigh_test(san_andreas.por$azi.PoR)

## ----confidence, echo=TRUE----------------------------------------------------
confidence_interval(san_andreas.por$azi.PoR, conf.level = 0.95, w = 1 / san_andreas$unc)

## ----dispersion, echo=TRUE----------------------------------------------------
circular_dispersion(san_andreas.por$azi.PoR, y = 135, w = 1 / san_andreas$unc)

## ----dispersion_MLE, echo=TRUE------------------------------------------------
circular_dispersion_boot(san_andreas.por$azi.PoR, y = 135, w = 1 / san_andreas$unc, R = 1000)

## ----rayleigh2, echo=TRUE-----------------------------------------------------
weighted_rayleigh(san_andreas.por$azi.PoR, mu = 135, w = 1 / san_andreas$unc)

