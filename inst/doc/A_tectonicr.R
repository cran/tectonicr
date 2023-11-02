## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=TRUE---------------------------------------------------------
library(tectonicr)
library(ggplot2) # load ggplot library

## ----direction_of_plate_motion, echo=TRUE-------------------------------------
# Example:
point <- data.frame(lat = 45, lon = 20)
por <- data.frame(lat = 90, lon = 0)
model <- model_shmax(point, por)
print(model)

## ----deviation_of_plate_motion, echo=TRUE-------------------------------------
deviation <- deviation_shmax(model, 90)
print(deviation)

## ----shmax_test, echo=TRUE----------------------------------------------------
data("nuvel1") # import example data set for Euler rotations
por <- subset(
  nuvel1, nuvel1$plate.rot == "na"
) # North America relative to Pacific plate
point <- data.frame(lat = 45, lon = 20)

prd <- model_shmax(point, por)
norm_chisq(obs = 90, prd$sc, unc = 10)

## ----nuvel1, eval=FALSE, include=TRUE-----------------------------------------
#  data("nuvel1")
#  head(nuvel1)

## ----cpm_models, eval=FALSE, include=TRUE-------------------------------------
#  data("cpm_models")
#  head(cpm_models)

## ----equivalent_rotation, eval=FALSE, include=TRUE----------------------------
#  gsrm <- subset(cpm_models, model == "GSRM2.1")
#  equivalent_rotation(gsrm, rot = "na", fixed = "eu")

