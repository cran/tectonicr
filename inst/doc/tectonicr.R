## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----installl, echo=FALSE, eval=FALSE-----------------------------------------
# install.packages("tectonicr")
# 
# # or install the current development version from github:
# remotes::install_github("tobiste/tectonicr")

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(tectonicr)

## ----direction_of_plate_motion, echo=TRUE-------------------------------------
# import example data set for Euler rotations
data("nuvel1")

# North America relative to Pacific plate
por <- subset(nuvel1, nuvel1$plate.rot == "na")

# Example stress:
point <- data.frame(lat = 45, lon = 20)
prd <- model_shmax(point, por)
print(prd)

## ----deviation_of_plate_motion, echo=TRUE-------------------------------------
deviation <- deviation_shmax(prd, 90)
print(deviation)

## ----shmax_test, echo=TRUE----------------------------------------------------
sapply(as.numeric(prd), function(p) {
  circular_dispersion(90, y = p, w = weighting(10))
}) |>
  setNames(nm = names(prd))

## ----nuvel1, eval=FALSE, include=TRUE-----------------------------------------
# data("nuvel1")
# head(nuvel1)

## ----cpm_models, eval=FALSE, include=TRUE-------------------------------------
# data("cpm_models")
# head(cpm_models)

## ----equivalent_rotation, eval=FALSE, include=TRUE----------------------------
# gsrm <- cpm_models[["GSRM2.1"]]
# equivalent_rotation(gsrm, rot = "na", fixed = "eu")

