## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=TRUE, message=FALSE------------------------------------------
library(tectonicr)
library(ggplot2) # load ggplot library

## ----load_wsm2016, echo=TRUE--------------------------------------------------
data("san_andreas")
head(san_andreas)

## ----san_andreas, echo=TRUE---------------------------------------------------
data("nuvel1")
por <- subset(nuvel1, nuvel1$plate.rot == "na")
san_andreas.prd <- PoR_shmax(san_andreas, por, type = "right")

## ----san_andreas2, echo=TRUE--------------------------------------------------
san_andreas.res <- data.frame(
  sf::st_drop_geometry(san_andreas),
  san_andreas.prd
)

## ----plates, echo=TRUE--------------------------------------------------------
data("plates") # load plate boundary data set

## ----trajectories, echo=TRUE--------------------------------------------------
trajectories <- eulerpole_loxodromes(por, 40, cw = FALSE)

## ----plot1, echo=TRUE, warning=FALSE, message=FALSE---------------------------
map <- ggplot() +
  geom_sf(
    data = plates,
    color = "red",
    lwd = 2,
    alpha = .5
  ) +
  scale_color_continuous(
    type = "viridis",
    limits = c(0, 90),
    name = "|Deviation| in (\u00B0)",
    breaks = seq(0, 90, 22.5)
  ) +
  scale_alpha_discrete(name = "Quality rank", range = c(1, 0.4))

## ----plot2, echo=TRUE, warning=FALSE, message=FALSE---------------------------
map +
  geom_sf(
    data = trajectories,
    lty = 2
  ) +
  geom_spoke(
    data = san_andreas.res,
    aes(
      x = lon,
      y = lat,
      angle = deg2rad(90 - azi),
      color = deviation_norm(dev),
      alpha = quality
    ),
    radius = 1,
    position = "center_spoke",
    na.rm = TRUE
  ) +
  coord_sf(
    xlim = range(san_andreas$lon),
    ylim = range(san_andreas$lat)
  )

## ----san_andreas_nchisq, echo=TRUE--------------------------------------------
norm_chisq(
  obs = san_andreas.res$azi.PoR,
  prd = 135,
  unc = san_andreas.res$unc
)

## ----san_andreas_distance, echo=TRUE------------------------------------------
plate_boundary <- subset(plates, plates$pair == "na-pa")
san_andreas.res$distance <-
  distance_from_pb(
    x = san_andreas,
    PoR = por,
    pb = plate_boundary,
    tangential = TRUE
  )

## ----san.andreas.distanceplot1, echo=TRUE, message=FALSE, warning=FALSE-------
azi_plot <- ggplot(san_andreas.res, aes(x = distance, y = azi.PoR)) +
  coord_cartesian(ylim = c(0, 180)) +
  labs(x = "Distance from plate boundary (\u00B0)", y = "Azimuth in PoR (\u00B0)") +
  geom_hline(yintercept = c(0, 45, 90, 135, 180), lty = 3) +
  geom_pointrange(
    aes(
      ymin = azi.PoR - unc, ymax = azi.PoR + unc,
      color = san_andreas$regime, alpha = san_andreas$quality
    ),
    size = .25
  ) +
  scale_x_continuous(breaks = seq(-10, 10, 2)) +
  scale_y_continuous(
    breaks = seq(-180, 360, 45),
    sec.axis = sec_axis(
      ~.,
      name = NULL,
      breaks = c(0, 45, 90, 135, 180),
      labels = c("Outward", "Tan (L)", "Inward", "Tan (R)", "Outward")
    )
  ) +
  scale_alpha_discrete(name = "Quality rank", range = c(1, 0.1)) +
  scale_color_manual(name = "Tectonic regime", values = stress_colors(), breaks = names(stress_colors()))
print(azi_plot)

## ----distance_bin, echo=TRUE, echo=TRUE, message=FALSE, warning=FALSE---------
san_andreas_binned <- distance_binned_stats(
  azi = san_andreas.res$azi.PoR,
  distance = san_andreas.res$distance,
  unc = san_andreas$unc,
  prd = 135,
  width.breaks = 2
)

azi_plot +
  geom_step(
    data = san_andreas_binned,
    aes(distance_median, mean - CI),
    lty = 2
  ) +
  geom_step(
    data = san_andreas_binned,
    aes(distance_median, mean + CI),
    lty = 2
  ) +
  geom_step(
    data = san_andreas_binned,
    aes(distance_median, mean)
  )

## ----san.andreas.distanceplot2, echo=TRUE, echo=TRUE, message=FALSE, warning=FALSE----
# plotting:
ggplot(san_andreas.res, aes(x = distance, y = nchisq)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Distance from plate boundary (\u00B0)", y = expression(Norm ~ chi^2)) +
  geom_hline(yintercept = c(0.15, .33, .7), lty = 3) +
  geom_point(aes(color = san_andreas$regime)) +
  scale_y_continuous(sec.axis = sec_axis(
    ~.,
    name = NULL,
    breaks = c(.15 / 2, (.33 - .15) / 2 + .15, (.7 - .33) / 2 + .33, .7 + 0.15),
    labels = c("Good fit", "Acceptable fit", "Random", "Systematic\nmisfit")
  )) +
  scale_x_continuous(breaks = seq(-10, 10, 2)) +
  scale_color_manual(name = "Tectonic regime", values = stress_colors(), breaks = names(stress_colors())) +
  geom_step(
    data = san_andreas_binned,
    aes(distance_median, nchisq)
  )

## ----plot_base, echo=TRUE, warning=FALSE, message=FALSE-----------------------
# Setup the colors for the deviation
cols <- tectonicr.colors(
  deviation_norm(san_andreas.res$dev),
  categorical = FALSE
)

# Setup the legend
col.legend <- data.frame(col = cols, val = names(cols)) |>
  dplyr::mutate(val2 = gsub("\\(", "", val), val2 = gsub("\\[", "", val2)) |>
  unique() |>
  dplyr::arrange(val2)

# Initialize the plot
plot(
  san_andreas$lon, san_andreas$lat,
  cex = 0,
  xlab = "PoR longitude", ylab = "PoR latitude",
  asp = 1
)

# Plot the axis and colors
axes(
  san_andreas$lon, san_andreas$lat, san_andreas$azi,
  col = cols, add = TRUE
)

# Plot the plate boundary
plot(sf::st_geometry(plates), col = "red", lwd = 2, add = TRUE)

# Plot the trajectories
plot(sf::st_geometry(trajectories), add = TRUE, lty = 2)

# Create the legend
graphics::legend(
  "bottomleft",
  title = "|Deviation| in (\u00B0)",
  inset = .05, cex = .75,
  legend = col.legend$val, fill = col.legend$col
)

## ----quick, eval=TRUE---------------------------------------------------------
results <- stress_analysis(san_andreas, por, "right", plate_boundary, plot = FALSE)
head(results$result)

## ----quick_stats, eval=TRUE---------------------------------------------------
results$stats

## ----quick_tests, eval=TRUE---------------------------------------------------
results$test

## ----quick_plot, eval=FALSE---------------------------------------------------
#  stress_analysis(san_andreas, por, "right", plate_boundary, plot = TRUE)

