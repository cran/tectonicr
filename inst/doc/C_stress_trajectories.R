## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=TRUE,message=FALSE-------------------------------------------
library(tectonicr)
library(ggplot2) # load ggplot library
library(sf)

theme_set(theme_bw())

## ----nuvel_eq, echo=TRUE------------------------------------------------------
data("nuvel1")
nuvel1.eu <- equivalent_rotation(nuvel1, fixed = "eu")
head(nuvel1.eu)

## ----pb2002_eq, echo=TRUE-----------------------------------------------------
data("pb2002")
pb2002.eu <- equivalent_rotation(pb2002, fixed = "eu")
head(pb2002.eu)

## ----nuvel_euin, echo=TRUE----------------------------------------------------
por <-
  subset(nuvel1.eu, nuvel1$plate.rot == "in") # India relative to Eurasia

## ----small_circles_around_ep, echo=TRUE---------------------------------------
por.sm <- eulerpole_smallcircles(por)
data("plates") # load plate boundary data set
# world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

ggplot() +
  # geom_sf(data = world, alpha = .5) +
  geom_sf(
    data = plates,
    color = "#FB8861FF",
    alpha = .5
  ) +
  labs(title = "India relative to Eurasia", subtitle = "source: NUVEL1") +
  geom_sf(
    data = por.sm,
    aes(lty = "small circles"),
    color = "#51127CFF", fill = NA,
    alpha = .5
  ) +
  geom_point(
    data = por,
    aes(lon, lat),
    shape = 21,
    colour = "#B63679FF",
    size = 2,
    fill = "#51127CFF",
    stroke = 1
  ) +
  geom_point(
    data = por,
    aes(lon + 180, -lat),
    shape = 21,
    colour = "#B63679FF",
    size = 2,
    fill = "#51127CFF",
    stroke = 1
  ) +
  coord_sf(default_crs = "WGS84", crs = sf::st_crs("ESRI:54030"))

## ----great_circles_around_ep, echo=TRUE---------------------------------------
por.gm <- eulerpole_greatcircles(por)

ggplot() +
  # geom_sf(data = world, alpha = .5) +
  geom_sf(
    data = plates,
    color = "#FB8861FF",
    alpha = .5
  ) +
  labs(title = "India relative to Eurasia", subtitle = "source: NUVEL1") +
  geom_sf(
    data = por.sm,
    aes(lty = "small circles"),
    color = "#51127CFF",
    alpha = .5
  ) +
  geom_sf(
    data = por.gm,
    aes(lty = "great circles"),
    color = "#51127CFF"
  ) +
  geom_point(
    data = por,
    aes(lon, lat),
    shape = 21,
    colour = "#B63679FF",
    size = 2,
    fill = "#51127CFF",
    stroke = 1
  ) +
  geom_point(
    data = por,
    aes(lon + 180, -lat),
    shape = 21,
    colour = "#B63679FF",
    size = 2,
    fill = "#51127CFF",
    stroke = 1
  ) +
  coord_sf(default_crs = "WGS84", crs = sf::st_crs("ESRI:54030"))

## ----loxodromes, echo=TRUE----------------------------------------------------
por.ld <- eulerpole_loxodromes(x = por, angle = 45, n = 10, cw = TRUE)

ggplot() +
  labs(title = "India relative to Eurasia", subtitle = "source: NUVEL1") +
  # geom_sf(data = world, alpha = .5) +
  geom_sf(
    data = plates,
    color = "#FB8861FF",
    alpha = .5
  ) +
  geom_sf(
    data = por.sm,
    aes(lty = "small circles"),
    color = "#51127CFF",
    alpha = .5
  ) +
  geom_sf(
    data = por.ld,
    aes(lty = "clockwise loxodromes"),
    color = "#51127CFF"
  ) +
  geom_point(
    data = por,
    aes(lon, lat),
    shape = 21,
    colour = "#B63679FF",
    size = 2,
    fill = "#51127CFF",
    stroke = 1
  ) +
  geom_point(
    data = por,
    aes(lon + 180, -lat),
    shape = 21,
    colour = "#B63679FF",
    size = 2,
    fill = "#51127CFF",
    stroke = 1
  ) +
  coord_sf(default_crs = "WGS84", crs = sf::st_crs("ESRI:54030"))

