


################################################################################
#####                                                                      #####
#####                       Aufbereitung Geodaten Hamburg                  #####
#####                                                                      #####
################################################################################



## Paktete laden ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(glue)

stadt <- "Hamburg"


## Geodaten laden --------------------------------------------------------------

url <- "C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Daten\\Geodaten\\Stadtteile_Hamburg.shp"

Stadtteile_roh <- st_read(url)


## Geodaten bearbeiten ---------------------------------------------------------

Geodaten_Stadtteile <- Stadtteile_roh %>%
  st_transform(crs = 4326) %>%
  select(stadtteil = stadtteil_) %>%
  filter(stadtteil != "Neuwerk")

Geodaten_Stadtteile <- st_zm(Geodaten_Stadtteile, drop = TRUE, what = "ZM")

which(!st_is_valid(Geodaten_Stadtteile))
# Geodaten_Stadtteile <- st_make_valid(Geodaten_Stadtteile)


# Geodaten speichern -----------------------------------------------------------

stadtteile_pfad <- glue("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\{stadt}\\Daten\\Geodaten\\Geo_Stadtteile_{stadt}.shp")

st_write(Geodaten_Stadtteile, stadtteile_pfad, delete_layer = TRUE)


geodaten_test <- st_read(stadtteile_pfad)

geodaten_test %>%
  ggplot() +
  geom_sf()

