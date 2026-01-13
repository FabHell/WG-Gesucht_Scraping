

#############    #############################################    ##############
############    ###############################################    ############# 
###########    ######                                     ######    ############
##########    #######          DATENUPDATE GEO-SQL        #######    ###########            
###########    ######                                     ######    ############
############    ###############################################    #############
#############    #############################################    ##############


library(DBI)
library(tidyverse)
library(futile.logger)
library(glue)
library(sf)

# usethis::edit_r_environ()      



## SQL-Verbindung herstellen ---------------------------------------------------

con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = Sys.getenv("DB_SERVER"),
                 Database = Sys.getenv("DB_NAME"),
                 Trusted_Connection = "Yes",
                 Encrypt = "No")


## SQL-Tabelle erstellen -------------------------------------------------------

dbExecute(con, "DROP TABLE IF EXISTS geodaten_stadtteile")

dbExecute(con, "
CREATE TABLE geodaten_stadtteile (
    stadt NVARCHAR(100),
    stadtteil NVARCHAR(100),
    geom GEOGRAPHY
)")


## Städtenamen WG-Daten --------------------------------------------------------

query_WGdaten_name <- glue_sql("
  SELECT stadt
  FROM analysedaten
", .con = con)

staedtename_WGdaten <- dbGetQuery(con, query_WGdaten_name) %>%
  distinct() %>% pull()


## Städtenamen Geodaten --------------------------------------------------------

query_Geodaten_name <- glue_sql("
  SELECT stadt
  FROM geodaten_stadtteile
", .con = con)

staedtename_Geodaten <- dbGetQuery(con, query_Geodaten_name) %>%
  distinct() %>% pull()


## Unterschied WG- und Geodaten ------------------------------------------------

staedtename_neu <- setdiff(staedtename_WGdaten, staedtename_Geodaten)


## Neue Geodaten laden ---------------------------------------------------------

if (!is.null(staedtename_neu)) {
  
  geodaten_neu <- list()
  
  for (stadt in staedtename_neu) {
    
    dateipfad <- glue("C:/Users/Fabian Hellmold/Desktop/WG-Gesucht-Scraper/{stadt}/Daten/Geodaten/Geo_Stadtteile_{stadt}.shp")
    message("Lade: ", stadt)
      daten <- st_read(dateipfad, quiet = TRUE) %>%
        st_transform(4326) %>%
        mutate(stadt = stadt, .before = "stadtteil")
      geodaten_neu[[stadt]] <- daten
  } 
  
  geodaten_neu_ges <- do.call(rbind, geodaten_neu)
  
} else {
  message("Geodatenbank bereits aktuell")
}


## Aufbereitung der neuen Geodaten ---------------------------------------------

geodaten_neu_ges_aufb <- geodaten_neu_ges %>% 
  st_make_valid() %>%
  mutate(geom = st_as_text(geometry)) %>%
  st_drop_geometry()


# Speichern der neuen Geodaten in Datenbank ------------------------------------

for (i in 1:nrow(geodaten_neu_ges_aufb)) {
  
  message("Speicher:", geodaten_neu_ges_aufb$stadtteil[i])
  
  stadt <- geodaten_neu_ges_aufb$stadt[i]
  stadtteil <- geodaten_neu_ges_aufb$stadtteil[i]
  geom_wkt <- geodaten_neu_ges_aufb$geom[i]
  
  geom_wkt <- gsub("'", "''", geom_wkt)
  
  sql <- glue("
    INSERT INTO geodaten_stadtteile (stadt, stadtteil, geom)
    VALUES ('{stadt}', '{stadtteil}', geography::STGeomFromText('{geom_wkt}', 4326))
  ")
  
  dbExecute(con, sql)
}


# Laden und darstelllen Geodaten -----------------------------------------------

# stadtteile_geodatenbank <- dbGetQuery(con, "
#   SELECT stadt, stadtteil, geom.STAsText() AS wkt
#   FROM geodaten_stadtteile
# ") %>%
#   st_as_sf(wkt = "wkt", crs = 4326)
# 
# stadtteile_geodatenbank %>%
#   ggplot() +
#   geom_sf()
