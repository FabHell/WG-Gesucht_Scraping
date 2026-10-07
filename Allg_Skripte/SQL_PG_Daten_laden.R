


################################################################################
###                                                                          ###
###                         DATENÜBERTRAGUNG ZU POSTGRES                     ###
###                                                                          ###
################################################################################


library(tidyverse)
library(fs)
library(sf)
library(DBI)
library(RPostgres)
library(tidygeocoder)



## Dateipfade für Analysedaten laden -------------------------------------------


basis_pfad <- "C:/Users/Fabian Hellmold/Desktop/WG-Gesucht-Scraper"

dateien <- dir_ls(
  basis_pfad,
  recurse = TRUE,
  glob = "*/Daten/Analysedaten/Analysedaten.csv"
) %>%
  discard(~ str_detect(.x, "Stadtdummy")) %>%
  discard(~ str_detect(.x, "Tübingen"))



## Einlesen der Analysedaten ---------------------------------------------------


spalten_typen <- cols(
  postleitzahl = col_character(),
  stadtteil_webseite = col_character(),
  .default = col_guess()
)

analysedaten <- dateien %>%
  set_names(~ path_file(path_dir(path_dir(path_dir(.x))))) |>
  map(read_csv, col_types = spalten_typen, show_col_types = FALSE) |>
  list_rbind()



## Einlesen Backup Tübingen ----------------------------------------------------


backup_pfad <- "C:/Users/Fabian Hellmold/Desktop/WG-Gesucht-Scraper/Tübingen/Daten/Backup/Analysedaten"

backup_dateien <- dir_ls(backup_pfad, glob = "*.csv")

analysedaten_tuebingen_backup <- backup_dateien %>%
  set_names(~ path_file(path_dir(path_dir(path_dir(.x))))) |>
  map(read_csv, col_types = spalten_typen, show_col_types = FALSE) %>%
  list_rbind() %>%
  select(-stadtteil, -stadtteil_quelle)

analysedaten_tuebingen_backup_geo_head <- analysedaten_tuebingen_backup %>%
  filter(is.na(geolocation) | !str_starts(geolocation, "POINT")) %>%
  select(-geolocation) %>%
  head(nrow(.)/2) %>%
  geocode(method = "osm", country = land, city = stadt,
          postalcode = postleitzahl, street = straße) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, na.fail = FALSE) %>%
  rename(geolocation = geometry)

analysedaten_tuebingen_backup_geo_tail <- analysedaten_tuebingen_backup %>%
  filter(is.na(geolocation) | !str_starts(geolocation, "POINT")) %>%
  select(-geolocation) %>%
  tail(nrow(.)/2) %>%
  geocode(method = "osm", country = land, city = stadt,
          postalcode = postleitzahl, street = straße) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, na.fail = FALSE) %>%
  rename(geolocation = geometry)



Geodaten_Stadtteile <- st_read(paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Tübingen\\Daten\\Geodaten\\Geo_Stadtteile_Tübingen.shp")) %>%
  st_transform(crs = st_crs(analysedaten_tuebingen_backup_geo_head)) 
  

St_Teile <- tibble(Geodaten_Stadtteile) %>%
  select(stadtteil) %>%
  pull()

analysedaten_tuebingen_backup_geo_gesamt <- rbind(analysedaten_tuebingen_backup_geo_head,
                                                  analysedaten_tuebingen_backup_geo_tail) %>%
  select(-stadtteil_geocoding) %>%
  st_join(Geodaten_Stadtteile) %>%
  mutate(stadtteil_geocoding = stadtteil, .after = stadtteil_webseite) %>%
  select(-stadtteil)


analysedaten_tuebingen_backup_final <- analysedaten_tuebingen_backup %>%
  filter(!is.na(geolocation) & str_starts(geolocation, "POINT")) %>%
  st_as_sf(wkt = "geolocation", crs = 4326) %>%
  rbind(analysedaten_tuebingen_backup_geo_gesamt) %>%
  mutate(bundesland = "Baden-Württemberg") %>%
  mutate(datum_scraping = as.Date(datum_scraping),
         uhrzeit_scraping_aufb = str_replace(uhrzeit_scraping, "-", ":") |> hm()) %>%
  arrange(datum_scraping, uhrzeit_scraping_aufb, stadt) %>%
  select(-uhrzeit_scraping_aufb)


write.csv(analysedaten_tuebingen_backup_final, 
          "C:/Users/Fabian Hellmold/Desktop/WG-Gesucht-Scraper/Tübingen/Daten/Analysedaten/Analysedaten.csv",
          row.names = FALSE)



## Zusammenfügen und sortieren der Analysedaten --------------------------------


analysedaten_ges <- analysedaten %>%
  st_as_sf(wkt = "geolocation", crs = 4326) %>%
  rbind(analysedaten_tuebingen_backup_final)

analysedaten_bereinigt <- analysedaten_ges |>
  mutate(datum_scraping = as.Date(datum_scraping),
         uhrzeit_scraping_aufb = str_replace(uhrzeit_scraping, "-", ":") |> hm()) %>%
  arrange(datum_scraping, uhrzeit_scraping_aufb, stadt) %>%
  select(-uhrzeit_scraping_aufb)



## Dubletten von Erhebungsfehler entfernen -------------------------------------


analysedaten_bereinigt_2 <- analysedaten_bereinigt |>
  add_count(across(c(link)), name = "dupe_count") |>
  mutate(ist_dublette = dupe_count > 1) |>
  mutate(
    ist_fehlerhafte_dublette = ist_dublette &
      datum_scraping == as_date("2026-06-23") &
      uhrzeit_scraping %in% c("07-03", "07-30", "07-45", "08-00")
  ) %>%
  filter(ist_fehlerhafte_dublette == F) %>%
  select(-dupe_count, -ist_dublette, -ist_fehlerhafte_dublette)




## 4. Verbindung zur PG-Datenbank aufbauen -------------------------------------

con_pg <- dbConnect(RPostgres::Postgres(),
                    host     = Sys.getenv("SERVER_PG_LOKAL"),
                    port     = as.integer(Sys.getenv("PORT_PG_LOKAL")),
                    dbname   = Sys.getenv("DATABASE_PG_LOKAL"),
                    user     = Sys.getenv("UID_PG_LOKAL"),
                    password = Sys.getenv("PWD_PG_LOKAL"))

## 5. In die Datenbank schreiben -----------------------------------------------



st_write(analysedaten_bereinigt_2, con_pg, "analysedaten_wgs", append = TRUE)




dbDisconnect(con)




## Alle Daten 02.08.2026 - 16:30


analysedaten_bereinigt_2 %>%
  filter(datum_scraping == as.character("02.08.2026"))
