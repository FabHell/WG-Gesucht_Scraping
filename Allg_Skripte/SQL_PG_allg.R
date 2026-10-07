


################################################################################
################################################################################
#####                                                                      #####
#####                            SQL-SERVER LOKAL                          #####
#####                                                                      #####
################################################################################
################################################################################


library(tidyverse)
library(sf)
library(DBI)
library(RPostgres)
library(futile.logger)


# Lokale Verbindung herstellen -------------------------------------------------


# usethis::edit_r_environ()

tryCatch({
  con_pg <- dbConnect(RPostgres::Postgres(),
                      host     = Sys.getenv("SERVER_PG_LOKAL"),
                      port     = as.integer(Sys.getenv("PORT_PG_LOKAL")),
                      dbname   = Sys.getenv("DATABASE_PG_LOKAL"),
                      user     = Sys.getenv("UID_PG_LOKAL"),
                      password = Sys.getenv("PWD_PG_LOKAL"))
  flog.info("PostgreSQL-Verbindung hergestellt")
}, error = function(e) {
  flog.error("Fehler bei PostgreSQL-Verbindung: %s", e$message)
})



# Neue Tabelle anlegen ---------------------------------------------------------


# dbExecute(con_pg, "DROP TABLE IF EXISTS analysedaten_wgs")

dbExecute(con_pg, "
CREATE TABLE analysedaten_wgs (
  id INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  land TEXT,
  bundesland TEXT,
  stadt TEXT,
  titel TEXT,
  link TEXT,
  profil TEXT,
  stadtteil_webseite TEXT,
  stadtteil_geocoding TEXT,
  postleitzahl NUMERIC,
  straße TEXT,
  geolocation GEOMETRY(POINT, 4326),
  gesamtmiete NUMERIC,
  kaltmiete NUMERIC,
  nebenkosten NUMERIC,
  kaution NUMERIC,
  sonstige_kosten NUMERIC,
  ablösevereinbarung NUMERIC,
  zimmergröße NUMERIC,
  personenzahl NUMERIC,
  wohnungsgröße NUMERIC,
  bewohneralter TEXT,
  einzugsdatum DATE,
  zusammensetzung TEXT,
  befristung_enddatum DATE,
  befristungsdauer NUMERIC,
  geschlecht_ges TEXT,
  alter_ges TEXT,
  wg_art TEXT,
  rauchen TEXT,
  sprache TEXT,
  angaben_zum_objekt TEXT,
  freitext_zimmer TEXT,
  freitext_lage TEXT,
  freitext_wg_leben TEXT,
  freitext_sonstiges TEXT,
  seite_scraping INTEGER,
  uhrzeit_scraping TEXT,
  datum_scraping DATE
);")



analysedaten_csv <- read_csv("C:/Users/Fabian Hellmold/Desktop/WG-Gesucht-Scraper/Berlin/Daten/Analysedaten/Analysedaten.csv")

analysedaten_clean <- analysedaten_csv %>%
  mutate(across(where(is.character), ~ str_remove_all(., "\\u0000")))

analysedaten_sf <- analysedaten_clean %>%
  st_as_sf(wkt = "geolocation", crs = 4326)


# st_write(analysedaten_sf, con_pg, "analysedaten", append = TRUE)


dbGetQuery(con_pg, "SELECT COUNT(*) FROM analysedaten_wgs;")
test <- dbGetQuery(con_pg, "SELECT * FROM analysedaten;")


analysedaten_geo %>%
  ggplot() +
  geom_sf()
  


analysedaten_geo <- st_read(con_pg, query = "SELECT * FROM analysedaten;") %>%
  st_as_sf(wkt = "geolocation", crs = 4326) %>%
  mutate(datum_scraping = as.Date(datum_scraping),
         uhrzeit_scraping_aufb = str_replace(uhrzeit_scraping, "-", ":") |> hm()) %>%
  arrange(datum_scraping, uhrzeit_scraping_aufb, stadt) %>%
  select(-uhrzeit_scraping_aufb)

st_write(analysedaten_geo, con_pg, "analysedaten_wgs", append = TRUE)

