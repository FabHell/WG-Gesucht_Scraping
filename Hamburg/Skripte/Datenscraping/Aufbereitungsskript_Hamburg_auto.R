


################################################################################
################################################################################
#####                                                                      #####
#####              AUFBEREITUNG VON WG-GESUCHT HAMBURG - AUTO              #####
#####                                                                      #####
################################################################################
################################################################################



library(readxl)
library(tidygeocoder)
library(sf)



################################################################################
###                                                                          ###
###                       AUFBEREITUNG STRINGVARIABLEN                       ###
###                                                                          ###
################################################################################



Analysedaten_neu <- Rohdaten_neu_gefiltert %>%
  
  mutate(Personenzahl = str_extract(WG_Konstellation, "\\d{1,2}er WG"),
         Personenzahl = str_remove(Personenzahl, "er WG"),
         Personenzahl = as.numeric(Personenzahl),
         
         Zusammensetzung = str_extract(WG_Konstellation, "\\d{1}w\\,\\d{1}m\\,\\d{1}d")) %>%
  
  select(-WG_Konstellation) %>%
  
  
  mutate(Zimmergröße = str_extract(Zimmergröße_Gesamtmiete, "\\d{1,2}m²"),
         Zimmergröße = str_remove(Zimmergröße, "m²"),
         Zimmergröße = as.numeric(Zimmergröße),
         
         Gesamtmiete = str_extract(Zimmergröße_Gesamtmiete, "\\d{1,4}€"),
         Gesamtmiete = str_remove(Gesamtmiete, "€"),
         Gesamtmiete = as.numeric(Gesamtmiete)) %>%
  
  select(-Zimmergröße_Gesamtmiete) %>%
  
  
  mutate(Datum = str_squish(str_replace_all(Datum, "\\s+", " ")),
         
         Einzugsdatum = str_extract(Datum, "frei ab: \\d{2}\\.\\d{2}\\.\\d{4}"),
         Einzugsdatum = str_remove(Einzugsdatum, "frei ab: "),
         Einzugsdatum = as.Date(Einzugsdatum, format = "%d.%m.%Y"),
         
         Befristung_Enddatum = str_extract(Datum, "frei bis: \\d{2}\\.\\d{2}\\.\\d{4}"),
         Befristung_Enddatum = str_remove(Befristung_Enddatum, "frei bis: "),
         Befristung_Enddatum = as.Date(Befristung_Enddatum, format = "%d.%m.%Y"),
         
         Befristungsdauer = as.numeric(Befristung_Enddatum - Einzugsdatum)) %>%
  
  select(-Datum) %>%
  
  
  mutate(WG_Details = str_squish(str_replace_all(WG_Details, "\\s+", " ")),
         
         Rauchen = str_extract(WG_Details, "Rauchen überall erlaubt|Rauchen im Zimmer erlaubt|Rauchen auf dem Balkon erlaubt|Rauchen nicht erwünscht"),
         
         Sprache = str_extract(WG_Details, "Sprache/n:\\s*[^|]+"),
         Sprache = str_remove(Sprache, "Sprache/n: "),
         
         Bewohneralter = str_extract(WG_Details, "Bewohneralter:\\s*\\d+ bis \\d+ Jahre"),
         Bewohneralter = str_remove(Bewohneralter, "Bewohneralter: "),
         Bewohneralter = str_remove(Bewohneralter, " Jahre"),
         
         Wohnungsgröße = str_extract(WG_Details, "(Wohnungsgröße:\\s*)\\d+(?=m²)"),
         Wohnungsgröße = str_remove(Wohnungsgröße, "Wohnungsgröße: "),
         Wohnungsgröße = as.numeric(Wohnungsgröße),
         
         Wg_Art = WG_Details %>%
           str_extract_all(
             "Studenten-WG|keine Zweck-WG|Männer-WG|Business-WG|Wohnheim|Vegetarisch/Vegan|Alleinerziehende|funktionale WG|Berufstätigen-WG|gemischte WG|WG mit Kindern|Verbindung|LGBTQIA\\+|Senioren-WG|inklusive WG|WG-Neugründung|Zweck-WG|Frauen-WG|Plus-WG|Mehrgenerationen|Azubi-WG|Wohnen für Hilfe|Internationals welcome") %>%
           map_chr(~ str_c(.x, collapse = ", ")),
         
         Geschlecht_ges = map_chr(str_extract_all(WG_Details, 
                                                  "(Mann|Frau|Divers|Geschlecht egal)"), ~ last(.x)),
         
         Alter_ges = str_extract(WG_Details, "(?<=zwischen )\\d{2} und \\d{2}")) %>%
  
  select(-WG_Details) %>%
  
  mutate(Straße = str_split(Adresse, "\\s{3,}", simplify = TRUE)[,1],
         
         PLZ_Stadtteil = str_split(Adresse, "\\s{3,}", simplify = TRUE)[,2],
         PLZ_Stadtteil = str_remove(PLZ_Stadtteil, "Hamburg "),
         Postleitzahl = str_extract(PLZ_Stadtteil, "\\d{5}"),
         Stadtteil = str_remove(PLZ_Stadtteil, "\\d{5} ")) %>%
  
  select(-PLZ_Stadtteil) %>%
  
  
  mutate(Kostenfeld = str_squish(str_replace_all(Kostenfeld, "\\s+", " ")),
         Kostenfeld = str_remove(Kostenfeld, " Nebenkosten sind geschätzte Kosten, die auf dem Verbrauch des Vormieters basieren und monatlich im Voraus bezahlt werden. Am Jahresende rechnet der Vermieter die Vorauszahlungen mit dem tatsächlichen Verbrauch des Mieters ab. Infolgedessen muss der Mieter eine Nachzahlung leisten oder er erhält eine Rückzahlung."),
         Kostenfeld = str_remove(Kostenfeld, " SCHUFA-Auskunft: In 3 Minuten bereit 1"),
         Kostenfeld = str_replace(Kostenfeld, "Kosten Miete: ", "Miete: "),
         
         Kaltmiete = str_extract(Kostenfeld, "Miete: \\d{1,4}"),
         Kaltmiete = str_remove(Kaltmiete, "Miete: "),
         Kaltmiete = as.numeric(Kaltmiete),
         
         Nebenkosten = str_extract(Kostenfeld, "Nebenkosten: \\d{1,4}"),
         Nebenkosten = str_remove(Nebenkosten, "Nebenkosten: "),
         Nebenkosten = as.numeric(Nebenkosten),
         
         Kaution = str_extract(Kostenfeld, "Kaution: \\d{1,4}"),
         Kaution = str_remove(Kaution, "Kaution: "),
         Kaution = as.numeric(Kaution),
         
         Sonstige_Kosten = str_extract(Kostenfeld, "Sonstige Kosten: \\d{1,4}"),
         Sonstige_Kosten = str_remove(Sonstige_Kosten, "Sonstige Kosten: "),
         Sonstige_Kosten = as.numeric(Sonstige_Kosten),
         
         Ablösevereinbarung = str_extract(Kostenfeld, "Ablösevereinbarung: \\d{1,4}"),
         Ablösevereinbarung = str_remove(Ablösevereinbarung, "Ablösevereinbarung: "),
         Ablösevereinbarung = as.numeric(Ablösevereinbarung)) %>%
  
  select(-Kostenfeld) %>%
  
  
  mutate(Angaben_zum_Objekt = str_squish(str_replace_all(Angaben_zum_Objekt, "\\s+", " "))) %>%
  
  
  select(Titel, Link, Stadtteil, Postleitzahl, Straße, Gesamtmiete, Kaltmiete, Nebenkosten,
         Kaution, Sonstige_Kosten, Ablösevereinbarung, Zimmergröße, Personenzahl,
         Wohnungsgröße, Bewohneralter, Einzugsdatum, Zusammensetzung,
         Befristung_Enddatum, Befristungsdauer, Geschlecht_ges, Alter_ges, Wg_Art, 
         Rauchen, Sprache, Angaben_zum_Objekt, Freitext_Zimmer, 
         Freitext_Lage, Freitext_WG_Leben, Freitext_Sonstiges, Datum_Scraping)


message("Bearbeitung der Sting-Variablen erfolgreich")




################################################################################
###                                                                          ###
###                         Geocoding der Stadtteile                         ###
###                                                                          ###
################################################################################



## Geodaten laden --------------------------------------------------------------


Geodaten_Stadtteile <- st_read("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Daten\\Geodaten\\Geo_Stadtteile\\Stadtteile_Hamburg.shp", quiet = T) %>%
  filter(stadtteil_ != "Neuwerk") %>%
  select(Stadtteil = stadtteil_)

St_Teile <- tibble(Geodaten_Stadtteile) %>%
  select(Stadtteil) %>%
  pull()



## Geocoding durchführen -------------------------------------------------------


Geocoding_Stadtteile <- Analysedaten_neu %>%
  filter(!(Stadtteil %in% St_Teile))

if (nrow(Geocoding_Stadtteile) > 0) {
  Geocoding_Stadtteile <- Geocoding_Stadtteile %>%
    mutate(country = "Deutschland",
           city = "Hamburg") %>%
    geocode(method = "osm", country = country, city = city,
            postalcode = Postleitzahl, street = Straße) %>%
    select(-Stadtteil, -country, -city) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326, na.fail = FALSE) %>%
    st_transform(crs = st_crs(Geodaten_Stadtteile)) %>%
    st_join(Geodaten_Stadtteile) %>%
    mutate(Stadtteil_Quelle = case_when(!is.na(Stadtteil) ~ "Geocode_OSM")) %>%
    as.data.frame() %>%
    select(-geometry)
}



## Datensätze wieder verbinden -------------------------------------------------


Analysedaten_neu_geo <- Analysedaten_neu %>%
  filter(Stadtteil %in% St_Teile) %>%
  mutate(Stadtteil_Quelle = "WG_Gesucht", .after = Stadtteil) %>%
  rbind(Geocoding_Stadtteile)


print(" ")
print(paste0(nrow(Geocoding_Stadtteile %>% filter(!is.na(Stadtteil))), " Stadtteil(e) über Geocoding ermittelt"))
print(paste0("Für ", nrow(Geocoding_Stadtteile %>% filter(is.na(Stadtteil))), " Anzeige(n) konnte kein Stadtteil ermittelt werden"))




################################################################################
###                                                                          ###
###                         SPEICHERN DER ANALYSEDATEN                       ###
###                                                                          ###
################################################################################



## Speichern Analyse-Gesamtdatensatz -------------------------------------------

Analysedaten_gesamt <- read_csv("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Daten\\Analysedaten\\Analysedaten.csv",
                                show_col_types = FALSE) %>%
  rbind(Analysedaten_neu_geo) 


write.csv(Analysedaten_gesamt, "C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Daten\\Analysedaten\\Analysedaten.csv", 
          row.names = FALSE)


## Speichern Backups Analysedaten ----------------------------------------------

write.csv(Analysedaten_neu_geo, paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Daten\\Backup\\Analysedaten\\", analysedaten_filename), 
          row.names = FALSE)


message("Speichern der Analysedaten erfolgreich")



