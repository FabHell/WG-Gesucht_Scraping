


################################################################################
################################################################################
#####                                                                      #####
#####               AUFBEREITUNG VON WG-GESUCHT HAMBURG - AUTO             #####
#####                                                                      #####
################################################################################
################################################################################



library(tidyverse)
library(tidygeocoder)
library(sf)
library(stringi)


flog.info("== BEGINN DATENARBEIT =========================")




################################################################################
###                                                                          ###
###                       AUFBEREITUNG STRINGVARIABLEN                       ###
###                                                                          ###
################################################################################


Rohdaten_neu_gefiltert <- Rohdaten_neu %>%
  filter(!(is.na(titel))) %>%
  distinct(across(-c(datum, seite_scraping)), .keep_all = TRUE)


Analysedaten_neu <- Rohdaten_neu_gefiltert %>%
  
  mutate(personenzahl = str_extract(wg_konstellation, "\\d{1,2}er WG"),
         personenzahl = str_remove(personenzahl, "er WG"),
         personenzahl = as.numeric(personenzahl),
         
         zusammensetzung = str_extract(wg_konstellation, "\\d{1}w\\,\\d{1}m\\,\\d{1}d")) %>%
  
  select(-wg_konstellation) %>%
  
  
  mutate(zimmergröße = str_extract(zimmergröße_gesamtmiete, "\\d{1,2}m²"),
         zimmergröße = str_remove(zimmergröße, "m²"),
         zimmergröße = as.numeric(zimmergröße),
         
         gesamtmiete = str_extract(zimmergröße_gesamtmiete, "\\d{1,4}€"),
         gesamtmiete = str_remove(gesamtmiete, "€"),
         gesamtmiete = as.numeric(gesamtmiete)) %>%
  
  select(-zimmergröße_gesamtmiete) %>%
  
  
  mutate(datum = str_squish(str_replace_all(datum, "\\s+", " ")),
         
         einzugsdatum = str_extract(datum, "frei ab: \\d{2}\\.\\d{2}\\.\\d{4}"),
         einzugsdatum = str_remove(einzugsdatum, "frei ab: "),
         einzugsdatum = as.Date(einzugsdatum, format = "%d.%m.%Y"),
         
         befristung_enddatum = str_extract(datum, "frei bis: \\d{2}\\.\\d{2}\\.\\d{4}"),
         befristung_enddatum = str_remove(befristung_enddatum, "frei bis: "),
         befristung_enddatum = as.Date(befristung_enddatum, format = "%d.%m.%Y"),
         
         befristungsdauer = as.numeric(befristung_enddatum - einzugsdatum)) %>%
  
  select(-datum) %>%
  
  
  mutate(wg_details = str_squish(str_replace_all(wg_details, "\\s+", " ")),
         
         rauchen = str_extract(wg_details, "Rauchen überall erlaubt|Rauchen im Zimmer erlaubt|Rauchen auf dem Balkon erlaubt|Rauchen nicht erwünscht"),
         
         sprache = str_extract(wg_details, "Sprache/n:\\s*[^|]+"),
         sprache = str_remove(sprache, "Sprache/n: "),
         
         bewohneralter = str_extract(wg_details, "Bewohneralter:\\s*\\d+ bis \\d+ Jahre"),
         bewohneralter = str_remove(bewohneralter, "Bewohneralter: "),
         bewohneralter = str_remove(bewohneralter, " Jahre"),
         
         wohnungsgröße = str_extract(wg_details, "(Wohnungsgröße:\\s*)\\d+(?=m²)"),
         wohnungsgröße = str_remove(wohnungsgröße, "Wohnungsgröße: "),
         wohnungsgröße = as.numeric(wohnungsgröße),
         
         wg_art = wg_details %>%
           str_extract_all(
             "Studenten-WG|keine Zweck-WG|Männer-WG|Business-WG|Wohnheim|Vegetarisch/Vegan|Alleinerziehende|funktionale WG|Berufstätigen-WG|gemischte WG|WG mit Kindern|Verbindung|LGBTQIA\\+|Senioren-WG|inklusive WG|WG-Neugründung|Zweck-WG|Frauen-WG|Plus-WG|Mehrgenerationen|Azubi-WG|Wohnen für Hilfe|Internationals welcome") %>%
           map_chr(~ str_c(.x, collapse = ", ")),
         
         geschlecht_ges = map_chr(str_extract_all(wg_details, 
                                                  "(Mann|Frau|Divers|Geschlecht egal)"), ~ last(.x)),
         
         alter_ges = str_extract(wg_details, "(?<=zwischen )\\d{2} und \\d{2}")) %>%
  
  select(-wg_details) %>%
  
  mutate(straße = str_split(adresse, "\\s{3,}", simplify = TRUE)[,1],
         
         plz_stadtteil = str_split(adresse, "\\s{3,}", simplify = TRUE)[,2],
         plz_stadtteil = str_remove(plz_stadtteil, paste0(stadt, " ")),
         postleitzahl = str_extract(plz_stadtteil, "\\b\\d{4,5}\\b"),
         stadtteil = str_remove(plz_stadtteil, "\\b\\d{4,5}\\b\\s*")) %>%
  
  select(-plz_stadtteil) %>%
  
  
  mutate(kostenfeld = str_squish(str_replace_all(kostenfeld, "\\s+", " ")),
         kostenfeld = str_remove(kostenfeld, " Nebenkosten sind geschätzte Kosten, die auf dem Verbrauch des Vormieters basieren und monatlich im Voraus bezahlt werden. Am Jahresende rechnet der Vermieter die Vorauszahlungen mit dem tatsächlichen Verbrauch des Mieters ab. Infolgedessen muss der Mieter eine Nachzahlung leisten oder er erhält eine Rückzahlung."),
         kostenfeld = str_remove(kostenfeld, " SCHUFA-Auskunft: In 3 Minuten bereit 1"),
         kostenfeld = str_replace(kostenfeld, "Kosten Miete: ", "Miete: "),
         
         kaltmiete = str_extract(kostenfeld, "Miete: \\d{1,4}"),
         kaltmiete = str_remove(kaltmiete, "Miete: "),
         kaltmiete = as.numeric(kaltmiete),
         
         nebenkosten = str_extract(kostenfeld, "Nebenkosten: \\d{1,4}"),
         nebenkosten = str_remove(nebenkosten, "Nebenkosten: "),
         nebenkosten = as.numeric(nebenkosten),
         
         kaution = str_extract(kostenfeld, "Kaution: \\d{1,4}"),
         kaution = str_remove(kaution, "Kaution: "),
         kaution = as.numeric(kaution),
         
         sonstige_kosten = str_extract(kostenfeld, "Sonstige Kosten: \\d{1,4}"),
         sonstige_kosten = str_remove(sonstige_kosten, "Sonstige Kosten: "),
         sonstige_kosten = as.numeric(sonstige_kosten),
         
         ablösevereinbarung = str_extract(kostenfeld, "Ablösevereinbarung: \\d{1,4}"),
         ablösevereinbarung = str_remove(ablösevereinbarung, "Ablösevereinbarung: "),
         ablösevereinbarung = as.numeric(ablösevereinbarung)) %>%
  
  select(-kostenfeld) %>%
  
  mutate(angaben_zum_objekt = angaben_zum_objekt %>%
           stri_replace_all_regex("\\p{Z}+", " ") %>%     
           stri_replace_all_regex("[\\p{C}&&[^\\r\\n\\t]]", "") %>% 
           str_squish()) %>%
  
  mutate(freitext_zimmer = str_squish(str_replace_all(freitext_zimmer, "\\s+", " ")),
         freitext_lage = str_squish(str_replace_all(freitext_lage, "\\s+", " ")),
         freitext_wg_leben = str_squish(str_replace_all(freitext_wg_leben, "\\s+", " ")),
         freitext_sonstiges = str_squish(str_replace_all(freitext_sonstiges, "\\s+", " "))) %>%
  
  
  select(land, stadt, titel, link, stadtteil, postleitzahl, straße, gesamtmiete, 
         kaltmiete, nebenkosten, kaution, sonstige_kosten, ablösevereinbarung, 
         zimmergröße, personenzahl, wohnungsgröße, bewohneralter, einzugsdatum, 
         zusammensetzung, befristung_enddatum, befristungsdauer, geschlecht_ges, 
         alter_ges, wg_art, rauchen, sprache, angaben_zum_objekt, freitext_zimmer, 
         freitext_lage, freitext_wg_leben, freitext_sonstiges, seite_scraping,
         uhrzeit_scraping, datum_scraping)



flog.info("Bearbeitung der Sting-Variablen erfolgreich")




################################################################################
###                                                                          ###
###                         Geocoding der Stadtteile                         ###
###                                                                          ###
################################################################################


## Geocoding durchführen -------------------------------------------------------


St_Teile <- tibble(Geodaten_Stadtteile) %>%
  select(stadtteil) %>%
  pull()

Geocoding_Stadtteile <- Analysedaten_neu %>%
  filter(!(stadtteil %in% St_Teile))

if (nrow(Geocoding_Stadtteile) > 0) {
  
  tryCatch({
    Geocoding_Stadtteile <- Geocoding_Stadtteile %>%
      geocode(method = "osm", country = land, city = stadt,
              postalcode = postleitzahl, street = straße) %>%
      select(-stadtteil) %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326, na.fail = FALSE) %>%
      st_transform(crs = st_crs(Geodaten_Stadtteile)) %>%
      st_join(Geodaten_Stadtteile) %>%
      mutate(stadtteil_quelle = case_when(!is.na(stadtteil) ~ "Geocode_OSM")) %>%
      as.data.frame() %>%
      select(-geometry)   
    
    flog.info("%d Stadtteil(e) über Geocoding ermittelt",
              nrow(Geocoding_Stadtteile %>% filter(!is.na(stadtteil))))
    flog.info("%d Anzeige(n) ohne gültige Stadtteilangabe",
              nrow(Geocoding_Stadtteile %>% filter(is.na(stadtteil))))
    
  }, error = function(e) {
    flog.error("Fehler beim Geocoding: %s", e$message)   
  })
} else {
  flog.info("Stadtteildaten vollständig: Kein Geocoding")
}



## Datensätze wieder verbinden -------------------------------------------------


Analysedaten_neu_geo <- Analysedaten_neu %>%
  filter(stadtteil %in% St_Teile) %>%
  mutate(stadtteil_quelle = "WG_Gesucht", .before = stadtteil) %>%
  rbind(Geocoding_Stadtteile)


flog.info("== ENDE DATENARBEIT ===========================")
flog.info("")
