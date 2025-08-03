


################################################################################
################################################################################
#####                                                                      #####
#####                        DATENSPEICHERUNG - AUTO                       #####
#####                                                                      #####
################################################################################
################################################################################



library(DBI)

flog.info("== BEGINN DATENSPEICHERUNG ====================")



################################################################################
#####                                                                      #####
#####                          Rohdaten speichern                          #####
#####                                                                      #####
################################################################################


## Speichern Backup Rohddaten --------------------------------------------------

write.csv(Rohdaten_neu_gefiltert, paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\",stadt,"\\Daten\\Backup\\Rohdaten\\", rohdaten_filename), 
          row.names = FALSE)


## Speichern Roh-Gesamtdatensatz -----------------------------------------------

Rohdaten_gesamt <- read_csv(paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\",stadt,"\\Daten\\Rohdaten\\Rohdaten.csv"),
                            show_col_types = FALSE) %>%
  rbind(Rohdaten_neu_gefiltert)  

write.csv(Rohdaten_gesamt, paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\",stadt,"\\Daten\\Rohdaten\\Rohdaten.csv"), row.names = FALSE)


flog.info("Speichern der Rohdaten erfolgreich")



################################################################################
###                                                                          ###
###                         SPEICHERN DER ANALYSEDATEN                       ###
###                                                                          ###
################################################################################


## Speichern Backup Analysedaten -----------------------------------------------

write.csv(Analysedaten_neu_geo, paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\",stadt,"\\Daten\\Backup\\Analysedaten\\", analysedaten_filename), 
          row.names = FALSE)


## Speichern Analyse-Gesamtdatensatz -------------------------------------------

Analysedaten_gesamt <- read_csv(paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\",stadt,"\\Daten\\Analysedaten\\Analysedaten.csv"),
                                show_col_types = FALSE) %>%
  rbind(Analysedaten_neu_geo) 

write.csv(Analysedaten_gesamt, paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\",stadt,"\\Daten\\Analysedaten\\Analysedaten.csv"), 
          row.names = FALSE)


flog.info("Speichern der Analysedaten erfolgreich")



################################################################################
#####                                                                      #####
#####                            SQL-SERVER LOKAL                          #####
#####                                                                      #####
################################################################################


# usethis::edit_r_environ()


## Verbindung zu lokalem SQL-Server herstellen ---------------------------------

tryCatch({
  
  con <- dbConnect(odbc::odbc(),
                   Driver = "SQL Server",
                   Server = Sys.getenv("DB_SERVER"),
                   Database = Sys.getenv("DB_NAME"),
                   Trusted_Connection = "Yes",
                   Encrypt = "No")
  flog.info("SQL-Verbindung hergestellt")
  
}, error = function(e) {
  
  flog.error("Fehler bei SQL-Verbindung: %s", e$message)
  
})



## String-Variablen aufbereiten ------------------------------------------------

remove_special_chars <- function(x) {
  x <- stri_enc_toutf8(x, validate = TRUE)
  x <- stri_replace_all_regex(x, "\\p{Z}+", " ")
  x <- stri_replace_all_regex(x, "[\\p{C}&&[^\\r\\n\\t]]", "")
  x <- stri_trim_both(x)
  x
}

Analysedaten_clean <- Analysedaten_neu_geo %>%
  mutate(across(where(is.character), ~ remove_special_chars(.))) %>%
  mutate(
    titel = substr(titel, 1, 750),
    link = substr(link, 1, 1000),
    angaben_zum_objekt = substr(angaben_zum_objekt, 1, 1750),
    freitext_zimmer_1 = substr(freitext_zimmer, 1, 3500),
    freitext_zimmer_2 = substr(freitext_zimmer, 3501, 7000),
    freitext_zimmer_3 = substr(freitext_zimmer, 7001, 10500),
    freitext_lage_1 = substr(freitext_lage, 1, 3500),
    freitext_lage_2 = substr(freitext_lage, 3501, 7000),
    freitext_lage_3 = substr(freitext_lage, 7001, 10500),
    freitext_wg_leben_1 = substr(freitext_wg_leben, 1, 3500),
    freitext_wg_leben_2 = substr(freitext_wg_leben, 3501, 7000),
    freitext_wg_leben_3 = substr(freitext_wg_leben, 7001, 10500),
    freitext_sonstiges_1 = substr(freitext_sonstiges, 1, 3500),
    freitext_sonstiges_2 = substr(freitext_sonstiges, 3501, 7000),
    freitext_sonstiges_3 = substr(freitext_sonstiges, 7001, 10500)
  ) %>%
  select(-freitext_zimmer,-freitext_lage,
         -freitext_wg_leben,-freitext_sonstiges)

flog.info("%s Freitextfelder getrimmt",
          sum(
            sum(nchar(Analysedaten_clean$freitext_zimmer_3) == 3500, na.rm = T),
            sum(nchar(Analysedaten_clean$freitext_lage_3) == 3500, na.rm = T),
            sum(nchar(Analysedaten_clean$freitext_wg_leben_3) == 3500, na.rm = T),
            sum(nchar(Analysedaten_clean$freitext_sonstiges_3) == 3500, na.rm = T)
            )
         )


## Anzeigen in SQL-Datenbank speichern -----------------------------------------

tryCatch({
  
  dbWriteTable(con, "analysedaten", Analysedaten_clean,
               append = TRUE)
  
  flog.info("%d Anzeigen in SQL gespeichert", nrow(Analysedaten_clean))
  
}, error = function(e) {
  
  flog.error("Fehler beim Speichern in  SQL: %s", e$message)
  
})



dbDisconnect(con)

flog.info("== ENDE DATENSPEICHERUNG ======================")
flog.info("")

