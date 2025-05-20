


################################################################################
################################################################################
#####                                                                      #####
#####                 SCRAPING VON WG-GESUCHT HAMBURG - AUTO               #####
#####                                                                      #####
################################################################################
################################################################################



library(tidyverse)
library(rvest)
library(httr)



################################################################################
#####                                                                      #####
#####                        VORBEREITUNG DES LOOPS                        #####
#####                                                                      #####
################################################################################


## Link WG-Gesucht Hamburg

Link_Stadt <- "https://www.wg-gesucht.de/wg-zimmer-in-Hamburg.55.0.1."



## Vektor für Selektionslinks nicht älter als 60 Tage erstellen

Selektionslinks <- read_csv("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Daten\\Analysedaten\\Analysedaten.csv", 
                            col_select = c("Link", "Datum_Scraping"),
                            show_col_types = FALSE) %>%
  
  bind_rows() %>%
  filter(Datum_Scraping > Sys.Date() - 60) %>%
  select(-Datum_Scraping) %>%
  pull()



## Leeren Datensatz erstellen

Rohdaten_neu <- tibble()



## Dateinamen festlegen

rohdaten_filename <- paste0("Rohdaten ", format(Sys.time(), "%d.%m.%Y {%H-%M}"), ".csv")
analysedaten_filename <- paste0("Analysedaten ", format(Sys.time(), "%d.%m.%Y {%H-%M}"), ".csv")
log_filename <- paste0("Scrapinglog ", format(Sys.time(), "%d.%m.%Y {%H-%M}"), ".txt")



## Spoofing User Agent

ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")



## Funktion für die einzelnen Variablen der Subdaten schreiben


Fun_Subdata = function(Link_Subdata) {
  
  WG_Angebot <- read_html(GET(Link_Subdata, ua))
  
  
  Titel <- WG_Angebot %>%
    html_node("h1.headline.headline-detailed-view-title span:last-child") %>%
    html_text(trim = TRUE)
  
  Sys.sleep(1)
  
  WG_Konstellation <- WG_Angebot %>%
    html_node("span.mr5") %>%
    html_attr("title")
  
  Sys.sleep(1)
  
  Zimmergröße_Gesamtmiete <- WG_Angebot %>%
    html_nodes("b.key_fact_value") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(1)
  
  Adresse <- WG_Angebot %>%
    html_node(".col-sm-6 .col-xs-12 .section_panel_detail") %>%
    html_text(trim = TRUE)
  
  Sys.sleep(1)
  
  Datum <- WG_Angebot %>%
    html_node(".col-sm-6+ .col-sm-6:nth-child(2)") %>%
    html_text(trim = TRUE)
  
  Sys.sleep(1)
  
  WG_Details <- WG_Angebot %>%
    html_nodes(".pl15 .section_panel_detail") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(1)
  
  Kostenfeld <- WG_Angebot %>%
    html_nodes(".row:nth-child(6) .section_panel") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(1)
  
  Angaben_zum_Objekt <- WG_Angebot %>%
    html_nodes(".utility_icons") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(1)
  
  Freitext_Zimmer <- WG_Angebot %>%
    html_nodes("#freitext_0 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(1)
  
  Freitext_Lage <- WG_Angebot %>%
    html_nodes("#freitext_1 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(1)
  
  Freitext_WG_Leben <- WG_Angebot %>%
    html_nodes("#freitext_2 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(1)
  
  Freitext_Sonstiges <- WG_Angebot %>%
    html_nodes("#freitext_3 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  return(c(Titel, WG_Konstellation, Zimmergröße_Gesamtmiete, Adresse, Datum,
           WG_Details, Kostenfeld, Angaben_zum_Objekt, Freitext_Zimmer,
           Freitext_Lage, Freitext_WG_Leben, Freitext_Sonstiges))
}





################################################################################
#####                                                                      #####
#####                             Scrapingloop                             #####
#####                                                                      #####
################################################################################


## Scrapinglog mit Ergebnissen speichern

sink(
  paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Logs\\", log_filename)
)


print(paste0("--- Scrapinglog ", format(Sys.time(), "%d.%m.%Y {%H-%M}"), " ---"))
print("")

message(" ")
message("---------- BEGINNE SCRAPING ------------")
message(" ")


for(Seite in seq(0, 4, 1)) {
  
  
  # Verzögerung der Seitenabfrage einbauen:
  
  Sys.sleep(5)
  
  message(paste0("------------ Starte Loop ",Seite + 1, " -------------"))
  
  # Zusammensetzung der Links bestimmen und als html auslesen
  
  link = paste0(Link_Stadt, Seite, ".html")
  Url <- read_html(GET(link, ua))
  
  
  
  
  # Linksammlung Subdaten und aussortieren bereits gescrapter Links
  
  Sublinks <- Url %>%
    html_nodes(".offer_list_item .truncate_title a") %>%
    html_attr("href") %>%
    paste0("https://www.wg-gesucht.de", .) %>%
    setdiff(Selektionslinks)
  
  
  
  
  ## Prüfen, ob neue Links generiert wurden
  
  if (length(Sublinks) > 0) {
    
    print(paste0("Neue Links gefunden | Seite: ", Seite + 1))
    
    WG_Subdaten <- sapply(Sublinks, Fun_Subdata)
    
    
    Rohdaten_neu <- rbind(Rohdaten_neu, 
                          tibble(Link = as.vector(Sublinks),
                                 Titel = WG_Subdaten[1,], 
                                 WG_Konstellation = WG_Subdaten[2,],
                                 Zimmergröße_Gesamtmiete = WG_Subdaten[3,], 
                                 Adresse = WG_Subdaten[4,], 
                                 Datum = WG_Subdaten[5,], 
                                 WG_Details = WG_Subdaten[6,],
                                 Kostenfeld = WG_Subdaten[7,],
                                 Angaben_zum_Objekt = WG_Subdaten[8,],
                                 Freitext_Zimmer = WG_Subdaten[9,],
                                 Freitext_Lage = WG_Subdaten[10,],
                                 Freitext_WG_Leben = WG_Subdaten[11,],
                                 Freitext_Sonstiges = WG_Subdaten[12,],
                                 Datum_Scraping = Sys.Date()))
    
    
    print({
      zeile <- replace(WG_Subdaten[1, ], is.na(WG_Subdaten[1, ]), "")
      
      if (all(zeile == "")) {
        paste0("S. ", Seite + 1, " | Kein Scraping")
        
      } else if (any(zeile == "")) {
        paste0("S. ", Seite + 1, " | Scraping teilweise erfolgreich: Erfolgreich ", 
               sum(zeile != ""), " / Fehlgeschlagen ", sum(zeile == ""))
        
      } else {
        paste0("S. ", Seite + 1, " | Scraping erfolgreich: ", length(zeile), 
               " Link(s) gescraped.")
      }
    })
    
    
  } else {
    print(paste0("Keine neuen Sublinks | Seite ", Seite + 1))
  }
  
  
  print(" ")
  
}  







################################################################################
#####                                                                      #####
#####                          Rohdaten speichern                          #####
#####                                                                      #####
################################################################################


if (nrow(Rohdaten_neu) == 0 || ncol(Rohdaten_neu) == 0) {
  print("KEINE NEUEN ANZEIGEN GESCRAPED")
  sink()
  stop("Skript wird beendet")
}


## Nicht vollständig gescrapte Fälle entfernen

Rohdaten_neu_gefiltert <- Rohdaten_neu %>%
  filter(!(is.na(Titel)))



## Speichern des Backups für Rohdaten

write.csv(Rohdaten_neu_gefiltert, paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Daten\\Backup\\Rohdaten\\", rohdaten_filename), 
          row.names = FALSE)



## Neue Daten mit altem Datensatz verbinden 

Rohdaten_gesamt <- read_csv("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Daten\\Rohdaten\\Rohdaten.csv",
                            show_col_types = FALSE) %>%
  rbind(Rohdaten_neu_gefiltert)  # %>%
#  distinct(Link, .keep_all = TRUE)




## Alten Datensatz mit neuem Überschreiben 

write.csv(Rohdaten_gesamt, "C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Daten\\Rohdaten\\Rohdaten.csv", row.names = FALSE)






################################################################################
#####                                                                      #####
#####                    Datenaufbereitung und Geocoding                   #####
#####                                                                      #####
################################################################################


message("---------- DATENAUFBEREITUNG -----------")

source("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Skripte\\Datenscraping\\Aufbereitungsskript_Hamburg_auto.R")

message(" ")






################################################################################
#####                                                                      #####
#####                       Daten in Cloud speichern                       #####
#####                                                                      #####
################################################################################


write.csv(Rohdaten_neu_gefiltert, paste0("C:\\Users\\Fabian Hellmold\\Dropbox\\WG_Gesucht\\Hamburg\\Daten\\Backup Rohdaten\\", rohdaten_filename), 
          row.names = FALSE)


write.csv(Analysedaten_gesamt, "C:\\Users\\Fabian Hellmold\\Dropbox\\WG_Gesucht\\Hamburg\\Daten\\Analysedaten\\Analysedaten.csv", 
          row.names = FALSE) 


file.copy(from = paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Logs\\", log_filename), 
          to = paste0("C:\\Users\\Fabian Hellmold\\Dropbox\\WG_Gesucht\\Hamburg\\Logs\\", log_filename), 
          overwrite = TRUE)


sink()

