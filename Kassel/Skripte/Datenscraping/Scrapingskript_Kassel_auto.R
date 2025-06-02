


################################################################################
################################################################################
#####                                                                      #####
#####                 SCRAPING VON WG-GESUCHT KASSEL - AUTO                #####
#####                                                                      #####
################################################################################
################################################################################


library(tidyverse)
library(futile.logger)
library(rvest)
library(httr)




################################################################################
#####                                                                      #####
#####                         VORBEREITUNG DER LOGS                        #####
#####                                                                      #####
################################################################################


dual_appender <- function(line) {
  appender.console()(line)
  appender.file(paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Log_Test\\Log_Kassel ",format(Sys.time(), "%d.%m.%Y {%H}"),".txt"))(line)
}

flog.layout(layout.format("~l [~t] ~m")) 
flog.appender(dual_appender)


flog.info("########### BEGINN SCRAPING KASSEL ##########")
flog.info("")




################################################################################
#####                                                                      #####
#####                        VORBEREITUNG DES LOOPS                        #####
#####                                                                      #####
################################################################################


## Link WG-Gesucht Kassel

Link_Stadt <- "https://www.wg-gesucht.de/wg-zimmer-in-Kassel.69.0.1."



## Vektor für Selektionslinks nicht älter als 60 Tage erstellen

Selektionslinks <- read_csv("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Kassel\\Daten\\Analysedaten\\Analysedaten.csv", 
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
Uhrzeit_Scraping <- format(Sys.time(), "%H-%M")



## Funktion für die einzelnen Variablen der Subdaten schreiben


Fun_Subdata = function(Link_Subdata) {
  
  WG_Angebot <- read_html(GET(Link_Subdata, proxy_obj, ua_obj))
  
  
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
#####                          Proxyserver laden                           #####
#####                                                                      #####
################################################################################


source("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Kassel\\Skripte\\Datenscraping\\Proxyzugang_Kassel_auto.R")




################################################################################
#####                                                                      #####
#####                             Scrapingloop                             #####
#####                                                                      #####
################################################################################


flog.info("== BEGINN DATENSCRAPING =====================")


for (Seite in seq(0, 1, 1)) {
  
  Sys.sleep(5)
  
  flog.info("--------------- Starte Loop %d ---------------", Seite + 1)
  
  tryCatch({
    
    link <- paste0(Link_Stadt, Seite, ".html")
    Url <- read_html(GET(link, proxy_obj, ua_obj))
    
    Sublinks <- Url %>%
      html_nodes(".offer_list_item .truncate_title a") %>%
      html_attr("href") %>%
      paste0("https://www.wg-gesucht.de", .) %>%
      setdiff(Selektionslinks)
    
    if (length(Sublinks) > 0) {
      
      flog.info("S.%d | DL1 / %d neue(r) Link(s) gefunden",  Seite + 1, length(Sublinks))
      
      proxy_obj <- proxy_tested %>%
        slice_sample(n = 1) %>%
        { use_proxy(url = .$ip,
                    port = as.numeric(.$port),
                    username = .$user,
                    password = .$password)
        }
      
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
                                   Datum_Scraping = Sys.Date(),
                                   Uhrzeit_Scraping = Uhrzeit_Scraping))
      
      if (all(is.na(Rohdaten_neu[,1]))) {
        
        flog.warn("S.%d | DL1 / Kein Scraping", Seite + 1)
        
        proxy_obj <- proxy_tested %>%
          slice_sample(n = 1) %>%
          { use_proxy(url = .$ip,
                      port = as.numeric(.$port),
                      username = .$user,
                      password = .$password)
          }
        
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
                                     Datum_Scraping = Sys.Date(),
                                     Uhrzeit_Scraping = Uhrzeit_Scraping))
        
        flog.info("Hier noch Feedback zur Wiederholung")
        
      } else if (any(is.na(Rohdaten_neu[,1]))) {
        
        flog.warn("S.%d | DL1 / Scraping tw erfolgreich: E%d F%d",
                  Seite + 1, sum(!is.na(Rohdaten_neu[,1])),
                  sum(is.na(Rohdaten_neu[,1])))
        
        Sublinks <- Rohdaten_neu %>% filter(is.na(Titel)) %>% select(Link) %>% pull()
        
        proxy_obj <- proxy_tested %>%
          slice_sample(n = 1) %>%
          { use_proxy(url = .$ip,
                      port = as.numeric(.$port),
                      username = .$user,
                      password = .$password)
          }
        
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
                                     Datum_Scraping = Sys.Date(),
                                     Uhrzeit_Scraping = Uhrzeit_Scraping))
        
        flog.info("S.%d | DL1 / Proxy: %s:%s", 
                  Seite+1, proxy_obj[["options"]][["proxy"]], 
                  proxy_obj[["options"]][["proxyport"]])
        
      } else {
        
        flog.info("S.%d | DL1 / Scraping erfolgreich: %d Link(s)",
                  Seite + 1, length(WG_Subdaten[1, ]))
        
        flog.info("S.%d | DL1 / Proxy: %s:%s", 
                  Seite+1, proxy_obj[["options"]][["proxy"]], 
                  proxy_obj[["options"]][["proxyport"]])        
      }
      
    } else {
      flog.info("S.%d | DL1 / Keine neuen Sublinks", Seite + 1)
    }
    
    flog.info("S.%d | DL1 / ERFOLGREICH", Seite + 1)
    
  }, error = function(e1) {
    
    flog.warn("S.%d | DL1 / Fehler: %s", Seite + 1, e1$message)
    
    tryCatch({
      
      Sys.sleep(30)
      
      link <<- paste0(Link_Stadt, Seite, ".html")
      Url <<- read_html(GET(link, proxy_obj, ua_obj))
      
      Sublinks <<- Url %>%
        html_nodes(".offer_list_item .truncate_title a") %>%
        html_attr("href") %>%
        paste0("https://www.wg-gesucht.de", .) %>%
        setdiff(Selektionslinks) 
      
      if (length(Sublinks) > 0) {
        
        flog.info("S.%d | DL2 / %d neue(r) Link(s) gefunden",  Seite + 1, length(Sublinks))
        
        proxy_obj <<- proxy_tested %>%
          slice_sample(n = 1) %>%
          { use_proxy(url = .$ip,
                      port = as.numeric(.$port),
                      username = .$user,
                      password = .$password)
          }
        
        WG_Subdaten <<- sapply(Sublinks, Fun_Subdata)
        
        Rohdaten_neu <<- rbind(Rohdaten_neu, 
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
                                      Datum_Scraping = Sys.Date(),
                                      Uhrzeit_Scraping = Uhrzeit_Scraping))
        
        if (all(is.na(Rohdaten_neu[,1]))) {
          
          flog.warn("S.%d | DL2 / Kein Scraping", Seite + 1)
          
          proxy_obj <<- proxy_tested %>%
            slice_sample(n = 1) %>%
            { use_proxy(url = .$ip,
                        port = as.numeric(.$port),
                        username = .$user,
                        password = .$password)
            }
          
          WG_Subdaten <<- sapply(Sublinks, Fun_Subdata)
          
          Rohdaten_neu <<- rbind(Rohdaten_neu, 
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
                                        Datum_Scraping = Sys.Date(),
                                        Uhrzeit_Scraping = Uhrzeit_Scraping))
          
          flog.info("S.%d | DL2 / Proxy: %s:%s", 
                    Seite+1, proxy_obj[["options"]][["proxy"]], 
                    proxy_obj[["options"]][["proxyport"]])          
          flog.info("Hier noch Feedback zur Wiederholung")
          
        } else if (any(is.na(Rohdaten_neu[,1]))) {
          
          flog.warn("S.%d | DL2 / Scraping tw erfolgreich: E%d F%d",
                    Seite + 1, sum(!is.na(Rohdaten_neu[,1])),
                    sum(is.na(Rohdaten_neu[,1])))
          
          Sublinks <<- Rohdaten_neu %>% filter(is.na(Titel)) %>% select(Link) %>% pull()
          
          proxy_obj <<- proxy_tested %>%
            slice_sample(n = 1) %>%
            { use_proxy(url = .$ip,
                        port = as.numeric(.$port),
                        username = .$user,
                        password = .$password)
            }
          
          WG_Subdaten <<- sapply(Sublinks, Fun_Subdata)
          
          Rohdaten_neu <<- rbind(Rohdaten_neu, 
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
                                        Datum_Scraping = Sys.Date(),
                                        Uhrzeit_Scraping = Uhrzeit_Scraping))
          
          flog.info("S.%d | DL2 / Proxy: %s:%s", 
                    Seite+1, proxy_obj[["options"]][["proxy"]], 
                    proxy_obj[["options"]][["proxyport"]])
        } else {
          flog.info("S.%d | DL2 / Scraping erfolgreich: %d Link(s)",
                    Seite + 1, length(WG_Subdaten[1, ]))
          
          flog.info("S.%d | DL2 / Proxy: %s:%s", 
                    Seite+1, proxy_obj[["options"]][["proxy"]], 
                    proxy_obj[["options"]][["proxyport"]])
        }
        
      } else {
        flog.info("S.%d | DL2 / Keine neuen Sublinks", Seite + 1)
      }
      
      flog.info("S.%d | DL2 / ERFOLGREICH", Seite + 1)
      
    }, error = function(e2) {
      
      flog.warn("S.%d | DL2 / Fehler: %s", Seite + 1, e2$message)
      flog.error("S.%d | DL1+2 / KEINE DATEN", Seite + 1)
      
    })
  })
  
}


## Nicht vollständig gescrapte Fälle und Dubletten entfernen

Rohdaten_neu_gefiltert <- Rohdaten_neu %>%
  filter(!(is.na(Titel))) %>%   
  distinct(across(-Datum), .keep_all = TRUE)


flog.info("== ENDE DATENSCRAPING =======================")
flog.info("")




################################################################################
#####                                                                      #####
#####                          Rohdaten speichern                          #####
#####                                                                      #####
################################################################################


if (nrow(Rohdaten_neu_gefiltert) > 0 || ncol(Rohdaten_neu_gefiltert) > 0) {
  
flog.info("== BEGINN DATENARBEIT =======================")
  
## Speichern des Backups für Rohdaten
write.csv(Rohdaten_neu_gefiltert, paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Kassel\\Daten\\Backup\\Rohdaten\\", rohdaten_filename), 
          row.names = FALSE)

## Neue Daten mit altem Datensatz verbinden 
Rohdaten_gesamt <- read_csv("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Kassel\\Daten\\Rohdaten\\Rohdaten.csv",
                            show_col_types = FALSE) %>%
  rbind(Rohdaten_neu_gefiltert)  

## Alten Datensatz mit neuem Überschreiben 
write.csv(Rohdaten_gesamt, "C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Kassel\\Daten\\Rohdaten\\Rohdaten.csv", row.names = FALSE)


flog.info("Speichern der Rohdaten erfolgreich")



################################################################################
#####                                                                      #####
#####                    Datenaufbereitung und Geocoding                   #####
#####                                                                      #####
################################################################################


source("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Kassel\\Skripte\\Datenscraping\\Aufbereitungsskript_Kassel_auto.R")




################################################################################
#####                                                                      #####
#####                       Daten in Cloud speichern                       #####
#####                                                                      #####
################################################################################


write.csv(Rohdaten_neu_gefiltert, paste0("C:\\Users\\Fabian Hellmold\\Dropbox\\WG_Gesucht\\Kassel\\Daten\\Backup Rohdaten\\", rohdaten_filename), 
          row.names = FALSE)


write.csv(Analysedaten_gesamt, "C:\\Users\\Fabian Hellmold\\Dropbox\\WG_Gesucht\\Kassel\\Daten\\Analysedaten\\Analysedaten.csv", 
          row.names = FALSE) 

flog.info("######### ABSCHLUSS SCRAPING KASSEL #########")


} else {
  
  flog.info("Keine neuen Anzeigen gescraped")
  flog.info("")
  flog.info("######### ABSCHLUSS SCRAPING KASSEL #########")
  
} 
  


file.copy(from = paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Log_Test\\Log_Kassel ",format(Sys.time(), "%d.%m.%Y {%H}"),".txt"), 
          to = paste0("C:\\Users\\Fabian Hellmold\\Dropbox\\WG_Gesucht\\Kassel\\Logs\\Log_Kassel ",format(Sys.time(), "%d.%m.%Y {%H}"),".txt"),
          overwrite = TRUE)


