


################################################################################
################################################################################
#####                                                                      #####
#####                    SCRAPING VON WG-GESUCHT - AUTO                    #####
#####                                                                      #####
################################################################################
################################################################################


library(tidyverse)
library(futile.logger)
library(rvest)
library(httr)
library(DBI)
library(RPostgres)




################################################################################
#####                                                                      #####
#####                         VORBEREITUNG DER LOGS                        #####
#####                                                                      #####
################################################################################


dual_appender <- function(line) {
  appender.console()(line)
  appender.file(paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\",stadt,"\\Logs\\Log_",stadt," ",format(Sys.time(), "%d.%m.%Y {%H}"),".txt")
  )(line)
}

flog.layout(layout.format("~l [~t] ~m")) 
flog.appender(dual_appender)


flog.info("########### BEGINN SCRAPING - %s ###########", stadt)
flog.info("")




################################################################################
#####                                                                      #####
#####                        VORBEREITUNG DES LOOPS                        #####
#####                                                                      #####
################################################################################


## Vektor für Selektionslinks nicht älter als 60 Tage erstellen

Selektionslinks <- read_csv(paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\",stadt,"\\Daten\\Analysedaten\\Analysedaten.csv"), 
                            col_select = c("link", "datum_scraping"),
                            show_col_types = FALSE) %>%
  
  bind_rows() %>%
  filter(datum_scraping > Sys.Date() - 60) %>%
  select(-datum_scraping) %>%
  pull()



## Leeren Datensatz erstellen

Rohdaten_neu <- tibble()



## Dateinamen festlegen

rohdaten_filename <- paste0("Rohdaten ", format(Sys.time(), "%d.%m.%Y {%H-%M}"), ".csv")
analysedaten_filename <- paste0("Analysedaten ", format(Sys.time(), "%d.%m.%Y {%H-%M}"), ".csv")
uhrzeit_scraping <- format(Sys.time(), "%H-%M")


## Funktion für die einzelnen Variablen der Subdaten schreiben


Fun_Subdata = function(Link_Subdata) {
  
  WG_Angebot <- read_html(GET(Link_Subdata, proxy_obj, ua_obj))
  
  titel <- WG_Angebot %>%
    html_element("h1.detailed-view-title span:last-child") %>%
    html_text(trim = TRUE)
  
  Sys.sleep(runif(1, min = 0.5, max = 1.5))
  
  wg_konstellation <- WG_Angebot %>%
    html_node("span.mr5") %>%
    html_attr("title")
  
  Sys.sleep(runif(1, min = 0.5, max = 1.5))
  
  zimmergröße_gesamtmiete <- WG_Angebot %>%
    html_nodes("b.key_fact_value") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(runif(1, min = 0.5, max = 1.5))
  
  adresse <- WG_Angebot %>%
    html_node(".col-sm-6 .col-xs-12 .section_panel_detail") %>%
    html_text(trim = TRUE)
  
  Sys.sleep(runif(1, min = 0.5, max = 1.5))
  
  datum <- WG_Angebot %>%
    html_node(".col-sm-6+ .col-sm-6:nth-child(2)") %>%
    html_text(trim = TRUE)
  
  Sys.sleep(runif(1, min = 0.5, max = 1.5))
  
  wg_details <- WG_Angebot %>%
    html_nodes(".pl15 .section_panel_detail") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(runif(1, min = 0.5, max = 1.5))
  
  kostenfeld <- WG_Angebot %>%
    html_nodes(".row:nth-child(6) .section_panel") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(runif(1, min = 0.5, max = 1.5))
  
  angaben_zum_objekt <- WG_Angebot %>%
    html_nodes(".utility_icons") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(runif(1, min = 0.5, max = 1.5))
  
  freitext_zimmer <- WG_Angebot %>%
    html_nodes("#freitext_0 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(runif(1, min = 0.5, max = 1.5))
  
  freitext_lage <- WG_Angebot %>%
    html_nodes("#freitext_1 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(runif(1, min = 0.5, max = 1.5))
  
  freitext_wg_leben <- WG_Angebot %>%
    html_nodes("#freitext_2 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  Sys.sleep(runif(1, min = 0.5, max = 1.5))
  
  freitext_sonstiges <- WG_Angebot %>%
    html_nodes("#freitext_3 p") %>%
    html_text(trim = TRUE) %>%
    paste(collapse = "|")
  
  return(c(titel, wg_konstellation, zimmergröße_gesamtmiete, adresse, datum,
           wg_details, kostenfeld, angaben_zum_objekt, freitext_zimmer,
           freitext_lage, freitext_wg_leben, freitext_sonstiges))
}




################################################################################
#####                                                                      #####
#####                          Proxyserver laden                           #####
#####                                                                      #####
################################################################################


source("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Allg_Skripte\\Proxyzugang_allg.R")




################################################################################
#####                                                                      #####
#####                             Scrapingloop                             #####
#####                                                                      #####
################################################################################


flog.info("== BEGINN DATENSCRAPING =======================")


scrape_retries <- function(Seite, max_versuche = Wiederholungen) {
  
  attempt <- 1
  daten <- NULL
  
  while (is.null(daten) && attempt <= max_versuche) {
    
    tryCatch({
      
      proxy_obj <<- proxy_tested %>%
        slice_sample(n = 1) %>%
        { use_proxy(url = .$ip, port = as.numeric(.$port), 
                    username = .$user, password = .$password) }
      
      flog.info("S.%d | DL%d / Proxy: %s", 
                Seite + 1, attempt,
                proxy_obj[["options"]][["proxyuserpwd"]])
      
      link <- paste0(Link_Stadt, Seite, ".html")
      Url <- read_html(GET(link, proxy_obj, ua_obj))
      
      Sublinks <- Url %>%
        html_nodes("h2.truncate_title a") %>%
        html_attr("href") %>%
        .[str_starts(., "/wg-zimmer")] %>%
        paste0("https://www.wg-gesucht.de", .) %>%
        setdiff(Selektionslinks) 
      
      if (length(Sublinks) == 0) {
        flog.info("S.%d | DL%d / Keine neuen Links", Seite + 1, attempt)
        return(NULL)
      }
      
      WG_Subdaten <- sapply(Sublinks, Fun_Subdata)
      
      neue_daten <- tibble(
        land = land,
        stadt = stadt,
        link = as.vector(Sublinks),
        titel = WG_Subdaten[1,], 
        wg_konstellation = WG_Subdaten[2,],
        zimmergröße_gesamtmiete = WG_Subdaten[3,], 
        adresse = WG_Subdaten[4,], 
        datum = WG_Subdaten[5,], 
        wg_details = WG_Subdaten[6,],
        kostenfeld = WG_Subdaten[7,],
        angaben_zum_objekt = WG_Subdaten[8,],
        freitext_zimmer = WG_Subdaten[9,],
        freitext_lage = WG_Subdaten[10,],
        freitext_wg_leben = WG_Subdaten[11,],
        freitext_sonstiges = WG_Subdaten[12,],
        seite_scraping = Seite + 1,
        uhrzeit_scraping = uhrzeit_scraping,
        datum_scraping = Sys.Date()
      )
      
      if (all(is.na(neue_daten$titel))) {
        
        flog.warn("S.%d | DL%d / Kein Scraping", Seite + 1, attempt)
        
      } else if (any(is.na(neue_daten$titel))) {
        
        flog.warn("S.%d | DL%d / Teilweise Scrapingfehler: E%d F%d",
                  Seite + 1, attempt,
                  sum(!is.na(neue_daten$titel)), 
                  sum(is.na(neue_daten$titel)))
        
      } else {
        
        flog.info("S.%d | DL%d / Scraping erfolgreich: %d Link(s)", 
                  Seite + 1, attempt, length(Sublinks))
        
        daten <- neue_daten
        
      }
      
    }, error = function(e1) {
      flog.warn("S.%d | DL%d / Fehler: %s", Seite + 1, attempt, e1$message)
    })
    
    attempt <- attempt + 1
    Sys.sleep(runif(1, min = 4, max = 6))
  }
  
  if (is.null(daten)) {
    flog.error("S.%d | Alle %d Versuche fehlgeschlagen", Seite + 1, max_versuche)
    return(NULL)
  }
  
  return(daten)
}



for (Seite in seq(0, Seitenzahl-1, 1)) {
  
  Sys.sleep(runif(1, min = 2.5, max = 3.5))
  
  flog.info("---------------- Starte Loop %d ----------------", Seite + 1)
  
  result <- scrape_retries(Seite)
  
  if (!is.null(result)) {
    Rohdaten_neu <- bind_rows(Rohdaten_neu, result)
    
  }
}


flog.info("== ENDE DATENSCRAPING =========================")
flog.info("")



if (nrow(Rohdaten_neu) > 0) {
  

################################################################################
#####                                                                      #####
#####                    Datenaufbereitung und Geocoding                   #####
#####                                                                      #####
################################################################################


source("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Allg_Skripte\\Aufbereitungsskript_allg.R")




################################################################################
#####                                                                      #####
#####                            Daten speichern                           #####
#####                                                                      #####
################################################################################


source("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Allg_Skripte\\Datenspeicherung_allg.R")
  
  
  
  
################################################################################
#####                                                                      #####
#####                     Abschluss und Log speichern                      #####
#####                                                                      #####
################################################################################


flog.info("########### ABSCHLUSS SCRAPING - %s ###########", stadt)


} else {
  
  flog.info("Keine neuen Anzeigen gescraped")
  flog.info("")
  flog.info("########### ABSCHLUSS SCRAPING - %s ###########", stadt)
  
} 



file.copy(from = paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\",stadt,"\\Logs\\Log_",stadt," ",format(Sys.time(), "%d.%m.%Y {%H}"),".txt"), 
          to = paste0("C:\\Users\\Fabian Hellmold\\Dropbox\\WG_Gesucht\\",stadt,"\\Logs\\Log_",stadt," ",format(Sys.time(), "%d.%m.%Y {%H}"),".txt"),
          overwrite = TRUE)

