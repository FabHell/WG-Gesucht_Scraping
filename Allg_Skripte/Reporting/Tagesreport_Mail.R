

################################################################################
#####                                                                      #####
#####                Automatisierungsprozess Mailerstellung                #####
#####                                                                      #####
################################################################################


library(emayili)
library(rmarkdown)
library(tidyverse)
library(futile.logger)



## Logging vorbereiten ---------------------------------------------------------


dual_appender <- function(line) {
  appender.console()(line)
  appender.file(paste0("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Allg_Reporting\\Tagesreport\\Logs\\Log_Tagesreport ",format(Sys.time(), "%d.%m.%Y"),".txt")
  )(line)
}

flog.layout(layout.format("~l [~t] ~m")) 
flog.appender(dual_appender)


flog.info("########### BEGINN TAGESREPORT ###########")
flog.info("")



## Pfade definieren ------------------------------------------------------------

Pfad_Rmd <- "C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Allg_Skripte\\Reporting\\Scrapingbericht_Mail.Rmd"

Pfad_HTML <- sprintf(
  "C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Allg_Reporting\\Tagesreport\\HTML-Files\\Tagesreport_%s.html",
  format(Sys.Date(), "%Y-%m-%d")
)



## Rendern der Rmd-Datei -------------------------------------------------------

tryCatch({
  
  render(
    input = Pfad_Rmd,
    output_file = Pfad_HTML,
    output_format = "html_document",
    quiet = TRUE
  )
  flog.info("Rmd-Datei erfolgreich gerendert")
  
}, error = function(e) {
  
  flog.error("Fehler bei Rmd-Rendering: %s", e$message)
  
})



## Mailinhalt erstellen --------------------------------------------------------

# usethis::edit_r_environ()

tryCatch({
  
  html_inhalt <- readLines(Pfad_HTML, warn = FALSE) %>% paste(collapse = "\n")
  
  Mail_Inhalt <- envelope(
    from = Sys.getenv("POSTEO_ACCOUNTNAME"),
    to = c(Sys.getenv("empfaenger1_mail")),
    subject = "Tagesreport WG-Scraper"
  ) %>%
    html(html_inhalt)
  
  flog.info("Mailinhalt erfolgreich erstellt")
  
}, error = function(e) {
  
  flog.error("Fehler bei Inhaltserstellung: %s", e$message)
  
})



## Serververbindung herstellen -------------------------------------------------

# usethis::edit_r_environ()

tryCatch({
  
  Serververbindung <- server(
    host = "posteo.de",
    port = 465,
    username = Sys.getenv("POSTEO_ACCOUNTNAME"),
    password = Sys.getenv("POSTEO_PASSWORD")
  )
  
  flog.info("Serververbindung erfolgreich hergestellt")
  
}, error = function(e) {
  
  flog.error("Fehler bei Serververbindung: %s", e$message)
  
})



## Mail versenden --------------------------------------------------------------

tryCatch({
  
  Serververbindung(Mail_Inhalt, verbose = FALSE)
  
  flog.info("Mail erfolgreich versendet")
  
}, error = function(e) {
  
  flog.error("Fehler beim Versenden: %s", e$message)
  
})

flog.info("")
flog.info("############ ENDE TAGESREPORT ############")

