

################################################################################
#####                                                                      #####
#####                Automatisierungsprozess Mailerstellung                #####
#####                                                                      #####
################################################################################


library(emayili)
library(rmarkdown)
library(tidyverse)




## Pfade definieren ------------------------------------------------------------

Pfad_Rmd <- "C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Skripte\\Scrapingbericht\\Scrapingbericht_Weekly.Rmd"
Pfad_HTML <- tempfile(fileext = ".html")  # temporäre HTML-Datei für Mailinhalt


## Rendern der Rmd-Datei -------------------------------------------------------

render(
  input = Pfad_Rmd,
  output_file = Pfad_HTML,
  output_format = "html_document",
  quiet = TRUE
)


## Mailinhalt erstellen --------------------------------------------------------

html_inhalt <- readLines(Pfad_HTML, warn = FALSE) %>% paste(collapse = "\n")

Mail_Inhalt <- envelope(
  from = "fabian.hellmold@posteo.de",
  to = "fabian.hellmold@posteo.de",
  subject = "Weekly Report Hamburg"
) %>%
  html(html_inhalt)


## Serververbindung herstellen -------------------------------------------------

# usethis::edit_r_environ()

Serververbindung <- server(
  host = "posteo.de",
  port = 465,
  username = "fabian.hellmold@posteo.de",
  password = Sys.getenv("POSTEO_PASSWORD")
)


## Mail versenden --------------------------------------------------------------

Serververbindung(Mail_Inhalt, verbose = TRUE)




