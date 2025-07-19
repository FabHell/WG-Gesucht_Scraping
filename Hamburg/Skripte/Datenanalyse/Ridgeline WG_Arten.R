
library(tidyverse)
library(ggridges)

Daten_Hamburg <- read_csv("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Daten\\Analysedaten\\Analysedaten.csv",
                          show_col_types = FALSE)

Daten_Kassel <- read_csv("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Kassel\\Daten\\Analysedaten\\Analysedaten.csv",
                         show_col_types = FALSE)

Daten_Hamburg <- Daten_Hamburg %>% 
  mutate(Uhrzeit_Scraping = NA,
         Stadt = "Hamburg")

Daten_Kassel <- Daten_Kassel %>% 
  mutate(Stadt = "Kassel")
Daten_ges <- rbind(Daten_Hamburg, Daten_Kassel)


df <- Daten_ges %>%
  filter(Datum_Scraping > Sys.Date()-24) %>%
  mutate(Datum_Scraping = as.Date(Datum_Scraping))

# Woche extrahieren: entweder nach Kalenderwoche oder nach Startdatum gruppieren
df <- df %>%
  mutate(
    Woche = floor_date(Datum_Scraping, unit = "week", week_start = 1)  # Woche beginnt Montag
  )

# Optional: Wochendatum als beschrifteten Faktor für die Achse (z.B. KW + Datum)
df <- df %>%
  mutate(
    Woche_Label = format(Woche, "%Y-%m-%d")
  )

# Ridgeline-Plot
df %>%
ggplot(aes(x = Gesamtmiete, fill = Stadt, y = reorder(Woche_Label, Woche))) +
  geom_density_ridges(alpha = 0.7, scale = 2) +
  labs(
    title = "Verteilung der Gesamtmiete pro Woche",
    x = "Gesamtmiete (€)",
    y = "Woche (Startdatum)"
  ) +
  theme_minimal()

