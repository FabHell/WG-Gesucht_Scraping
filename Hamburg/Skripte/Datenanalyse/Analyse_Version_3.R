

library(tidyverse)
library(foreign)
library(sf) 


## Daten laden -----------------------------------------------------------------

Daten_Hamburg <- read_csv("Hamburg/Daten/Analysedaten/Analysedaten.csv",
                          show_col_types = FALSE)
  
Geodaten_Stadtteile <- st_read("Hamburg/Daten/Geodaten/Geo_Stadtteile/Stadtteile_Hamburg.shp") %>%
  filter(stadtteil_ != "Neuwerk") %>%
  select(Stadtteil = stadtteil_)


Daten_Hamburg <- Daten_Hamburg %>% 
  distinct()


## Aufbereitung Stadtteilbezeichnung -------------------------------------------


# Vektor für Stadtteilbezeichnungen

St_Teile <- as.data.frame(Geodaten_Stadtteile) %>%
  select(Stadtteil) %>%
  pull()

Restfälle <- Daten_Hamburg %>%
  filter(!(Stadtteil %in% St_Teile)) 



## Anpassung

# Daten_Hamburg <- Daten_Hamburg %>%
#   mutate(Stadtteil = if_else(Stadtteil == "Süd",
#                              "Südstadt", Stadtteil),
#          Stadtteil = if_else(Stadtteil == "Wilhelmshöhe/Wahlershausen",
#                              "Bad Wilhelmshöhe", Stadtteil),
#          Stadtteil = if_else(Stadtteil == "West",
#                              "Vorderer Westen", Stadtteil),
#          Stadtteil = if_else(Stadtteil == "Schillerviertel",
#                              "Nord-Holland", Stadtteil))






## Erste Analyse ---------------------------------------------------------------




## Für welche Stadtteile habe ich Fälle?

Geodaten_Stadtteile %>%
  right_join(Daten_Hamburg, by = "Stadtteil") %>%
  filter(is.na(Befristungsdauer) | Befristungsdauer >= 60) %>%
  filter(Gesamtmiete > 250) %>%
  
  ggplot() +
  geom_sf(data = Geodaten_Stadtteile) +
  geom_sf(aes(fill = "red"), show.legend = F) +
  theme_void()


## Geschlecht gesucht

table(Daten_Hamburg$Geschlecht_ges)
table(Daten_Hamburg$Datum_Scraping)

##

library(tidytext)
library(stopwords)

tibble(title = Daten_Hamburg$Titel) %>%
  unnest_tokens(word, title, strip_numeric = T) %>%
  filter(nchar(word) > 2) %>%  
  count(word, sort = TRUE) %>%
  anti_join(tibble(word = stopwords("de")), by = "word") %>%
  print(n = 50)

## Umgang mit Rauchen

table(Daten_Hamburg$Rauchen)


## Mittelwert Miete nach Datum

Daten_Hamburg %>%
  group_by(Datum_Scraping) %>%
  summarise(Mean_Gesamtmiete = mean(Gesamtmiete, na.rm = TRUE),
            Fallzahl = n())



## Mittelwert Miete nach Stadtteil

Daten_Hamburg %>%
  filter(is.na(Befristungsdauer) | Befristungsdauer >= 60) %>%   # Keine kurzen Befristigungen
  filter(Stadtteil %in% St_Teile) %>%
  group_by(Stadtteil) %>%
  summarise(Mean_Gesamtmiete = mean(Gesamtmiete, na.rm = TRUE),
            Fallzahl = n()) %>%
  filter(Fallzahl >= 5) %>%
  arrange(desc(Mean_Gesamtmiete)) %>%
  print(n = 103)




Analysedaten %>%
# filter(is.na(Befristungsdauer) | Befristungsdauer >= 60) %>%
  filter(Stadtteil %in% St_Teile) %>%
  
  mutate(Höher_Mean = Gesamtmiete >= mean(Gesamtmiete)) %>%
  group_by(Stadtteil) %>%
  summarise(Über_Mean = mean(Höher_Mean),
            Fallzahl = n()) %>%
  filter(Fallzahl >= 5) %>%
  arrange(desc(Über_Mean)) %>%
  select(-Fallzahl) %>%
  mutate(Unter_Mean = Über_Mean -1) %>%
  pivot_longer(-Stadtteil, names_to = "Kategorie", values_to = "Anteil_Mean") %>%
  mutate(Anteil_Mean = na_if(Anteil_Mean, 0)) %>%

  ggplot(aes(x = Anteil_Mean, 
             y = reorder(Stadtteil, Anteil_Mean), 
             fill = Kategorie, 
             label = abs(round(Anteil_Mean, 1)))) +
    geom_col(show.legend = F) +
    geom_text(position = position_stack(vjust = 0.5)) +
#    geom_vline(xintercept = 0) +
  annotate(geom = "text",
           x = c(-0.5, 0, 0.5),
           y = c(18.5, 18.5, 18.5),
           label = c("Anteil\nUnter Mean", "Mean", "Anteil\nüber Mean"),
           fontface = "bold",
           size = 2.5) +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title =  element_blank(),
        panel.background = element_blank())
  

Daten_Hamburg %>%
  filter(is.na(Befristungsdauer) | Befristungsdauer >= 60) %>%
  filter(Stadtteil %in% St_Teile) %>%
  
  mutate(Elbe = ifelse(Stadtteil %in% c("Cranz", "Finkenwerder", "Neuenfelde", "Francop",
                                        "Neugraben-Fischbek", "Welterhof", "Altenwerder",
                                        "Moorburg", "Hausbruch", "Steinwerder", "Kleiner Grasbrook",
                                        "Heimfeld", "Eißendorf", "Harburg", "Neuland", "Wilstorf",
                                        "Gut Moor", "Marmstorf", "Langenbek", "Rönneburg", "Sinstorf",
                                        "Veddel", "Wilhelmsburg", "Waltershof"), 0, 1)) %>%
  
  group_by(Stadtteil) %>%
  mutate(Fallzahl = n()) %>%
  filter(Fallzahl >= 5) %>%
  
  ggplot(aes(x = Gesamtmiete, 
             y = reorder(Stadtteil, Gesamtmiete, FUN = median),
             fill = Elbe)) +
  geom_vline(xintercept = median(Daten_Hamburg$Gesamtmiete, na.rm = TRUE),
             linetype = "dashed") +
  geom_boxplot(outlier.shape = NA, coef = 0,
               alpha = 0.5, show.legend = F, width = 0.6) +
  geom_jitter(color = "black", size = 2, show.legend = F, height = 0.2,
              shape = 21) +
  theme_classic() +
  theme(axis.title.y = element_blank())




