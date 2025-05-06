


################################################################################
#####                                                                      #####
#####                           Visualisierung 1                           #####
#####                                                                      #####
################################################################################


library(tidyverse)
library(foreign)
library(sf) 
library(showtext)
library(patchwork)
library(ggtext)


font_add_google("Libre Franklin", "franklin")
font_add_google("Domine", "domine")
showtext_opts(dpi = 300)
showtext_auto()




## Daten laden -----------------------------------------------------------------

Daten_Hamburg <- read_csv("Hamburg/Daten/Analysedaten/Analysedaten.csv",
                          show_col_types = FALSE)

Geodaten_Stadtteile <- st_read("Hamburg/Daten/Geodaten/geo_Stadtteile/Stadtteile_Hamburg.shp") %>%
  filter(stadtteil_ != "Neuwerk") %>%
  select(Stadtteil = stadtteil_, Stadtbezirk = bezirk_nam)

Grenzen_Elbe <- st_read("Hamburg/Daten/Geodaten/Geo_Elbe/Elbe.shp")

St_Teile <- as.data.frame(Geodaten_Stadtteile) %>%
  select(Stadtteil) %>%
  pull()





## Boxplot ---------------------------------------------------------------------


source("Hamburg/Skripte/Datenanalyse/Auswahl_Stadtteile.R")


Daten_Boxplot <- Daten_Hamburg %>%
  filter(is.na(Befristungsdauer) | Befristungsdauer >= 60) %>%
  filter(Gesamtmiete >= 250) %>%
  filter(Stadtteil %in% St_Teile) %>%
  
  mutate(Elbe = ifelse(Stadtteil %in% c("Cranz", "Finkenwerder", "Neuenfelde", "Francop",
                                        "Neugraben-Fischbek", "Welterhof", "Altenwerder",
                                        "Moorburg", "Hausbruch", "Steinwerder", "Kleiner Grasbrook",
                                        "Heimfeld", "Eißendorf", "Harburg", "Neuland", "Wilstorf",
                                        "Gut Moor", "Marmstorf", "Langenbek", "Rönneburg", "Sinstorf",
                                        "Veddel", "Wilhelmsburg", "Waltershof"), 0, 1)) %>%
  filter(Stadtteil %in% Auswahlliste_Fallzahl) %>%
  group_by(Stadtteil) %>%
  mutate(Fallzahl = n(),
         Stadtteil = factor(Stadtteil)) %>%
  filter(Fallzahl >= 5)


Offset <- -61      # Entfernung Bars und Boxplots 
Mulitplik <- 1.0   # Länge der Bars

Daten_Crossbar <- Daten_Boxplot %>%
  mutate(xmin = min(Daten_Hamburg$Gesamtmiete, na.rm = T) + Offset,
         xmax = (xmin + Fallzahl),
         x = (xmin + xmax)/2) %>%
  select(Stadtteil, Fallzahl, xmin, xmax, x)




Boxplot <- Daten_Boxplot %>%

  ggplot(aes(x = Gesamtmiete, 
             y = reorder(Stadtteil, Gesamtmiete, FUN = median),
             fill = Elbe)) +
  geom_vline(xintercept = median(Daten_Hamburg$Gesamtmiete, na.rm = TRUE),
             linetype = "dashed") +
  geom_jitter(color = "black", size = 1.20, show.legend = F, height = 0.2,
              shape = 21) +
  geom_boxplot(outlier.shape = NA, coef = 0,
               alpha = 0.33, show.legend = F, width = 0.6) +
  scale_x_continuous(breaks = c(-45, 250, 500, 750, 1000, 1250),
                     labels = c("<b style='color:#E9C46A;'>Anzahl</b>", 250, 500, 750, 1000, 1250)) +
  geom_crossbar(data = Daten_Crossbar,
                mapping = aes(xmin = xmin, xmax = xmax+(Fallzahl*Mulitplik), x = x, y = Stadtteil),
                inherit.aes = F, fill = "#E9C46A", color = NA) +
  geom_text(data = Daten_Crossbar, mapping = aes(x = xmax+(Fallzahl*Mulitplik)+Offset + 85,
                                                 y = Stadtteil, label = Fallzahl),
            size = 2, family = "franklin", hjust = 0, fontface = "italic", inherit.aes = F) +
  annotate("segment", x = c(-60, 127.5), xend = c(77.5, 1350) , y = 0.25, yend = 0.25,
           colour = "gray40", linewidth = 0.5) +

  coord_cartesian(clip = "off", expand = F, xlim = c(-60, 1350), ylim = c(0.25, 20.5)) +
  labs(x = "Miete") +
  theme_classic() +
  theme(axis.ticks = element_line(color = "gray40"),
        axis.line = element_line(color = "gray40"),
        axis.text.x = element_markdown(family = "franklin", size = 7),
        axis.line.x = element_blank(),
        axis.text.y = element_markdown(family = "franklin", size = 8),
        axis.title.x = element_markdown(size = 9, family = "franklin", 
                                        margin = margin(t = 10, l = 30)), 
        axis.title.y = element_blank())




## Karte -----------------------------------------------------------------------


Karte <- Geodaten_Stadtteile %>%
  right_join(Daten_Hamburg, by = "Stadtteil") %>%
  filter(is.na(Befristungsdauer) | Befristungsdauer >= 60) %>%
  filter(Gesamtmiete >= 250) %>%
  group_by(Stadtteil) %>%
  
  summarise(Median_Gesamtmiete = median(Gesamtmiete, na.rm = TRUE),
            Fallzahl = n()) %>%
  filter(Fallzahl >= 5) %>%
  
  ggplot() +
  geom_sf(data = Geodaten_Stadtteile, color = "darkgrey", fill = "gray95") +
  geom_sf(aes(fill = Median_Gesamtmiete)) +
  scale_fill_gradient(
    low = "gray90", high = "black",
    name = "Medianmiete") +
  geom_sf(data = Grenzen_Elbe, aes(color = Elbe), linewidth = 0.8, 
          fill = NA, show.legend = F) +
  geom_sf(data = Grenzen_Elbe %>% filter(Elbe == 0) %>% st_transform(25832) %>%  
            mutate(geometry = st_buffer(geometry, dist = -0.007 * sqrt(st_area(geometry) / pi))) %>% 
            st_transform(st_crs(Grenzen_Elbe)),
          linewidth = 0.4, fill = NA, color = "#0f2153") +
  labs(fill = "Medianmiete",
       x = "Stadtteilgrenzen Hamburgs") +
  theme_void() +
  theme(legend.title = element_markdown(size = 7, family = "franklin"),
        legend.position = c(0.925,0.65),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.55, "cm"),
        axis.title.x = element_text(family = "franklin", size = 9,
                                    margin = margin(t = 20)),
        axis.line.x = element_line(color = "gray40"))





## Zusammenfügen ---------------------------------------------------------------


Boxplot_Karte <- Boxplot + Karte +
  plot_layout(ncol = 2, widths = c(0.75, 1.5)) +
  plot_annotation(
    title = "WG-Zimmer in Hamburg",
    caption = "Datengrundlage sind die Anzeigen der Webseite WG-Gesucht. Anzeigen mit einer befristeten Mietdauer von unter 60 Tagen wurden aus der Analyse ausgeschlossen",
    subtitle = "Alle Viertel mit einer hohen Medianmiete liegen <b><span style='color:#56b1f7'>nördlich der Elbe</span></b>, während alle Viertel <b><span style='color:#0f2153'>südlich der Elbe</span></b> eine niedrige Medianmiete aufweisen. Die <b><span style='color:#E9C46A'>meisten<br>WG-Anzeigen</span></b> gibt es in Harburg, Eimsbüttel und Winterhude.",
    theme = theme(plot.title = element_text(size = 12, family = "domine",
                                            margin = margin(l = -4)),
                  plot.subtitle = element_markdown(family = "franklin", size = 8,
                                                   margin = margin(l = 8, b = 5, t = 6)),
                  plot.caption = element_text(size = 6, family = "franklin")))



file_save <- "Hamburg/Ergebnisse/Abbildungen/Anzahl_Boxplot_Karte.png"
ggsave(filename = file_save, plot = Boxplot_Karte, 
       width = 9, height = 6, units = "in", dpi = 300)

shell.exec(normalizePath(file_save))

