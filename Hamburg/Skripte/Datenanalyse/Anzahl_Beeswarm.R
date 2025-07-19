
library(ggplot2)
library(ggbeeswarm)
library(patchwork)




Daten_Hamburg <- read_csv("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Hamburg\\Daten\\Analysedaten\\Analysedaten.csv",
                          show_col_types = FALSE) %>%
  mutate(Uhrzeit_Scraping = NA,
         Stadt = "Hamburg")

Daten_Kassel <- read_csv("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Kassel\\Daten\\Analysedaten\\Analysedaten.csv",
                         show_col_types = FALSE) %>% 
  mutate(Stadt = "Kassel")


Gesamt <- rbind(Daten_Hamburg, Daten_Kassel) %>%
  filter(is.na(Befristungsdauer) | Befristungsdauer >= 60) %>%
  filter(Gesamtmiete >= 250 & Gesamtmiete <= 1300) %>%
  filter(Datum_Scraping >= Sys.Date() - 7)
  

# Mittelwerte berechnen
mittelwerte <- Gesamt %>%
  group_by(Stadt) %>%
  summarise(mean_miete = mean(Gesamtmiete, na.rm = TRUE)) %>%
  mutate(x_pos = 1.48, 
         y_pos = mean_miete+10)  

# Plot
Bee <- ggplot(Gesamt, aes(x = "", y = Gesamtmiete, colour = Stadt)) +
  geom_quasirandom(alpha = 0.7, show.legend = FALSE) +
  geom_hline(data = mittelwerte, 
             aes(yintercept = mean_miete), 
             linetype = "dashed", color = "black") +
  geom_text(data = mittelwerte, 
            aes(x = x_pos, y = y_pos, 
                label = round(mean_miete, 0)),
            hjust = 0, vjust = 0, size = 3, color = "black") +
  scale_y_continuous(breaks = seq(250,1500,250))+
  facet_wrap(~Stadt) +
  labs(x = NULL, y = "Gesamtmiete") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(r=10)))

kreis_df <- Gesamt %>%
  filter(Datum_Scraping >= Sys.Date() - 7) %>%
  count(Stadt, name = "Anzahl") %>%
  mutate(y = 0) 

Kreis <- ggplot(kreis_df, aes(x = "", y = 1, size = Anzahl, fill = Stadt)) +
  geom_point(shape = 21, color = "black", show.legend = FALSE) +
  geom_text(aes(label = Anzahl, size = Anzahl / 25), color = "white", fontface = "bold", show.legend = FALSE) +
  scale_size_area(max_size = 35) +
  facet_wrap(~Stadt) +
  coord_cartesian(clip = "off") +
  labs(y = "Anzahl", x = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.title.y = element_text(margin = margin(r=10)),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(0, 0, 0, 0))


Bee + Kreis +
  plot_layout(ncol = 1, heights = c(1, 0.4))

