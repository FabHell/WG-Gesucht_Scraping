

################################################################################
#####                                                                      #####
#####                   Auswahllisten für Visualisierung                   #####
#####                                                                      #####
################################################################################



## Aufbereitung des Datensatz mit gängigen Filtern -----------------------------


Auswahlliste <- Daten_Hamburg %>%
  
  filter(is.na(Befristungsdauer) | Befristungsdauer >= 60) %>%
  filter(Gesamtmiete >= 200) %>%
  filter(Stadtteil %in% St_Teile) %>%
  
  mutate(Elbe = ifelse(Stadtteil %in% c("Cranz", "Finkenwerder", "Neuenfelde", "Francop",
                                        "Neugraben-Fischbek", "Welterhof", "Altenwerder",
                                        "Moorburg", "Hausbruch", "Steinwerder", "Kleiner Grasbrook",
                                        "Heimfeld", "Eißendorf", "Harburg", "Neuland", "Wilstorf",
                                        "Gut Moor", "Marmstorf", "Langenbek", "Rönneburg", "Sinstorf",
                                        "Veddel", "Wilhelmsburg", "Waltershof"), 0, 1)) %>%
  group_by(Stadtteil, Elbe) %>%
  summarise(Median_Gesamtmiete = median(Gesamtmiete, na.rm = TRUE),
            Fallzahl = n(),
            .groups = "drop") %>%
  filter(Fallzahl >= 5)





## Erstellung der Auswahlliste Fallzahl ----------------------------------------


Auswahlliste_Fallzahl <- Auswahlliste %>%
  arrange(desc(Fallzahl)) %>%
  slice_head(n = 20) %>%
  select(Stadtteil) %>%
  pull()




## Erstellung der Auswahlliste Elbe + Fallzahl ---------------------------------


elbe_1_top2 <- Auswahlliste %>%
  filter(Elbe == 1) %>%
  arrange(desc(Median_Gesamtmiete)) %>%
  slice_head(n = 2)

elbe_1_bottom2 <- Auswahlliste %>%
  filter(Elbe == 1) %>%
  arrange(Median_Gesamtmiete) %>%
  slice_head(n = 2)

elbe_0_top2 <- Auswahlliste %>%
  filter(Elbe == 0) %>%
  arrange(desc(Median_Gesamtmiete)) %>%
  slice_head(n = 2)

elbe_0_bottom2 <- Auswahlliste %>%
  filter(Elbe == 0) %>%
  arrange(Median_Gesamtmiete) %>%
  slice_head(n = 2)


top_bottom <- bind_rows(elbe_1_top2, elbe_1_bottom2,
                        elbe_0_top2, elbe_0_bottom2) %>%
              select(Stadtteil) %>%
              pull()

higest_n <- Auswahlliste %>%
  filter(!Stadtteil %in% top_bottom) %>%
  slice_max(Fallzahl, n = 12) %>%
  select(Stadtteil) %>%
  pull()

Auswahlliste_Elbe <- c(top_bottom, higest_n)

rm(elbe_0_bottom2)
rm(elbe_0_top2)
rm(elbe_1_bottom2)
rm(elbe_1_top2)
rm(higest_n)
rm(top_bottom)
