


################################################################################
################################################################################
#####                                                                      #####
#####                            SQL-SERVER LOKAL                          #####
#####                                                                      #####
################################################################################
################################################################################


library(DBI)



# Lokale Verbindung herstellen -------------------------------------------------


# usethis::edit_r_environ()

con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = Sys.getenv("DB_SERVER"),
                 Database = Sys.getenv("DB_NAME"),
                 Trusted_Connection = "Yes",
                 Encrypt = "No")



# Neue Tabelle anlegen ---------------------------------------------------------


dbExecute(con, "DROP TABLE IF EXISTS analysedaten")

dbExecute(con, "
CREATE TABLE analysedaten (
    id INT IDENTITY(1,1) PRIMARY KEY,
    land NVARCHAR(50),
    stadt NVARCHAR(50),
    titel NVARCHAR(1000),
    link NVARCHAR(1000),
    stadtteil_quelle NVARCHAR(50),
    stadtteil NVARCHAR(100),
    postleitzahl FLOAT,
    straße NVARCHAR(100),
    gesamtmiete FLOAT,
    kaltmiete FLOAT,
    nebenkosten FLOAT,
    kaution FLOAT,
    sonstige_kosten FLOAT,
    ablösevereinbarung FLOAT,
    zimmergröße FLOAT,
    personenzahl FLOAT,
    wohnungsgröße FLOAT,
    bewohneralter NVARCHAR(50),
    einzugsdatum DATE,
    zusammensetzung NVARCHAR(50),
    befristung_enddatum DATE,
    befristungsdauer FLOAT,
    geschlecht_ges NVARCHAR(100),
    alter_ges NVARCHAR(50),
    wg_art NVARCHAR(1000),
    rauchen NVARCHAR(100),
    sprache NVARCHAR(500),
    angaben_zum_objekt NVARCHAR(2000),
    freitext_zimmer_1 NVARCHAR(4000),
    freitext_zimmer_2 NVARCHAR(4000),
    freitext_zimmer_3 NVARCHAR(4000),
    freitext_lage_1 NVARCHAR(4000),
    freitext_lage_2 NVARCHAR(4000),
    freitext_lage_3 NVARCHAR(4000),
    freitext_wg_leben_1 NVARCHAR(4000),
    freitext_wg_leben_2 NVARCHAR(4000),
    freitext_wg_leben_3 NVARCHAR(4000),
    freitext_sonstiges_1 NVARCHAR(4000),
    freitext_sonstiges_2 NVARCHAR(4000),
    freitext_sonstiges_3 NVARCHAR(4000),
    seite_scraping INT,
    uhrzeit_scraping NVARCHAR(10),
    datum_scraping DATE
)")


flog.info("%s Freitextfelder getrimmt",
          sum(
            sum(nchar(Analysedaten_neu_geo$freitext_zimmer) > 4000),
            sum(nchar(Analysedaten_neu_geo$freitext_lage) > 4000),
            sum(nchar(Analysedaten_neu_geo$freitext_wg_leben) > 4000),
            sum(nchar(Analysedaten_neu_geo$freitext_sonstiges) > 4000)
            )
          )


remove_special_chars <- function(x) {
  x <- stri_enc_toutf8(x, validate = TRUE)
  x <- stri_replace_all_regex(x, "\\p{Z}+", " ")
  x <- stri_replace_all_regex(x, "[\\p{C}&&[^\\r\\n\\t]]", "")
  x <- stri_trim_both(x)
  x
}


Analysedaten_clean <- Analysedaten_neu_geo %>%
  mutate(across(where(is.character), ~ remove_special_chars(.))) %>%
  mutate(
    titel = substr(titel, 1, 750),
    link = substr(link, 1, 750),
    angaben_zum_objekt = substr(angaben_zum_objekt, 1, 1750),
    freitext_zimmer = substr(freitext_zimmer, 1, 3500),
    freitext_lage = substr(freitext_lage, 1, 3500),
    freitext_wg_leben = substr(freitext_wg_leben, 1, 3500),
    freitext_sonstiges = substr(freitext_sonstiges, 1, 3500)
  )


dbWriteTable(con, "testdaten", Analysedaten_clean,
             append = TRUE)


testdaten <- dbReadTable(con, "analysedaten")


