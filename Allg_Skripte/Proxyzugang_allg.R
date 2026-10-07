


################################################################################
################################################################################
#####                                                                      #####
#####                      PROXYZUGANG WEBSHARE - AUTO                     #####
#####                                                                      #####
################################################################################
################################################################################



library(httr)
library(tidyverse)
library(futile.logger)


flog.info("== START PROXY-SETUP ==========================")


## Ggf. neue Proxyliste laden --------------------------------------------------

# usethis::edit_r_environ()


# API_response <- GET(Sys.getenv("WEBSHARE_ROTATING_RESIDENTIAL"))


## Proxyliste in Dataframe umwandeln -------------------------------------------

# proxy_df <- API_response %>%
#   content("text", encoding = "UTF-8") %>%
#   read.csv(text = ., header = FALSE, stringsAsFactors = FALSE) %>%
#   setNames("proxy_string") %>%
#   separate(proxy_string, into = c("ip", "port", "user", "password"),
#            sep = ":", remove = TRUE)
# 
# 
# write.csv2(proxy_df, "C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Allg_Proxies\\Proxies2.csv")



ua_obj <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")




proxy_tested <- read.csv("C:\\Users\\Fabian Hellmold\\Desktop\\WG-Gesucht-Scraper\\Allg_Proxies\\Proxies2.csv", sep = ";") %>%
  select(-X)
flog.info("Residentialproxies geladen")


flog.info("== ENDE PROXY-SETUP ===========================")
flog.info(" ")  
