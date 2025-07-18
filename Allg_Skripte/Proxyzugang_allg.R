


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


## Aktuelle Proxyliste von Webshare laden und User-Agent erstellen -------------

# usethis::edit_r_environ()

tryCatch({
  
  API_response <- GET(Sys.getenv("WEBSHARE_ROTATING_RESIDENTIAL"))
  flog.info("Residentialproxies geladen")
  
}, error = function(e1) {
  
  flog.warn("Fehler Residentialproxies: %s", e1$message)
  tryCatch({
    
    API_response <<- GET(Sys.getenv("WEBSHARE_FREE_SERVER"))
    flog.info("Serverproxies erfolgreich geladen")
    
  }, error = function(e2) {
    
    flog.warn("Fehler Serverproxies: %s", e2$message)
    flog.fatal("Abbruch: Proxy-API's nicht erreichbar")
    stop("Ende Joink")
    
  })
})


ua_obj <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")



## Proxyliste als Dataframe laden ----------------------------------------------

proxy_df <- API_response %>%
  content("text", encoding = "UTF-8") %>%
  read.csv(text = ., header = FALSE, stringsAsFactors = FALSE) %>%
  setNames("proxy_string") %>%
  separate(proxy_string, into = c("ip", "port", "user", "password"), 
           sep = ":", remove = TRUE) %>%
  sample_n(20, replace = T)



## Proxy-Server testen ---------------------------------------------------------

test_proxy <- function(ip, port, user, password) {
  proxy <- use_proxy(url = ip, port = as.numeric(port), username = user, password = password)
  test_url <- paste0(Link_Stadt, "0.html")
  
  res <- try(GET(test_url, proxy, ua_obj, timeout(7)), silent = FALSE)
  if (inherits(res, "try-error") || status_code(res) != 200) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


proxy_tested <- proxy_df %>%
  mutate(works = pmap_lgl(list(ip, port, user, password), test_proxy)) %>%
  filter(works)


if (sum(proxy_tested$works) > 0) {
  
  flog.info("%d von 20 Proxys funktionsfähig", nrow(proxy_tested))
  
} else {
  
  flog.warn("Keine funktionierenden Proxys ermittelt")
  
  tryCatch({
    
    proxy_tested <- GET(Sys.getenv("WEBSHARE_FREE_SERVER")) %>%
      content("text", encoding = "UTF-8") %>%
      read.csv(text = ., header = FALSE, stringsAsFactors = FALSE) %>%
      setNames("proxy_string") %>%
      separate(proxy_string, into = c("ip", "port", "user", "password"), 
               sep = ":", remove = TRUE) %>%
      sample_n(20, replace = T)
    
    flog.info("Serverproxies erfolgreich geladen")
    
  }, error = function(e) {
    
    flog.warn("Fehler Serverproxies: %s", e$message)
    flog.fatal("Abbruch: Keine funktionierenden Proxys")
    stop("Ende Joink") 
    
  })
  
}


flog.info("== ENDE PROXY-SETUP ===========================")
flog.info(" ")  
