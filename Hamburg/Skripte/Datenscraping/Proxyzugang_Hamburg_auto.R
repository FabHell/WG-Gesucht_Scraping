


################################################################################
################################################################################
#####                                                                      #####
#####                      PROXYZUGANG WEBSHARE - AUTO                     #####
#####                                                                      #####
################################################################################
################################################################################



library(httr)
library(tidyverse)



## Aktuelle Proxyliste von Webshare laden und User-Agent erstellen -------------

# usethis::edit_r_environ()
API_response <- GET(Sys.getenv("WEBSHARE_PROXYAPI"))

ua_obj <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")



## Proxyliste als Dataframe laden ----------------------------------------------

proxy_df <- API_response %>%
  content("text", encoding = "UTF-8") %>%
  read.csv(text = ., header = FALSE, stringsAsFactors = FALSE) %>%
  setNames("proxy_string") %>%
  separate(proxy_string, into = c("ip", "port", "user", "password"), sep = ":", remove = TRUE)



## Proxy-Server testen ---------------------------------------------------------

test_proxy <- function(ip, port, user, password) {
  proxy <- use_proxy(url = ip, port = as.numeric(port), username = user, password = password)
  test_url <- "https://www.wg-gesucht.de/wg-zimmer-in-Kassel.69.0.1.0.html"

  res <- try(GET(test_url, proxy, ua_obj, timeout(7)), silent = FALSE)
  if (inherits(res, "try-error") || status_code(res) != 200) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

proxy_tested <- proxy_df %>%
  mutate(works = pmap_lgl(list(ip, port, user, password), test_proxy))

print(paste0(nrow(proxy_tested %>% filter(works)) ," von 10 Proxys zum Scraping geeignet"))


## Funktionierenden Proxy zufällig auswählen und Proxy-Objekt erstellen -------- 

working_proxy <- proxy_tested %>%
  filter(works) %>%
  slice_sample(n = 1)

proxy_obj <- use_proxy(
  url = working_proxy$ip,
  port = as.numeric(working_proxy$port),
  username = working_proxy$user,
  password = working_proxy$password)

print(paste0(working_proxy$ip," als Proxyserver verwendet"))
print(" ")
print(" ")


