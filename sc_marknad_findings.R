market_findings <- function(){
  suppressPackageStartupMessages(require("rvest"))
  suppressPackageStartupMessages(require("tidyverse"))
  suppressPackageStartupMessages(require("xtable"))
  last_check <- Sys.Date() - 1
  try(load("sc_marknad_checkpoint.Rdata"),silent = TRUE)
  
  time_format    <- "%a, %d %b %Y %H:%M:%S"
  searchterms    <- readLines("https://raw.githubusercontent.com/Napoleongl/SC_marknadslarm/master/searchterms.txt") %>% tolower()
  marknad_rss    <- read_xml('https://www.sweclockers.com/feeds/marknad')
  rss_time       <- marknad_rss %>%  html_node('lastBuildDate') %>% 
    html_text() %>% 
    strptime(., time_format)
  
  ads            <- marknad_rss %>% html_nodes('item')
  
  marknad_data   <- tibble(titles = ads %>% html_nodes('title') %>% 
                             html_text() %>% tolower(),
                           addesc =   ads %>% html_nodes('description') %>% 
                             html_text() %>% tolower(),
                           link = ads %>% html_nodes('guid') %>% 
                             html_text() %>% tolower(),
                           time =   ads %>% html_nodes('pubDate') %>% 
                             html_text() %>% 
                             strptime(., time_format)) %>% 
    filter((str_detect(titles, "säljes") & time > last_check)) %>% 
    mutate(fulltext = paste(titles, addesc))
  
  # Makes matrix of ads and searchterms with startpos of match converted to logical
  terms_found <- sapply(searchterms, regexpr, marknad_data$fulltext, ignore.case=TRUE) > 0
  last_check <- rss_time
  save(last_check, file = "sc_marknad_checkpoint.Rdata")
  
  if(is.null(dim(terms_found))){
    return(0)
  } else{
    findings <- lapply(seq_along(searchterms), function(term){
      matches <- terms_found[, term]
      tibble(Term = rep(searchterms[term],sum(matches)),
             Link = marknad_data$link[matches], 
             Time = as.character(marknad_data$time[matches], format = "%H:%M, %e/%m"))
    }) %>% 
      bind_rows() 
    findings %>% xtable() %>% 
      print( type="html", file = "findings.txt") 
    return(nrow(findings))
  }
}
