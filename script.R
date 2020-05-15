library(rvest)
library(dplyr)
library(tidytext)

#first counter is 02/14    257  + 60 days = april 15 2020

start <- as.Date("14/02/14","%d/%m/%y")
end   <- as.Date("15/04/14","%d/%m/%y")


getamloscripts <- function(){
  theDate <- start
  iterator <- 257
  
  list_tibbles <- list()
  i = 1
  
  while (theDate <= end)
  {
    my_url <- print(paste0('https://lopezobrador.org.mx/2020/', 
                 format(theDate, "%m/%d"), 
                 '/version-estenografica-de-la-conferencia-de-prensa-matutina-del-presidente-andres-manuel-lopez-obrador-',
                 as.character(iterator), '/'))
    
    tryCatch(
      script <-
        read_html(my_url) %>%
        html_nodes('.entry-content p') %>%
        html_text(),
      error = function(e){NA}
    )
  
    
    
    tibblescript <- tibble(line = 1:length(267), text = script)
  
    list_tibbles[[as.character(format(theDate, "%m/%d"))]] <- tibblescript

    i <- i + 1
    theDate <- theDate + 1       
    iterator <- iterator + 1
  }
  return(list_tibbles)
}

allscripts <- getamloscripts()


tibblescript %>%
  unnest_tokens(word, text)

