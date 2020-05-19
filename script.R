library(rvest)
library(dplyr)
library(tidytext)

#first counter is 02/14    257  + 60 days = april 15 2020
start <- as.Date("14/02/20","%d/%m/%y")
end   <- as.Date("15/04/20","%d/%m/%y")

getamloscripts <- function(){
  theDate <- start
  iterator <- 257
  list_tibbles <- list()

  while (theDate <= end)
  {
    if(as.character(theDate) == "2020-02-21"){
      theDate <- theDate + 3    
    }
    
    my_url <- print(paste0('https://lopezobrador.org.mx/2020/', 
                 format(theDate, "%m/%d"), 
                 '/version-estenografica-de-la-conferencia-de-prensa-matutina-del-presidente-andres-manuel-lopez-obrador-',
                 as.character(iterator), '/'))
    tryCatch(
      script <-
        read_html(my_url) %>%
        html_nodes('.entry-content p') %>%
        html_text(),
      error = function(e){script <- NA
      print(e)}
    )
  
    tibblescript <- tibble(line = 1:length(script), text = script)
  
    list_tibbles[[as.character(format(theDate, "%m/%d"))]] <- tibblescript

    if(as.character(theDate) == "2020-04-03") {
      theDate <- theDate + 1
    } else if(as.character(theDate) == "2020-04-04") {
      theDate <- theDate + 2
    }
      else if(weekdays(theDate) == "Friday" ){
      theDate <- theDate + 3
    } else {
      theDate <- theDate + 1       
    }
    iterator <- iterator + 1
  }
  return(list_tibbles)
}

allscripts <- getamloscripts()

custom_stop_words <- bind_rows(tibble(word = c('andrés', 'manuel', 'lópez', 'obrador', 'presidente')), tibble(word = tm::stopwords("spanish")))

tidy_list <- list()
for(i in names(allscripts)) {
  tidy_list[[i]] <- allscripts[[i]] %>%
    unnest_tokens(word, text) %>% anti_join(custom_stop_words) 
}

count_list <- list()
for(i in names(tidy_list)){
  count_list[[i]] <- tidy_list[[i]] %>% count(word, sort = TRUE)
}

firstscript <- allscripts$`02/14` %>%
  unnest_tokens(word, text)


tidy_script <- firstscript %>% anti_join(custom_stop_words)

tidy_script %>%
  count(word, sort = TRUE) 


library(ggplot2)

tidy_script %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
