library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)
library(gganimate)   
library(data.table)

start <- as.Date("14/02/20","%d/%m/%y")
end   <- as.Date("15/05/20","%d/%m/%y")

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
    } else if(weekdays(theDate) == "Friday" ){
      theDate <- theDate + 3
    } else {
      theDate <- theDate + 1       
    }
    iterator <- iterator + 1
  }
  return(list_tibbles)
}

allscripts <- getamloscripts()

#saveRDS(allscripts, file = "amlo_scripts_scraped.rds")
allscripts <- readRDS(file = "amlo_scripts_scraped.rds")


custom_stop_words <- bind_rows(tibble(word = c('andrés', 'manuel', 'lópez', 'obrador', 'presidente', 
                                               'si', 'entonces', 'pregunta', 'interlocutora', 'va')), 
                               tibble(word = tm::stopwords("spanish")))

tidy_list <- list()
for(i in names(allscripts)) {
  tidy_list[[i]] <- allscripts[[i]] %>%
    unnest_tokens(word, text) %>% anti_join(custom_stop_words) 
}

count_list <- list()
for(i in names(tidy_list)){
  count_list[[i]] <- tidy_list[[i]] %>% count(word, sort = TRUE) %>% head(10)
}

counted_dated <- list()
for(i in names(count_list)){
  counted_dated[[i]] <- tibble(word = count_list[[i]]$word, n = count_list[[i]]$n, date = c(i))
}

binded <- counted_dated %>% bind_rows

binded$datenumeric <- gsub('/', '', binded$date)
binded$date <- as.factor(binded$date)
binded$datenumeric <- as.numeric(binded$datenumeric)

binded %>%  
  # for each year we assign a rank
  group_by(datenumeric) %>%  
  arrange(datenumeric, -n) %>%  
  # assign ranking
  mutate(rank = row_number()) %>%  
  ungroup() ->  
  ranked_by_date


my_theme <- theme_classic(base_family = "") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "gainsboro")) +
  theme(plot.background = element_rect(fill = "gainsboro")) +
  theme(panel.background = element_rect(fill = "gainsboro"))


ranked_by_date %>%  
  ggplot() +  
  aes(xmin = 0 ,  
      xmax = n) +  
  aes(ymin = rank - .45,  
      ymax = rank + .45,  
      y = rank) +   
  scale_x_continuous(  
    limits = c(-120, 100),  
    breaks = c(0, 10, 20, 30)) +
  geom_rect(alpha = .7) +
  scale_fill_viridis_d(option = "magma",  
                       direction = -1) +  
  geom_text(col = "gray13",  
            hjust = "right",
            size = 9,
            aes(label = word),  
            x = -35) +  
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = 'n') +  
  labs(y = "") +  
  my_theme ->  
  my_plot

a <- my_plot +  
  facet_null() +  
  scale_x_continuous(  
    limits = c(-110, 100),  
    breaks = c(0, 10, 20, 30)) +  
  geom_text(x = 60 , y = -10,  
            family = "",  
            aes(label = as.character(date)),  
            size = 25, col = "black") +  
  exit_disappear() +
  gganimate::transition_states(datenumeric, transition_length = 50, state_length = 60)

animate(a, fps = 2)

anim_save("racewords-day.gif", a)

tidy_script %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
