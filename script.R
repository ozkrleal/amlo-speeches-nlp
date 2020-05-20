library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)
library(gganimate) 

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
  count_list[[i]] <- tidy_list[[i]] %>% count(word, sort = TRUE) %>% head(20)
}

counted_dated <- list()
for(i in names(count_list)){
  counted_dated[[i]] <- tibble(word = count_list[[i]]$word, n = count_list[[i]]$n, date = c(i))
}

binded <- counted_dated %>% bind_rows

binded$datenumeric <- gsub('/', '', binded$date)
binded$date <- as.factor(binded$date)


binded %>%  
  # for each year we assign a rank
  group_by(date) %>%  
  arrange(date, -n) %>%  
  # assign ranking
  mutate(rank = 1:n()) %>%  
  filter(rank <= 20) ->  
  ranked_by_date

my_theme <- theme_classic(base_family = "Times") +
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
  facet_wrap(~ date) +  
  geom_rect(alpha = .7) +
  scale_fill_viridis_d(option = "magma",  
                       direction = -1) +  
  geom_text(col = "gray13",  
            hjust = "right",  
            aes(label = word),  
            x = -50) +  
  scale_y_reverse() +  
  labs(fill = NULL) +  
  labs(x = 'n') +  
  labs(y = "") +  
  my_theme ->  
  my_plot

a <- my_plot +  
  facet_null() +  
  scale_x_continuous(  
    limits = c(-80, 100),  
    breaks = c(0, 5, 10, 20)) +  
  geom_text(x = 1000 , y = -10,  
            family = "Times",  
            aes(label = as.character(date)),  
            size = 30, col = "red") +  
  gganimate::transition_time(as.double(binded$datenumeric))

animate(a, duration = 100, fps = 30)
anim_save("racewords-day.gif")

firstscript <- allscripts$`02/14` %>%
  unnest_tokens(word, text)

tidy_script <- firstscript %>% anti_join(custom_stop_words)

tidy_script %>%
  count(word, sort = TRUE) 

tidy_script %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
