library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)
library(gganimate)   
library(data.table)
library(tidyr)
library(topicmodels)
library(quanteda)


getamloscripts <- function(){
  start <- as.Date("14/02/20","%d/%m/%y")
  end   <- as.Date("15/05/20","%d/%m/%y")
  
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

custom_stop_words <- bind_rows(tibble(word = c('mil', 'andrés', 'manuel', 'lópez', 'obrador', 'presidente', 
                                               'si', 'entonces', 'pregunta', 'interlocutora', 'va', 'vamos', NA)), 
                               tibble(word = tm::stopwords("spanish")))

transform_bigrams <- function(){
  tidy_bigrams <- list()
  for(i in names(allscripts)) {
    tidy_bigrams[[i]] <- allscripts[[i]] %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2)
  }
  
  bigrams_stopped <- list()
  for(i in names(tidy_bigrams)){
    bigrams_stopped[[i]] <- tidy_bigrams[[i]] %>% separate(bigram, c("word1", "word2"), sep = " ") %>% 
      filter(!word1 %in% custom_stop_words$word,
             !word2 %in% custom_stop_words$word) %>% 
      count(word1, word2, sort = TRUE)
    
  }
  
  return(bigrams_stopped)
}

transform_trigrams <- function(){
  tidy_bigrams <- list()
  for(i in names(allscripts)) {
    tidy_bigrams[[i]] <- allscripts[[i]] %>%
      unnest_tokens(trigram, text, token = "ngrams", n = 3)
  }
  
  trigrams_stopped <- list()
  for(i in names(tidy_bigrams)){
    trigrams_stopped[[i]] <- tidy_bigrams[[i]] %>% separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
      filter(!word1 %in% custom_stop_words$word,
             !word2 %in% custom_stop_words$word,
             !word3 %in% custom_stop_words$word) %>% 
      count(word1, word2, word3, sort = TRUE)
    
  }
  
  return(trigrams_stopped)
}

transform_bind <- function(scripts){
  tidy_list <- list()
  for(i in names(scripts)) {
    tidy_list[[i]] <- scripts[[i]] %>%
      unnest_tokens(word, text) %>% anti_join(custom_stop_words) 
  }
  
  count_list <- list()
  for(i in names(tidy_list)){
    count_list[[i]] <- tidy_list[[i]] %>% count(word, sort = TRUE) %>% head(25)
  }
  
  counted_dated <- list()
  for(i in names(count_list)){
    counted_dated[[i]] <- tibble(word = count_list[[i]]$word, n = count_list[[i]]$n, date = c(i))
  }
  
  return(counted_dated %>% bind_rows)
}




bigrams <- transform_bigrams()
trigrams <- transform_trigrams()


binded <- transform_bind(allscripts)
binded$datenumeric <- gsub('/', '', binded$date)
binded$date <- as.factor(binded$date)
binded$datenumeric <- as.numeric(binded$datenumeric)

#Casting of matrix
dtm_one_words <- binded %>% cast_dtm(datenumeric, word, n)
ap_lda <- LDA(dtm_one_words, k = 2, control = list(seed = 123)) 
ap_topics <- tidy(ap_lda, matrix = "beta")

#General topic modelling of his scripts
top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


chapters_gamma <- tidy(ap_lda, matrix = "gamma")

chapters_gamma %>%
  mutate(title = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document)





binded %>% cast_dfm(datenumeric, word, n)


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
    limits = c(-120, 120),  
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
  labs(x = 'n') +  
  labs(y = "") +  
  my_theme ->  
  my_plot

a <- my_plot +  
  facet_null() +  
  scale_x_continuous(  
    limits = c(-110, 120),  
    breaks = c(0, 10, 35, 70, 100)) +  
  geom_text(x = 60 , y = -10,  
            family = "",  
            aes(label = as.character(date)),  
            size = 25, col = "black") +  
  exit_disappear() +
  gganimate::transition_states(datenumeric, transition_length = 1, state_length = 2)

animate(a, nframes = 150)

anim_save("racewords-day.gif", a)

tidy_script %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
