library(tidyverse)
tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

glimpse(tidy_anime)

# Investigate which combination of variables uniquely identify observations

%nls_tran %>% group_by(id,transnum) %>% summarise(n_per_key=n()) %>% ungroup %>% count(n_per_key)
tidy_anime %>% group_by(animeID) %>% summarise(n_per_key=n()) %>% ungroup %>% count(n_per_key)

tidy_anime %>% group_by(animeID,start_date) %>% summarise(n_per_key=n()) %>% ungroup %>% count(n_per_key)

tidy_anime %>% group_by(animeID,name) %>% summarise(n_per_key=n()) %>% ungroup %>% count(n_per_key)
