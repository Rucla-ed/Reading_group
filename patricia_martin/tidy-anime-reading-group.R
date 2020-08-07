tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

View(tidy_anime)

tidy_anime %>%
  group_by(animeID, name, genre, producers) %>%
  summarise(n_per_group = n()) %>%
  ungroup() %>%
  count(n_per_group)

