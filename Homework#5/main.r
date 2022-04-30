library(tidyverse)
population <- tibble(state=c("California", "California", "California", "California"), county=c("Orange", "Orange", "Los Angeles", "Los Angeles"), year=c(2000, 2010, 2000, 2010), population=c(2846289, 3010232, 3694820, 3792621))
population
countyseats <- tibble(statename=c("California", "California", "California", "Oregon"), countyname=c("Orange","Los Angeles", "San Diego", "Wasco"), countyseat=c("Santa Ana", "Los Angeles", "San Diego", "The Dalles"))
countyseats


billboard
billboard2 <- billboard %>% pivot_longer(wk1:wk76, names_to = "week", values_to = "rank", values_drop_na = TRUE)
billboard2
billboard3 <- billboard2 %>% mutate(week = as.integer(gsub("wk", "", week)),date = as.Date(date.entered) + 7 * (week - 1),date.entered = NULL)
billboard3

billboard3 %>% select(track, week)
billboard3 %>% arrange(desc(week)) %>% select(track, week)
billboard3 %>% group_by(track) %>% top_n(1, rank) %>% select(track, rank)
billboard3 %>% group_by(track) %>% top_n(1, -rank) %>% select(track, rank)
billboard3 %>% group_by(artist) %>% top_n(1, rank) %>% select(artist, rank)
billboard3 %>% group_by(artist) %>% top_n(1, -rank) %>% select(artist, rank)
billboard3 %>% filter(rank > 35) %>% select(track, rank)
billboard3 %>% filter(rank > 35) %>% select(artist, track, rank)

demo <- read_csv("demographics.csv")

demo_tidy <- demo %>% pivot_wider(names_from = "Series Code", values_from = YR2015)
demo_tidy