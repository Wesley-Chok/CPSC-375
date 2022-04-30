library(nycflights13)
library(ggplot2)
library(dplyr)
library(tidyverse)

data("flights")

print(flights)
flights %>% filter(year==2013,month==2,day==12)
flights %>% filter(dep_delay>120)
flights %>% filter(carrier=="UA",carrier=="AA",carrier=="DL")
flights %>% mutate(mph = distance / air_time * 60) %>% arrange(
  desc(mph))
flights %>% arrange(desc(distance))
flights %>% arrange(desc(distance)) %>% select(origin, dest)
flights %>% mutate(total_delay = dep_delay + arr_delay) %>% arrange(
  desc(total_delay)) %>% select(origin, dest)
flights %>%
  filter(min_rank(-(dep_delay)) %in% 1:10) %>% select(origin, dest)
flights %>% mutate(total_delay = dep_delay + arr_delay) %>% summarise(mean(total_delay))
flights %>% group_by(dep_time) %>% mutate(total_delay = dep_delay + arr_delay) %>% summarise(mean(total_delay))
flights %>% group_by(dep_time, arr_time) %>% mutate(total_delay = dep_delay + arr_delay) %>% summarise(mean(total_delay))