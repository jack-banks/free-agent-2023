library(readr)
players_all <- read_csv("players-all.csv")

library(tidyverse)
players_all_1 <- players_all %>%
  filter(Team != "FA") %>%
  group_by(Team, POS) %>%
  summarise(WAR = sum(WAR))
  
players_all_1