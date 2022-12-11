
library(tidyverse)

data = read_csv("data/player-values.csv")
data = data %>% drop_na() %>% rename(key_bbref = bbrefID)
map = read_csv("data/chadwick-map.csv")
map = map %>% select(key_bbref, key_fangraphs)

data = data %>% inner_join(map, by = "key_bbref") %>% 
  select(playerid = key_fangraphs,value, spotrac = to) %>% drop_na() %>% 
  mutate(spotrac = "FA")

write_csv(data, "data/player-values.csv")
