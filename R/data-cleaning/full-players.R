
library(tidyverse)

pit = read_csv("data/pitcher-projections.csv")
hit = read_csv("data/hitter-projections.csv")

all = rbind(pit, hit)

fa = read_csv("data/player-values.csv")
fa$playerid = as.character(fa$playerid)

all = all %>% left_join(fa, by = "playerid")

all$Team[is.na(all$Team)] = "FA"

for(i in 1:nrow(all)) {
  if(all$Team[i] == "FA") {
    all$spotrac[i] = "FA"
    if(is.na(all$value[i])) {
      all$value[i] = 2000000
    }
  }
  
  if(!is.na(all$spotrac[i])) {
    all$Team[i] = "FA"
  }
}

dupe = {
  all %>% 
    group_by(playerid) %>% 
    summarise(n = n())
}
dupe = all %>% left_join(dupe) %>% filter(n > 1 & POS != "DH") %>% arrange(playerid)

write_csv(dupe, "data/multi-positions.csv")

fa = all %>% filter(Team == "FA") %>% select(-spotrac)

