
library(tidyverse)

d_list = list()
pos_vec = c("c", "1b", "2b", "3b", "ss",
            "lf", "cf", "rf", "dh")


for(i in 1:9) {
  d_list[[i]] = read_csv(paste0("data/archived/fangraphs-",pos_vec[i],".csv"))
  
  d_list[[i]]$POS = toupper(pos_vec[i])
}

data = do.call("rbind", d_list)

# original teams

old = read_csv("data/archived/hitter-fangraphs-projections.csv")
old = old %>% select(playerid, Team)

data = {
  data %>% 
    select(-Team) %>% 
    left_join(old, by = "playerid") %>% 
    mutate(IP = 0) %>% 
    select(playerid, Name, Team, POS, G, PA, IP, WAR) %>% 
    arrange(desc(WAR))
}

write_csv(data, "data/hitter-projections.csv")


