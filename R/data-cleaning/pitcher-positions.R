
library(tidyverse)

data = read_csv("data/pitcher-projections.csv")

data = {
  data %>% 
    mutate(G = ifelse(G == 0, 1, G),
           fracGS = GS/G,
           POS = ifelse(fracGS >= 0.5, "SP", "RP"),
           PA = 0) %>% 
    select(playerid, Name, Team, POS, G, PA, IP, WAR)
}

write_csv(data, "data/pitcher-projections.csv")


