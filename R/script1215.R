
library(tidyverse)

hit = read_csv("data/non-duplicate-hit-proj.csv")[,-1] %>% filter(PA > 1)

all = read_csv("data/players-all.csv")

pit = {
  all %>% 
    filter(POS %in% c("SP", "RP"),
           IP > 1) %>% 
    select(-spotrac, -value) %>% 
    mutate(Team = ifelse(Team == "FA", NA, Team)) %>% 
    arrange(Team, desc(WAR))
}

sp <- {
  pit %>%
    filter(!is.na(Team),
           POS == "SP")
}

sp_list = split(sp, sp$Team)

for(i in 1:30) {
  sp_list[[i]]$rank = seq(1, nrow(sp_list[[i]]))
}
sp_rank = data.table::rbindlist(sp_list) %>%
  filter(rank <= 5) %>% mutate(POS = paste0(POS, as.character(rank))) %>% 
  select(-rank)

pit = rbind(pit, sp_rank)

all = rbind(hit, pit) %>% arrange(Team, POS, desc(WAR))
all[which(all$playerid == "5222"),3] = NA


pos_a <- {
  all %>%
    filter(!is.na(Team)) %>%
    group_by(Team, POS) %>%
    summarise(WAR = sum(WAR))
}

pos = spread(pos_a, key = POS, value = WAR)
pos[is.na(pos)] = 0

pos = pos %>% select(Team, SP, SP1, SP2, SP3, SP4, SP5,
                     RP, C, `1B`, `2B`, `3B`, SS, LF, CF, RF, DH)

values = read_csv("data/player-values.csv") %>% select(-spotrac) %>%
  mutate(playerid = as.character(playerid))

fa = all %>% filter(is.na(Team)) %>% left_join(values) %>%  select(playerid, value)
fa$value[is.na(fa$value)] = 2000000

all = all %>% left_join(fa)


write_csv(all, "data/players-all.csv")
write_csv(pos, "data/positions.csv")

scaled_pos = pos

for(i in 2:17) {
  scaled_pos[,i] = 1 - 0.1 * scale(scaled_pos[,i])[,1]
}

write_csv(scaled_pos, "data/pos-multipliers.csv")


