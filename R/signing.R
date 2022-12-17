
library(tidyverse)

sim_cap_spaces = readr::read_csv("data/sim_cap_spaces.csv")

cap_space = sim_cap_spaces$space_default
names(cap_space) = sim_cap_spaces$Team

cap_space

winner = "ARI"
salary = 5000000

cap_space[winner] = cap_space[winner] - salary

cap_space

players_all = readr::read_csv("data/players-all.csv")
pos = readr::read_csv("data/positions.csv") %>% select(-Team)
pos_multipliers = readr::read_csv("data/pos-multipliers.csv") %>% select(-Team)

pos_mat = as.matrix(pos)
rownames(pos_mat) = names(cap_space)

pos_mult_mat = as.matrix(pos_multipliers)
rownames(pos_mult_mat) = names(cap_space)

pos = pos_mat
pos_multipliers = pos_mult_mat

write.table(pos_mat, file = "data/positions.txt")
write.table(pos_mult_mat, file = "data/pos-multipliers.txt")

signing = function(playerid, team, salary, players_all) {
  # assign team
  players_all$Team[which(players_all$playerid == playerid)] = team
  
  # extract player info and war
  player_pos = players_all$POS[which(players_all$playerid == playerid)]
  player_war = players_all$WAR[which(players_all$playerid == playerid)]
  
  # update positions matrix
  pos[team, player_pos] = pos[team, player_pos] + player_war
  for(i in 1:16) {
    pos_multipliers[,i] = 1 - 0.1 * scale(pos[,i])[,1]
  }

  assign("pos", pos, .GlobalEnv)
  assign("pos_multipliers", pos_multipliers, .GlobalEnv)
  assign("players_all", players_all, .GlobalEnv)
}

#signing("10954", "ARI", 30000000)


contract_char = function(contract) {
  
  x = contract
  
  x = as.character(round(x, -3))
  
  for(i in 1:length(x)) {
    figs = nchar(x[i])
    if(figs == 8) {
      x1 = substr(x[i],1,2)
      x2 = substr(x[i],3,5)
      x3 = substr(x[i],6,8)
      
      x[i] = paste0("$",x1,",",x2,",",x3)
    } else if(figs == 7) {
      x1 = substr(x[i],1,1)
      x2 = substr(x[i],2,4)
      x3 = substr(x[i],5,7)
      
      x[i] = paste0("$",x1,",",x2,",",x3)
    }
  }
  return(x)
}

contract_num = function(contract) {
  
  x = contract
  
  x = stringr::str_remove_all(x, "\\$")
  x = stringr::str_remove_all(x, ",")
  
  return(as.numeric(x))
}


save(players_all, pos, pos_multipliers, sim_cap_spaces,
     signing, contract_num, contract_char, file = "Environment.Rdata")


cap_space
pos
pos_multipliers


