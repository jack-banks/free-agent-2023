
cap_space = sim_cap_spaces$space_default
names(cap_space) = sim_cap_spaces$Team

cap_space

winner = "ARI"
salary = 5000000

cap_space[winner] = cap_space[winner] - salary

cap_space

players_all = read_csv("data/players-all.csv")
pos = read_csv("data/positions.csv") %>% select(-Team)
pos_multipliers = read_csv("data/pos-multipliers.csv") %>% select(-Team)

pos_mat = as.matrix(pos)
rownames(pos_mat) = names(cap_space)

pos_mult_mat = as.matrix(pos_multipliers)
rownames(pos_mult_mat) = names(cap_space)

pos = pos_mat
pos_multipliers = pos_mult_mat

write.table(pos_mat, file = "data/positions.txt")
write.table(pos_mult_mat, file = "data/pos-multipliers.txt")

signing = function(playerid, team, salary) {
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
  
  # update cap_space vector
  cap_space[team] = cap_space[team] - salary
  
  assign("cap_space", cap_space, .GlobalEnv)
  assign("pos", pos, .GlobalEnv)
  assign("pos_multipliers", pos_multipliers, .GlobalEnv)
  assign("players_all", players_all, .GlobalEnv)
}

signing("10954", "ARI", 30000000)

save(players_all, pos, pos_multipliers, sim_cap_spaces,
     signing, file = "Environment.Rdata")

