#cleaning data before analysis

ply.temp <- read_csv("data/player-values.csv")

ply.temp[2,1] <- "Players"

colnames(ply.temp) <- ply.temp[2,]

ply <- ply.temp[-c(1:2),]

player_last <- c()
player_first <- c()
for(i in 1:nrow(ply)) {
  last <- str_split(ply$Players, " ")[[i]][2]
  player_last <- c(player_last, last)
  player_first <- c(player_first, str_split(ply$Players, last)[[i]][2])
  
}
ply2 <- cbind(player_name = paste0(player_first, player_last), ply)




tm_bs <- read_csv("data/team-base.csv")