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

money <- c()
char_money <- c()
for(j in 1:nrow(ply2)) {
  dol <- str_split(ply2$Dollars, " ")[[j]][5]
  num <- ifelse(!is.na(dol), str_split(dol,"\\$")[[1]][2], "2,000,000")
  x.sal <- paste0(str_split(num, ",")[[1]][1], str_split(num, ",")[[1]][2], str_split(num, ",")[[1]][3])
  char_money <- c(char_money, num)
  money <- c(money, as.numeric(x.sal))
}
ply3 <- cbind(ply2, exp_salary = money, exp_salary_char = paste0("$",char_money))



free_agents <- ply3 %>% select(player_name, Pos., Age, Bats, Throws, From, To, exp_salary_char, exp_salary)


tm_bs <- read_csv("data/team-base.csv")

cap <- c()
char_cap <- c()
for(z in 1:nrow(tm_bs)) {
  cap.num <- str_split(tm_bs$X2,"\\$")[[z]][2]
  x.cap <- paste0(str_split(cap.num, ",")[[1]][1], str_split(cap.num, ",")[[1]][2], str_split(cap.num, ",")[[1]][3])
  char_cap <- c(char_cap, cap.num)
  cap<- c(cap, as.numeric(x.cap))
}
tm_cap <- cbind(Team = tm_bs$X1, exp_cap_char = paste0("$",char_cap), exp_cap = cap)