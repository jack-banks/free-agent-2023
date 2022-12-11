
library(tidyverse)

data = read_csv("data/archived/player-values-raw.csv")
#write_csv(data, "data/archived/player-values-raw.csv")

names(data) = as.character(data[2,])

data = data[-c(1:2),]

data = {
  data %>% 
    select(player = `Player (3)`, from = From, to = To, value = Yrs) %>% 
    mutate(first = NA_character_,
           last = NA_character_,
           name_char = NA_integer_)
}

name_list = strsplit(data$player, " ")

for(i in 1:nrow(data)) {
  if(data$value[i] == "0-") {
    data$value[i] = "0  Market Value: $2,000,000"
  }
}

value_list = strsplit(data$value, ": ")

for(i in 1:nrow(data)) {
  data$first[i] = name_list[[i]][1]
  data$last[i] = name_list[[i]][2]
  data$name_char[i] = nchar(data$last[i])
  data$player[i] = paste(substr(data$first[i],
                                 data$name_char[i]+1,
                                 nchar(data$first[i])),
                         data$last[i])
  
  data$value[i] = substr(value_list[[i]][2],
                         2, nchar(value_list[[i]][2]))
  data$value[i] = str_remove_all(data$value[i], ",")
  
}
data$player[187] = "Jackie Bradley Jr."
data$player[239] = "Chi Chi Gonzalez"
data$player[250] = "Locke St. John"
data$value = as.numeric(data$value)
data = data[,1:4]


names = {
  Lahman::People %>% 
    mutate(final_year = lubridate::year(finalGame)) %>% 
    filter(final_year >= 2019) %>% 
    mutate(player = paste(nameFirst, nameLast)) %>% 
    select(player, bbrefID)
}

data = data %>% left_join(names, by = "player")

write_csv(data, "data/player-values.csv")
