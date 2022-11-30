
library(tidyverse)

# Player market values

url = rvest::read_html("https://www.spotrac.com/mlb/free-agents/available/")
players = rvest::html_table(url)[[1]]

readr::write_csv(players, "data/player-values.csv")

# Team base salaries

url = rvest::read_html("https://www.spotrac.com/spots/early-offseason-mlb-payroll-projections-1665/")
teams = rvest::html_table(url)[[1]]

readr::write_csv(teams, "data/team-caps.csv")

# Prior season salaries

l = list(d20 = NULL, d21 = NULL, d22 = NULL)

for(i in 1:3) {
  u = rvest::read_html(paste0("https://www.spotrac.com/mlb/payroll/",
                              2019 + i, "/"))
  d = rvest::html_table(u)[[1]]
  
  d$Season = 2019 + i
  
  names(d)[c(5, 10)] = c("Active Roster", "Total Payroll")
  
  d = dplyr::filter(d, Team != "League Average")
  
  l[[i]] = d
}

past_sal = rbind(l$d20, l$d21, l$d22)

past_sal$`Total Payroll` = as.numeric(str_remove_all(past_sal$`Total Payroll`, "[$,]"))

past_sal = {
  past_sal %>% 
    mutate(TaxCap = ifelse(Season == 2020, 208000000,
                           ifelse(Season == 2021, 210000000,
                                  230000000))) %>% 
    mutate(wPayroll = `Total Payroll`/TaxCap * 100)
}
