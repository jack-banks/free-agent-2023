library(shiny)
library(DT)
library(data.table)
library(ggrepel)
library(shinyWidgets)
library(tidyverse)

setwd('C:/Users/adity/Documents/free-agent-2023')
players <- read_csv("data/players-all.csv")
final_cap <- read_csv("data/sim_cap_spaces.csv")
needs <- read_csv("data/pos-multipliers.csv")
players <- players %>% filter(is.na(Team)) %>% arrange(desc(value)) %>% head(100)

final_cap$Team = sub("CHW", "CWS", final_cap$Team)
final_cap$Team = sub("KC", "KCR", final_cap$Team)
final_cap$Team = sub("SD", "SDP", final_cap$Team)
final_cap$Team = sub("SF", "SFG", final_cap$Team)
final_cap$Team = sub("TB", "TBR", final_cap$Team)
final_cap$Team = sub("WSH", "WSN", final_cap$Team)



ui <- fluidPage(
  
  titlePanel("2022-2023 Baseball Free Agency Simulator"),
  br(),
  tags$h3("Authors: ", tags$p("Aditya Fuldeore, Aaron Wang, Jack Banks, Jason Rhee",)),
  br(),
  tags$p("This is a baseball free agency simulator. By adjusting for market size and team strategy, you can change how teams approach free agency in this simulation. This simulator is taking into account team rosters at the end of the 2022 season, prior to major trades and signings of the offseason. The team rosters, projected WAR, and projected player values are via Fangraphs."),
  br(),
  
  tags$p("First, pick the type of market spending you want to simulate your free agency with. This determines how much money a team will spend at the maximum. Big market teams spend more than small market teams. There are three options: one for how team markets currently stand, one if you want to favor big market teams, and one if you want all teams to have a relatively equal spending market."),
  br(),
  
  selectizeInput("mkt", "Select Market Spending:",
                 choices = c("Regular Markets", "Favor Big Markets", "Equal Markets"),
                 selected = c("Regular Markets"),
                 multiple = F),
  
  tags$p("Next, pick the type of team strategy. This determines how much of a team's remaining cap space they spend. There are three more options: one for teams spending based on their current championship contentions status, one where World Series favorites would be more favored for free agents, and one where all teams are set to equal contention status."),
  br(),
  
  #if team strategy 1 selected, they get default number of money to spend, and certain WAR to fill
  selectizeInput("ts", "Select Team Strategy:",
                 choices = c("Spending Based On Team Contention Status", 
                             "Favor World Series Favorites", "All Teams Equal Contention"),
                 selected = c("Spending Based On Team Contention Status"),
                 multiple = F),
  br(),
  tags$h2("Results"),
  br(),
  fluidRow(
    column(12, align = "center",
           DTOutput("table_out"))
  ),
  br(),
  fluidRow(
    column(12, align = "center",
           DTOutput("table_out2"))
  )
  
  
)

server <- function(input, output) {
  
  #table
  output$table_out <- renderDT({
    if(input$ts == "Spending Based On Team Contention Status") { #real contention status
      
      if(input$mkt == "Regular Markets") { #default
        
        for(p in 1:nrow(players)) {
          bids <- c()
          for(t in 1:nrow(final_cap)) {
            bid <- players[p,]$value * final_cap[t,]$aggress * needs[t,players[p,]$POS][1][[1]]
            if(final_cap[t,]$space_default >= bid) {
              bids[t] <- bid
            } else {
              bids[t] <- 0
            }
          }
          ind <- sample(which(bids == max(bids)), 1)
          winner <- final_cap[ind,]$Team
          #subtract cap of winner 
          players[p,]$Team <- winner
        }
        
        return(data.table(players))
        
      } else if(input$mkt == "Favor Big Markets") { #real and big mkt
        
        for(p in 1:nrow(players)) {
          bids <- c()
          for(t in 1:nrow(final_cap)) {
            bid <- players[p,]$value * final_cap[t,]$aggress * needs[t,players[p,]$POS][1][[1]]
            if(final_cap[t,]$space.real.favs>= bid) {
              bids[t] <- bid
            } else {
              bids[t] <- 0
            }
          }
          ind <- sample(which(bids == max(bids)), 1)
          winner <- final_cap[ind,]$Team
          #subtract cap of winner 
          players[p,]$Team <- winner
        }
        
        return(data.table(players))
        
      } else if(input$mkt == "Equal Markets") { #real and equal mkt
        
        for(p in 1:nrow(players)) {
          bids <- c()
          for(t in 1:nrow(final_cap)) {
            bid <- players[p,]$value * final_cap[t,]$aggress * needs[t,players[p,]$POS][1][[1]]
            if(final_cap[t,]$space.real.eq >= bid) {
              bids[t] <- bid
            } else {
              bids[t] <- 0
            }
          }
          ind <- sample(which(bids == max(bids)), 1)
          winner <- final_cap[ind,]$Team
          #subtract cap of winner 
          players[p,]$Team <- winner
        }
        
        return(data.table(players))
        
      }
      
      
    } else if(input$ts == "Favor World Series Favorites") { #contender heavy team strategy
      
      if(input$mkt == "Regular Markets") { #contending and real mkt
        
        for(p in 1:nrow(players)) {
          bids <- c()
          for(t in 1:nrow(final_cap)) {
            bid <- players[p,]$value * final_cap[t,]$aggress_fav * needs[t,players[p,]$POS][1][[1]]
            if(final_cap[t,]$space.favs.real >= bid) {
              bids[t] <- bid
            } else {
              bids[t] <- 0
            }
          }
          ind <- sample(which(bids == max(bids)), 1)
          winner <- final_cap[ind,]$Team
          #subtract cap of winner 
          players[p,]$Team <- winner
        }
        
        return(data.table(players))
        
      } else if(input$mkt == "Favor Big Markets") { #both contending strat and big mkt
        
        for(p in 1:nrow(players)) {
          bids <- c()
          for(t in 1:nrow(final_cap)) {
            bid <- players[p,]$value * final_cap[t,]$aggress_fav * needs[t,players[p,]$POS][1][[1]]
            if(final_cap[t,]$space_favs >= bid) {
              bids[t] <- bid
            } else {
              bids[t] <- 0
            }
          }
          ind <- sample(which(bids == max(bids)), 1)
          winner <- final_cap[ind,]$Team
          #subtract cap of winner 
          players[p,]$Team <- winner
        }
        
        return(data.table(players))
        
      } else if(input$mkt == "Equal Markets") { #contending and equal mkt
        
        for(p in 1:nrow(players)) {
          bids <- c()
          for(t in 1:nrow(final_cap)) {
            bid <- players[p,]$value * final_cap[t,]$aggress_fav * needs[t,players[p,]$POS][1][[1]]
            if(final_cap[t,]$space.favs.eq >= bid) {
              bids[t] <- bid
            } else {
              bids[t] <- 0
            }
          }
          ind <- sample(which(bids == max(bids)), 1)
          winner <- final_cap[ind,]$Team
          #subtract cap of winner 
          players[p,]$Team <- winner
        }
        
        return(data.table(players))
        
      }
      
      
    } else if(input$ts == "All Teams Equal Contention") { #equal spending contender status
      
      if(input$mkt == "Regular Markets") { #equal and real mkt
        
        for(p in 1:nrow(players)) {
          bids <- c()
          for(t in 1:nrow(final_cap)) {
            bid <- players[p,]$value * final_cap[t,]$aggress_equal * needs[t,players[p,]$POS][1][[1]]
            if(final_cap[t,]$space.eq.real >= bid) {
              bids[t] <- bid
            } else {
              bids[t] <- 0
            }
          }
          ind <- sample(which(bids == max(bids)), 1)
          winner <- final_cap[ind,]$Team
          #subtract cap of winner 
          players[p,]$Team <- winner
        }
        
        return(data.table(players))
        
      } else if(input$mkt == "Favor Big Markets") { #equal and big mkt
        
        for(p in 1:nrow(players)) {
          bids <- c()
          for(t in 1:nrow(final_cap)) {
            bid <- players[p,]$value * final_cap[t,]$aggress_equal * needs[t,players[p,]$POS][1][[1]]
            if(final_cap[t,]$space.eq.favs >= bid) {
              bids[t] <- bid
            } else {
              bids[t] <- 0
            }
          }
          ind <- sample(which(bids == max(bids)), 1)
          winner <- final_cap[ind,]$Team
          #subtract cap of winner 
          players[p,]$Team <- winner
        }
        
        return(data.table(players))
        
      } else if(input$mkt == "Equal Markets") { #both equal strat and mkt
        
        for(p in 1:nrow(players)) {
          bids <- c()
          for(t in 1:nrow(final_cap)) {
            bid <- players[p,]$value * final_cap[t,]$aggress_equal * needs[t,players[p,]$POS][1][[1]]
            if(final_cap[t,]$space_equal >= bid) {
              bids[t] <- bid
            } else {
              bids[t] <- 0
            }
          }
          ind <- sample(which(bids == max(bids)), 1)
          winner <- final_cap[ind,]$Team
          #subtract cap of winner 
          players[p,]$Team <- winner
        }
        
        return(data.table(players))
        
      }
      
    } else {
      return(data.table(0))
    }
  })
  
  output$table_out2 <- renderDT({
    
    results <- final_cap %>% select(Team)
    results <- cbind(results, data.frame("PlayersAdded" = 1, "PlayersLost" = 1, "Spent" = 0))
    
    return(data.table(results))
  })
  
  
}


shinyApp(ui, server)