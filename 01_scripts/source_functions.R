library(tidyverse)
load("yahoo_token.Rdata")

###########
standings_page <- GET("https://fantasysports.yahooapis.com/fantasy/v2/game/nfl",
                      add_headers(Authorization=paste0("Bearer ", yahoo_token$access_token)))
XMLstandings<- content(standings_page, as="parsed", encoding="utf-8")
doc <- xmlTreeParse(XMLstandings, useInternal=TRUE)
full_yahoo_json <- xmlToList(xmlRoot(doc))
game_key = full_yahoo_json$game$game_key

# listviewer::jsonedit(full_yahoo_json)

baseURL <- "https://fantasysports.yahooapis.com/fantasy/v2/league/"

leagueID <- "115120"
# leagueID    <- "1244633"



get_game_information <-function(game_of_week = 1, yahoo_json){
  
  TeamA = yahoo_json$league$scoreboard$matchups[game_of_week]$matchup$teams[1]$team$name
  TeamB = yahoo_json$league$scoreboard$matchups[game_of_week]$matchup$teams[2]$team$name
  PointsA = yahoo_json$league$scoreboard$matchups[game_of_week]$matchup$teams[1]$team$team_points$total
  PointsB = yahoo_json$league$scoreboard$matchups[game_of_week]$matchup$teams[2]$team$team_points$total
  ProjectedPointsA = yahoo_json$league$scoreboard$matchups[game_of_week]$matchup$teams[1]$team$team_projected_points$total
  ProjectedPointsB = yahoo_json$league$scoreboard$matchups[game_of_week]$matchup$teams[2]$team$team_projected_points$total
  
  vec = tibble(TeamA = TeamA, TeamB = TeamB, PointsA = PointsA, PointsB = PointsB,ProjectedPointsA = ProjectedPointsA, ProjectedPointsB = ProjectedPointsB, game_of_week = game_of_week)
  return(vec)
}




get_weekly_points <- function(week_number = 1, token = yahoo_token){
  
  standingsURL <-paste(baseURL,game_key,".l.",leagueID,"/scoreboard;week=",week_number,sep = "")
  
  standings_page <- GET(standingsURL,
                        add_headers(Authorization = paste0("Bearer ", yahoo_token$access_token)))
  XMLstandings <- content(standings_page, as = "parsed", encoding = "utf-8")
  
  doc <- xmlTreeParse(XMLstandings, useInternal = TRUE)
  yahoo_json <- xmlToList(xmlRoot(doc))
  
  # listviewer::jsonedit(yahoo_json)
  
  out <- map_dfr(1:5, .f = ~get_game_information(game_of_week = .x, yahoo_json = yahoo_json))
  
  return(out)
}

get_coaches_token <- function() {
  standingsURL <- paste(baseURL,game_key,".l.",leagueID,"/scoreboard;week=",1,sep = "")
  
  standings_page <- GET(standingsURL,add_headers(Authorization = paste0("Bearer ", yahoo_token$access_token)))
  XMLstandings <- content(standings_page, as = "parsed", encoding = "utf-8")
  doc <- xmlTreeParse(XMLstandings, useInternal = TRUE)
  yahoo_json <- xmlToList(xmlRoot(doc))
  
  # listviewer::jsonedit(yahoo_json)
  
  get_team_keys <- function(team_num = 1) {
    hold <- yahoo_json$league$scoreboard$matchups %>% pluck(team_num)
    
    out <- tibble(
      team_key = c(hold$teams[1]$team$team_key, hold$teams[2]$team$team_key),
      team_name = c(hold$teams[1]$team$name, hold$teams[2]$team$name)
    )
    
    return(out)
    
  }
  
  team_keys <-
    1:5 %>% map_dfr( ~ get_team_keys(team_num = .x))
  
  return(team_keys)
  
}

coaches_tokens <- get_coaches_token()

get_team_roster_for_week <- function(coaches_token = coaches_tokens$team_key[1],week = 1) {
    standings_page <- GET(str_c("https://fantasysports.yahooapis.com/fantasy/v2/team/",coaches_token,"/roster/players;week=",week),
                          add_headers(Authorization = paste0("Bearer ", yahoo_token$access_token))
                          )
    XMLstandings <-content(standings_page, as = "parsed", encoding = "utf-8")
    doc <- xmlTreeParse(XMLstandings, useInternal = TRUE)
    full_yahoo_json <- xmlToList(xmlRoot(doc))
    
    # listviewer::jsonedit(full_yahoo_json)
    
    get_player <- function(player_num = 1) {
      hold <- full_yahoo_json$team$roster$players %>% pluck(player_num)
      
      player_info <-
        tibble(
          player_key = hold$player_key,
          name = hold$name$full,
          eligible_positions = hold$eligible_positions$position,
          slotted_position = hold$selected_position$position,
          is_flex = hold$selected_position$is_flex,
        )
      
      return(player_info)
    }
    
    team_roster_for_week <-
      1:c(length(full_yahoo_json$team$roster$players) - 1) %>%
      map_dfr( ~ get_player(player_num = .x)) %>%
      mutate(team_key = coaches_token) %>%
      mutate(week = week)
    
    return(team_roster_for_week)
    
  }

# team_projections_by_week <-
# coaches_tokens$team_key %>% 
#   map_dfr(~get_team_roster_for_week(.x)) %>% 
#   left_join(coaches_tokens)



# standings_page <- GET(str_c("https://fantasysports.yahooapis.com/fantasy/v2/league/406.l.115120/scoreboard;week=5"),
#                       add_headers(Authorization = paste0("Bearer ", yahoo_token$access_token))
# )
# XMLstandings <-content(standings_page, as = "parsed", encoding = "utf-8")
# doc <- xmlTreeParse(XMLstandings, useInternal = TRUE)
# full_yahoo_json <- xmlToList(xmlRoot(doc))
# 
# 
# # "406.p.5228"
# 
# listviewer::jsonedit(full_yahoo_json)


####

# standings_page <- GET("https://fantasysports.yahooapis.com/fantasy/v2/game/nfl",
#                       add_headers(Authorization=paste0("Bearer ", yahoo_token$access_token)))
# XMLstandings<- content(standings_page, as="parsed", encoding="utf-8")
# doc <- xmlTreeParse(XMLstandings, useInternal=TRUE)
# full_yahoo_json <- xmlToList(xmlRoot(doc))
# game_key = full_yahoo_json$game$game_key
# 
# 
# 
# standings_page <- GET(str_c("https://fantasysports.yahooapis.com/fantasy/v2/league/406.l.115120/players;player_keys=406.p.7200/projection"),
#                       add_headers(Authorization = paste0("Bearer ", yahoo_token$access_token))
# )
# standings_page <- GET(str_c("https://fantasysports.yahooapis.com/fantasy/v2/league/406.l.115120/players;position=QB/stats"),
#                       add_headers(Authorization = paste0("Bearer ", yahoo_token$access_token))
# )
# 
# 
# XMLstandings <-content(standings_page, as = "parsed", encoding = "utf-8")
# doc <- xmlTreeParse(XMLstandings, useInternal = TRUE)
# full_yahoo_json <- xmlToList(xmlRoot(doc))
# 
# 
# listviewer::jsonedit(full_yahoo_json)
