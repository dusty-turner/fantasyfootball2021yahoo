library(tidyverse)
library(ggrepel)
library(httr)
library(XML)

source("01_scripts/source_functions.R")
# source("01_scripts/create_yahoo_token.R")

# week_num <- 2

run_it_all <- function(week_num = week_num){

# yahoo_token <- httr::oauth2.0_access_token(yahoo,myapp,code="t7nmxbu")
# save(yahoo_token,file="yahoo_token.Rdata")

# source("01_scripts/source_functions.R")

# yahoo_token

season_projections <-
map_dfr(.x = 1:15, .f = ~get_weekly_points(week_number = .x, token = yahoo_token)) %>% 
  mutate(week = sort(rep(1:15,5))) %>% 
  mutate(across(.cols = c(PointsA,PointsB,ProjectedPointsA,ProjectedPointsB), .fns = ~as.numeric(.))) %>% 
  filter(week <= week_num) %>% 
  mutate(winner = if_else(PointsA > PointsB, TeamA, TeamB)) %>% 
  mutate(Loser = if_else(PointsA < PointsB, TeamA, TeamB))

every_week_every_team <-
season_projections %>% 
  select(Team = TeamA, Points = PointsA , week) %>%
  bind_rows(
    season_projections %>% 
      select(Team = TeamB, Points = PointsB , week) 
  ) %>% 
  group_by(week) %>%  
  mutate(wins = rank(Points)-1, losses = 9-wins) %>%   
  group_by(Team) %>% 
  summarise(wins = sum(wins), losses = sum(losses)) %>% 
  mutate(every_week_win_perc = wins / (wins + losses))

standings <-  
season_projections %>% 
  select(Team = TeamA, winner , week) %>%
  mutate(win = if_else(Team == winner,1,0)) %>% 
  mutate(lose = if_else(Team != winner,1,0)) %>% 
  bind_rows(
    season_projections %>% 
      select(Team = TeamB, winner , week) %>%
      mutate(win = if_else(Team == winner,1,0)) %>% 
      mutate(lose = if_else(Team != winner,1,0))
  ) %>% 
  select(-winner) %>% 
  group_by(Team) %>% 
  summarise(across(.cols = c(win, lose), .fns = ~sum(.))) %>% 
  mutate(win_perc = win / (win + lose))
  
total_standings <- 
every_week_every_team %>% 
  left_join(standings) %>% 
  mutate(luck = (-win_perc + every_week_win_perc) / sqrt(2)) 

luck_help_df <- tibble(win_perc = c(0.3,0.3,.7,.7), week_win_perc = c(0,1,1,0), labs = c("Bad","Unlucky","Good","Lucky"))


luck_chart <<-
total_standings %>% 
  ggplot(aes(x = win_perc, y = every_week_win_perc, color = luck)) +
  geom_point() +
  geom_abline(aes(intercept = 0,slope = 1)) +
  xlim(0,1) + ylim(0,1) +
  scale_color_gradient2(low = "green",mid = "grey" ,high = "red",midpoint = 0,limits=c(range(total_standings$luck))) +
  # scale_color_continuous(low = "green", high = "red",limits=c(range(total_standings$luck))) +
  geom_label_repel(aes(label = str_c(Team)), color = "Black",max.iter = 10000) +
  geom_label(data = luck_help_df,mapping = aes(x=win_perc,y=week_win_perc,label = labs), color = "Black", label.size = 1) +
  labs(title = "How Lucky Is Your Team?",subtitle = "Are You Winning As Much As You Should?",
       x = "Traditional Win Percentage", y = "Every Game Every Week Win Percentage", color = "Luck") +
  theme(legend.position = "none") +
  labs(x = "Winning Percentage", y = "Every Week Win Percentage",
       title = "How lucky is your team?")

projected_minus_actual <-
  season_projections %>% 
  select(Team = TeamA, Points = PointsA, Project = ProjectedPointsA, week) %>% 
  bind_rows(
    season_projections %>% select(Team = TeamB, Points = PointsB, Project = ProjectedPointsB, week)
  ) %>% 
  mutate(Proj_minus_Act = Project - Points) %>% 
  group_by(Team) %>% 
  summarise(Net_Points = sum(Proj_minus_Act), Projected = sum(Project), Actual = sum(Points))

projected_plot <<-
projected_minus_actual %>% 
  ggplot(aes(x = Projected, y = Actual, color = Net_Points)) +
  geom_point() +
  geom_abline(aes(intercept = 0,slope = 1)) +
  # xlim(min(c(projected_minus_actual$Projected,projected_minus_actual$Actual))-10, max(c(projected_minus_actual$Projected,projected_minus_actual$Actual) + 10)) +
  ylim(min(c(projected_minus_actual$Projected,projected_minus_actual$Actual))-10, max(c(projected_minus_actual$Projected,projected_minus_actual$Actual) + 10)) +
  scale_color_gradient2(low = "green",mid = "grey" ,high = "red",midpoint = 0,limits=c(range(projected_minus_actual$Net_Points))) +
  # scale_color_continuous(low = "green", high = "red",limits=c(range(total_standings$luck))) +
  geom_label_repel(aes(label = str_c(Team)), color = "Black",max.iter = 10000) +
  # geom_label(data = luck_help_df,mapping = aes(x=win_perc,y=week_win_perc,label = labs), color = "Black", label.size = 1) 
  labs(title = "Is your team over or under performing?",subtitle = "Above the line: Over performing \nUnder The Line: Under performing",
       x = "Season Projected Points", y = "Season Actual Points", color = "Amount of Over Performance",
       caption = "Black line represents scoring exactly what you were projected") +
  theme(legend.position = "none")


# get_weekly_points(week_number = 1, token = yahoo_token)


# get_game_information(game_of_week = 1, yahoo_json = full_yahoo_json)

coaches_tokens <- get_coaches_token()

team_projections_by_week <-
  coaches_tokens$team_key %>% 
  map_dfr(~get_team_roster_for_week(.x)) %>% 
  left_join(coaches_tokens)

max_min_points <-
season_projections %>% 
  select(Team = TeamA, Points = PointsA, Project = ProjectedPointsA, week) %>% 
  bind_rows(
    season_projections %>% select(Team = TeamB, Points = PointsB, Project = ProjectedPointsB, week)
  ) %>% 
  group_by(week) %>% 
  filter(Points == max(Points) | Points == min(Points))

max_team <<- max_min_points %>% filter(week == week_num) %>% filter(Points == max(Points)) %>% pull(Team)
max_points <<- max_min_points %>% filter(week == week_num) %>% filter(Points == max(Points)) %>% pull(Points)
min_team <<- max_min_points %>% filter(week == week_num) %>% filter(Points == min(Points)) %>% pull(Team)
min_points <<- max_min_points %>% filter(week == week_num) %>% filter(Points == min(Points)) %>% pull(Points)

max_points_tally <<-  
max_min_points %>% 
  filter(Points == max(Points)) %>% 
  count(Team) %>% 
  arrange(-n) %>% 
  ungroup() %>% 
  select(-week) %>% 
  group_by(Team) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  arrange(-n)


min_points_tally <<-  
max_min_points %>% 
  filter(Points == min(Points)) %>% 
  count(Team) %>% 
  arrange(-n) %>%
  ungroup() %>% 
  select(-week) %>% 
  group_by(Team) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  arrange(-n)

week_n_projected_minus_actual <-
season_projections  %>% 
  select(Team = TeamA, Points = PointsA, Project = ProjectedPointsA, week) %>% 
  bind_rows(
    season_projections %>% select(Team = TeamB, Points = PointsB, Project = ProjectedPointsB, week)
  ) %>% 
  filter(week == week_num) %>% 
  mutate(Proj_minus_Act = Project - Points) %>% 
  group_by(Team) %>% 
  summarise(Net_Points = sum(Proj_minus_Act), Projected = sum(Project), Actual = sum(Points)) 

luck_by_week <<-
week_n_projected_minus_actual %>% 
  ggplot(aes(x = Projected, y = Actual, color = Net_Points)) +
  geom_point() +
  geom_abline(aes(intercept = 0,slope = 1)) +
  # xlim(min(c(projected_minus_actual$Projected,projected_minus_actual$Actual))-10, max(c(projected_minus_actual$Projected,projected_minus_actual$Actual) + 10)) +
  ylim(min(c(week_n_projected_minus_actual$Projected,week_n_projected_minus_actual$Actual))-10, max(c(week_n_projected_minus_actual$Projected,week_n_projected_minus_actual$Actual) + 10)) +
  scale_color_gradient2(low = "green",mid = "grey" ,high = "red",midpoint = 0,limits=c(range(projected_minus_actual$Net_Points))) +
  # scale_color_continuous(low = "green", high = "red",limits=c(range(total_standings$luck))) +
  geom_label_repel(aes(label = str_c(Team)), color = "Black",max.iter = 10000) +
  # geom_label(data = luck_help_df,mapping = aes(x=win_perc,y=week_win_perc,label = labs), color = "Black", label.size = 1) 
  labs(title = str_c("Did your team over or under perform in week ", week_num, "?"),subtitle = "Above the line: Over performing \nUnder The Line: Under performing",
       x = "Week Projected Points", y = "Week Actual Points", color = "Amount of Over Performance",
       caption = "Black line represents scoring exactly what you were projected") +
  theme(legend.position = "none")





}
