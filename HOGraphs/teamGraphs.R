library(jsonlite)
library(httr)
library(tidyverse)
library(ggplot2)
#0 = shl, 1 = smjhl
league <- 0
season <- 68

player_list <- list()

player_stats <- GET("https://index.simulationhockey.com/api/v1/schedule", query = list(season = 68))
player_stats <- fromJSON(rawToChar(player_stats$content))
player_stats <- do.call(data.frame, player_stats)
player_list[[68]] <- player_stats


teams <- GET("https://index.simulationhockey.com/api/v1/teams", query = list(season = 68))
teams <- fromJSON(rawToChar(teams$content))
teams <- do.call(data.frame, teams)
teams <- teams[c('id','abbreviation')]
#print(teams)

team_results <- do.call(rbind.data.frame, player_list) %>%
  mutate(totalGoals = homeScore + awayScore) %>%
  filter(totalGoals != 0)
team_results$date <- as.Date(team_results$date) 
#print(player_list)
home_df <- team_results[c('date', 'homeTeam','homeScore', 'awayScore', 'overtime')] %>%
  mutate(
    points = if_else(homeScore > awayScore, 2, if_else(overtime == 1, 1, 0))
  ) %>%
  rename(
    id = homeTeam
  ) %>%
  inner_join(
    teams, by="id"
  )
home_df <- home_df[c('date', 'abbreviation', 'points')]
away_df <- team_results[c('date', 'awayTeam','homeScore', 'awayScore', 'overtime')] %>%
  mutate(
    points = if_else(awayScore > homeScore, 2, if_else(overtime == 1, 1, 0))
  ) %>%
  rename(
    id = awayTeam
  )%>%
  inner_join(
    teams, by="id"
  )
away_df <- away_df[c('date', 'abbreviation', 'points')]
pointResults <- rbind(home_df, away_df)  %>%
  group_by(abbreviation) %>%
  mutate(totalPoints = cumsum(points),
         game = 1,
         gamesPlayed = cumsum(game))

print(pointResults)
ggplot(pointResults, aes(x = gamesPlayed, y = totalPoints, color = abbreviation)) +
  geom_line()