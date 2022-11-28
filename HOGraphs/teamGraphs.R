library(jsonlite)
library(httr)
library(tidyverse)
library(ggplot2)
#0 = shl, 1 = smjhl
league <- 0
season <- 68
# divisions <- c(
#   "Pacific" = c("SEA", "CGY", "EDM", "SFP", "LAP"),
#   "Central" = c("CHI", "WPG", "NOL", "MIN", "TEX"),
#   "Atlantic" = c("ATL", "PHI", "MAN", "BAP", "TBB"),
#   "North East" = c("TOR", "BUF", "MTL", "NEW", "HAM")
# )

schedule_list <- list()

team_schedule <-
  GET("https://index.simulationhockey.com/api/v1/schedule",
      query = list(season = 68))
team_schedule <- fromJSON(rawToChar(team_schedule$content))
team_schedule <- do.call(data.frame, team_schedule)
schedule_list[[68]] <- team_schedule


teams <-
  GET("https://index.simulationhockey.com/api/v1/teams",
      query = list(season = 68))
teams <- fromJSON(rawToChar(teams$content))
teams <- do.call(data.frame, teams)
print(teams)
teams <- teams[c('id', 'abbreviation', 'colors.primary')]
#print(teams)

team_results <- do.call(rbind.data.frame, schedule_list) %>%
  mutate(totalGoals = homeScore + awayScore) %>%
  filter(totalGoals != 0)
team_results$date <- as.Date(team_results$date)
#print(schedule_list)
home_df <-
  team_results[c('date', 'homeTeam', 'homeScore', 'awayScore', 'overtime')] %>%
  mutate(points = if_else(homeScore > awayScore, 2, if_else(overtime == 1, 1, 0))) %>%
  rename(id = homeTeam) %>%
  inner_join(teams, by = "id")
home_df <-
  home_df[c('date', 'abbreviation', 'points', 'colors.primary')]
away_df <-
  team_results[c('date', 'awayTeam', 'homeScore', 'awayScore', 'overtime')] %>%
  mutate(points = if_else(awayScore > homeScore, 2, if_else(overtime == 1, 1, 0))) %>%
  rename(id = awayTeam) %>%
  inner_join(teams, by = "id")
away_df <-
  away_df[c('date', 'abbreviation', 'points', 'colors.primary')]
pointResults <- rbind(home_df, away_df)  %>%
  group_by(abbreviation) %>%
  mutate(
    totalPoints = cumsum(points),
    game = 1,
    gamesPlayed = cumsum(game),
    net = if_else(points == 2, 1, -1),
    netWins = cumsum(net)
  )
print(pointResults)

divisions <- vector(mode = "list", length = 4)
names(divisions) <- c("Pacific", "Central", "Atlantic", "NorthEast")
divisions[[1]] <-
  c("SEA", "CGY", "EDM", "SFP", "LAP")
divisions[[2]] <-
  c("CHI", "WPG", "NOL", "MIN", "TEX")
divisions[[3]] <-
  c("ATL", "PHI", "MAN", "BAP", "TBB")
divisions[[4]] <- c("TOR", "BUF", "MTL", "NEW", "HAM")
names(divisions)

for (division in names(divisions)) {
  tempDF <-
    filter(pointResults, abbreviation %in% divisions[[division]])
  print(ggplot(tempDF,
               aes(
                 x = gamesPlayed, y = netWins, group = abbreviation
               )) +
          geom_line(aes(color = colors.primary)))
}
# ggplot(pointResults,
#        aes(x = gamesPlayed, y = netWins, color = abbreviation)) +
#   geom_line()