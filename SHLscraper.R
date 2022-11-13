library(jsonlite)
library(httr)
library(tidyverse)
#0 = shl, 1 = smjhl
league <- 0

#i believe s60 is when the build scale changed
seasons <- c(66:67)

#scrape ratings
ratings_list <- list()

for (i in seasons) {
  ratings <- GET("https://index.simulationhockey.com/api/v1/players/ratings", query = list(league = league, season = i))
  ratings <- fromJSON(rawToChar(ratings$content))
  
  ratings_list[[i]] <- ratings
} 

ratings <- do.call(bind_rows, ratings_list)
order <- c('screening',
            'gettingOpen',
            'passing',
            'puckHandling',
            'shootingAccuracy',
            'shootingRange',
            'offensiveRead',
            'checking',
            'hitting',
            'positioning',
            'stickChecking',
            'shotBlocking',
            'faceoffs.x',
            'defensiveRead',
            'acceleration',
            'agility',
            'balance',
            'speed',
            'stamina',
            'strength',
            'fighting',
            'aggression',
            'bravery'
           )


#scrape player stats
player_list <- list()
for (i in seasons) {
  player_stats <- GET("http://index.simulationhockey.com/api/v1/players/stats", query = list(season = i))
  player_stats <- fromJSON(rawToChar(player_stats$content))
  player_stats <- do.call(data.frame, player_stats)
  player_list[[i]] <- player_stats
}
combined_player_stats <- do.call(rbind, player_list)

#join
all_index <- left_join(ratings, combined_player_stats, by = c("id", "season"))

#long format
stat_average <- all_index %>%
  filter(position.x %in% c('LW', 'C', 'RW')) %>%
  filter(!is.na(points)) %>%
  gather(key = "stat", value = "rating", screening:professionalism) %>%
  group_by(stat, rating) %>%
  summarise(points = mean(points)) %>%
  spread(key = "rating", value = "points")

stat_average <- stat_average[match(order, stat_average$stat), ]