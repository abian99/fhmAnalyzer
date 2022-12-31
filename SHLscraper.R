library(jsonlite)
library(httr)
library(tidyverse)
library(xlsx)
library(openxlsx)
#0 = shl, 1 = smjhl
league <- 0
updateScale <- c(1, 2, 3, 4, 6, 8, 13, 18, 30, 42, 67, 97, 137, 187, 242)

#i believe s60 is when the build scale changed
seasons <- c(66:68)
updateValues <- c(1, 1, 2, 3, 4, 6, 8, 13, 18, 30, 42, 67, 97, 137, 187, 242)

buildString = 'Offensive Ratings 
Screening: 13
Getting Open: 17
Passing: 17
Puckhandling: 17
Shooting Accuracy: 16
Shooting Range: 11
Offensive Read: 18 

Defensive Ratings 
Checking: 17
Hitting: 16
Positioning: 15
Stickchecking: 15
Shot Blocking: 11
Faceoffs: 5
Defensive Read: 18

Physical Ratings 
Acceleration: 15
Agility: 11
Balance: 17
Speed: 15
Stamina: 17
Strength: 17
Fighting: 5

Mental Ratings 
Aggression: 5
Bravery: 10
*Determination: 15 
*Team Player: 15 
*Leadership: 15 
*Temperament: 15 
*Professionalism: 15 '

#scrape ratings
ratings_list <- list()

for (i in seasons) {
  ratings <-
    GET(
      "https://index.simulationhockey.com/api/v1/players/ratings",
      query = list(league = league, season = i)
    )
  ratings <- fromJSON(rawToChar(ratings$content))
  ratings_list[[i]] <- ratings
}

ratings <- do.call(bind_rows, ratings_list)
order <- c(
  'screening',
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
  player_stats <-
    GET("http://index.simulationhockey.com/api/v1/players/stats",
        query = list(season = i))
  player_stats <- fromJSON(rawToChar(player_stats$content))
  player_stats <- do.call(data.frame, player_stats)
  player_list[[i]] <- player_stats
}
combined_player_stats <- do.call(rbind, player_list)

#join
all_index <-
  left_join(ratings, combined_player_stats, by = c("id", "season"))

#long format
point_average <- all_index %>%
  filter(position.x %in% c('LW', 'C', 'RW')) %>%
  filter(!is.na(advancedStats.CFPctRel)) %>%
  gather(key = "stat", value = "rating", screening:professionalism) %>%
  group_by(stat, rating) %>%
  summarise(CFPct = mean(advancedStats.CFPctRel)) %>%
  spread(key = "rating", value = "CFPct")

point_average <- point_average[-c(2)]

point_average <- point_average[match(order, point_average$stat), ]
point_divide <- point_average[-c(1)]
point_divide <- mapply('/', point_divide, updateValues)

testStrip = strsplit(buildString, "\n")
for (s in testStrip){
  print(s)
  print('hi')
  s = gsub(" ", "", s)
}
  # summarise(points = mean(points)) %>%
  # spread(key = "rating", value = "points")
point_average <- point_average[-c(2)]
point_average <- point_average[match(order, point_average$stat), ]

cf_average_forwards <- all_index %>%
  filter(position.x %in% c('LW', 'C', 'RW')) %>%
  filter(!is.na(advancedStats.CFPctRel)) %>%
  gather(key = "stat", value = "rating", screening:professionalism) %>%
  group_by(stat, rating) %>%
  summarise(`CF%` = mean(advancedStats.CFPctRel)) %>%
  spread(key = "rating", value = "CF%")
cf_average_forwards <- cf_average_forwards[-c(2)]
cf_average_forwards <- cf_average_forwards[match(order, cf_average_forwards$stat), ]

cf_average_dmen <- all_index %>%
  filter(position.x %in% c('LD', 'RD')) %>%
  filter(!is.na(advancedStats.CFPctRel)) %>%
  gather(key = "stat", value = "rating", screening:professionalism) %>%
  group_by(stat, rating) %>%
  summarise(`CF%` = mean(advancedStats.CFPctRel)) %>%
  spread(key = "rating", value = "CF%")
cf_average_dmen <- cf_average_dmen[-c(2)]
cf_average_dmen <- cf_average_dmen[match(order, cf_average_dmen$stat), ]

mat <- as.matrix(cf_average_forwards[-c(1)])
dataset_names <- list('Dmen_CF%Rel' = cf_average_forwards, 'Fwd_CF%Rel' = cf_average_dmen)
openxlsx::write.xlsx(dataset_names, file = 'mydata.xlsx') 
#sweep(mat, 2, updateScale, `/`)