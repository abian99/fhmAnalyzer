stats <-
  c(
    'Goals',
    'A',
    'Points',
    'PIM',
    'HIT',
    'SB',
    'SOG',
    'GF/60',
    'GA/60',
    'SF/60',
    'SA/60',
    'CF% rel',
    'FF% rel',
    'PPG'
  )

time_converter <- function(Time, GP) {
  time_pg <- Time / GP
  mins <- floor(time_pg / 60)
  secs <- floor(60 * (((time_pg / 60) - floor(time_pg / 60))))
  time <- sprintf("%02d:%02d", mins, secs)
  return(time)
}

calculate_min <- function(value, min_value) {
  if (value %in% stats) {
    table_data <- filter(table_data, value >= min_value)
    return(table_data)
  }
  
}

createNHLData <- function(){
  if (exists("table_data") && is.data.frame(get("table_data"))) {
    return(table_data)
  }
  else{
    player_info <- fread('data/test_1/player_master.csv')
    player_info$Name <-
      paste(player_info$'First Name', player_info$'Last Name')
    player_info <- player_info[,-c(3:6, 9:16)]
    
    team_info <- fread('data/test_1/team_data.csv')
    team_info <- subset(team_info, LeagueId == 0)
    team_info <- team_info[,-c(2:4, 6:18)]
    
    player_ratings <- fread('data/test_1/player_ratings.csv')
    
    player_db <- merge(player_info, team_info, by = c("TeamId"))
    player_db$PlayerId <- as.integer(player_db$PlayerId)
    player_stats <- fread('data/test_1/player_skater_stats_rs.csv')
    player_stats$PlayerId <- as.integer(player_stats$PlayerId)
    player_stats <-
      merge(player_db, player_stats, by = c("PlayerId", "TeamId"))
    
    for (x in 2:5) {
      filename <-
        paste('data/test_',
              x,
              '/import_export/csv/player_skater_stats_rs.csv',
              sep = "")
      temp_stats <- fread(filename)
      temp_stats$PlayerId <- as.integer(temp_stats$PlayerId)
      temp_stats <-
        merge(player_db, temp_stats, by = c("PlayerId", "TeamId"))
      player_stats <- rbind(player_stats, temp_stats)
    }
    player_stats <- player_stats[,-c(2, 7)]
    table_data <- player_stats
    table_data <- mutate(
      table_data,
      Goals = G,
      Points = G + A,
      PPG = round(mean(Points) / mean(GP), digits = 2),
      TOI = time_converter(TOI, GP),
      PPTOI = time_converter(PPTOI, GP),
      SHTOI = time_converter(SHTOI, GP)
    )

    table_data <- table_data %>%
      summarize(
        PlayerId,
        Name,
        Abbr,
        Height,
        Weight,
        GP,
        Goals,
        A,
        Points,
        PIM,
        HIT,
        SB,
        SOG,
        TOI,
        PPTOI,
        SHTOI,
        `GF/60`,
        `GA/60`,
        `SF/60`,
        `SA/60`,
        `CF% rel`,
        `FF% rel`,
        PPG
      )
    table_data <- unique(table_data)
    table_data <- merge(table_data, player_ratings, by = c("PlayerId"))
    
    return(table_data)
  }
}

createSHLData <- function(){
  if (exists("shl_data") && is.data.frame(get("shl_data"))) {
    return(shl_data)
  }
  else{
    player_master_combined <- rbind(fread('data/SHLData/player_master_66.csv'), fread('data/SHLData/player_master_67.csv'), fill = TRUE) %>%
      unique()
    team_data_combined <- rbind(fread('data/SHLData/team_data_66.csv'),fread('data/SHLData/team_data_67.csv'), allow.cartesian=TRUE, fill = TRUE) %>%
      unique()
    player_ratings_combined <- rbind(fread('data/SHLData/player_ratings_66.csv'),fread('data/SHLData/player_ratings_67.csv'), allow.cartesian=TRUE, fill = TRUE) %>%
      unique()
    player_stats_combined <- rbind(fread('data/SHLData/player_skater_stats_rs_66.csv'),fread('data/SHLData/player_skater_stats_rs_67.csv'), allow.cartesian=TRUE, fill = TRUE) %>%
      unique()
    player_info <- player_master_combined
    player_info$Name <- paste(player_info$'First Name', player_info$'Last Name')
    
    team_info <- team_data_combined
    team_info <- subset(team_info, LeagueId == 0)
    team_info <- team_info[,-c(2:4, 6:18)]
    
    player_ratings <- player_ratings_combined
    print(player_info)
    player_db <- merge(player_info, team_info, by = c("TeamId"))
    print(player_db)
    player_db$PlayerId <- as.integer(player_db$PlayerId)
    player_stats <- player_stats_combined
    player_stats$PlayerId <- as.integer(player_stats$PlayerId)
    player_stats <-
      merge(player_db, player_stats, by = c("PlayerId", "TeamId"))
    #player_stats <- player_stats[,-c(2, 7)]
    player_stats <- player_stats %>%
      group_by(PlayerId, Name, Abbr) %>%
      mutate(
        GP = round(mean(GP), digits = 2),
        Goals = round(mean(G), digits = 2),
        A = round(mean(A), digits = 2),
        PIM = round(mean(PIM), digits = 2),
        HIT = round(mean(HIT), digits = 2),
        SB = round(mean(SB), digits = 2),
        SOG = round(mean(SOG), digits = 2),
        TOI = round(mean(TOI), digits = 2),
        PPTOI = round(mean(PPTOI), digits = 2),
        SHTOI = round(mean(SHTOI), digits = 2),
        `GF/60` = round(mean(`GF/60`), digits = 2),
        `GA/60` = round(mean(`GA/60`), digits = 2),
        `SF/60` = round(mean(`SF/60`), digits = 2),
        `SA/60` = round(mean(`SA/60`), digits = 2),
        `CF% rel` = round(mean(`CF% rel`), digits = 2),
        `FF% rel` = round(mean(`FF% rel`), digits = 2),
      )
    
    shl_data <- mutate(
      player_stats,
      Points = Goals + A,
      PPG = round(mean(Points) / mean(GP), digits = 2),
      TOI = time_converter(TOI, GP),
      PPTOI = time_converter(PPTOI, GP),
      SHTOI = time_converter(SHTOI, GP)
    )
    
    shl_data <- shl_data %>%
      summarize(
        PlayerId,
        Name,
        Abbr,
        Height,
        Weight,
        GP,
        Goals,
        A,
        Points,
        PIM,
        HIT,
        SB,
        SOG,
        TOI,
        PPTOI,
        SHTOI,
        `GF/60`,
        `GA/60`,
        `SF/60`,
        `SA/60`,
        `CF% rel`,
        `FF% rel`,
        PPG
      )
    
    shl_data$Points <- as.numeric(shl_data$Points)
    shl_data$Goals <- as.numeric(shl_data$Goals)
    shl_data <- unique(shl_data)
    shl_data <- merge(shl_data, player_ratings, by = c("PlayerId"))
    
    return(shl_data)
  }
}

# createFHM6SHLData <- function(){
#   if (exists("shl_data") && is.data.frame(get("shl_data"))) {
#     return(shl_data)
#   }
#   else{
#     player_info <- fread('data/SHLData/player_master.csv')
#     player_info$Name <-
#       paste(player_info$'First Name', player_info$'Last Name')
#     #player_info <- player_info[,-c(3:6, 9:16)]
#     
#     team_info <- fread('data/SHLData/team_data.csv')
#     team_info <- subset(team_info, LeagueId == 0)
#     team_info <- team_info[,-c(2:4, 6:18)]
#     
#     player_ratings <- fread('data/SHLData/player_ratings.csv')
#     print(player_info)
#     player_db <- merge(player_info, team_info, by = c("TeamId"))
#     print(player_db)
#     player_db$PlayerId <- as.integer(player_db$PlayerId)
#     player_stats <- fread('data/SHLData/player_skater_stats_rs.csv')
#     player_stats$PlayerId <- as.integer(player_stats$PlayerId)
#     player_stats <-
#       merge(player_db, player_stats, by = c("PlayerId", "TeamId"))
#     #player_stats <- player_stats[,-c(2, 7)]
#     player_stats <- player_stats %>%
#       group_by(PlayerId, Name, Abbr) %>%
#       mutate(
#         GP = round(mean(GP), digits = 2),
#         Goals = round(mean(G), digits = 2),
#         A = round(mean(A), digits = 2),
#         PIM = round(mean(PIM), digits = 2),
#         HIT = round(mean(HIT), digits = 2),
#         SB = round(mean(SB), digits = 2),
#         SOG = round(mean(SOG), digits = 2),
#         TOI = round(mean(TOI), digits = 2),
#         PPTOI = round(mean(PPTOI), digits = 2),
#         SHTOI = round(mean(SHTOI), digits = 2),
#         `GF/60` = round(mean(`GF/60`), digits = 2),
#         `GA/60` = round(mean(`GA/60`), digits = 2),
#         `SF/60` = round(mean(`SF/60`), digits = 2),
#         `SA/60` = round(mean(`SA/60`), digits = 2),
#         `CF% rel` = round(mean(`CF% rel`), digits = 2),
#         `FF% rel` = round(mean(`FF% rel`), digits = 2),
#       )
#     
#     shl_data <- mutate(
#       player_stats,
#       Points = Goals + A,
#       PPG = round(mean(Points) / mean(GP), digits = 2),
#       TOI = time_converter(TOI, GP),
#       PPTOI = time_converter(PPTOI, GP),
#       SHTOI = time_converter(SHTOI, GP)
#     )
#     
#     shl_data <- shl_data %>%
#       summarize(
#         PlayerId,
#         Name,
#         Abbr,
#         Height,
#         Weight,
#         GP,
#         Goals,
#         A,
#         Points,
#         PIM,
#         HIT,
#         SB,
#         SOG,
#         TOI,
#         PPTOI,
#         SHTOI,
#         `GF/60`,
#         `GA/60`,
#         `SF/60`,
#         `SA/60`,
#         `CF% rel`,
#         `FF% rel`,
#         PPG
#       )
#     
#     shl_data$Points <- as.numeric(shl_data$Points)
#     shl_data$Goals <- as.numeric(shl_data$Goals)
#     shl_data <- unique(shl_data)
#     shl_data <- merge(shl_data, player_ratings, by = c("PlayerId"))
#     
#     return(shl_data)
#   }
# }

createSMJHLData <- function(){
  if (exists("smjhl_data") && is.data.frame(get("smjhl_data"))) {
    return(smjhl_data)
  }
  else{
    player_info <- fread('data/SMJHLData/player_master.csv')
    player_info$Name <-
      paste(player_info$'First Name', player_info$'Last Name')
    
    team_info <- fread('data/SMJHLData/team_data.csv')
    team_info <- subset(team_info, LeagueId == 0)
    team_info <- team_info[,-c(2:4, 6:18)]
    
    player_ratings <- fread('data/SMJHLData/player_ratings.csv')
    
    player_db <- merge(player_info, team_info, by = c("TeamId"))
    player_db$PlayerId <- as.integer(player_db$PlayerId)
    player_stats <- fread('data/SMJHLData/player_skater_stats_rs.csv')
    player_stats$PlayerId <- as.integer(player_stats$PlayerId)
    player_stats <-
      merge(player_db, player_stats, by = c("PlayerId", "TeamId"))
    # player_stats <- player_stats %>%
    #   group_by(PlayerId, Name, Abbr) %>%
    #   mutate(
    #     GP = round(mean(GP), digits = 2),
    #     Goals = round(mean(G), digits = 2),
    #     A = round(mean(A), digits = 2),
    #     PIM = round(mean(PIM), digits = 2),
    #     HIT = round(mean(HIT), digits = 2),
    #     SB = round(mean(SB), digits = 2),
    #     SOG = round(mean(SOG), digits = 2),
    #     TOI = round(mean(TOI), digits = 2),
    #     PPTOI = round(mean(PPTOI), digits = 2),
    #     SHTOI = round(mean(SHTOI), digits = 2),
    #     `GF/60` = round(mean(`GF/60`), digits = 2),
    #     `GA/60` = round(mean(`GA/60`), digits = 2),
    #     `SF/60` = round(mean(`SF/60`), digits = 2),
    #     `SA/60` = round(mean(`SA/60`), digits = 2),
    #     `CF% rel` = round(mean(`CF% rel`), digits = 2),
    #     `FF% rel` = round(mean(`FF% rel`), digits = 2),
    #   )
    
    smjhl_data <- mutate(
      player_stats,
      Points = Goals + A,
      PPG = round(mean(Points) / mean(GP), digits = 2),
      TOI = time_converter(TOI, GP),
      PPTOI = time_converter(PPTOI, GP),
      SHTOI = time_converter(SHTOI, GP)
    )
    
    smjhl_data <- smjhl_data %>%
      summarize(
        PlayerId,
        Name,
        Abbr,
        Height,
        Weight,
        GP,
        Goals,
        A,
        Points,
        PIM,
        HIT,
        SB,
        SOG,
        TOI,
        PPTOI,
        SHTOI,
        `GF/60`,
        `GA/60`,
        `SF/60`,
        `SA/60`,
        `CF% rel`,
        `FF% rel`,
        PPG
      )
    
    smjhl_data$Points <- as.numeric(smjhl_data$Points)
    smjhl_data$Goals <- as.numeric(smjhl_data$Goals)
    smjhl_data <- unique(smjhl_data)
    smjhl_data <- merge(smjhl_data, player_ratings, by = c("PlayerId"))
    
    return(smjhl_data)
  }
  
}