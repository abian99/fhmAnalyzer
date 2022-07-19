#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)

time_converter <- function(Time, GP) {
  time_pg <- Time/GP
  mins <- floor(time_pg/60)
  secs <- floor(60*(((time_pg/60)-floor(time_pg/60))))
  time <- sprintf("%02d:%02d", mins, secs)
  return(time)
}

calculate_min <- function(value, min_value) {
  # generate bins based on input$bins from ui.R
  if (value == 'Goals'){
    table_data <- filter(table_data, Goals >= min_value)
    return(table_data)
  }
  else if (value == 'A'){
    table_data <- filter(table_data, A >= min_value)
    return(table_data)
  }
  else if (value == 'Points'){
    table_data <- filter(table_data, Points >= min_value)
    return(table_data)
  }
  else if (value == 'PIM'){
    table_data <- filter(table_data, PIM >= min_value)
    return(table_data)
  }
  else if (value == 'HIT'){
    table_data <- filter(table_data, HIT >= min_value)
    return(table_data)
  }
  else if (value == 'SB'){
    table_data <- filter(table_data, SB >= min_value)
    return(table_data)
  }
  else if (value == 'SOG'){
    table_data <- filter(table_data, SOG >= min_value)
    return(table_data)
  }
  else if (value == 'GF/60'){
    table_data <- filter(table_data, `GF/60` >= min_value)
    return(table_data)
  }
  else if (value == 'GA/60'){
    table_data <- filter(table_data, `GA/60` >= min_value)
    return(table_data)
  }
  else if (value == 'SF/60'){
    table_data <- filter(table_data, `SF/60` >= min_value)
    return(table_data)
  }
  else if (value == 'SA/60'){
    table_data <- filter(table_data, `SA/60` >= min_value)
    return(table_data)
  }
  else if (value == 'CF% rel'){
    table_data <- filter(table_data, `CF% rel` >= min_value)
    return(table_data)
  }
  else if (value == 'FF% rel'){
    table_data <- filter(table_data, `FF% rel` >= min_value)
    return(table_data)
  }
  else if (value == 'PPG'){
    table_data <- filter(table_data, PPG >= min_value)
    return(table_data)
  }
}

return_rating <- function(){
  
}

player_info <- fread('data/test_1/player_master.csv')
player_info$Name <- paste(player_info$'First Name', player_info$'Last Name')
player_info <- player_info[,-c(3:6,9:16)]

team_info <- fread('data/test_1/team_data.csv')
team_info <- subset(team_info, LeagueId == 0)
team_info <- team_info[,-c(2:4,6:18)]

player_ratings <- fread('data/test_1/player_ratings.csv')

player_db <- merge(player_info,team_info,by=c("TeamId"))
player_db$PlayerId <- as.integer(player_db$PlayerId )
player_stats <- fread('data/test_1/player_skater_stats_rs.csv')
player_stats$PlayerId <- as.integer(player_stats$PlayerId )
player_stats <- merge(player_db,player_stats, by=c("PlayerId", "TeamId"))

for (x in 2:5) {
  filename <- paste('data/test_', x, '/import_export/csv/player_skater_stats_rs.csv', sep = "")
  temp_stats <- fread(filename)
  temp_stats$PlayerId <- as.integer(temp_stats$PlayerId )
  temp_stats <- merge(player_db,temp_stats, by=c("PlayerId", "TeamId"))
  player_stats <- rbind(player_stats, temp_stats)
}
player_stats <- player_stats[,-c(2,7)]
player_stats <- player_stats %>% 
  group_by(PlayerId, Name, Abbr) %>%
  mutate(
    GP = round(mean(GP), digits=2),
    Goals = round(mean(G), digits=2),
    A = round(mean(A), digits=2),
    PIM = round(mean(PIM), digits=2),
    HIT = round(mean(HIT), digits=2),
    SB = round(mean(SB), digits=2),
    SOG = round(mean(SOG), digits=2),
    TOI = round(mean(TOI), digits=2),
    PPTOI = round(mean(PPTOI), digits=2),
    SHTOI = round(mean(SHTOI), digits=2),
    `GF/60` = round(mean(`GF/60`), digits=2),
    `GA/60` = round(mean(`GA/60`), digits=2),
    `SF/60` = round(mean(`SF/60`), digits=2),
    `SA/60` = round(mean(`SA/60`), digits=2),
    `CF% rel` = round(mean(`CF% rel`), digits=2),
    `FF% rel` = round(mean(`FF% rel`), digits=2),
  )

table_data <- mutate(
  player_stats, Points = Goals + A,
  PPG = round(mean(Points)/mean(GP), digits = 2),
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

table_data$Points <- as.numeric(table_data$Points)
table_data$Goals <- as.numeric(table_data$Goals)
table_data <- unique(table_data)
table_data <- merge(table_data,player_ratings, by=c("PlayerId"))
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    #utitlePanel("Breaking (Down) The Sim: An analysis of the various ratings and how they affect stats"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          lass = "filters",
          selectInput(
            "position", "Positions:", c('All','Forward', 'Dman')
          ),
          selectInput(
            "stat", "Stat:", c('Goals', 'A', 'Points', 'PIM', 'HIT', 'SB', 'SOG', 'GF/60', 'GA/60', 'SF/60', 'SA/60', 'CF% rel', 'FF% rel', 'PPG')
          ),
          selectInput(
            "rating", "Ratings:", c('Aggression', 'Bravery', 'Height', 'Weight', 'Acceleration', 'Agility', 'Balance', 'Speed', 'Stamina', 'Strength', 'Fighting', 'Screening', 'Getting Open', 'Passing', 'Puck Handling', 'Shooting Accuracy', 'Shooting Range', 'Offensive Read', 'Checking', 'Faceoffs', 'Hitting', 'Positioning', 'Shot Blocking', 'Stickchecking', 'Defensive Read')
          ),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
          textInput("min_stat", 
                    label = "Minimum Stat", 
                    value =, 
                    width = "100%",
                    placeholder = 0)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("plot1")
        )
    ),
    
    fluidRow (class = "table", dataTableOutput("table"))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$distPlot <- renderPlot({
      if (input$position != 'All'){
        if (input$position == "Forward"){
          table_data <- table_data %>%
            filter((LW == 20)|(C == 20)|(RW == 20))
        }
        else if (input$position == "Dman"){
          table_data <- table_data %>%
            filter((LD == 20)|(RD == 20))
        }
      }
      if (input$min_stat > 0){
        table_data <- calculate_min(input$stat, as.double(input$min_stat)) 
      }
      data <- eval(parse(text=paste("table_data$", "`", input$stat, "`", sep = "")))
      bins <- seq(min(data), max(data), length.out = input$bins + 1)
      h = hist(data, breaks = bins, col = 'darkgray', border = 'white')
      h$density = h$counts/sum(h$counts)*100
      plot(h,freq=FALSE)
    })
    
    output$plot1 <- renderPlot({
      if (input$position != 'All'){
        if (input$position == "Forward"){
          table_data <- table_data %>%
            filter((LW == 20)|(C == 20)|(RW == 20))
        }
        else if (input$position == "Dman"){
          table_data <- table_data %>%
            filter((LD == 20)|(RD == 20))
        }
      }
      if (input$min_stat > 0){
        table_data <- calculate_min(input$stat, as.double(input$min_stat)) 
      }
      a <- eval(parse(text=paste("table_data$", "`", input$rating, "`", sep = "")))
      data <- eval(parse(text=paste("table_data$", "`", input$stat, "`", sep = "")))
      df <- data.frame(x=a,
                       y = data)
      # equation of the line : 
      ggplot(df, aes(x=x, y=y)) +
        geom_point(col='black', size=2) +
        geom_smooth(method=lm, se=FALSE, col='red', linetype='dashed') +
        annotate("text",x=4,y=0,label=(paste0("slope==",coef(lm(df$y~df$x))[2])),parse=TRUE, size=5)
    })
    
    output$table <- renderDataTable({
      validate (need(nrow(table_data) > 0, ""))
      table_data[, c(2:3,6:14,17:22)]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
