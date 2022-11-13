library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)
library(shinythemes)

source("build_tools.R")


ui <- fluidPage(
  navbarPage(
    "FHM Analyzer",
    collapsible = TRUE,
    theme = shinytheme("darkly"),
    tabPanel(
      "FHM - Build Analysis",
      sidebarLayout(
        sidebarPanel(
          lass = "filters",
          selectInput("database", "Select DB:", c("NHL", "SHL", "SMJHL")),
          selectInput("position", "Positions:", c('All', 'Forward', 'Dman')),
          selectInput(
            "stat",
            "Stat:",
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
          ),
          selectInput(
            "rating",
            "Ratings:",
            c(
              'Aggression',
              'Bravery',
              'Height',
              'Weight',
              'Acceleration',
              'Agility',
              'Balance',
              'Speed',
              'Stamina',
              'Strength',
              'Fighting',
              'Screening',
              'Getting Open',
              'Passing',
              'Puck Handling',
              'Shooting Accuracy',
              'Shooting Range',
              'Offensive Read',
              'Checking',
              'Faceoffs',
              'Hitting',
              'Positioning',
              'Shot Blocking',
              'Stickchecking',
              'Defensive Read'
            )
          ),
          sliderInput(
            "bins",
            "Number of bins:",
            min = 1,
            max = 50,
            value = 30
          ),
          textInput(
            "min_stat",
            label = "Minimum Stat",
            value = ,
            width = "100%",
            placeholder = 0
          ),
          
          width =3,
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot"),
          plotOutput("plot1"),
          tags$br(),
          dataTableOutput("table")
        )
      )
    )
  )
  ,)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    if (input$database == "NHL"){
      table_data <- createNHLData()
    }
    else if (input$database == "SHL"){
      table_data <- createSHLData()
    }
    else if (input$database == "SMJHL"){
      table_data <- createSMJHLData()
    }
    
    if (input$position != 'All') {
      if (input$position == "Forward") {
        table_data <- table_data %>%
          filter((LW == 20) | (C == 20) | (RW == 20))
      }
      else if (input$position == "Dman") {
        table_data <- table_data %>%
          filter((LD == 20) | (RD == 20))
      }
    }
    if (input$min_stat > 0) {
      table_data <- calculate_min(input$stat, as.double(input$min_stat))
    }
    data <-
      eval(parse(text = paste(
        "table_data$", "`", input$stat, "`", sep = ""
      )))
    bins <- seq(min(data), max(data), length.out = input$bins + 1)
    h = hist(data,
             breaks = bins,
             col = 'darkgray',
             border = 'white')
    h$density = h$counts / sum(h$counts) * 100
    plot(h, freq = FALSE,
         xlab=input$stat,
         ylab="Count")
  })
  
  output$plot1 <- renderPlot({
    if (input$database == "NHL"){
      table_data <- createNHLData()
    }
    else if (input$database == "SHL"){
      table_data <- createSHLData()
    }
    else if (input$database == "SMJHL"){
      table_data <- createSMJHLData()
    }
    
    if (input$position != 'All') {
      if (input$position == "Forward") {
        table_data <- table_data %>%
          filter((LW == 20) | (C == 20) | (RW == 20))
      }
      else if (input$position == "Dman") {
        table_data <- table_data %>%
          filter((LD == 20) | (RD == 20))
      }
    }
    if (input$min_stat > 0) {
      table_data <- calculate_min(input$stat, as.double(input$min_stat))
    }
    a <-
      eval(parse(text = paste(
        "table_data$", "`", input$rating, "`", sep = ""
      )))
    data <-
      eval(parse(text = paste(
        "table_data$", "`", input$stat, "`", sep = ""
      )))
    df <- data.frame(x = a,
                     y = data)
    # equation of the line :
    ggplot(df, aes(x = x, y = y)) +
      geom_smooth(
        method = lm,
        se = FALSE,
        col = 'red',
        linetype = 'dashed'
      ) +
      geom_jitter(shape=16, position=position_jitter(0.3)) +
      geom_boxplot(aes(group = x), alpha = 0.65) +
      ggtitle((paste0("slope==", coef(lm(
        df$y ~ df$x
      ))[2])))
  })
  
  output$table <- renderDataTable({
    if (input$database == "NHL"){
      table_data <- createNHLData()
    }
    else if (input$database == "SHL"){
      table_data <- createSHLData()
    }
    else if (input$database == "SMJHL"){
      table_data <- createSMJHLData()
    }
    validate (need(nrow(table_data) > 0, ""))
    table_data[, c(2:3, 6:14, 17:22)]
  })
  
  output$ratingstable <- renderDataTable({
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
