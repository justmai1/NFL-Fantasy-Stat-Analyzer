library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(textshaping)
library(magick)
library(rsconnect)

# UI
ui <- navbarPage(
  
  theme = shinytheme("cerulean"),
  
  title = "The NFL Stat",
  
  tabPanel(
    title = "Welcome",
    headerPanel("Welcome"),
    sidebarLayout(
      sidebarPanel(strong("The NFL Stat:"),
      "Get ahead of your fantasy football mates! This project demonstrates recent NFL player trends
              (2012 - 2022) from fantasy points to yards per game, interceptions, etc. The",
      strong("Fantasy Football"), "tab is used to compare total PPR fantasy football points by player. The",
      strong("Position Tabs"), "are used to measure different stat. types for each player per season, it can 
      also be used as a comparison tool. Finally, the", strong("Season Analysis"), "tab compares PPR fantasy football points
      per player by season (information given for each week played)."),
      mainPanel(tags$img(src="NFL.png", height = "525px", width = "525px"))
    )),

# Fantasy UI
  tabPanel(
    title = "Fantasy Football",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          inputId = 'name1', 
          label = 'Player 1 Name', 
          choices = fantasy$name),
        selectizeInput(
          inputId = 'name2',
          label = 'Player 2 Name',
          choices = fantasy$name
        )),
      
      mainPanel(plotlyOutput('trend'))
    )
  ),
# Quartback UI
  tabPanel(
    title = "Quartbacks",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          inputId = 'qb1',
          label = 'Quarterback 1 Name:',
          choices = longqb$name
        ),
        selectizeInput(
          inputId = 'qb2',
          label = 'Quarterback 2 Name:',
          choices = longqb$name
        ),
        selectizeInput(
          inputId = 'stat_type',
          label = 'Stat. Type',
          choices = longqb$stat_type
        )
      ),
      mainPanel(plotlyOutput('qb_trend'))
    )
  ),
# RB UI
tabPanel(
  title = "Running Backs",
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = 'rb1',
        label = 'Running Back 1 Name:',
        choices = longrb$name
      ),
      selectizeInput(
        inputId = 'rb2',
        label = 'Running Back 2 Name:',
        choices = longrb$name
      ),
      selectizeInput(
        inputId = 'rb_stat_type',
        label = 'Stat. Type',
        choices = longrb$stat_type
      )
    ),
    mainPanel(plotlyOutput('rb_trend'))
  )
),

# WR UI
tabPanel(
  title = "Wide Receivers",
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = 'wr1',
        label = 'Wide Receiver 1 Name:',
        choices = longwr$name
      ),
      selectizeInput(
        inputId = 'wr2',
        label = 'Wide Receiver 2 Name:',
        choices = longwr$name
      ),
      selectizeInput(
        inputId = 'wr_stat_type',
        label = 'Stat. Type',
        choices = longwr$stat_type
      )
    ),
    mainPanel(plotlyOutput('wr_trend'))
  )
),

#TE UI
tabPanel(
  title = "Tight Ends",
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = 'te1',
        label = 'Tight End 1 Name:',
        choices = longte$name
      ),
      selectizeInput(
        inputId = 'te2',
        label = 'Tight End 2 Name:',
        choices = longte$name
      ),
      selectizeInput(
        inputId = 'te_stat_type',
        label = 'Stat. Type',
        choices = longte$stat_type
      )
    ),
    mainPanel(plotlyOutput('te_trend'))
  )
),

# Weekly Data
tabPanel(
  title = "Season Analysis",
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = 'player',
        label = 'Player Name',
        choices = unique(weekly$name)
      ),
      sliderInput(
        inputId = 'season',
        label = 'Season:',
        min = min(weekly$season),
        max = max(weekly$season),
        step = 1,
        value = 1
      )
    ),
    mainPanel(plotlyOutput('season_trend'))
  )
)
)

# Server
server <- function(input, output, session) {

# Fantasy Data
  output$trend <- renderPlotly({
    
    players <- c(input$name1,input$name2)
    
    data <- fantasy[fantasy$name %in% players,]
    
    if (nrow(data) > 0) {
      ggplotly(ggplot(data, aes(season, fantasy_points_ppr, col=name, shape=team)) +
                 geom_point() +
                 geom_line() +
                 labs(x = "Season (Year)",
                      y = "Fantasy Points (PPR)",
                      title = "Fantasy Player Comparison (PPR)",
                 ))}
    
    else {
      plot_ly()}
    
  })
# Quarterback data
  output$qb_trend <- renderPlotly({
    
    qbs <- c(input$qb1,input$qb2)
    
    qb_data <- subset(longqb, name %in% qbs & stat_type == input$stat_type)
    
    if(nrow(qb_data) > 0){
      ggplotly(ggplot(qb_data, aes(season, stats, shape = team, col = name)) +
                 geom_point() +
                 geom_line() +
                 labs(x = "Season (Year)",
                      y =paste(input$stat_type),
                      title = paste("Quarterback Stats:",input$name))
      )
    } else {
      plot_ly()
    }
  })
  
# Running back data
  output$rb_trend <- renderPlotly({
    
    rbs <- c(input$rb1,input$rb2)
    
    rb_data <- subset(longrb, name %in% rbs & stat_type == input$rb_stat_type)
    
    if(nrow(rb_data) > 0){
      ggplotly(ggplot(rb_data, aes(season, stats, shape = team, col = name)) +
                 geom_point() +
                 geom_line() +
                 labs(x = "Season (Year)",
                      y =paste(input$rb_stat_type),
                      title = paste("Running Back Stat. Comparison"))
      )
    } else {
      plot_ly()
    }
  })
  
# Wide Receiver Data
  output$wr_trend <- renderPlotly({
    
    wrs <- c(input$wr1,input$wr2)
    
    wr_data <- subset(longwr, name %in% wrs & stat_type == input$wr_stat_type)
    
    if(nrow(wr_data) > 0){
      ggplotly(ggplot(wr_data, aes(season, stats, shape = team, col = name)) +
                 geom_point() +
                 geom_line() +
                 labs(x = "Season (Year)",
                      y =paste(input$wr_stat_type),
                      title = paste("Wide Receiver Stat. Comparison"))
      )
    } else {
      plot_ly()
    }
  })
  
# TE Data
  output$te_trend <- renderPlotly({
    
    tes <- c(input$te1,input$te2)
    
    te_data <- subset(longte, name %in% tes & stat_type == input$te_stat_type)
    
    if(nrow(te_data) > 0){
      ggplotly(ggplot(te_data, aes(season, stats, shape = team, col = name)) +
                 geom_point() +
                 geom_line() +
                 labs(x = "Season (Year)",
                      y =paste(input$te_stat_type),
                      title = paste("Tight End Stat. Comparison"))
      )
    } else {
      plot_ly()
    }
  })
  
# Weekly Data
  output$season_trend <- renderPlotly({
    
    playerData <- subset(weekly, name == input$player & season == input$season)
    
    if(nrow(playerData) > 0){
      ggplotly(ggplot(playerData,aes(week,fantasy_points_ppr,col = name)) +
                 geom_point() +
                 geom_line() + 
                 labs(x = "Weeks",
                      y = "PPR Fantasy Points",
                      title = paste("PPR Fantasy Points,", input$season, "Season:", input$player)))
    } else{
      cat("No Data for ", input$player, "in", input$season)
    }
  })
}

shinyApp(ui = ui, server = server)
