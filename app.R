library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(GeomMLBStadiums)
library(RColorBrewer)
library(ggpubr)
library(caret)
library(randomForest)
library(broom)
library(shiny)

#install.packages("shiny")
library(shiny)

library(tidyverse)
#install.packages("dplyr")
library(dplyr)

#install.packages("DT")
library(DT)

#install.packages("ggplot2")
library(ggplot2)
library(baseballr)
#install.packages('tidyverse')
library(tidyverse)


## Colors for the heatmaps
heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::RdBu", n = 9, direction = -1))(16)

## Read PreMade CSVs ## 
TestTrackMan<-list.files(path = "C:/RStudioProjects/AthleteLab/MLB_Pitcher_ADV_Scouting", pattern = "*.csv") %>% 
  map_df(~read_csv(.))


### To Try the app you can use this instead ###

#x <- map_df(.x = seq.Date(as.Date('2024-09-05'), 
#                          as.Date('2024-09-06'), 
#                          'day'), 
#            ~get_game_pks_mlb(date = .x, 
#                              level_ids = c(1))
#)

#safe_mlb <- safely(get_pbp_mlb)

#TestTrackMan <- map(.x = x %>%
#              filter(status.codedGameState == "F") %>% 
#              pull(game_pk), 
#            ~safe_mlb(game_pk = .x)) %>%
#  map('result') %>%
#  bind_rows()


#New Variables to make things easier later
TestTrackMan$Pitcher<- TestTrackMan$matchup.pitcher.fullName 
TestTrackMan$Date<- TestTrackMan$game_date
TestTrackMan<- subset(TestTrackMan, !is.na(details.type.code))
TestTrackMan$PlayResult<- ifelse(TestTrackMan$details.isInPlay == TRUE & TestTrackMan$result.event == "Single" , "Single",
                         ifelse(TestTrackMan$details.isInPlay == TRUE & TestTrackMan$result.event == "Double" , "Double",
                         ifelse(TestTrackMan$details.isInPlay == TRUE & TestTrackMan$result.event == "Triple" , "Triple",
                         ifelse(TestTrackMan$details.isInPlay == TRUE & TestTrackMan$result.event == "Home Run" , "Home Run",
                         ifelse(TestTrackMan$details.isInPlay == TRUE & TestTrackMan$result.event %in% c("Groundout", "Double Play","Lineout","Pop Out","Flyout","Grounded Into DP","Field Error","Forceout","Bunt Pop Out","Fielders Choice Out") , "Out",
                         ifelse(TestTrackMan$details.isInPlay == TRUE & TestTrackMan$result.event == "Sac Fly" , "Sacrifice",
                         ifelse(TestTrackMan$details.call.description %in% c("Swinging Strike" , "Called Strike", "Swinging Strike (Blocked)","Foul Tip") & TestTrackMan$details.isOut == TRUE,"Strikeout",
                         ifelse(TestTrackMan$result.event == "Walk" & TestTrackMan$count.balls.start == 4, "Walk" ,NA))))))))

TestTrackMan$Outs<- ifelse(TestTrackMan$details.isOut == TRUE & TestTrackMan$result.event %in% c("Groundout","Lineout","Pop Out","Flyout","Forceout","Bunt Pop Out","Fielders Choice Out","Sac Fly", "Strikeout"), 1 ,
                   ifelse(TestTrackMan$details.isOut == TRUE & TestTrackMan$result.event %in% c("Double Play" , "Grounded Into DP" ), 2 ,
                          ifelse(TestTrackMan$details.isInPlay == TRUE & TestTrackMan$result.event %in% c("Single", "Double", "Triple", "Home Run", "Field Error"),0, NA
                          )))

TestTrackMan$HardHit<- ifelse(TestTrackMan$details.isInPlay == TRUE & TestTrackMan$hitData.launchSpeed > 95 , 1, 0)


TestTrackMan$Strikes<- ifelse(TestTrackMan$details.isStrike == TRUE, TestTrackMan$count.strikes.start - 1,
                              TestTrackMan$count.strikes.start)

TestTrackMan$Balls<- ifelse(TestTrackMan$details.isBall == TRUE, TestTrackMan$count.balls.start -1, 
                            TestTrackMan$count.balls.start)

TestTrackMan$Count<- paste(TestTrackMan$Balls , TestTrackMan$Strikes, sep = "-")

#StrikeZone Measurements

Left <- -8.5/12
Right <- 8.5/12
Bottom <- 18.29/12
Top <- 44.08/12

# This is to Make a 3x3 Strike Zone (Vertical and Horizontal Lines in Zone)
Width <- (Right - Left) / 3
Height <- (Top - Bottom) / 3


ui <- navbarPage(
  
  " Advanced Scouting App ", theme = shinythemes::shinytheme("yeti"),
  
  tabPanel("Hitters",
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput("Team", label = "Choose Team",
                           
                           choices =
                             levels(as.factor(TestTrackMan$batting_team))),
               
               selectInput("Batter", label = "Choose Batter",
                           
                           choices = levels(as.factor(TestTrackMan$matchup.batter.fullName))),
               
               selectInput("PitcherThrows",
                           label = "Choose Pitcher Throws", choices =
                             levels(as.factor(TestTrackMan$matchup.splits.batter))),
               
               dateRangeInput("Date", label = "Choose Date Range",
                              start = min(TestTrackMan$game_date),
                              end = max(TestTrackMan$game_date),
                              min = min(TestTrackMan$game_date),
                              max = max(TestTrackMan$game_date),
                              format = "yyyy-mm-dd",
                              separator = "to"),
               
               checkboxGroupInput("Pitch",
                                  label = "Choose Pitch Type(s)", choices =
                                    levels(as.factor(TestTrackMan$details.type.description)))
               
             ),
             
             mainPanel(
               fluidRow(column(10, offset = 1, h3(strong("Player Stats")),dataTableOutput("Stats"))),
               br(),
               fluidRow(column(10, offset = 1, h3(strong("Swing Per Pitch Type")),dataTableOutput("Swing_Pitch"))),
               br(),

               br(),
               fluidRow(column( plotOutput("Pitch_Freq"), width = 6, height = 24),
                        column( plotOutput("Swing"), width = 6, height = 24)),
               br(),
               br(),
               br(),
               br(),
               fluidRow(column( plotOutput("Whiff"), width = 6, height = 24),
                        column( plotOutput("HardHit"), width = 6, height = 24)),
               br(),br(),         
               fluidRow(column( plotOutput("SprayChart"), width = 12, height = 48)),
               br(),
               fluidRow(column(10, offset = 1, h3(strong("Approach")),dataTableOutput("Approach"))),
               
               
             )
           )
  ),
  
  
  #2nd Tab:
  tabPanel("Pitchers",
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput("TeamP", label = "Choose Pitcher Team",
                           
                           choices =
                             levels(as.factor(TestTrackMan$fielding_team))),
               
               selectInput("Pitcher", label = "Choose Pitcher",
                           
                           choices = levels(as.factor(TestTrackMan$Pitcher))),
               
               selectInput("BatterSide",
                           label = "Choose Batter Handeness", choices =
                             levels(as.factor(TestTrackMan$matchup.splits.pitcher))),
               
               dateRangeInput("DateP", label = "Choose Date Range",
                              start = min(TestTrackMan$game_date),
                              end = max(TestTrackMan$game_date),
                              min = min(TestTrackMan$game_date),
                              max = max(TestTrackMan$game_date),
                              format = "yyyy-mm-dd",
                              separator = "to"),
               
               checkboxGroupInput("Count",
                                  label = "Choose Count(s)", choices =
                                    levels(as.factor(TestTrackMan$Count))),
               checkboxGroupInput("Situation",
                                  label = "Choose Situation(s)", choices =
                                    levels(as.factor(TestTrackMan$matchup.splits.menOnBase)))
               
             ),
             
             
             mainPanel(
               fluidRow(column(10, offset = 1, h3(strong("Pitcher Stats")),dataTableOutput("PStats"))),
               br(),
               fluidRow(column(10, offset = 1, h3(strong("Pitch Stats")),dataTableOutput("StatsPitch"))),
               
               fluidRow(column(10, offset = 1, h3(strong("Pitch Metrics and Usage")),dataTableOutput("Metrics"))),
               br(),
               br(),
               br(),
               br(),
               fluidRow(
                 column(plotOutput("Movement"), width = 6, height = 24),
                 column(plotOutput("Release"), width = 6, height = 24)
               ),
               br(),
               br(),
               br(),
               fluidRow(
                 column( plotOutput("Pitch_Location"), align = "center", width = 12, height = 48),

               ),
               br(),
               br(),
               br(),
               
               fluidRow(column( plotOutput("SprayChartP"), width = 12, height = 48)),
               
               
             )
           )
  ) 
  
  
  
  
  
  
)








server <- function(input, output, session) {
  
  
  #Adding Reactives ####
  
  observeEvent(
    input$Team,
    updateSelectInput(session,
                      "Batter", "Choose Batter",
                      choices = levels(factor(filter(TestTrackMan,
                                                     batting_team ==
                                                       isolate(input$Team))$matchup.batter.fullName))))
  
  
  # Date Range Based on When Pitcher Threw
  
  observeEvent(
    input$Batter,
    updateDateRangeInput(session,
                         "Date", "Choose Date Range",
                         start = min(TestTrackMan$game_date),
                         end = max(TestTrackMan$game_date)))
  
  
  
  
  
  #### Pitcher Reactive ########
  
  observeEvent(
    input$TeamP,
    updateSelectInput(session,
                      "Pitcher", "Choose Pitcher",
                      choices = levels(factor(filter(TestTrackMan,
                                                     fielding_team ==
                                                       isolate(input$TeamP))$Pitcher))))
  
  
  # Date Range Based on When Pitcher Threw
  
  observeEvent(
    input$Pitcher,
    updateDateRangeInput(session,
                         "DateP", "Choose Date Range",
                         start = min(TestTrackMan$Date),
                         end = max(TestTrackMan$Date)))
  
  
  
  
  
  ### Hitters Tab ##
  
  
  output$Stats<- renderDataTable({
   
    table<- TestTrackMan  %>%
        filter(#batting_team ==input$Team ,
               matchup.batter.fullName == input$Batter,
               game_date >= input$Date[1] &
               game_date <= input$Date[2],
               matchup.splits.batter == input$PitcherThrows) %>%
      
        summarise('Pitches' = n(),
                  "PA" = sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout","Out", "Walk" )),
                  "AB" = sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout","Out" )),
                  "AVG" = round(sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run"))/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout","Out" )),3),
                  "SLUG" = round((sum(PlayResult == "Single", na.rm=T) + 2* sum(PlayResult == "Double", na.rm=T)+ 3* sum(PlayResult == "Triple", na.rm=T) + 4* sum(PlayResult == "Home Run", na.rm=T) )/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout", "Out" )),3),
                  "OBP" = round(sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run", "Walk"))/n_distinct(atBatIndex),3),
                  'K %' = round(100*sum(PlayResult =="Strikeout" , na.rm = T)/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout","Out", "Walk" )),1),
                  'BB %' = round(100*sum(PlayResult =="Walk", na.rm = T) /sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout","Out", "Walk" )),1),
                  'AVG Exit Velo' = round(mean(hitData.launchSpeed, na.rm = TRUE),1),
                  "GB%" = round(100* sum(hitData.trajectory == "ground_ball" , na.rm = T) / sum(hitData.trajectory %in% c("line_drive","fly_ball","ground_ball","popup","bunt_popup"), na.rm = T),0),
                  "HardHit%" = round(sum(HardHit, na.rm = T)/sum(details.call.description == "InPlay", na.rm = T),3)*100,
                  'Whiff%' = round(sum(details.call.description %in% c("Swinging Strike","Swinging Strike (Blocked)"))/
                                        sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                            "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)")),3)*100,
                  'O-Swing %' = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                          "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(11,12,13,14), na.rm=T)/sum(pitchData.zone %in% c(11,12,13,14)),2)*100,
                  'Z-Swing %' = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                          "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(1,2,3,4,5,6,7,8,9), na.rm=T)/sum(pitchData.zone %in% c(1:9)),2)*100,
                  

        ) %>% mutate( "OPS" = SLUG + OBP)
    
    
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
  })
  
  
  output$Swing_Pitch<- renderDataTable({
    
    table<- TestTrackMan  %>%
      filter(#batting_team ==input$Team ,
             matchup.batter.fullName == input$Batter,
             game_date >= input$Date[1] &
               game_date <= input$Date[2],
             matchup.splits.batter == input$PitcherThrows) %>% group_by("PitchType" = details.type.description) %>%
      
      summarise('Pitches' = n(),
                "AVG" = round(sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run"))/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout","Out" )),3),
                "SLUG" = round((sum(PlayResult == "Single", na.rm=T) + 2* sum(PlayResult == "Double", na.rm=T)+ 3* sum(PlayResult == "Triple", na.rm=T) + 4* sum(PlayResult == "Home Run", na.rm=T) )/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout", "Out" )),3),
                'AVG Exit Velo' = round(mean(hitData.launchSpeed, na.rm = TRUE),1),
                "GB%" = round(100* sum(hitData.trajectory == "ground_ball" , na.rm = T) / sum(hitData.trajectory %in% c("line_drive","fly_ball","ground_ball","popup","bunt_popup"), na.rm = T),0),
                "HardHit%" = round(sum(HardHit, na.rm = T)/sum(details.call.description == "InPlay", na.rm = T),3)*100,
                "Swing%" = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                        "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)")) / n() ,2)*100,
                'Whiff%' = round(sum(details.call.description %in% c("Swinging Strike","Swinging Strike (Blocked)"))/
                                      sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                          "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)")),3)*100,
                'O-Swing %' = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                        "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(11,12,13,14), na.rm=T)/sum(pitchData.zone %in% c(11,12,13,14)),2)*100,
                'Z-Swing %' = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                        "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(1,2,3,4,5,6,7,8,9), na.rm=T)/sum(pitchData.zone %in% c(1:9)),2)*100,
                'O-Contact %' = round(sum(details.call.description %in% c("Foul", "Foul Tip",
                                                                        "In play, out(s)","In play, no out","In play, run(s)") & pitchData.zone %in% c(11,12,13,14), na.rm=T)/sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                                                                                                                                                                       "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(11,12,13,14)),2)*100,
                'Z-Contact %' = round(sum(details.call.description %in% c( "Foul", "Foul Tip",
                                                                          "In play, out(s)","In play, no out","In play, run(s)") & pitchData.zone %in% c(1:9), na.rm=T)/sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                                                                                                                                                                    "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(1:9)),2)*100,
                

      ) 
    
    table <- table[order(table$Pitches,decreasing = TRUE),]
    
    
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
  })
  
  
  
## Pitch Frequency HeatMap ##
  
  output$Pitch_Freq<- renderPlot({
    
      
      dataFilter <- reactive({
        TestTrackMan  %>%
          filter(#batting_team ==input$Team ,
                 matchup.batter.fullName == input$Batter,
                 game_date >= input$Date[1] &
                 game_date <= input$Date[2],
                 matchup.splits.batter == input$PitcherThrows,
                 details.type.description %in% input$Pitch)
      }) 
      ggplot(data = dataFilter() , 
             aes(x = pitchData.coordinates.pX * (-1), y = pitchData.coordinates.pZ)) +
        xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Plot" ), subtitle = "Pitcher's View" )+
        stat_density2d_filled(contour_var = "ndensity")  +
        scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
        geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
        geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
        geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
        geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
        
        # Horizontal Lines (Bottom Inner, Top Inner)
        geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
        geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
        
        # Vertical Lines (Left Inner, Right Inner)
        geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
        geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
        #Plate
        geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
        geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
        geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
        geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
        geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
        theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
        theme(legend.position = "none",legend.text = element_text(size = 10), axis.title = element_blank())  +
        theme(strip.text = element_text(size = 7, face = 'bold'),
              axis.text.x=element_blank(), #remove x axis labels
              axis.text.y=element_blank(),  #remove y axis labels
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
  
  })
  
  ## Swing HeatMap ##
  
  output$Swing<- renderPlot({
    
    
    dataFilter <- reactive({
      TestTrackMan  %>%
        filter(#batting_team ==input$Team ,
               matchup.batter.fullName == input$Batter,
               game_date >= input$Date[1] &
                 game_date <= input$Date[2],
               matchup.splits.batter == input$PitcherThrows,
               details.type.description %in% input$Pitch,
               details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                               "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)"))
    }) 
    ggplot(data = dataFilter() , 
           aes(x = pitchData.coordinates.pX * (-1), y = pitchData.coordinates.pZ)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Swing Plot" ), subtitle = "Pitcher's View" )+
      stat_density2d_filled(contour_var = "ndensity")  +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
      
      # Horizontal Lines (Bottom Inner, Top Inner)
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
      
      # Vertical Lines (Left Inner, Right Inner)
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
      #Plate
      geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
      geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
      geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
      geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none",legend.text = element_text(size = 10), axis.title = element_blank())  +
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) 
  })
  
  
  ### Whiff HeatMap ##
  output$Whiff<- renderPlot({
    
    
    dataFilter <- reactive({
      TestTrackMan  %>%
        filter(#batting_team ==input$Team ,
               matchup.batter.fullName == input$Batter,
               game_date >= input$Date[1] &
                 game_date <= input$Date[2],
               matchup.splits.batter == input$PitcherThrows,
               details.type.description %in% input$Pitch,
               details.call.description %in% c("Swinging Strike","Swinging Strike (Blocked)"))
    }) 
    ggplot(data = dataFilter() , 
           aes(x = pitchData.coordinates.pX * (-1), y = pitchData.coordinates.pZ)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Whiff Plot" ), subtitle = "Pitcher's View" )+
      stat_density2d_filled(contour_var = "ndensity")  +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
      
      # Horizontal Lines (Bottom Inner, Top Inner)
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
      
      # Vertical Lines (Left Inner, Right Inner)
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
      #Plate
      geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
      geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
      geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
      geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none",legend.text = element_text(size = 10), axis.title = element_blank())  +
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  })
  
 
  
  ### HardHit HeatMap ##
  output$HardHit<- renderPlot({
    
    
    dataFilter <- reactive({
      TestTrackMan  %>%
        filter(#batting_team ==input$Team ,
               matchup.batter.fullName == input$Batter,
               game_date >= input$Date[1] &
               game_date <= input$Date[2],
               matchup.splits.batter == input$PitcherThrows,
               details.type.description %in% input$Pitch,
               HardHit == 1)
    }) 
    ggplot(data = dataFilter() , 
           aes(x = pitchData.coordinates.pX * (-1), y = pitchData.coordinates.pZ)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("EV > 95 Plot" ), subtitle = "Pitcher's View" )+
      stat_density2d_filled(contour_var = "ndensity")  +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
      
      # Horizontal Lines (Bottom Inner, Top Inner)
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
      
      # Vertical Lines (Left Inner, Right Inner)
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
      #Plate
      geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
      geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
      geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
      geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none",legend.text = element_text(size = 10), axis.title = element_blank())  +
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  }) 
  
  
  
  ## SprayChart ##
  
  output$SprayChart<- renderPlot({
    
    
    dataFilter <- reactive({
      TestTrackMan  %>%
        filter(#batting_team ==input$Team ,
          matchup.batter.fullName == input$Batter,
          game_date >= input$Date[1] &
            game_date <= input$Date[2],
          matchup.splits.batter == input$PitcherThrows,
          details.type.description %in% input$Pitch,
          details.call.description %in% c("In play, out(s)","In play, no out","In play, run(s)"))
    }) 
    ggplot(dataFilter(), aes(x = hitData.coordinates.coordX , y = hitData.coordinates.coordY, color = PlayResult )) +
      geom_point(size = 1.25) +
      #stat_density_2d(aes(fill = ..level..), geom = "polygon")+
      #scale_fill_distiller(palette="RdBu", direction=-1)+
      geom_spraychart(stadium_transform_coords = FALSE, stadium_segments = "all", stadium_ids = "astros") +
      coord_fixed() +ggtitle(label = 'Spray Chart')+ scale_y_reverse() + scale_x_reverse()+
      #scale_colour_manual(values = c("Out" == "darkred", "Single" == "forestgreen", "Double" == "yellow" , "Triple" == "orange" , "HomeRun" ==  "skyblue" , "Sacrifice" == "pink", "Error" == "green" , "NA" == "grey" ))+
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.text = element_text(size = 10), axis.title = element_blank())  +
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) 
  
  })
  
  
  #### Approach ###
  output$Approach<- renderDataTable({
    
    table<- TestTrackMan  %>%
      filter(#batting_team ==input$Team ,
        matchup.batter.fullName == input$Batter,
        game_date >= input$Date[1] &
          game_date <= input$Date[2],
        matchup.splits.batter == input$PitcherThrows,
        details.type.description %in% input$Pitch) %>% group_by("Count" = Count) %>%
      
      summarise('Pitches' = n(),
                "AVG" = round(sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run"))/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout","Out" )),3),
                "SLUG" = round((sum(PlayResult == "Single", na.rm=T) + 2* sum(PlayResult == "Double", na.rm=T)+ 3* sum(PlayResult == "Triple", na.rm=T) + 4* sum(PlayResult == "Home Run", na.rm=T) )/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout", "Out" )),3),
                'AVG Exit Velo' = round(mean(hitData.launchSpeed, na.rm = TRUE),1),
                "GB%" = round(100* sum(hitData.trajectory == "ground_ball" , na.rm = T) / sum(hitData.trajectory %in% c("line_drive","fly_ball","ground_ball","popup","bunt_popup"), na.rm = T),0),
                "HardHit%" = round(sum(HardHit, na.rm = T)/sum(details.call.description == "InPlay", na.rm = T),3)*100,
                "Swing%" = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                     "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)")) / n() ,2)*100,
                'Whiff%' = round(sum(details.call.description %in% c("Swinging Strike","Swinging Strike (Blocked)"))/
                                   sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                       "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)")),3)*100,
                'O-Swing %' = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                        "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(11,12,13,14), na.rm=T)/sum(pitchData.zone %in% c(11,12,13,14)),2)*100,
                'Z-Swing %' = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                        "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(1,2,3,4,5,6,7,8,9), na.rm=T)/sum(pitchData.zone %in% c(1:9)),2)*100,
                'O-Contact %' = round(sum(details.call.description %in% c("Foul", "Foul Tip",
                                                                          "In play, out(s)","In play, no out","In play, run(s)") & pitchData.zone %in% c(11,12,13,14), na.rm=T)/sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                                                                                                                                                                    "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(11,12,13,14)),2)*100,
                'Z-Contact %' = round(sum(details.call.description %in% c( "Foul", "Foul Tip",
                                                                           "In play, out(s)","In play, no out","In play, run(s)") & pitchData.zone %in% c(1:9), na.rm=T)/sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                                                                                                                                                             "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(1:9)),2)*100,
                
                
      ) 
    
    table <- table[order(table$Pitches,decreasing = TRUE),]
    
    
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
  })
  
  
  
  
  ### Pitchers Tab ###
  
  

  
  output$PStats<- renderDataTable({
    
    table<- TestTrackMan  %>%
      filter(#fielding_team ==input$TeamP ,
             matchup.pitcher.fullName == input$Pitcher,
             game_date >= input$DateP[1] &
               game_date <= input$DateP[2],
             matchup.splits.pitcher == input$BatterSide) %>%
        summarise("Pitches" = n(),
                 # 'IP' = round((sum(Outs, na.rm = T))/3, 1),
                  'BF' = n_distinct(about.inning, matchup.batter.fullName, about.atBatIndex),
                  'K%' = round(100*sum(PlayResult =="Strikeout" , na.rm = T)/n_distinct(about.inning, matchup.batter.fullName, about.atBatIndex),1),
                  'BB%' = round(100*sum(PlayResult =="Walk", na.rm = T)/n_distinct(about.inning, matchup.batter.fullName, about.atBatIndex),1),
                  #'HBP' = sum(result.event == "Hit By Pitch", na.rm = T),
                  'BIP' = sum(details.isInPlay == TRUE, na.rm = T) ,
                  'H' = sum(PlayResult %in% c('Single','Double','Triple','Home Run'), na.rm = T),
                  'XBH' = sum(PlayResult %in% c('Double','Triple','Home Run'), na.rm = T),
                  #'ER' = sum( result.rbi[details.description == "In play, run(s)"], na.rm = TRUE),
                  "HardHit%" = round(sum(HardHit, na.rm = T) / sum(details.isInPlay == TRUE, na.rm = T) * 100, 1) ,
                  "GB%" = round(100* sum(hitData.trajectory == "ground_ball" , na.rm = T) / sum(hitData.trajectory %in% c("line_drive","fly_ball","ground_ball","popup","bunt_popup"), na.rm = T),0),
                  "Whiff % " = round(100*sum(details.description %in% c("Swinging Strike", "Swinging Strike (Blocked)" ), na.rm = TRUE)/n(),1),
                  "CSW%" = round(sum(details.description %in% c("Swinging Strike", "Swinging Strike (Blocked)","Called Strike" ))/ n() , 3)*100
        )
    
    
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
  })
  
  
  output$Metrics <- renderDataTable({
   table<- TestTrackMan %>% filter(#fielding_team ==input$TeamP ,
                            matchup.pitcher.fullName == input$Pitcher,
                            game_date >= input$DateP[1] &
                              game_date <= input$DateP[2],
                            matchup.splits.pitcher == input$BatterSide,
                            Count %in% input$Count,
                            matchup.splits.menOnBase %in% input$Situation) %>%
        group_by('Pitch' = details.type.description) %>%
        dplyr::summarize('No' = n(),
                         'Usage' = n(),
                         'Usage %' = n(),
                         'Velo' = round(mean(pitchData.startSpeed, na.rm = TRUE),1),
                         'VeloMax' = round(max(pitchData.startSpeed, na.rm = TRUE),1),
                         'Spin' = round(mean(pitchData.breaks.spinRate, na.rm = TRUE),0),
                         'Vert' = round(mean(pitchData.breaks.breakVerticalInduced, na.rm = TRUE),1),
                         'Horz' = round(mean(pitchData.breaks.breakHorizontal, na.rm = TRUE),1),
                         'RelHt' = round(mean(pitchData.coordinates.z0, na.rm = TRUE),1),
                         'RelSide' = round(mean(pitchData.coordinates.x0, na.rm = TRUE),1),
                         'Ext' = round(mean(pitchData.extension, na.rm = TRUE),1)
        ) %>%
        mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100) %>% dplyr::select(-Usage)
   
   table <- table[order(table$No,decreasing = TRUE),]
   
   
   tableFilter <- reactive({table})
   datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
     formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
   
  })
  
  
  
  output$StatsPitch<- renderDataTable({
    
    table<- TestTrackMan %>% filter(#fielding_team ==input$TeamP ,
                              matchup.pitcher.fullName == input$Pitcher,
                              game_date >= input$DateP[1] &
                                game_date <= input$DateP[2],
                              matchup.splits.pitcher == input$BatterSide,
                              Count %in% input$Count,
                              matchup.splits.menOnBase %in% input$Situation) %>%
      group_by('Pitch' = details.type.description) %>%
      
      summarise('Pitches' = n(),
                "AVG" = round(sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run"))/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout","Out" )),3),
                "SLUG" = round((sum(PlayResult == "Single", na.rm=T) + 2* sum(PlayResult == "Double", na.rm=T)+ 3* sum(PlayResult == "Triple", na.rm=T) + 4* sum(PlayResult == "Home Run", na.rm=T) )/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout", "Out" )),3),
                'AVG Exit Velo' = round(mean(hitData.launchSpeed, na.rm = TRUE),1),
                "GB%" = round(100* sum(hitData.trajectory == "ground_ball" , na.rm = T) / sum(hitData.trajectory %in% c("line_drive","fly_ball","ground_ball","popup","bunt_popup"), na.rm = T),0),
                "HardHit%" = round(sum(HardHit, na.rm = T)/sum(details.call.description == "InPlay", na.rm = T),3)*100,
                "Swing%" = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                        "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)")) / n() ,2)*100,
                'Whiff%' = round(sum(details.call.description %in% c("Swinging Strike","Swinging Strike (Blocked)"))/
                                      sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                          "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)")),3)*100,
                'O-Swing %' = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                        "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(11,12,13,14), na.rm=T)/sum(pitchData.zone %in% c(11,12,13,14)),2)*100,
                'Z-Swing %' = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                        "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(1,2,3,4,5,6,7,8,9), na.rm=T)/sum(pitchData.zone %in% c(1:9)),2)*100,
                'O-Contact %' = round(sum(details.call.description %in% c( "Foul", "Foul Tip",
                                                                          "In play, out(s)","In play, no out","In play, run(s)") & pitchData.zone %in% c(11,12,13,14), na.rm=T)/sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                                                                                                                                                                    "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(11,12,13,14)),2)*100,
                'Z-Contact %' = round(sum(details.call.description %in% c("Foul", "Foul Tip",
                                                                          "In play, out(s)","In play, no out","In play, run(s)") & pitchData.zone %in% c(1:9), na.rm=T)/sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                                                                                                                                                            "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)") & pitchData.zone %in% c(1:9)),2)*100,
                
                
      ) 
    
    table <- table[order(table$Pitches,decreasing = TRUE),]
    
    
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
  })
  
  
  
  
  output$Movement <- renderPlot({
    
   
      dataFilter <- reactive({
        TestTrackMan %>% filter(#fielding_team ==input$TeamP ,
                                matchup.pitcher.fullName == input$Pitcher,
                                game_date >= input$DateP[1] &
                                  game_date <= input$DateP[2],
                                matchup.splits.pitcher == input$BatterSide,
                                Count %in% input$Count,
                                matchup.splits.menOnBase %in% input$Situation)  })
      ggplot(data = dataFilter(),
             aes(x = pitchData.breaks.breakHorizontal, y = pitchData.breaks.breakVerticalInduced, color = details.type.code)) +
        labs(title = "Pitch Movement" ,color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" ) + 
        xlim(-25, 25) + ylim(-25, 25) +
        geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") +
        geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
        geom_point(size =4, alpha = .75) +
        # we manually set the pitch colors so that they are uniform across each plot and tables
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
        theme(legend.text = element_text(size = 8), axis.title = element_text(size = 8))
      
      
    }) 
  
  
  
  output$Release <- renderPlot({
    
    
    dataFilter <- reactive({
      TestTrackMan %>% filter(#fielding_team ==input$TeamP ,
                              matchup.pitcher.fullName == input$Pitcher,
                              game_date >= input$DateP[1] &
                                game_date <= input$DateP[2],
                              matchup.splits.pitcher == input$BatterSide,
                              Count %in% input$Count,
                              matchup.splits.menOnBase %in% input$Situation)
      })
    ggplot(data = dataFilter(),
           aes(x = pitchData.coordinates.x0 * -1, y = pitchData.coordinates.z0, color = details.type.code)) +
      xlim(-5,5) + ylim(0,10) + labs(color = "", title = "Pitch Release") +
      geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
      geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
      geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
      geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
      geom_point(aes(colour = factor(details.type.code)), size = 4) +
      scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                    'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
      theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
      theme(legend.text = element_text(size = 12), axis.title = element_blank())
    
  }) 
  
  
  
  output$Pitch_Location<- renderPlot({
    
    
    dataFilter <- reactive({
      TestTrackMan %>% filter(#fielding_team ==input$TeamP ,
                              matchup.pitcher.fullName == input$Pitcher,
                              game_date >= input$DateP[1] &
                                game_date <= input$DateP[2],
                              matchup.splits.pitcher == input$BatterSide,
                              Count %in% input$Count,
                              matchup.splits.menOnBase %in% input$Situation)
    }) 
    ggplot(data = dataFilter() , 
           aes(x = pitchData.coordinates.pX * (-1), y = pitchData.coordinates.pZ)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Plot" ), subtitle = "Pitcher's View" )+
      stat_density2d_filled(contour_var = "ndensity")  +
      scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
      
      # Horizontal Lines (Bottom Inner, Top Inner)
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
      
      # Vertical Lines (Left Inner, Right Inner)
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
      #Plate
      geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
      geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
      geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
      geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.position = "none",legend.text = element_text(size = 10), axis.title = element_blank())  +
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) + facet_wrap(~ details.type.description )
    
  })
  
  
  ## SprayChart ##
  
  output$SprayChartP<- renderPlot({
    
    
    dataFilter <- reactive({
      TestTrackMan  %>%
        filter(#fielding_team ==input$TeamP ,
          matchup.pitcher.fullName == input$Pitcher,
          game_date >= input$DateP[1] &
            game_date <= input$DateP[2],
          matchup.splits.pitcher == input$BatterSide,
          Count %in% input$Count,
          matchup.splits.menOnBase %in% input$Situation,
          details.call.description %in% c("In play, out(s)","In play, no out","In play, run(s)"))
    }) 
    ggplot(dataFilter(), aes(x = hitData.coordinates.coordX , y = hitData.coordinates.coordY, color = PlayResult )) +
      geom_point(size = 1.25) +
      #stat_density_2d(aes(fill = ..level..), geom = "polygon")+
      #scale_fill_distiller(palette="RdBu", direction=-1)+
      geom_spraychart(stadium_transform_coords = FALSE, stadium_segments = "all", stadium_ids = "astros") +
      coord_fixed() +ggtitle(label = 'Spray Chart')+ scale_y_reverse() + scale_x_reverse()+
      #scale_colour_manual(values = c("Out" == "darkred", "Single" == "forestgreen", "Double" == "yellow" , "Triple" == "orange" , "HomeRun" ==  "skyblue" , "Sacrifice" == "pink", "Error" == "green" , "NA" == "grey" ))+
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(legend.text = element_text(size = 10), axis.title = element_blank())  +
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) 
    
  })
  
  
  
  
  
  
}
  
  
shinyApp(ui = ui, server = server)