#Read in packages
library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)
library(shinydashboard)
library(DT)

#Bring in data and manipulate it
cbb <- read_csv("cbb.csv")
cbb %>% mutate(WinPct = round((W/G), 3)) %>% rename(TwoPt_O = "2P_O", TwoPt_D = "2P_D", ThreePt_O = "3P_O", ThreePt_D = "3P_D") -> cbb
cbb %>% select(-TEAM, -CONF, -G, -W, -POSTSEASON, -SEED, -YEAR) -> cbb4Model
cbb %>% select(-POSTSEASON, -SEED) -> cbb4Dat
cbb %>% select(-POSTSEASON, -SEED, -TEAM, -CONF, -YEAR) -> cbb4Dat2


shinyServer(function(input, output) {

    #Create text info
    output$inf <- renderUI({
        HTML(
        paste0("This app portrays data from the 2015, 2016, 2017, 2018, and 2019 Division I college basketball seasons. The data contains variables pertaining to the school, the conference, team success, and per-game statistics for each individual season. An extra variable, WinPct (W/G), has also been added to the data for modeling and prediction purposes. The description for the unclear abbreviated variables are as follows:", '<br/>',  '<br/>',
              "ADJOE: Adjusted Offensive Efficiency (An estimate of the offensive efficiency (points scored per 100 possessions) a team would have against the average Division I defense)", '<br/>', '<br/>',
"ADJDE: Adjusted Defensive Efficiency (An estimate of the defensive efficiency (points allowed per 100 possessions) a team would have against the average Division I offense)", '<br/>', '<br/>',
"BARTHAG: Power Rating (Chance of beating an average Division I team)", '<br/>', '<br/>',
"EFG_O: Effective Field Goal Percentage Shot", '<br/>', '<br/>',
"EFG_D: Effective Field Goal Percentage Allowed", '<br/>', '<br/>',
"TOR: Turnover Percentage Allowed (Turnover Rate)", '<br/>', '<br/>',
"TORD: Turnover Percentage Committed (Steal Rate)", '<br/>', '<br/>',
"ORB: Offensive Rebound Rate", '<br/>', '<br/>',
"DRB: Offensive Rebound Rate Allowed", '<br/>', '<br/>',
"FTR : Free Throw Rate (How often the given team shoots Free Throws)", '<br/>', '<br/>',
"FTRD: Free Throw Rate Allowed", '<br/>', '<br/>',
"TwoPt_O: Two-Point Shooting Percentage", '<br/>', '<br/>',
"TwoPt_D: Two-Point Shooting Percentage Allowed", '<br/>', '<br/>',
"ThreePt_O: Three-Point Shooting Percentage", '<br/>', '<br/>',
"ThreePt_D: Three-Point Shooting Percentage Allowed", '<br/>', '<br/>',
"ADJ_T: Adjusted Tempo (An estimate of the tempo (possessions per 40 minutes) a team would have against the team that wants to play at an average Division I tempo)", '<br/>', '<br/>',
"WAB: Wins Above Bubble (The bubble refers to the cut off between making the NCAA March Madness Tournament and not making it)"))
    })
    
    output$purp <- renderUI({
        HTML(
            paste0("The purpose of this app is to view what components of team play within college basketball led to success (in the form of a high winning percentage) or failure. Individual teams' statistical trends can be viewed over the course of five seasons, and they can be compared against the average values of each variable over that time span. Supervised and unsupervised prediction models can be used to take a closer look at the response variable of WinPct (winning percentage). Plots viewed on the data exploration tab may be saved, as well as the dataset as a whole (or a subset of it) in the data save tab." , '<br/>',  '<br/>',
                   "This information tab just gives an overview of the data and app. Clicking on the Data Exploration tab allows you to look at variables for different teams along with their averages. The plot in this tab allows hovering and clicking on the graph as an additional functionality. The Clustering tab allows the user to specify certain aspects of the clustering method and produces a dendogram that may be saved. The Modeling tab allows for two separate supervised learning models to predict for the response of winning percentage based on user-chosen settings. The Data Save tab allows the data set to be viewed, subsetted, and saved.")
        )
    })
    
    output$src <- renderUI({
        h3("This data set comes from", a(href = "https://www.kaggle.com/andrewsundberg/college-basketball-dataset?select=cbb.csv", " the College Basketball Dataset on Kaggle."))
    })
    
    output$graphTitle <- renderUI({
        text <- paste0(input$tm, " Trend Over 5 Year Span for the Variable ", input$stat)
        h3(text)
    })
    
    #Create team select input
    output$team_selected <- renderUI({
        teams_available <- cbb4Dat[cbb4Dat$CONF == input$conf, "TEAM"]
        
        selectInput("tm", strong("Select a Team"),
                    choices = unique(teams_available),
                    selected = '')
    })
    
    #Team trend line plot
    plotInput <- reactive({
        g <- ggplot(cbb4Dat[cbb4Dat$TEAM == input$tm, ], aes(x = YEAR))
        ggplotly(g + geom_line(aes_string(y = input$stat)) + geom_point(aes_string(y = input$stat)) +
                     theme(axis.text.x =element_text(face="bold", color="black", size=12), 
                           axis.text.y = element_text(face="bold", color="black", size=12)))
    })
    
    output$teamPlot <- renderPlotly({
        print(plotInput())
    })
    
    #Create download button
    output$dplot <- downloadHandler(
        filename = function() { paste(input$tm, input$stat, '.png', sep='') },
        content = function(file) {
            ggsave(file, plot = plotInput(), device = "png")
        })
    
    #Create summary
    output$avgs <- renderText({
        value <- as.name(input$stat)
        paste0("The average value across all D1 college basketball for the selected variable of ", input$stat, " is: ", colMeans(cbb[, value]))
    })
    

    
    

})
