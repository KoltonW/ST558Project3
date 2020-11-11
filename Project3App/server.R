#Read in packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinydashboard)

#Bring in data and manipulate it
cbb <- read_csv("cbb.csv")
cbb %>% mutate(WinPct = round((W/G), 3)) -> cbb
cbb %>% select(-TEAM, -CONF, -G, -W, -POSTSEASON, -SEED, -YEAR) -> cbb4Model
cbb %>% select(-POSTSEASON, -SEED) -> cbb4Dat


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
"2P_O: Two-Point Shooting Percentage", '<br/>', '<br/>',
"2P_D: Two-Point Shooting Percentage Allowed", '<br/>', '<br/>',
"3P_O: Three-Point Shooting Percentage", '<br/>', '<br/>',
"3P_D: Three-Point Shooting Percentage Allowed", '<br/>', '<br/>',
"ADJ_T: Adjusted Tempo (An estimate of the tempo (possessions per 40 minutes) a team would have against the team that wants to play at an average Division I tempo)", '<br/>', '<br/>',
"WAB: Wins Above Bubble (The bubble refers to the cut off between making the NCAA March Madness Tournament and not making it)"))
    })
    
    output$purp <- renderUI({
        HTML(
            paste0("The purpose of this app is to view what components of team play within college basketball led to success (in the form of a high winning percentage) or failure. Individual teams' statistical trends can be viewed over the course of five seasons, and they can be compared against another team as well. Supervised and unsupervised prediction models can be used to take a closer look at the response variable of WinPct (winning percentage). Plots viewed on the data exploration tab may be saved, as well as the dataset as a whole (or a subset of it) in the data save tab." , '<br/>',  '<br/>',
                   "This information tab just gives an overview of the data and app. Clicking on the Data Exploration tab allows you to look at variables for different teams along with the data set as a data table. The Clustering tab allows the user to specify certain aspects of the clustering method and produces a dendogram that may be saved. The Modeling tab allows for two separate supervised learning models to predict for the response of winning percentage based on user-chosen settings. The Data Save tab allows the data set to be viewed, subsetted, and saved.")
        )
    })
    
    output$src <- renderUI({
        h3("This data set comes from", a(href = "https://www.kaggle.com/andrewsundberg/college-basketball-dataset?select=cbb.csv", " the College Basketball Dataset on Kaggle."))
    })



})
