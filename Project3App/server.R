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
    output$inf <- renderText({
        paste("test")
    })



})
