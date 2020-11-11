#Read in packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinydashboard)

#Bring in data and manipulate it
cbb <- read_csv("cbb.csv")
cbb %>% mutate(WinPct = round((W/G), 3)) -> cbb
cbb %>% select(-POSTSEASON, -SEED) -> cbb4Dat
cbb %>% select(-TEAM, -CONF, -G, -W, -POSTSEASON, -SEED, -YEAR) -> cbb4Model

#Create App dashboard page
dashboardPage(
    dashboardHeader(title = "App Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info", icon = icon("info")),
            menuItem("Data Exploration", tabName = "datinfo", icon = icon("book-open")),
            menuItem("Clustering", tabName = "clust", icon = icon("ethernet")),
            menuItem("Modeling", tabName = "modeling", icon = icon("brain")),
            menuItem("Data Save", tabName = "save", icon = icon("folder"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "info",
                    box(title = "Information",  status = "primary", solidHeader = TRUE, 
                        textOutput("inf")))
        )
    )
)