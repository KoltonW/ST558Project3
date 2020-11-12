#Read in packages
library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(ggplot2)
library(DT)

#Bring in data and manipulate it
cbb <- read_csv("cbb.csv")
cbb %>% mutate(WinPct = round((W/G), 3)) %>% rename(TwoPt_O = "2P_O", TwoPt_D = "2P_D", ThreePt_O = "3P_O", ThreePt_D = "3P_D") -> cbb
cbb %>% select(-POSTSEASON, -SEED) -> cbb4Dat
cbb %>% select(-POSTSEASON, -SEED, -TEAM, -CONF, -YEAR) -> cbb4Dat2
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
                        htmlOutput("inf")),
                    box(title = "Purpose", status = "primary", solidHeader = TRUE,
                        htmlOutput("purp")),
                    box(title = "Source", status = "primary", solidHeader = TRUE,
                        uiOutput("src"))),
            
            tabItem(tabName = "datinfo",
                    box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 4,
                        h4("Choose options to find team and statistic:"),
                        selectInput("conf", strong("Select a Conference"),
                                    choices = c("Enter a conference..."="", sort(cbb4Dat$CONF)),
                                    selected = ''),
                        conditionalPanel(
                            condition = "input.conf != '' ",
                            uiOutput("team_selected")
                        ),
                        conditionalPanel(
                            condition = "input.conf !== '' ",
                            selectInput("stat", strong("Select a Statistic"),
                                        choices = c("Choose a variable..."="", names(cbb4Dat2)),
                                        selected = '')
                        ),
                        
                        downloadButton("dplot", "Download this Plot")
                        
                        ),
                    box(title = uiOutput("graphTitle"), status = "primary", solidHeader = TRUE,
                        plotlyOutput("teamPlot"), width = 8),
                    
                    box(title = "Averages", status = "primary", solidHeader = TRUE, width = 12,
                        textOutput("avgs"))
                    )
        )
    )
)