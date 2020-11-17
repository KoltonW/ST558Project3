#Read in packages
library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(ggplot2)
library(DT)
library(caret)

#Bring in data and manipulate it
cbb <- read_csv("cbb.csv")
cbb %>% mutate(WinPct = round((W/G), 3)) %>% rename(TwoPt_O = "2P_O", TwoPt_D = "2P_D", ThreePt_O = "3P_O", ThreePt_D = "3P_D") -> cbb
cbb %>% select(-POSTSEASON, -SEED) -> cbb4Dat
cbb %>% select(-POSTSEASON, -SEED, -TEAM, -CONF, -YEAR) -> cbb4Dat2
cbb %>% select(-TEAM, -CONF, -G, -W, -POSTSEASON, -SEED, -YEAR) -> cbb4Model
cbb %>% select(-TEAM, -CONF, -G, -W, -POSTSEASON, -SEED, -YEAR, - WinPct) -> cbb4ModelVarPick

set.seed(1)
train <- sample(1:nrow(cbb4Model), size = nrow(cbb4Model)*0.8)
test <- dplyr::setdiff(1:nrow(cbb4Model), train)

cbbTrain <- cbb4Model[train, ]
cbbTest <- cbb4Model[test, ]

#Create App dashboard page with several tabs
dashboardPage(
    dashboardHeader(title = "App Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info", icon = icon("info")),
            menuItem("Data Exploration", tabName = "datinfo", icon = icon("book-open")),
            menuItem("Principal Component Analysis", tabName = "pca", icon = icon("dna")),
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
                                    choices = unique(cbb4Dat$CONF)),
                        uiOutput("team_selected"), 
                        selectInput("stat", strong("Select a Statistic"),
                                        choices = names(cbb4Dat2),
                                        selected = 'WinPct'),
                        downloadButton("dplot", "Download this Plot")
                        
                        ),
                    box(title = uiOutput("graphTitle"), status = "primary", solidHeader = TRUE,
                        plotlyOutput("teamPlot"), width = 8),
                    
                    box(title = "Averages", status = "primary", solidHeader = TRUE, width = 12,
                        textOutput("avgs"))
                    ),
            
            tabItem(tabName = "pca",
                    box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 4,
                        h4("Click the checkbox for variables to include in the PC analysis:"),
                        checkboxGroupInput("varsChosen", "Select 2 Or More Variables", 
                                           choices = names(cbb4Model), selected = ''),
                        actionButton("showPlot", "Display Biplot")
                        ), 
                    box(title = "PCA Biplot for College Basketball Data", status = "primary", 
                        solidHeader = TRUE, width = 8, height = 800,
                        plotOutput("pcaBiplot", width = "100%"))
            ),
            
            tabItem(tabName = "modeling", withMathJax(),
                    box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 4,
                        h4("Choose model type, variables, and model options for response variable WinPct:"),
                        selectInput("modType", strong("Select Model Type"), 
                                    choices = list("Linear Regression Model", "Boosted Tree Model", " "),
                                    selected = " "),
                        conditionalPanel(
                            condition = "input.modType == 'Linear Regression Model'",
                            checkboxGroupInput("lrmVars", label = (helpText('Select \\(\\beta\\)s (predictor variables) for Linear Regression Model')),
                                               choices = names(cbb4ModelVarPick), selected = names(cbb4ModelVarPick),
                                               inline = TRUE),
                            actionButton("showLM", "Estimate Model")
                        ),
                        conditionalPanel(
                            condition = "input.modType == 'Boosted Tree Model'",
                            checkboxGroupInput("rfVars", label = (helpText('Select \\(\\beta\\)s (predictor variables) for Boosted Tree Model')),
                                               choices = names(cbb4ModelVarPick), selected = names(cbb4ModelVarPick),
                                               inline = TRUE),
                            numericInput("intDepth", "Interaction Depth:", value = 5, min = 1,
                                         max = 10, step = 1),
                            numericInput("nTrees", "Number of Trees:", value = 200, min = 50,
                                         max = 2000, step = 50),
                            numericInput("nminObs", "Number of Min Obs in Node", value = 10, min = 3, max = 12,
                                         step = 1),
                            actionButton("showBTM", "Estimate Model")
                        )
                        ),
                    box(title = "Model Output", status = "primary", solidHeader = TRUE, width = 8,
                        htmlOutput("pred"))
                    ),
            
            tabItem(tabName = "save",
                    box(title = "College Basketball Data", status = "primary", solidHeader = TRUE,
                        DT::dataTableOutput("table"), width = 12),
                    box(title = "Data Download", status = "warning", solidHeader = TRUE,
                        downloadButton("dfile", "Download Filtered Data"))
            )
            
        )
    )
)