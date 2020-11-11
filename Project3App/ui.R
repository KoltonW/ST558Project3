
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinydashboard)

cbb <- read_csv("cbb.csv")

dashboardPage(
    dashboardHeader(title = "App Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabname = "info", icon = icon("info")),
            menuItem("Data Exploration", tabname = "datinfo", icon = icon("book-open")),
            menuItem("Clustering", tabname = "clust", icon = icon("ethernet")),
            menuItem("Modeling", tabname = "modeling", icon = icon("brain")),
            menuItem("Data Save", tabname = "save", icon = icon("folder"))
        )
    ),
    dashboardBody()
)