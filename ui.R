## ---------------------------
##
## Script name: shiny.R
##
## Purpose of script: create shiny app, ui version
##
## Last edited: 10-29-2021
##
## ---------------------------

##---load packages
options(scipen = 6, digits = 4)
memory.limit(30000000) 

if (!require("pacman")) install.packages("pacman")
pacman::p_load("shiny","ggplot2", "dplyr","ggmap", "maps","mapproj","plotly","shinydashboard","shinythemes",
               "DT","leaflet","shinyjs","V8","reshape","htmltools","widgetframe","htmlwidgets")


# map -------------------------------------------

ui <- fluidPage(
  
  titlePanel("Toxicity on Strawberry in the US"), 
  sidebarLayout(
    sidebarPanel(
      #inputs
      selectizeInput("StateInput", "State", 
                     choices = unique(toxicity_val$State),
                     selected = "CALIFORNIA", multiple = FALSE)
    ),
    mainPanel(
      plotOutput("toxicityplot")
    )
  )
)
