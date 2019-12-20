library(plyr)
library(shiny)
library(shinyjqui)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyAce)
library(styler)
library(shinyWidgets)
library(shinyEffects)
library(DT)
library(visNetwork)
library(plotly)


pairs_plot <- function(columns){
  n <- length(columns)
  data <- subset(sim_state$dataset_rbinded, select = columns)
  if(nrow(data) > 500) data <- data[sample(1:nrow(data), 500),]
  p <- ggpairs(data=data) 
  ggplotly(p) %>% layout(height = n*200, width = n*200)
}

# tabs
source("simulate_tab.R")
source("query_tab.R")
source("data_tab.R")
source("causal_interface.R")