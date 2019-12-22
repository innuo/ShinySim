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
library(GGally)
library(jsonlite)

## NOT USED
pairs_plot <- function(columns){
  n <- length(columns)
  data <- subset(sim_state$dataset_rbinded, select = columns)
  if(nrow(data) > 500) data <- data[sample(1:nrow(data), 500),]
  is.factor <- do.call(c, lapply(data, class)) == "factor"
  group = if (any(is.factor)) data[,which(is.factor)[1]] else NULL
  p <- pairsD3(data, group=group)
  p
}

data_plot <- function(x, y, num_samples, color, facet_row, facet_col){
  
  print(x)
  print(y)
  print(num_samples)
  print(color)
  print(facet_row)
  print(facet_col)
  
  data <- sim_state$dataset_rbinded
  data <- data[sample(1:nrow(data), num_samples),]
  
  # build graph with ggplot syntax
  p <- ggplot(data, aes_string(x = x, y = y, color = color)) 
  
  if(class(data[[x]]) == "factor")
    p <- p+geom_jitter(width=0.1)
  else
    p <- p+geom_point() 
  
  
  # if at least one facet column/row is specified, add it
  facets <- paste(facet_row, '~', facet_col)
  if (facets != '. ~ .') p <- p + facet_grid(facets)
  
  
  ggplotly(p) 
}


simulation_plot <- function(sim.df, input.col, ouput_col){
  print(input.col)
  data <- subset(sim_state$dataset_rbinded, select=c(input.col, output.col))
  data$Provenance <- "Raw Data"
  data <- na.omit(data)
  sim.df$Provenance <- "Simulated"
  data <- rbind.data.frame(data, subset(sim.df, select =c(input.col, output.col, "Provenance")))
  
  p <- ggplot(data, aes_string(x = input.col, y = output.col, color = Provenance)) 
  
  if(class(data[[input.col]]) == "factor")
    p <- p+geom_jitter(width=0.1)
  else
    p <- p+geom_point() 
  
  ggplotly(p)
  
}


# tabs
source("simulate_tab.R")
source("query_tab.R")
source("data_tab.R")
source("causal_interface.R")
