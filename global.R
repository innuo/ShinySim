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
library(stringr)
library(dglm)
library(nnet)

data_plot <- function(x, y, num_samples, color, facet_row, facet_col){
  
  dataset_ids <- sim_state$dataset$matching_dataset_ids(c(x, y))
  data <- sim_state$dataset$dataset_from_ids(dataset_ids)
  data <- data[sample(1:nrow(data), min(num_samples, nrow(data))),]
  
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


simulation_plot <- function(num_samples, input.col, output.col){
  sim.df <- sim_state$sim$sample(num_samples)

  dataset_ids <- sim_state$dataset$matching_dataset_ids(c(input.col, output.col))
  data <- sim_state$dataset$dataset_from_ids(dataset_ids)
  
  data <- subset(data, select=c(input.col, output.col))
  data <- na.omit(data)
  data <- data[sample(1:nrow(data), min(num_samples, nrow(data))),]
  
  data$Data_Source <- "Raw Data"
  sim.df$Data_Source <- "Simulated"
  
  data <- rbind.data.frame(data, subset(sim.df, select =c(input.col, output.col, "Data_Source")))
  
  p <- ggplot(data, aes_string(x = input.col, y = output.col, color = "Data_Source")) 
  
  if(class(data[[input.col]]) == "factor")
    p <- p+geom_jitter(width=0.1)
  else
    p <- p+geom_point() 
  
  ggplotly(p)
  
}

random_string <- function(n) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  ret <- paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  return(ret)
}

# tabs
source("simulate_tab.R")
source("query_tab.R")
source("data_tab.R")
source("causal_interface.R")
