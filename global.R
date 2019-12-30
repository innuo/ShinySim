library(dplyr)
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


get_dataset_from_cols <- function(cols){
  cols <- unique(cols[cols != "."])
  dataset_ids <- sim_state$dataset$matching_dataset_ids(cols, impose.minimum.size=FALSE)
  
  if(length(dataset_ids) > 0){
    title <- paste(names(sim_state$dataset_list)[dataset_ids], collapse=", ")
    data <- sim_state$dataset$dataset_from_vars(cols, impose.minimum.size=FALSE)
    print(data)
    print(names(sim_state$dataset_list))
    print(dataset_ids)
  }
  else{
    title <- paste("No raw datasets with these columns")
    data <- data.frame(matrix(NA, nrow=0, ncol=length(cols)))
    names(data) <- cols
  }
  
  print(summary(data))
  data <- na.omit(data[, cols, drop=FALSE])
  print(title)
  return(list(data, title))
  
}

data_plot <- function(x, y, num_samples, color, facet.row, facet.col){
  
  ret <- get_dataset_from_cols(c(x, y, color, facet.row, facet.col))
  data <- ret[[1]]
  title <- ret[[2]]
  data <- data[sample(1:nrow(data), min(num_samples, nrow(data))),, drop=F]
  
  # build graph with ggplot syntax
  p <- ggplot(data, aes_string(x = x, y = y, color = color)) 
  
  if(class(data[[x]]) == "factor")
    p <- p+geom_jitter(width=0.1)
  else
    p <- p+geom_point() 
  
  # if at least one facet column/row is specified, add it
  facets <- paste(facet.row, '~', facet.col)
  if (facets != '. ~ .') p <- p + facet_grid(facets)
  
  p <- p+ggtitle(title)
  
  ggplotly(p) 
}


simulation_plot <- function(num_samples, input.col, output.col,
                            facet.row, facet.col){

  print("In simdf")
  sim.df <- sim_state$sim$sample(num_samples)
  sim.df$Data_Source <- "Simulated"
  print("Got simulated simdf")

  ret <- get_dataset_from_cols(c(input.col, output.col, facet.row, facet.col))
  data <- ret[[1]]
  title <- ret[[2]]
  
  if(nrow(data) > 0) {
    data <- data[sample(1:nrow(data), min(num_samples, nrow(data))),, drop=FALSE]
    data$Data_Source <- "Raw Data"
    data <- CausalSimR:::factor_safe_bind_rows(data, sim.df)
    #print(head(data, 10))
  }
  else{
    data <- sim.df
  }
  
  p <- ggplot(data, aes_string(x = input.col, y = output.col, color = "Data_Source")) 
  
  if(class(data[[input.col]]) == "factor")
    p <- p+geom_jitter(width=0.1)
  else
    p <- p+geom_point() 
  
  # if at least one facet column/row is specified, add it
  facets <- paste(facet.row, '~', facet.col)
  if (facets != '. ~ .') p <- p + facet_grid(facets)
  
  p <- p + ggtitle(title)
  
  sim_state$sim.df <<- sim.df
  
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
