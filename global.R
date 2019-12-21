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


pairs_plot <- function(columns){
  n <- length(columns)
  data <- subset(sim_state$dataset_rbinded, select = columns)
  if(nrow(data) > 500) data <- data[sample(1:nrow(data), 500),]
  # p <- ggpairs(data=data, combo="dot",
  #              diag = list(continuous = "naDiag", discrete = "naDiag", na = "naDiag")) 
  # l <-ggplotly(p) 
  # l$x$layout$width <- NULL
  # l$x$layout$height <- NULL
  # l$width <- NULL
  # l$height <- NULL
  # l
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

# tabs
source("simulate_tab.R")
source("query_tab.R")
source("data_tab.R")
source("causal_interface.R")