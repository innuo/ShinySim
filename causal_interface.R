library(CausalSimR)



sim_state <- list(dataset_list = list(),
                  dataset = NULL,
                  graph_list= NULL,
                  #sim_dataset = NULL,
                  sim = NULL,
                  ready = FALSE)


attach_data <- function(path, header, missing, name){
  df <- read.csv(path, header = header == "True", na.strings = c("", "NA"))
  if(missing == "Drop") df <- na.omit(df)
  
  dataset_name <- tail(unlist(strsplit(name, "[/]|[\\\\]")), 1)
  sim_state$dataset_list[[dataset_name]] <<- df
  
  if(is.null(sim_state$dataset)){
    sim_state$dataset <<- DataSet$new(df)
  }
  else{
    sim_state$dataset$attach_data(df)
  }
  print(sim_state$dataset)
  
  withProgress(message = 'Updating Model', value = 0, {

    incProgress(1/2, detail = paste("Initial imputation"))
    sim_state$dataset$fill_missing()
    incProgress(1/4, detail = paste("Guessing structure"))
    sim <- CausalSimModel$new(sim_state$dataset)
    print(head(sim$dataset$data))
    sim$learn_structure() 
  })
    
  sim_state$sim <<- sim 
  sim_state$graph_list <<- sim$structure$to_list() 
  
  #print(sim_state$graph_list)
}


get_causal_graph <- function(){
  
  
}

update_causal_graph <- function(){
  
}

sample_from_simulator <- function(){
  
  
}

