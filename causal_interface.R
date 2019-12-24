library(CausalSimR)



sim_state <- list(dataset_list = list(),
                  dataset = NULL,
                  graph_list= NULL,
                  sim_df = NULL,
                  sim = NULL,
                  ready = FALSE)


attach_data <- function(path, header, missing, name){
  df <- read.csv(path, header = header == "True", na.strings = c("", "NA"))
  if(missing == "Drop") df <- na.omit(df)
  
  dataset_name <- tail(unlist(strsplit(name, "[/]|[\\\\]")), 1)
  sim_state$dataset_list[[dataset_name]] <<- df
  
  withProgress(message = 'Attaching Data', value = 0, {
    incProgress(1/2, detail = paste("(Rbind and Fill)"))
    if(is.null(sim_state$dataset)){
      sim_state$dataset <<- DataSet$new(df)
    }
    else{
      sim_state$dataset$attach_data(df)
    }
  }
  )
}


learn_models <- function(structure.json){
  withProgress(message = 'Learning Model', value = 0, {
    incProgress(1/2, detail = paste("(conditional samplers)"))
    sim_state$sim$structure_from_json_string(structure.json) 
    sim_state$sim$learn_samplers()
    print("Done learning")
  })
  
}

guess_causal_graph <- function(){
  sim <- CausalSimModel$new(sim_state$dataset)
  sim$learn_structure() 
  sim_state$sim <<- sim 
  sim_state$graph_list <<- sim$structure$to_list() 
}

sample_from_simulator <- function(){
  
  
}

