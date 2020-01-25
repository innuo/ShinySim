library(CausalSimR)



sim_state <- list(dataset_list = list(),
                  dataset = NULL,
                  graph_list= NULL,
                  sim = NULL,
                  fit_scores = NA)

sim_data <- list(sim_df=NULL,
                 ab_df=NULL)


attach_data <- function(path, header, missing, name){
  df <- read.csv(path, header = header == "True", na.strings = c("", "NA"))
  if(missing == "Drop") df <- na.omit(df)
  
  dataset_name <- tail(unlist(strsplit(name, "[/]|[\\\\]")), 1)
  sim_state$dataset_list[[dataset_name]] <<- df
  print("In attach")
  print(names(sim_state$dataset_list))
  
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
    fit_scores = sim_state$sim$learn_samplers(estimate.fit.score = TRUE)
    print("Done learning")
    sim_state$fit_scores <<- fit_scores
    print(fit_scores)
  })
  
}

guess_causal_graph <- function(){
  sim <- CausalSimModel$new(sim_state$dataset)
  sim$learn_structure() 
  sim_state$sim <<- sim 
  update_causal_graph() 
}

update_causal_graph<- function(){
  sim_state$graph_list <<- sim_state$sim$structure$to_list()
}


col.info <- function(col){
  type <- sim_state$dataset$col.types[col]
  if(type=='factor')
    choices <- levels(sim_state$dataset$data[[col]])
  else
    choices <- range(sim_state$dataset$data[[col]], na.rm = TRUE, finite = TRUE)
  
  #print (choices)
  return(list(type, choices))
}

sample_from_simulator <- function(){
  
  
}

