library(CausalSimR)



sim_state <- list(causal_graph = NULL,
                  dataset_list = list(),
                  sim_dataset = NULL,
                  sim = NULL,
                  ready = FALSE)


attach_data <- function(path, header, missing, name){
  df <- read.csv(path, header = header == "True", na.strings = c("", "NA"))
  if(missing == "Drop") df <- na.omit(df)
  print(path)
  dataset_name <- tail(unlist(strsplit(name, "[/]|[\\\\]")), 1)
  sim_state$dataset_list[[dataset_name]] <<- df
  print(length(sim_state$dataset_list))
}

get_causal_graph <- function(){
  
  
}

update_causal_graph <- function(){
  
}

sample_from_simulator <- function(){
  
  
}

