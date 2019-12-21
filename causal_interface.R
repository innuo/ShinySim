library(CausalSimR)



sim_state <- list(dataset_list = list(),
                  dataset_rbinded = NULL,
                  causal_graph = NULL,
                  #sim_dataset = NULL,
                  sim = NULL,
                  ready = FALSE)


attach_data <- function(path, header, missing, name){
  df <- read.csv(path, header = header == "True", na.strings = c("", "NA"))
  if(missing == "Drop") df <- na.omit(df)
  #print(path)
  dataset_name <- tail(unlist(strsplit(name, "[/]|[\\\\]")), 1)
  sim_state$dataset_list[[dataset_name]] <<- df
  rbind_data()
}

rbind_data <- function(){
  sim_state$dataset_rbinded <<- do.call(rbind.fill, sim_state$dataset_list)
}

get_causal_graph <- function(){
  
  
}

update_causal_graph <- function(){
  
}

sample_from_simulator <- function(){
  
  
}

