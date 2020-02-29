library(CausalSimR)



sim_state <- list(dataset_list = list(),
                  dataset = NULL,
                  graph_list= NULL,
                  sim = NULL,
                  fit_scores = NA)

sim_data <- list(sim_df=NULL,
                 ab_df=NULL)



check_data <- function(df){
  all_ok <- TRUE
  alert_text <- ""
  ii <- 0
  col.types <- do.call(rbind, lapply(df, class))
  unhandled.cols <- !(col.types[,1] %in% c("numeric", "factor"))
 
  if (any(unhandled.cols)){
    all_ok <- FALSE
    ii <- ii+1
    alert_text <- paste0(alert_text, "\n", ii, ". Not all columns are numeric or categorical")
  }
  for(i in 1:nrow(col.types)){
    if((col.types[i, 1] == "factor") && length(levels(df[,i])) > 10){
      all_ok <- FALSE
      ii <- ii+1
      alert_text <- paste0(alert_text, "\n", ii, ". Too many levels in column: ", names(df)[i])
    }
  }
 
  return(list(all_ok=all_ok, alert_text=alert_text)) 
}

attach_data <- function(path, header_rows, missing, name){
  groups <- NULL
  io_tags <- NULL
  skiplines = 0
  if("groups" %in% header_rows){
    header <- scan(path, nlines = 1, what = character())
    groups <- strsplit(header, ",")[[1]]
    skiplines = 1
  }
  if("io" %in% header_rows){
    header <- scan(path, nlines = 1, skip = skiplines, what = character())
    io_tags = tolower(strsplit(header, ",")[[1]])
    skiplines = skiplines + 1
  }

  df <- read.csv(path, header = "header" %in% header_rows, skip = skiplines, na.strings = c("", "NA"))
  if(missing == "Drop") df <- na.omit(df)
  
  
  
  dataset_name <- tail(unlist(strsplit(name, "[/]|[\\\\]")), 1)
  sim_state$dataset_list[[dataset_name]] <<- df
  print("In attach")
  print(names(sim_state$dataset_list))
  
  input_vars <- NULL
  output_vars <- NULL
  if(!is.null(io_tags)){
    input_vars <- names(df)[io_tags=="input"]
    output_vars <- names(df)[io_tags=="output"]
  }

  withProgress(message = 'Attaching Data', value = 0, {
    incProgress(1/2, detail = paste("(Rbind and Fill)"))
    if(is.null(sim_state$dataset)){
      sim_state$dataset <<- DataSet$new(df, input_vars=input_vars, output_vars=output_vars)
    }
    else{
      sim_state$dataset$attach_data(df)
    }
  }
  )
  return(check_data(df))
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

