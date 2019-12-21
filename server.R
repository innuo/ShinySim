source("global.R")

server = function(input, output, session) {
  
  num_datasets <- reactiveVal(0)
  
  output$num_datasets <- reactive(num_datasets())

  observeEvent(input$file1$datapath, 
               attach_data(input$file1$datapath, input$header, input$missing, input$file1$name))
  observeEvent(input$file1$datapath, num_datasets(num_datasets()+1))
  
  # observeEvent(input$file1$datapath, output$pairs_plot_checkbox <- renderUI({
  #     choices <-  colnames(sim_state$dataset_rbinded)
  #     checkboxGroupInput("columns_checkbox","Select Columns to Plot", choices = choices, selected = choices[1:3], inline = TRUE)
  # }))
  
  ### Graph editing 
  observeEvent(num_datasets(), output$editable_network <- renderVisNetwork({
    validate(
      need(num_datasets()>0, FALSE)
    )
    withProgress(message = 'Updating Model', value = 0, {
      dataset <- DataSet$new(sim_state$dataset_rbinded)
      incProgress(1/2, detail = paste("Initial imputation"))
      dataset$fill_missing()
      incProgress(1/4, detail = paste("Guessing structure"))
      sim <- CausalSimModel$new(dataset)
      sim$learn_structure() ## TODO: use previous interactions
      })
      
      sim_state$sim <<- sim
    
      graph_list <- sim$structure$to_list()
      graph_data$nodes <- data.frame(id = graph_list$nodes, 
                                     label=graph_list$nodes)
      graph_data$edges <- graph_list$edges
      
      visNetwork(graph_data$nodes, graph_data$edges, width="100%") %>%
        visEdges(arrows = 'from') %>%
        visOptions(manipulation = T)
     })
    )

    graph_data <- reactiveValues(
                  nodes = NULL,
                  edges = NULL
                  )
    
    output$graph_updated <- reactive(!is.null(graph_data$nodes))
    
  
  #on switch tab
  #observeEvent(input$switchtab, {
  #  newtab <- input$my_left_tabs
  #})
  
  
  ## Selection in data tab
  observeEvent(num_datasets(), output$dataplot_ui <- renderUI({
     validate(
      need(length(sim_state$dataset_list)>0, FALSE)
    )

    cols=names(sim_state$dataset_rbinded)
    print(cols)
    splitLayout(
    sliderInput('sampleSize', 'Sample Size', min = 100, max = 500,
                value = 200, step = 20, round = 0),
    selectInput('x', 'X', choices = cols, selected = cols[1]),
    selectInput('y', 'Y', choices = cols, selected = cols[2]),
    selectInput('color', 'Color', choices = cols),
    
    selectInput('facet_row', 'Facet Row', c(None = '.', cols)),
    selectInput('facet_col', 'Facet Column', c(None = '.', cols)),
    width = 6)}
  ))
  # Plot in data tab
  observeEvent(input$pairs_plot_button, 
               output$data_plot <-  renderPlotly(data_plot(input$x, input$y, 
                                                                              input$sampleSize, input$color, 
                                                                              input$facet_row, input$facet_col
                                                                              )))
  outputOptions(output, "graph_updated", suspendWhenHidden = FALSE)
 
}
