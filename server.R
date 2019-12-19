source("global.R")

server = function(input, output, session) {

  observeEvent(input$file1$datapath, attach_data(input$file1$datapath, input$header, input$missing, input$file1$name))
  
  values <- reactiveValues(num_datasets = length(sim_state$dataset_list))
  
  observeEvent(values$num_datasets, output$data_tables <- renderUI({
    # validate(
    #   need(length(sim_state$dataset_list)>0, FALSE)
    # )
    output_list <- list()
    data_table_names <- names(sim_state$dataset_list)
    myTabs = lapply(data_table_names, function(x) tabPanel(x, DT::renderDT(sim_state$dataset_list[[x]])))
    do.call(tabsetPanel, myTabs)
  }))
  
  graph_data = reactiveValues(
    nodes = sim_state$causal_graph$nodes,
    edges = sim_state$causal_graph$edges
  )
  
  # Render the graph.
  output$editable_network <- renderVisNetwork({
    validate(
      need(sim_state$causal_graph, FALSE)
    )
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visOptions(manipulation = T)
  })
  
  observeEvent(input$switchtab, {
    newtab <- input$my_left_tabs
  })
  
  #isolate({updateTabItems(session, "my_left_tabs", new_tab)})
}
