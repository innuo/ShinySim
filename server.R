source("global.R")

server = function(input, output, session) {


  output$contents <- renderTable({
    # input$file1 will be NULL initially.
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header == "True", na.strings = c("", "NA"))
    return(df)
  })
  
  
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
