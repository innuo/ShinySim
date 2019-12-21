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
  
  
  observeEvent(values$num_datasets, output$pairs_plot_checkbox <- renderUI({
      rbind_data()
      choices <-  colnames(sim_state$dataset_rbinded)
      checkboxGroupInput("columns_checkbox","Select Columns to Plot", choices = choices, selected = choices[1:3], inline = TRUE)
    }))
  
  #observeEvent(input$pairs_plot_button, 
  #             output$pairs_plot <- renderPairsD3(pairs_plot(input$columns_checkbox)))
   
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
  
  
  observeEvent(input$my_left_tabs, output$dataplot_ui <- renderUI({
     validate(
      need(length(sim_state$dataset_list)>0, FALSE)
    )
    rbind_data()
    cols=names(sim_state$dataset_rbinded)
    print(cols)
    sidebarPanel(
    sliderInput('sampleSize', 'Sample Size', min = 100, max = 500,
                value = 200, step = 20, round = 0),
    selectInput('x', 'X', choices = cols, selected = cols[1]),
    selectInput('y', 'Y', choices = cols, selected = cols[2]),
    selectInput('color', 'Color', choices = cols),
    
    selectInput('facet_row', 'Facet Row', c(None = '.', cols)),
    selectInput('facet_col', 'Facet Column', c(None = '.', cols))
    )}
  ))
  
  observeEvent(input$pairs_plot_button, 
               output$data_plot <-  renderPlotly(data_plot(input$x, input$y, 
                                                                              input$sampleSize, input$color, 
                                                                              input$facet_row, input$facet_col
                                                                              )))
  
  #isolate({updateTabItems(session, "my_left_tabs", new_tab)})
}
