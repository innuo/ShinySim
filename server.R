source("global.R")

server = function(input, output, session) {
  
  num.datasets <- reactiveVal(0)
  simulation.done <- reactiveVal(FALSE)
  
  output$num.datasets <- reactive(num.datasets())

  observeEvent(input$file1$datapath, 
               attach_data(input$file1$datapath, input$header, input$missing, input$file1$name))
  observeEvent(input$file1$datapath, num.datasets(num.datasets()+1))
  
  ### Graph editing 
  observeEvent(num.datasets(), output$editable_network <- renderVisNetwork({
    shiny::validate(
      need(num.datasets()>0, FALSE)
    )
      graph_data$nodes <- data.frame(id = sim_state$graph_list$nodes, 
                                     label=sim_state$graph_list$nodes)

      graph_data$edges <- sim_state$graph_list$edges

      
      visNetwork(graph_data$nodes, graph_data$edges, width="100%") %>%
        visEdges(arrows = 'to') %>%
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
  observeEvent(num.datasets(), output$dataplot_ui <- renderUI({
     shiny::validate(
      need(num.datasets()>0, FALSE)
    )

    cols=names(sim_state$dataset$col.names.to.model()) #TODO move to interface

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
                                                input$facet_row, input$facet_col )
  ))

  
  #Selection in simulate tab
  observeEvent(input$simulate, output$simulate_ui <- renderUI({
    
    structure.json <- jsonlite::toJSON(list(nodes = graph_data$nodes$id, edges=graph_data$edges))
    
   
    #TODO move these lines to interface
    sim_state$sim$structure_from_json_string(structure.json) 
    sim_state$sim$learn_samplers()
    print("Done learning")
    
    input_vars <- setdiff(graph_data$edges$from, graph_data$edges$to)
    output_vars <- setdiff(graph_data$edges$to, graph_data$edges$from)
    
    print(input_vars)
    print(output_vars)
    
    
    print(sim_state$sim$structure_to_json_string())
    simulation.done(TRUE)
    splitLayout(
      selectInput('plot_input_var', 'Input Variable', choices = input_vars, selected = input_vars[1]),
      selectInput('plot_output_var', 'Output Variable', choices = output_vars, selected = output_vars[1]),
      width = 6)

    })
  )
  
  # Plot in simulate tab
  observeEvent(simulation.done(),  
            output$simulation_plot <-  renderPlotly({
              shiny::validate(
                need(simulation.done() == TRUE, FALSE)
              )
              print(input)
              simulation_plot(input$n_sim_samples, input$plot_input_var, input$plot_output_var)
        }))
  
  outputOptions(output, "graph_updated", suspendWhenHidden = FALSE)
 
}
