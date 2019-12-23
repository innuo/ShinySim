source("global.R")

server = function(input, output, session) {
  
  num.datasets <- reactiveVal(0)
  simulation.done <- reactiveVal(FALSE)
  graph_data <- reactiveValues(
    nodes = NULL,
    edges = NULL
  )
  
  
  output$num.datasets <- reactive(num.datasets())

  observeEvent(input$file1$datapath, 
               attach_data(input$file1$datapath, input$header, input$missing, input$file1$name))
  observeEvent(input$file1$datapath, num.datasets(num.datasets()+1))
  
  ### Graph editing 
  observeEvent(input$make_graph, {
    shiny::validate(
      need(!is.null(sim_state$dataset), FALSE)
    )
    graph_data$nodes <- data.frame(id = sim_state$graph_list$nodes, 
                                   label=sim_state$graph_list$nodes)
    edges <- sim_state$graph_list$edges
    graph_data$edges <- cbind.data.frame(id = random_string(nrow(edges)),edges)
    
    print(graph_data$edges)
  })
  
  ###
  observeEvent(input$make_graph, output$editable_network <- renderVisNetwork({
    shiny::validate(
      need(!is.null(graph_data$nodes), FALSE)
    )
      visNetwork(graph_data$nodes, graph_data$edges, width="100%") %>%
        visEdges(arrows = 'to') %>%
        visOptions(manipulation = T)

     })
    )

    output$graph_updated <- reactive(!is.null(graph_data$nodes))
  
  #on switch tab
  #observeEvent(input$switchtab, {
  #  newtab <- input$my_left_tabs
  #})
  
  
  ## Selection in data tab
  observeEvent(num.datasets(), output$dataplot_ui <- renderUI({
     shiny::validate(
       need(!is.null(sim_state$dataset), FALSE)
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
    
    print(graph_data$nodes)
    print(graph_data$edges)
    
    structure.json <- jsonlite::toJSON(list(nodes = graph_data$nodes$id, 
                                            edges=subset(graph_data$edges, select=c(from, to))))
    
   
    #TODO move these lines to interface
    sim_state$sim$structure_from_json_string(structure.json) 
    sim_state$sim$learn_samplers()
    print("Done learning")
    
    input_vars <- setdiff(graph_data$edges$from, graph_data$edges$to)
    output_vars <- setdiff(graph_data$edges$to, graph_data$edges$from)
    
    print(input_vars)
    print(output_vars)
    
    
    print(sim_state$sim$structure_to_json_string())
    
    splitLayout(
      selectInput('plot_input_var', 'Input Variable', choices = input_vars, selected = input_vars[1]),
      selectInput('plot_output_var', 'Output Variable', choices = output_vars, selected = output_vars[1]),
      actionButton(inputId='plot_simulated_data', label="Plot"),
      width = 6)

    
    })
  )
  
  # Plot in simulate tab
  observeEvent(input$plot_simulated_data,  
            output$simulation_plot <-  renderPlotly({
              shiny::validate(
                need(simulation.done() == TRUE, FALSE)
              )
              print(input)
              simulation_plot(input$n_sim_samples, input$plot_input_var, input$plot_output_var)
        }))
  
  ##### VISNETWORK
  observeEvent(input$editable_network_graphChange, {
    # If the user added a node, add it to the data frame of nodes.
    if(input$editable_network_graphChange$cmd == "addNode") {
      temp = bind_rows(
        graph_data$nodes,
        data.frame(id = input$editable_network_graphChange$id,
                   label = input$editable_network_graphChange$label,
                   stringsAsFactors = F)
      )
      graph_data$nodes = temp
    }
    # If the user added an edge, add it to the data frame of edges.
    else if(input$editable_network_graphChange$cmd == "addEdge") {
      from = input$editable_network_graphChange$from
      to = input$editable_network_graphChange$to
      temp = bind_rows(
        graph_data$edges,
        data.frame(id = random_string(1), #input$editable_network_graphChange$id,
                   from = from,
                   to = to,
                   stringsAsFactors = F)
      )
      graph_data$edges = temp
    }
    # If the user edited a node, update that record.
    else if(input$editable_network_graphChange$cmd == "editNode") {
      temp = graph_data$nodes
      temp$label[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$label
      graph_data$nodes = temp
    }
    # If the user edited an edge, update that record.
    else if(input$editable_network_graphChange$cmd == "editEdge") {
      temp = graph_data$edges
      temp$from[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$from
      temp$to[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$to
      graph_data$edges = temp
    }
    # If the user deleted something, remove those records.
    else if(input$editable_network_graphChange$cmd == "deleteElements") {
      for(node.id in input$editable_network_graphChange$nodes) {
        temp = graph_data$nodes
        temp = temp[temp$id != node.id,]
        graph_data$nodes = temp
      }
      for(edge.id in input$editable_network_graphChange$edges) {
        print(">>>>>>>>>")
        print (edge.id)
        temp = graph_data$edges
        temp = temp[temp$id != edge.id,]
        graph_data$edges = temp
      }
    }
  })
  
  outputOptions(output, "graph_updated", suspendWhenHidden = FALSE)
 
}
