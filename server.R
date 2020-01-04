source("global.R")

server = function(input, output, session) {
  
  sim_state <<- list(dataset_list = list(),
                    dataset = NULL,
                    graph_list= NULL,
                    sim_df = NULL,
                    sim = NULL,
                    ready = FALSE)
  
  num.datasets <- reactiveVal(0)
  simulation.done <- reactiveVal(FALSE)
  new.graph <- reactiveVal(0)
  graph.updated <- reactiveVal('none')

  graph_data <- reactiveValues(
    nodes = NULL,
    edges = NULL
  )
  
  
  output$num.datasets <- reactive(num.datasets())
  
  
  observeEvent(input$model_file$datapath,{
    sim_state <<- readRDS(input$model_file$datapath)
    update_causal_graph()
    num.datasets(length(sim_state$dataset_list))
    graph_data$nodes <- data.frame(id = sim_state$graph_list$nodes, 
                                   label=sim_state$graph_list$nodes)
    edges <- sim_state$graph_list$edges
    graph_data$edges <- cbind.data.frame(id = random_string(nrow(edges)),edges)
    graph.updated('yes')
    
  })

  observeEvent(input$file1$datapath, {
           attach_data(input$file1$datapath, 
                    input$header, input$missing, input$file1$name)
           num.datasets(num.datasets()+1)
           graph.updated('yes')
                 
   })
  
  ### Graph editing 
  observeEvent(input$file1$datapath, {
    shiny::validate(
      need(num.datasets()>0, FALSE)
    )
    guess_causal_graph()
    
    graph_data$nodes <- data.frame(id = sim_state$graph_list$nodes, 
                                   label=sim_state$graph_list$nodes)
    edges <- sim_state$graph_list$edges
    graph_data$edges <- cbind.data.frame(id = random_string(nrow(edges)),edges)
    
    graph.updated('yes')
  })
  
  ###
  observeEvent(graph.updated(), output$editable_network <- renderVisNetwork({
    shiny::validate(
      need(graph.updated() != 'none', FALSE)
    )
    
    visNetwork(graph_data$nodes, graph_data$edges, width="100%") %>%
      visHierarchicalLayout(direction = "LR", nodeSpacing=200, sortMethod="directed") %>%
        visEdges(arrows = 'to') %>%
        visOptions(manipulation = list(enabled = TRUE, addNode = FALSE,
                                       deleteNode = FALSE, multiple=TRUE))

     })
    )
  
  #on switch tab
  #observeEvent(input$switchtab, {
  #  newtab <- input$my_left_tabs
  #})
  
  
  ## Selection in data tab
  observeEvent(num.datasets(), output$dataplot_ui <- renderUI({
     shiny::validate(
       need(num.datasets()>0, FALSE)
    )

    cols <- sim_state$dataset$col.names.to.model() #TODO move to interface

    splitLayout(
    selectInput('x', 'X', choices = cols, selected = cols[1]),
    selectInput('y', 'Y', choices = cols, selected = cols[2]),
    selectInput('color', 'Color', choices = cols, selected = cols[2]),
    
    selectInput('facet_row', 'Facet Row', c(None = '.', cols)),
    selectInput('facet_col', 'Facet Column', c(None = '.', cols)),
    width = 6)}
  ))
  # Plot in data tab
  observeEvent(input$pairs_plot_button, 
               output$data_plot <-  renderPlotly(data_plot(input$x, input$y, 
                                                input$data_plot_sample_size, input$color, 
                                                input$facet_row, input$facet_col )
  ))

  observeEvent(input$simulate, {
    isolate({
      structure.json <- jsonlite::toJSON(list(nodes = graph_data$nodes$id, 
                                              edges=subset(graph_data$edges, select=c(from, to))))
      
      
      learn_models(structure.json)
      graph.updated('no')
    })
  })
  
  #Selection in simulate tab
  observeEvent(input$simulate, output$simulate_ui <- renderUI({
    shiny::validate(
      need(graph.updated() == 'no', FALSE)
    )
    
    #input_vars <- setdiff(graph_data$nodes$id, graph_data$edges$to)
    #output_vars <- setdiff(graph_data$nodes$id, graph_data$edges$from)
    input_vars <- sim_state$dataset$col.names.to.model() #TODO move to interface
    output_vars <- sim_state$dataset$col.names.to.model() #TODO move to interface
    #names(input_vars) <- input_vars
    
    selected_input_var <- ifelse(is.null(input$sim_plot_input_var), input_vars[1], input$sim_plot_input_var)
    selected_output_var <- ifelse(is.null(input$sim_plot_input_var), output_vars[1], input$sim_plot_output_var)
    
    splitLayout(
      selectInput('sim_plot_input_var', 'X', choices = input_vars, selected = selected_input_var),
      selectInput('sim_plot_output_var', 'Y', choices = output_vars, selected = selected_output_var),
      selectInput('sim_plot_facet_row', 'Facet Row', c(None = '.', input_vars)),
      selectInput('sim_plot_facet_col', 'Facet Column', c(None = '.', input_vars)),
      actionButton(inputId='plot_simulated_data', label="Plot"),
      downloadButton(outputId='save_model', label="Save Model"),
      width = 6)
     })
  )
  
  # Plot in simulate tab
  observeEvent(input$plot_simulated_data,  
            output$simulation_plot <-  renderPlotly({
              
              simulation_plot(input$n_sim_samples, 
                              input$sim_plot_input_var, input$sim_plot_output_var,
                              input$sim_plot_facet_row, input$sim_plot_facet_col
                              )
        }))
  
  # download model in simulate tab
  output$save_model <-  downloadHandler(
         filename = function() {
           "my_model_with_data.RDS"
         },
         content = function(file) {
           saveRDS(sim_state, file)
         }
    )
  
  ##### VISNETWORK
  observeEvent(input$editable_network_graphChange, {
    graph.updated('yes')
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
        temp = graph_data$edges
        temp = temp[temp$id != edge.id,]
        graph_data$edges = temp
      }
    }
  })
  
  output$graph_updated <- renderText({
    graph.updated()
  })
  
  outputOptions(output, "graph_updated", suspendWhenHidden=FALSE)
 
}
