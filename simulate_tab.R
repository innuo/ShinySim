simulate_tab <- tabItem(
  tabName = "sim",
  
  br(),
  #actionButton(inputId ="make_graph", label="Guess Graph"),
 
  br(),
  visNetworkOutput('editable_network'),


  splitLayout(
    #textOutput("graph_updated"),
    conditionalPanel(
      condition = "output.graph_updated == 'yes'",
      actionButton(inputId ="simulate", label="Learn & Simulate")
    ),
    width = 4
  ),
  conditionalPanel(
    condition = "output.graph_updated == 'no'",
    fluidRow(span(textOutput("fit_scores"), style="color:blue;font-size:12px; font-style: bold")),
    fluidRow(uiOutput("simulate_ui")),
    fluidRow(plotlyOutput("simulation_plot"))
  ),

  br()
)
