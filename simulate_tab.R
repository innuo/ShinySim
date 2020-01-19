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
    fluidRow(
      textOutput('fit_scores'),
      tags$head(tags$style("#fit_scores{color: blue;
                             font-size: 16px;
                         font-style: bold;
                         }"))),
    fluidRow(uiOutput("simulate_ui")),
    fluidRow(plotlyOutput("simulation_plot"))
  ),

  br()
)
