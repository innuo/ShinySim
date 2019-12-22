simulate_tab <- tabItem(
  tabName = "sim",
  br(),
  visNetworkOutput('editable_network'),


  splitLayout(
    conditionalPanel(
      condition = "output.graph_updated == true",
      actionButton(inputId ="simulate", label="Learn & Simulate")
    ),
    width = 4
  ),
  fluidRow(uiOutput("simulate_ui")),
  fluidRow(plotlyOutput("simulation_plot")),
  
  br()
)