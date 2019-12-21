simulate_tab <- tabItem(
  tabName = "sim",
  br(),
  visNetworkOutput('editable_network'),

  HTML('<hr {
    border-bottom: 2px groove;}'),

  splitLayout(
    conditionalPanel(
      condition = "output.graph_updated == true",
      actionButton(inputId ="simulate", label="Simulate")
    ),
    width = 4
  )
  #tableOutput("contents")
)