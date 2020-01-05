query_tab <- tabItem(
  tabName = "query",
  
  br(),
  
  fluidRow(
    width = 12,
    align = "center",
    h1("A/B Testing")
  ),
  br(),
  conditionalPanel(
    condition = "output.learning_done == 'yes'",
    uiOutput("ab_dropdown"),
    uiOutput("ab_choices"),
    actionButton("run_ab_test", "Run Experiment")
  ),
  fluidRow(plotlyOutput("ab_test_plot")),
  br()
  
)
