cf_tab <- tabItem(
  tabName = "cf",

  br(),
  h1("Counterfactual Query (Beta)"),
  
  fluidRow(
    column(12,actionButton("cf_reset", "Reset"))),
  br(),
  fluidRow(
      column(12,
             div(DT::dataTableOutput("orig_data_table"), style = "overflow-x: scroll;font-size:80%;color=blue;font-style:bold")
             
             
  )),
  
  br(),
  fluidRow(
      column(12,div(DT::dataTableOutput("selected_row"), style = "overflow-x: scroll;font-size:110%"))
  ),
  fluidRow(
    column(12,div(DT::dataTableOutput("cf_row"), style = "overflow-x: scroll;font-size:110%"))
  ),
  br()
  
)
