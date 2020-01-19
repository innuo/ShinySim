cf_tab <- tabItem(
  tabName = "cf",
  
  br(),
  h1("Counterfactual Query (Beta)"),
  br(),
  fluidRow(
      column(12,
             div(DT::dataTableOutput("orig_data_table"), style = "font-size:80%;color=blue;font-style:bold")
             
             
  )),
  
  br(),
  fluidRow(
      column(12,div(DT::dataTableOutput("selected_row"), style = "font-size:110%"))
  ),
  fluidRow(
    column(12,div(DT::dataTableOutput("cf_row"), style = "font-size:110%"))
  ),
  br()
  
)