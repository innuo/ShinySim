data_tab <- tabItem(
  tabName = "data",
  
  br(),br(),br(),
  
  uiOutput('data_tables')  
  # tabsetPanel(
  #   id = 'dataset',
  #   tabPanel("diamonds", DT::dataTableOutput("mytable1")),
  #   tabPanel("mtcars", DT::dataTableOutput("mytable2")),
  #   tabPanel("iris", DT::dataTableOutput("mytable3"))
  # )
)