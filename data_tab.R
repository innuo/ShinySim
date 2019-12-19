data_tab <- tabItem(
  tabName = "data",
  
  br(),
  
  tabsetPanel(
    id = 'dataset',
    tabPanel("diamonds", DT::dataTableOutput("mytable1")),
    tabPanel("mtcars", DT::dataTableOutput("mytable2")),
    tabPanel("iris", DT::dataTableOutput("mytable3"))
  )
)