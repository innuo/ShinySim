data_tab <- tabItem(
  tabName = "data",
  
  br(),br(),br(),
  
  actionButton(inputId ="pairs_plot_button", label="Make Pairs Plot"),
  uiOutput('pairs_plot_checkbox'),
  
  br(), br(),
  plotlyOutput("pairs_plot")
  
  # tabsetPanel(
  #   id = 'dataset',
  #   tabPanel("diamonds", DT::dataTableOutput("mytable1")),
  #   tabPanel("mtcars", DT::dataTableOutput("mytable2")),
  #   tabPanel("iris", DT::dataTableOutput("mytable3"))
  # )
)
