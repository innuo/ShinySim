data_tab <- tabItem(
  tabName = "data",
  br(),br(),br(),
  fluidRow(
    column(3, actionButton(inputId ="pairs_plot_button", label="Make Plot"), offset=1),
     
    br(),br(), 
    uiOutput('dataplot_ui')),
  fluidRow(plotlyOutput("data_plot"))
  
  # tabsetPanel(
  #   id = 'dataset',
  #   tabPanel("diamonds", DT::dataTableOutput("mytable1")),
  #   tabPanel("mtcars", DT::dataTableOutput("mytable2")),
  #   tabPanel("iris", DT::dataTableOutput("mytable3"))
  # )
)
