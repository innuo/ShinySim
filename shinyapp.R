source("global.R")

shinyApp(
  ui = dashboardPagePlus(skin = "yellow", md = FALSE,
                         dashboardHeaderPlus(
                           fixed = TRUE,
                           title = tagList(
                             span(class = "logo-lg", "Causal Sim")),
                           enable_rightsidebar = TRUE,
                           rightSidebarIcon = "microscope",
                           left_menu = tagList(
                             dropdownBlock(
                               id = "select_num_samples",
                               title = "Number of Samples",
                               icon = "sliders",
                               badgeStatus = "warning",
                               sliderInput(
                                 inputId = "n_sim_samples",
                                 label = "Number of observations",
                                 min = 50, 
                                 max = 1000, 
                                 value = 100
                               )
                             ),
                             dropdownBlock(
                               id = "select_plotting",
                               title = "Plotting",
                               icon = "chart-bar",
                               badgeStatus = "warning",
                               prettyRadioButtons(
                                 inputId = "plotting_library",
                                 label = "Plotting Library", 
                                 thick = TRUE,
                                 choices = c("Plotly", "Bokeh"),
                                 animation = "pulse"
                               )
                             ),
                             dropdownBlock(
                               id = "upload_file",
                               title = "Attach Data",
                               icon = "file-upload",
                               badgeStatus = "warning",
                               fileInput("file1", "Choose CSV File",
                                         multiple = TRUE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               
                               # Horizontal line ----
                               tags$hr(),
                               prettyRadioButtons(
                                 inputId = "header",
                                 label = "Header", 
                                 thick = TRUE,
                                 choices = c("True", "False"),
                                 animation = "pulse"
                               )
                             )
                           ),
                           dropdownMenu(
                             type = "notifications",
                             icon = icon("question-circle"),
                             badgeStatus = NULL,
                             headerText = "Causal Sim (by Metonymize)",
                             messageItem("", "A tool to do blah, blah. Find out more at ... ", icon=icon("info"))
                           )
                         ),
                         dashboardSidebar(
                           sidebarMenu(
                             id="my_left_tabs",
                             menuItem(
                               text = "Causal Simulate", 
                               tabName = "sim",
                               icon = icon("dice-two")
                             ),
                           menuItem(
                             text = "Causal Query", 
                             tabName = "query",
                             icon = icon("question")
                           ),
                           menuItem(
                             text = "View Data", 
                             tabName = "data",
                             icon = icon("database")
                           )
                          )
                         ),
                         rightsidebar = rightSidebar(
                           background = "dark",
                           rightSidebarTabContent(
                             id = 1,
                             title = "Not Implemented",
                             icon = "project-diagram",
                             active = TRUE#,
                             #sliderInput(
                             #   inputId = "inputsidebar1", 
                             #  label = "Number of observations:",
                             #   min = 0, 
                             #   max = 1000, 
                             #   value = 500
                             #)
                           ),
                           rightSidebarTabContent(
                             id = 2,
                             icon = "paint-brush",
                             title = "Not Implemented"#,
                             #numericInput(
                            #   inputId = "inputsidebar3", 
                            #  label = "Observations:", 
                            #   value = 10, 
                            #   min = 1, 
                            #   max = 100
                            # )
                           )
                         ),
                         dashboardBody(
                           tags$head( 
                             tags$style(HTML(".main-sidebar { font-size: 16px; }"),
                                        HTML(".sidebar-menu li { margin-bottom: 20px; }")) 
                           ),
                           
                           # use a bit of shinyEffects
                           setShadow(class = "dropdown-menu"),
                           setShadow(class = "box"),
                           
                           shiny::tags$head(
                             shiny::tags$style(
                               rel = "stylesheet",
                               type = "text/css",
                               href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/qtcreator_dark.min.css"
                             ),
                             
                             shiny::tags$script(
                               src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
                             )
                           ),
                           
                           # All tabs
                           tabItems(
                              simulate_tab,
                              query_tab,
                              data_tab
                           #   buttons_tab,
                           #   box_elements_tab,
                           #   extra_elements_tab
                           )
                         ),
                         title = "shinyDashboardPlus",
                         footer = dashboardFooter(
                           left_text = "By Metonymize",
                           right_text = "© 2019"
                         )
  ),
  server = function(input, output, session) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
    
    output$distPlot2 <- renderPlot({
      hist(rnorm(input$obs2))
    })
    
    output$flipPlot <- renderPlot({
      hist(rnorm(input$flip_obs))
    })
    
    output$data <- renderTable({
      head(mtcars[, c("mpg", input$variable), drop = FALSE])
    }, rownames = TRUE)
    
    output$value <- renderText({input$somevalue})
    
    output$boxSidebarPlot <- renderPlot({
      hist(rnorm(input$slider_boxsidebar))
    })
    
    output$contents <- renderTable({
      # input$file1 will be NULL initially.
       req(input$file1)
       df <- read.csv(input$file1$datapath,
                     header = input$header == "True")
      return(df)
    })
  
    new_tab = "sim"
    observeEvent(input$switchtab, {
      newtab <- input$my_left_tabs
    })
    
    isolate({updateTabItems(session, "my_left_tabs", new_tab)})
  }
)