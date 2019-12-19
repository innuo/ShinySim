library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

shinyApp(
  ui = dashboardPagePlus(md = FALSE,
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
                               prettyRadioButtons(
                                 inputId = "plotting_library",
                                 label = "Plotting Library", 
                                 thick = TRUE,
                                 choices = c("Plotly", "Bokeh"),
                                 animation = "pulse", 
                                 status = "primary"
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
                            #, userOutput("about")
                         ),
                         dashboardSidebar(
                           sidebarMenu(
                             menuItem(
                               text = "New rightSidebar", 
                               tabName = "rightsidebar",
                               badgeLabel = "new", 
                               badgeColor = "green",
                               icon = icon("gears")
                             ),
                             menuItem(
                               text = "Improved header", 
                               tabName = "header",
                               badgeLabel = "new", 
                               badgeColor = "green",
                               icon = icon("folder-open")
                             ),
                             menuItem(
                               text = "New boxes", 
                               tabName = "boxes",
                               badgeLabel = "new", 
                               badgeColor = "green",
                               icon = icon("briefcase")
                             ),
                             menuItem(
                               text = "New buttons", 
                               tabName = "buttons",
                               badgeLabel = "new", 
                               badgeColor = "green",
                               icon = icon("cubes")
                             ),
                             menuItem(
                               text = "New Box elements", 
                               tabName = "boxelements",
                               badgeLabel = "new", 
                               badgeColor = "green",
                               icon = icon("th")
                             ),
                             menuItem(
                               text = "New extra elements", 
                               tabName = "extraelements",
                               badgeLabel = "new", 
                               badgeColor = "green",
                               icon = icon("plus-circle")
                             )
                           )
                         ),
                         rightsidebar = rightSidebar(
                           background = "dark",
                           rightSidebarTabContent(
                             id = 1,
                             title = "Tab 1",
                             icon = "desktop",
                             active = TRUE,
                             sliderInput(
                               inputId = "inputsidebar1", 
                               label = "Number of observations:",
                               min = 0, 
                               max = 1000, 
                               value = 500
                             )
                           ),
                           rightSidebarTabContent(
                             id = 2,
                             title = "Tab 2",
                             textInput(
                               inputId = "inputsidebar2", 
                               label = "Caption", 
                               "Data Summary"
                             )
                           ),
                           rightSidebarTabContent(
                             id = 3,
                             icon = "paint-brush",
                             title = "Tab 3",
                             numericInput(
                               inputId = "inputsidebar3", 
                               label = "Observations:", 
                               value = 10, 
                               min = 1, 
                               max = 100
                             )
                           )
                         ),
                         dashboardBody(
                           
                           # use a bit of shinyEffects
                           setShadow(class = "dropdown-menu"),
                           setShadow(class = "box"),
                           
                           shiny::tags$head(
                             # shiny::includeCSS(
                             #   system.file("css", "qtcreator_dark.css", package = "shinydashboardPlus")
                             # ),
                             # shiny::includeScript(
                             #   system.file("js", "highlight.pack.js", package = "shinydashboardPlus")
                             # )
                             shiny::tags$style(
                               rel = "stylesheet",
                               type = "text/css",
                               href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/qtcreator_dark.min.css"
                             ),
                             
                             shiny::tags$script(
                               src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js"
                             )
                           )#,
                           
                           # All tabs
                           # tabItems(
                           #   rightsidebar_tab,
                           #   header_tab,
                           #   boxes_tab,
                           #   buttons_tab,
                           #   box_elements_tab,
                           #   extra_elements_tab
                           # )
                         ),
                         title = "shinyDashboardPlus",
                         footer = dashboardFooter(
                           left_text = "By Metonymize",
                           right_text = "© 2019"
                         )
  ),
  server = function(input, output) {
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
    
    # output$about <- renderUser({
    #   dashboardUser(
    #     name = "Causal Sim", 
    #     src = "https://upload.wikimedia.org/wikipedia/commons/1/1f/Feynmann_Diagram_Gluon_Radiation.svg", 
    #     title = "Metonymize",
    #     subtitle = "Author", 
    #     fluidRow(
    #       dashboardUserItem(
    #         width = 6,
    #         descriptionBlock(
    #           text = "A tool to do blah blah", 
    #           right_border = TRUE,
    #           margin_bottom = FALSE
    #         )
    #       )
    #     )
    #   )
    # }
    # )
  }
)