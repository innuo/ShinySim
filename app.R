if("CausalSimR" %in% rownames(installed.packages()) == FALSE)
  devtools::install_github("innuo/CausalSimR")

shinyApp(ui, server)