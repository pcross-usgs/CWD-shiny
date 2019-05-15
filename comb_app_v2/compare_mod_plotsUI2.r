compare_mod_plotsUI2 <- function(id){
  ns <- NS(id)
  tagList(
    h4("Comparison Plots"),
    h6("Both simulations need to run to display a comparison"),
    plotOutput(ns('TotsComparePlot')),
    plotOutput(ns('HuntComparePlot')),
    includeMarkdown("disclaimer.md")
    )
}