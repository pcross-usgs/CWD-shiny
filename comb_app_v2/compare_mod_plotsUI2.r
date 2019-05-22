compare_mod_plotsUI2 <- function(id){
  ns <- NS(id)
  tagList(
    h3("Comparison Plots"),
    h6("Distributions of simulated outcomes for scenarios A and B. Both simulations need to run to display a comparison"),
    plotOutput(ns('ComparePlot')), hr(),
    h4(htmlOutput(ns('CompareText'))), hr(),
    includeMarkdown("disclaimer.md")
    )
}