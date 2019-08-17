compare_det_plotsUI2 <- function(id){
  ns <- NS(id)
  tagList(
    h3("Comparison Plots"),
    plotOutput(ns('ComparePlotDet')), hr(),
    includeMarkdown("disclaimer.md")
    )
}