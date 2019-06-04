compare_stoch_plotsUI <- function(id){
  ns <- NS(id)
  wellPanel(
    tagList(
      #    fluidRow(
      h4("Plots"),
      h4(htmlOutput(ns('R0text1'))),
      withSpinner(plotOutput(ns('PrevPlot'))), hr(),
      withSpinner(plotOutput(ns('TotalPlot'))), hr(),
      withSpinner(plotOutput(ns('DeathPlot')))
      )
  )
      #plotOutput(ns('ClassPlot')
}