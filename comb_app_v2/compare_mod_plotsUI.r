compare_mod_plotsUI <- function(id){
  ns <- NS(id)
  wellPanel(
    tagList(
      #    fluidRow(
      h4("Plots"),
      h4(htmlOutput(ns('R0text1'))),
      plotOutput(ns('PrevPlot')), hr(),
      plotOutput(ns('TotalPlot')), hr(),
      plotOutput(ns('DeathPlot'))
      )
  )
      #plotOutput(ns('ClassPlot')
}