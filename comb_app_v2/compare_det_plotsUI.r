compare_det_plotsUI <- function(id){
  ns <- NS(id)
  wellPanel(
    tagList(
      h4("Plots"),
      h4(htmlOutput(ns('R0text1_det')),
      plotOutput(ns('TotalPlot_det')), hr(),
      plotOutput(ns('DeathPlot_det')), hr(),
      plotOutput(ns('PrevPlot_det'))
      )
  )
  )
  
}