#' Shiny app server function
#'
#' Server for the bottom portion of the comparison pane of the
#' deterministic model.
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

compare_det_plots_server2 <- function(input, output, session,
                                      simout_a, simout_b){

  output$ComparePlotDet <- renderPlot({
        outa <- simout_a()
        outb <- simout_b()
        plot_compare_all_det(outa, outb)
  })

}
