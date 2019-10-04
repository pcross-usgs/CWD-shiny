#' Shiny app server function
#'
#' Server for the comparison pane of the deterministic model
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

compare_det_plots_server <- function(input, output, session, simout){

      output$TotalPlot_det <- renderPlot({
        out <- simout()
        p1 <- plot_tots(out$counts)
        p1
      })

      output$PrevPlot_det <- renderPlot({
        out <- simout()
        p1 <- plot_prev_time(out$counts)
        p2 <- plot_prev_age_end(out$counts)
        p3 <- plot_grid(p1, p2, nrow = 1)
        p3
      })

      output$DeathPlot_det <- renderPlot({
        out <- simout()
        plot_deaths(out$deaths)
      })

}
