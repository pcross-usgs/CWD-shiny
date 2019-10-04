#' Shiny server function for the stochastic comparison
#'
#' server function to define the plots of the stochastic comparison
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

compare_stoch_plots_server <- function(input, output, session, simout){


      output$R0text1 <- renderUI({
        out <- simout()
        str1 <- paste("Female direct transmission R0 = ", round(out$fem.R0, 1))
        str2 <- paste("Male direct transmission R0 = ", round(out$male.R0, 1))
        HTML(paste(str1, str2, sep="<br/>"))
      })
      output$TotalPlot <- renderPlot({
        out <- simout()
        p1 <- plot_stoch_disease(out$counts, error.bars = c(0.05, 0.95))
        p1
      })

      output$PrevPlot <- renderPlot({
        out <- simout()
        p1 <- plot_stoch_prev(out$counts, all.lines = T, error.bars = c(0.05, 0.95))
        p2 <- plot_stoch_prev_age_end(out$counts, error.bars = c(0.05, 0.95))
        plot_grid(p1, p2, nrow = 1)
      })

      output$DeathPlot <- renderPlot({
        out <- simout()
        plot_stoch_perc_deaths(out$deaths, error.bars = c(0.05, 0.95))
      })

      output$DeathPlot2 <- renderPlot({
        out <- simout()
        plot_stoch_deaths(out$deaths, error.bars = c(0.05, 0.95))
      })

}
