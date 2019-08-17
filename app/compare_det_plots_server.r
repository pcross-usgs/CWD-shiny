compare_det_plots_server <- function(input, output, session, simout){

      output$R0text1_det <- renderUI({
        out <- simout()
        str1 <- paste("Female direct transmission R0 = ", round(out$fem.R0, 1))
        str2 <- paste("Male direct transmission R0 = ", round(out$male.R0, 1))
        HTML(paste(str1, str2, sep = "<br/>"))
      })

     output$TotalPlot_det <- renderPlot({
        out <- simout()
        p1 <- plot_tots(out$counts, error.bars = c(0.05, 0.95))
        p1
      })

      output$PrevPlot_det <- renderPlot({
        out <- simout()
        p1 <- plot_prev_time(out$counts, ylim = c(0,1))
        p2 <- plot_prev_age_end(out$counts, ylim = c(0,1))
        p3 <- plot_grid(p1, p2, nrow = 1)
        p3
      })

      output$DeathPlot_det <- renderPlot({
        out <- simout()
        plot_deaths(out$deaths)
      })

}
