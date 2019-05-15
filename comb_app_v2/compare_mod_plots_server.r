compare_mod_plots_server <- function(input, output, session, simout){


      output$R0text1 <- renderUI({
        out <- simout()
        str1 <- paste("Female Direct transmission R0 = ", round(out$fem.R0, 2))
        str2 <- paste("Male Direct transmission R0 = ", round(out$male.R0, 2))
        HTML(paste(str1, str2, sep="<br/>"))
      })
      output$TotalPlot <- renderPlot({
        out <- simout()
        p1 <- plot.stoch.tots.2(out$counts, error.bars = c(0.05, 0.95))
        p1
      })

      output$PrevPlot <- renderPlot({
        out <- simout()
        p1 <- plot.stoch.prev(out$counts, all.lines = T, error.bars = T,
                              cis <- c(0.05, 0.95))
        p2 <- plot.stoch.prev.age.2(out$counts, error.bars = c(0.05, 0.95))
        plot_grid(p1, p2, nrow = 1)
      })

      output$DeathPlot <- renderPlot({
        out <- simout()
        #p1 <- plot.stoch.deaths(out$deaths, error.bars = c(0.05, 0.95))
        plot.stoch.perc.deaths(out$deaths, error.bars = c(0.05, 0.95))
        #plot_grid(p1, p2, nrow = 2)
      })

      #plot fawn.adult and buck:doe
      output$ClassPlot <- renderPlot({
        out <- simout()
        p1 <- plot.stoch.fawn.adult(out$counts, all.lines = T, error.bars = c(0.05, 0.95))
        p2 <- plot.stoch.buck.doe(out$counts, all.lines = T, error.bars = c(0.05, 0.95))
        plot_grid(p1, p2)
      })

}
