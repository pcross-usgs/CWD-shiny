compare_det_plots_server <- function(input, output, session, simout){

      output$R0text1_det <- renderUI({
        out <- simout()
        str1 <- paste("Female direct transmission R0 = ", round(out$fem.R0, 1))
        str2 <- paste("Male direct transmission R0 = ", round(out$male.R0, 1))
        HTML(paste(str1, str2, sep="<br/>"))
      })
 
     output$TotalPlot_det <- renderPlot({
        out <- simout()
        p1 <- plot.tots(out$counts, error.bars = c(0.05, 0.95))
        p1
      })

      output$PrevPlot_det <- renderPlot({
        out <- simout()
        par(mar = c(6,6,1,1))
        plot.prev.2(out$counts, ylim = c(0, .7))
      })

      output$DeathPlot_det <- renderPlot({
        out <- simout()
        plot.deaths(out$deaths)
        #p2 <- plot.perc.deaths(out$deaths)
        #plot_grid(p1, p2, nrow = 2)
      })

}