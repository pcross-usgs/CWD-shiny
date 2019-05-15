compare_mod_plots_server2 <- function(input, output, session, simout_a, simout_b){

  output$TotsComparePlot <- renderPlot({
        outa <- simout_a()
        outb <- simout_b()
        p1 <- plot.compare.tots(outa$counts, outb$counts)
        p2 <- plot.compare.prev(outa$counts, outb$counts)
        plot_grid(p1,p2, nrow = 1)
      })

  output$HuntComparePlot <- renderPlot({
    outa <- simout_a()
    outb <- simout_b()
    p1 <- plot.compare.hunted(outa$deaths, outb$deaths)
    p2 <- plot.compare.hunted.end(outa$deaths, outb$deaths)
    p3 <- plot.compare.buckshunted(outa$deaths, outb$deaths)
    p4 <- plot.compare.buckshunted.end(outa$deaths, outb$deaths)
    plot_grid(p1,p2,p3,p4, nrow = 2)
  })

}
