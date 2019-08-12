compare_det_plots_server2 <- function(input, output, session, simout_a, simout_b){

  output$ComparePlotDet <- renderPlot({
        outa <- simout_a()
        outb <- simout_b()
        plot.compare.all.det(outa, outb)
  })

}
