
server <- function(input, output, session) {
  callModule(det_mod_server, id = "det")

  out_det_a <- callModule(compare_det_server, "scenario_det_a")
  out_det_b <- callModule(compare_det_server, "scenario_det_b")

  callModule(compare_det_plots_server, "plots_det_a",
             simout = out_det_a)
  callModule(compare_det_plots_server, "plots_det_b",
             simout = out_det_b)

  callModule(compare_det_plots_server2, "compare_plots_det",
             simout_a = out_det_a,
             simout_b = out_det_b)
  
  session$onSessionEnded(stopApp)
}
