#' Shiny app UI function
#'
#' @import shiny


ui <- fluidPage(theme = "common.css",
                div(class = "header", includeHTML("www/header.html")),

                titlePanel(h4("Prepared in cooperation with Montana Fish,
                Wildlife and Parks"),
                           windowTitle = "CWD model"),

                navbarPage("Chronic Wasting Disease",
                           tabPanel("Description",
                                    withMathJax(includeHTML("description_combo.html"))),
                           tabPanel("Deterministic model",
                                    det_modUI(id = "det")),
                           tabPanel("Stochastic model",
                                    stoch_modUI(id = "stoch")),
                           tabPanel("Deterministic comparison",
                                    column(6, h3("Scenario A"),
                                           compare_detUI("scenario_det_a"),
                                           compare_det_plotsUI("plots_det_a")
                                    ),

                                    column(6, h3("Scenario B"),
                                           compare_detUI("scenario_det_b"),
                                           compare_det_plotsUI("plots_det_b")
                                    ),
                                    hr(),
                                    fluidRow(compare_det_plotsUI2("compare_plots_det"))
                           ),
                           tabPanel("Stochastic comparison",
                                    includeMarkdown("stoch_text.md"),
                                    column(6, h3("Scenario A"),
                                           compare_stochUI("scenario_a"),
                                           compare_stoch_plotsUI("plots_a")
                                    ),
                                    column(6, h3("Scenario B"),
                                           compare_stochUI("scenario_b"),
                                           compare_stoch_plotsUI("plots_b")
                                    ), hr(),
                                    fluidRow(compare_stoch_plotsUI2("compare_plots"))
                           )
                ),
                #footer
                div(class = "footer", includeHTML("www/footer.html"))
)
