#' Shiny app UI function
#'
#' @import shiny


ui <- fluidPage(theme = "common.css",
                div(class = "header", includeHTML("www/header.html")),

                titlePanel(h4(""),
                           windowTitle = "CWD model"),

                navbarPage("CWD Predator Model",
                           tabPanel("Description",
                                    withMathJax(includeHTML("description_combo.html"))),
                           tabPanel("Deterministic model",
                                    det_modUI(id = "det")),
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
                           )
                ),
                #footer
                div(class = "footer", includeHTML("www/footer.html"))
)
