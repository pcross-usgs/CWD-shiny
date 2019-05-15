# CWD shiny app stochastic model user interface
rm(list = ls())
library(shiny)
library(reshape2)
library(popbio)
library(magrittr)
library(tidyverse)
library(cowplot)
library(ggridges)
library(knitr)
library(shinydashboard)

source("stoch_model_fxn_ver2.r", local = T)
source("det_pop_model_fxn_ver2.r", local = T)
source("plot_stoch_fxns.r", local = T)
source("plot_params.r", local = T)
source("estBetaParams.r", local = T)
source("det_modUI.r", local = T)
source("stoch_modUI.r", local = T)
source("stoch_mod.r", local = T)
source("compare_modUI.r", local = T)
source("compare_mod_plotsUI.r", local = T)
source("compare_mod_plotsUI2.r", local = T)
source("compare_mod_server.r", local = T)
source("compare_mod_plots_server.r", local = T)
source("compare_mod_plots_server2.r", local = T)
source("plot_compare_scenarios.r", local = T)
source("det_mod.r", local = T)
source("plot_fxns.r", local = T)
knit("description_combo.Rmd", quiet = T)

ui <- fluidPage(theme = "common.css",
  div(class = "header", includeHTML("www/header.html")),

  titlePanel(h4("Prepared in cooperation with Montana Fish, Wildlife and Parks"),
             windowTitle = "CWD model"),

  navbarPage("Chronic Wasting Disease Model",
                tabPanel("Description",
                      withMathJax(includeMarkdown("description_combo.md"))),
                tabPanel("Deterministic Model",
                        det_modUI(id = "det")),
                tabPanel("Stochastic Model",
                        stoch_modUI(id = "stoch")),
                tabPanel("Scenario comparison",
                          column(6, h3("Scenario A"),
                                  compare_modUI("scenario_a"),
                                  compare_mod_plotsUI("plots_a")
                                 ),
                          column(6, h3("Scenario B"),
                                compare_modUI("scenario_b"),
                                compare_mod_plotsUI("plots_b")
                                ), hr(),
                         fluidRow(compare_mod_plotsUI2("compare_plots"))
                           )
             ),
  #footer
  div(class = "footer", includeHTML("www/footer.html"))
)


server <- function(input, output, session) {
  callModule(stoch_mod, id = "stoch")
  callModule(det_mod, id = "det")

  output_a <- callModule(compare_mod_server, "scenario_a")
  output_b <- callModule(compare_mod_server, "scenario_b")

  plots_a <- callModule(compare_mod_plots_server, "plots_a", simout = output_a)
  plots_b <- callModule(compare_mod_plots_server, "plots_b", simout = output_b)
  compare_plots <- callModule(compare_mod_plots_server2, "compare_plots",
                              simout_a = output_a,
                              simout_b = output_b)

}

shinyApp(ui, server)

