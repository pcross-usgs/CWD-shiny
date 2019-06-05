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
library(markdown)
library(shinydashboard)
library(shinycssloaders)

source("estBetaParams.r", local = T)
source("stoch_model_fxn_ver2.r", local = T)
source("det_pop_model_fxn_ver2.r", local = T)
source("plot_compare_scenarios.r", local = T)
source("plot_fxns.r", local = T)
source("plot_stoch_fxns.r", local = T)
source("plot_params.r", local = T)
source("det_mod.r", local = T)
source("det_modUI.r", local = T)
source("stoch_modUI.r", local = T)
source("stoch_mod.r", local = T)
source("compare_stochUI.r", local = T)
source("compare_stoch_plotsUI.r", local = T)
source("compare_stoch_plotsUI2.r", local = T)
source("compare_stoch_server.r", local = T)
source("compare_stoch_plots_server.r", local = T)
source("compare_stoch_plots_server2.r", local = T)
source("compare_detUI.r", local = T)
source("compare_det_plotsUI.r", local = T)
source("compare_det_server.r", local = T)
source("compare_det_plots_server.r", local = T)

#knit("description_combo.Rmd", quiet = T)

ui <- fluidPage(theme = "common.css",
  div(class = "header", includeHTML("www/header.html")),

  titlePanel(h4("Prepared in cooperation with Montana Fish, Wildlife and Parks"),
             windowTitle = "CWD model"),

  navbarPage("Chronic Wasting Disease Model",
                tabPanel("Description",
                         withMathJax(includeHTML("description_combo.html"))),
                      #withMathJax(includeMarkdown("description_combo.Rmd"))),
                tabPanel("Deterministic Model",
                        det_modUI(id = "det")),
                tabPanel("Stochastic Model",
                        stoch_modUI(id = "stoch")),
             tabPanel("Deterministic comparison",
                      column(6, h3("Scenario A"),
                             compare_detUI("scenario_det_a"),
                             compare_det_plotsUI("plots_det_a")
                      ),
                      column(6, h3("Scenario B"),
                             compare_detUI("scenario_det_b"),
                             compare_det_plotsUI("plots_det_b")),
                      includeMarkdown("disclaimer.md")
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

server <- function(input, output, session) {
  callModule(stoch_mod, id = "stoch")
  callModule(det_mod, id = "det")

  out_det_a <- callModule(compare_det_server, "scenario_det_a")
  out_det_b <- callModule(compare_det_server, "scenario_det_b")
  
  callModule(compare_det_plots_server, "plots_det_a", 
                            simout = out_det_a)
  callModule(compare_det_plots_server, "plots_det_b", 
                            simout = out_det_b)

  out_stoch_a <- callModule(compare_stoch_server, "scenario_a")
  out_stoch_b <- callModule(compare_stoch_server, "scenario_b")

  callModule(compare_stoch_plots_server, "plots_a", 
                        simout = out_stoch_a)
  callModule(compare_stoch_plots_server, "plots_b", 
                        simout = out_stoch_b)
  callModule(compare_stoch_plots_server2, "compare_plots",
                              simout_a = out_stoch_a,
                              simout_b = out_stoch_b)

}

shinyApp(ui, server)

