#' Shiny user interface for the stochastic CWD model
#'
#' defines the sliders and output plots for the main stoch. model page
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

stoch_modUI <- function(id) {
  ns <- NS(id)
tagList(
fluidRow(
    column(4, h4("Parameters"), actionButton(ns("go"), "Run simulation"),
      tabsetPanel(
      tabPanel("Vital rates",
               sliderInput(ns("fawn.an.sur"), "Fawn survival",
                           value = 0.6, min = 0.1, max = .99, step = 0.05),
               sliderInput(ns("juv.an.sur"), "Juvenile survival",
                           value = 0.8, min = 0.3, max = .99, step = 0.05),
               sliderInput(ns("ad.an.f.sur"),  "Adult female survival",
                           value = 0.95,min = 0.5, max = .99, step = 0.01),
               sliderInput(ns("ad.an.m.sur"), "Adult male survival",
                           value = 0.9, min = 0.5, max = .99, step = 0.01),
               sliderInput(ns("juv.repro"), "Fawns per Juv",
                           value = 0.6, min = 0.2, max = 1.5, step = 0.1),
               sliderInput(ns("ad.repro"), "Fawns per adult",
                           value = 1, min = 0.5, max = 2, step = 0.1),
               includeText("vital_text.txt")),

      tabPanel("Disease",
               sliderInput(ns("an.env.foi"), "Indirect transmission",
                           value = 0, min = 0, max = 0.1, step = 0.01),
               sliderInput(ns("r0_peryear"), "Direct R0 per year",
                           value = .8, min = 0.1, max = 1.5, step = 0.05),
               sliderInput(ns("theta"), "Theta",
                           value = 1, min = 0, max = 1, step = 0.1),
               sliderInput(ns("gamma.m"), "Relative male infection",
                           value = 2, min = 0.5, max = 5, step = 0.1),
               sliderInput(ns("p"), "index of disease mortality",
                           value = 0.43, min = 0, max = .9, step = 0.01),
               sliderInput(ns("rel.risk"), "Relative risk hunting infecteds",
                           value = 1, min = .1, max = 3, step = 0.1),
               includeMarkdown("disease_text.md")),

      tabPanel("Hunting",
               sliderInput(ns("hunt.mort.fawn"),"% of fawns hunted",
                           value = 0.01, min = 0.01, max = 0.1,step = 0.01),
               sliderInput(ns("hunt.mort.juv.f"),"% of female 1.5yr hunted",
                           value = 0.1, min = 0.01, max = 0.8,step = 0.05),
               sliderInput(ns("hunt.mort.juv.m"),"% of male 1.5yr hunted",
                           value = 0.1, min = 0.01, max = 0.8,step = 0.05),
               sliderInput(ns("hunt.mort.ad.f"),"% of females hunted",
                           value = 0.1, min = 0.01, max = 0.8,step = 0.05),
               sliderInput(ns("hunt.mort.ad.m"),"% of males hunted",
                           value = 0.2, min = 0.01, max = 0.8,step = 0.05),
               includeText("hunt_text.txt")),

      tabPanel("Simulation",
               sliderInput(ns("n.years"), "# of years",
                           value = 10, min = 5, max = 30, step = 5),
               sliderInput(ns("sims"), "# of sims",
                           value = 30, min = 10, max = 100, step = 5),
               sliderInput(ns("n0"), "Initial population",
                           value = 1000, min = 100, max = 5000, step = 100),
               sliderInput(ns("ini.fawn.prev"), "Fawn prevalence",
                           value = 0.01, min = 0, max = 0.1, step = 0.01),
               sliderInput(ns("ini.juv.prev"), "Juvenile prevalence",
                           value = 0.03, min = 0, max = 0.4, step = 0.01),
               sliderInput(ns("ini.ad.f.prev"), "Female prevalence",
                           value = 0.04, min = 0, max = 0.4, step = 0.01),
               sliderInput(ns("ini.ad.m.prev"), "Male prevalence",
                           value = 0.04, min = 0, max = 0.4, step = 0.01)),
      tabPanel("Variance",
               sliderInput(ns("fawn.sur.var"), "Fawns survival variance",
                           value = .005, min = 0.0001, max = .01, step = 0.0001),
               sliderInput(ns("sur.var"), "Survival variance",
                           value = .005, min = 0.0001, max = .01, step = 0.0001),
               sliderInput(ns("repro.var"), "Reproduction variance",
                           value = .005, min = 0.0001, max = .01, step = 0.0001),
               sliderInput(ns("hunt.var"),"Hunting variance",
                           value = 0.005, min = 0.0001, max = 0.01, step = 0.0001))
      )
     ),
    column(8, h4("Plots"),
           includeMarkdown("stoch_text.md"),
           tabsetPanel(
             tabPanel("Totals", plotOutput(ns('TotalPlot')), hr(), hr(),
                      h4(htmlOutput(ns('R0text1')))),
             tabPanel("Prevalence", plotOutput(ns('PrevPlot')), hr(),
                      includeText("prev_text.txt"), hr(),
                      h4(htmlOutput(ns('R0text2')))),
             tabPanel("Deaths", plotOutput(ns('DeathPlot')), hr(),hr(),
                      h4(htmlOutput(ns('R0text3')))),
             tabPanel("Ratios", plotOutput(ns('ClassPlot')), hr(),hr(),
                      h4(htmlOutput(ns('R0text4')))),
             tabPanel("Age Distribution", plotOutput(ns('AgePlot')), hr(),
                      includeText("age_text.txt"), hr(),
                      h4(htmlOutput(ns('R0text5')))),
             tabPanel("Parameters", plotOutput(ns('ParamPlot')), hr(),
                      includeText("param2_text.txt"), hr(),
                      h4(htmlOutput(ns('R0text6'))))
           ),
           includeMarkdown("disclaimer.md")
        )
      )
)
}
