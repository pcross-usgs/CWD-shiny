#' Shiny app UI function
#'
#' UI function to define the sliders and output plots for the single
#' deterministic model page of the application
#'
#' @param input provided by shiny
#'
#' @import shiny
#'
#' @export

det_modUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 4, h4("Parameters"),
            tabsetPanel(
              tabPanel("Vital rates",
                       sliderInput(ns("n.age.cats"),"Number of age categories",
                                   value = 18, min = 10, max = 20, step = 1),
                       sliderInput(ns("juvs"),"Juvenile Ages",
                                   value = c(1,2), min = 1, max = 20, step = 1),
                       sliderInput(ns("adults"),"Prime Adults",
                                   value = c(3,13), min = 1, max = 20, step = 1),
                       sliderInput(ns("old"),"Senecent Adults",
                                   value = c(14,18), min = 1, max = 20, step = 1),
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
                                   value = 1, min = 0.7, max = 2, step = 0.1),
                       includeText("vital_text.txt")),
              tabPanel("Predator Population",
                       sliderInput(ns("n.predators"), "Initial predator population",
                                   value = 70, min = 0, max = 200, step = 1),
                       sliderInput(ns("min.predators"), "Min. predator population",
                                   value = 20, min = 0, max = 200, step = 1),
                       sliderInput(ns("max.predators"), "Max. predator population",
                                   value = 150, min = 0, max = 200, step = 1),
                       sliderInput(ns("k_max"), "Max. per capita kill rate",
                                   value = 1, min = 0, max = 6, step = 0.01),
                       selectInput(ns("functional.form"), "Functional Response Type",
                                   choices = list("Type2","Type3"), selected = "Type3"),
                       sliderInput(ns("k_inflect"), "Inflection point for functional response",
                                   value = 3000, min = 1000, max = 5000, step = 100),
                       selectInput(ns("numeric.form"), "Numeric Response Type",
                                   choices = list("Type2","Type3"), selected = "Type2"),
                       sliderInput(ns("n_inflect"), "Inflection point for numerical response",
                                   value = 2000, min = 1000, max = 5000, step = 100)),
              tabPanel("Predator Selection",
                       selectInput(ns("selection.form"), "Form of increase in prey selection as CWD progresses",
                                   choices = list("exponential", "linear", "equal"), selected = "exponential"),
                       sliderInput(ns("r"), "Rate of increase in prey selection as CWD progresses",
                                   value = 0.25, min = 0, max = .5, step = 0.01),
                       sliderInput(ns("base.juv"), "Base selection on juveniles relative prime age adults",
                                   value = 3, min = 1, max = 5, step = 0.1), 
                       sliderInput(ns("base.old"), "Base selection on old relative prime age adults",
                                   value = 2, min = 1, max = 5, step = 0.1), 
                       sliderInput(ns("stages"), "Total number of CWD stages to include",
                                   value = c(0,10), min = 0, max = 10, step = 1),
                       includeText("pred_text.txt")),
              tabPanel("Disease",
                       sliderInput(ns("an.env.foi"), "Indirect transmission",
                                   value = 0, min = 0, max = 0.1, step = 0.01),
                       sliderInput(ns("beta.f.low"), "Female direct transmission",
                                   value = 0.025, min = 0, max = .1, step = 0.001),
                       sliderInput(ns("beta.m.low"), "Male direct transmission",
                                   value = 0.05, min = 0, max = 0.1, step = 0.001),
                       sliderInput(ns("S"), "Transmission increase for I8-I10",
                                   value = 5, min = 1, max = 10, step = 0.1),
                       sliderInput(ns("theta"), "Theta",
                                   value = 1, min = 0, max = 1, step = 0.1),
                       sliderInput(ns("p"), "index of disease mortality",
                                   value = 0.43, min = 0, max = .9, step = 0.01),
                       sliderInput(ns("rel.risk"),"Relative hunting risk",
                                   value = 1, min = 0.1, max = 3, step = 0.1),
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
                       sliderInput(ns("n0"),"Initial population size",
                                   value = 1000, min = 100, max = 20000,step = 100),
                       sliderInput(ns("n.years"),"# of years",
                                   value = 10, min = 5, max = 50, step = 1),
                       sliderInput(ns("ini.fawn.prev"), "Fawn prevalence",
                                   value = 0.01, min = 0, max = 0.4, step = 0.01),
                       sliderInput(ns("ini.juv.prev"), "Juvenile prevalence",
                                   value = 0.03, min = 0, max = 0.4, step = 0.01),
                       sliderInput(ns("ini.ad.f.prev"), "Male prevalence",
                                   value = 0.04, min = 0, max = 0.4, step = 0.01),
                       sliderInput(ns("ini.ad.m.prev"), "Female prevalence",
                                   value = 0.05, min = 0, max = 0.4, step = 0.01))
            )),
     column(width = 8, h4("Plots"),
            tabsetPanel(
              tabPanel("Totals", plotOutput(ns('TotalPlot')), hr(), hr(),
                       h4(htmlOutput(ns('R0text1')))),

              tabPanel("Prevalence", plotOutput(ns('PrevPlot')), hr(),
                       includeText("prev_text.txt"), hr(),
                       h4(htmlOutput(ns('R0text2')))),

              tabPanel("Deaths", plotOutput(ns('DeathPlot')), hr(), hr(),
                       h4(htmlOutput(ns('R0text3')))),
              
              tabPanel("Predator vs. Prey", plotOutput(ns('PredPreyPlot')), hr(), hr(),
                       h4(htmlOutput(ns('R0text4')))),
              
              tabPanel("Predation Params", plotOutput(ns('PredParamPlot')), hr(), hr(),
                       h4(htmlOutput(ns('R0text5')))),
              
              tabPanel("Predation by Age", plotOutput(ns('PredAge')), hr(), hr(),
                       h4(htmlOutput(ns('R0text6')))),
              
              tabPanel("Predation by Stage", plotOutput(ns('StagePlot')), hr(), hr(),
                       h4(htmlOutput(ns('R0text7')))),

              tabPanel("Ratios", plotOutput(ns('ClassPlot')), hr(), hr(),
                       h4(htmlOutput(ns('R0text8')))),

              tabPanel("Age Distribution", plotOutput(ns('AgePlot')),hr(),
                       includeText("age_text.txt"), hr(),
                       h4(htmlOutput(ns('R0text9')))),

              tabPanel("Parameters", plotOutput(ns('ParamPlot')), hr(),
                       includeText("param1_text.txt"), hr(),
                       h4(htmlOutput(ns('R0text10'))))
            ),
            includeMarkdown("disclaimer.md")
          )
      )
)}
