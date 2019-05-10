# Function for deterministic module UI
det_modUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 4, h4("Parameters"),
            tabsetPanel(
              tabPanel("Vital rates",
                       sliderInput(ns("fawn.an.sur"), "Fawn survival",
                                   value = 0.4, min = 0.1, max = .99, step = 0.05),
                       sliderInput(ns("juv.an.sur"), "Juvenile survival",
                                   value = 0.8, min = 0.3, max = .99, step = 0.05),
                       sliderInput(ns("ad.an.f.sur"),  "Adult female survival",
                                   value = 0.85,min = 0.5, max = .99, step = 0.01),
                       sliderInput(ns("ad.an.m.sur"), "Adult male survival",
                                   value = 0.85, min = 0.5, max = .99, step = 0.01),
                       sliderInput(ns("juv.repro"), "Fawns per Juv",
                                   value = 0.7, min = 0.2, max = 1.5, step = 0.1),
                       sliderInput(ns("ad.repro"), "Fawns per adult",
                                   value = 1.6, min = 0.7, max = 2, step = 0.1)),
              tabPanel("Disease",
                       sliderInput(ns("an.env.foi"), "Envi FOI",
                                   value = 0, min = 0, max = 0.1, step = 0.01),
                       sliderInput(ns("r0_peryear"), "R_0_per year",
                                   value = .8, min = .1, max = 3, step = .1),
                       sliderInput(ns("beta.m"), "Relative male infection",
                                   value = 1, min = 0.8, max = 3, step = 0.1),
                       sliderInput(ns("theta"), "Theta",
                                   value =1, min = 0, max = 1, step = 0.1),
                       sliderInput(ns("p"), "index of disease mortality",
                                   value = 0.43, min = 0, max = .9, step = 0.01),
                       sliderInput(ns("rel.risk"),"Relative hunting risk",
                                   value = 1, min = 0.1, max = 2, step = 0.1)),
              tabPanel("Hunting",
                       sliderInput(ns("hunt.mort.fawn"),"% fawns hunted",
                                   value = 0.03,min = 0.01, max = 0.1,step = 0.01),
                       sliderInput(ns("hunt.mort.juv"),"% juv hunted",
                                   value = 0.07,min = 0.01, max = 0.15,step = 0.01),
                       sliderInput(ns("hunt.mort.ad.f"),"% Does hunted",
                                   value = 0.08,min = 0.01, max = 0.2,step = 0.01),
                       sliderInput(ns("hunt.mort.ad.m"),"% bucks hunted",
                                   value = 0.15,min = 0.01, max = 0.5,step = 0.01)),
              tabPanel("Simulation",
                       sliderInput(ns("n0"),"Initial population size",
                                   value = 2000, min = 100, max = 6000,step = 10),
                       sliderInput(ns("n.years"),"# of years",
                                   value = 10, min = 5, max = 50, step = 1),
                       sliderInput(ns("ini.fawn.prev"), "Fawn prevalence",
                                   value = 0.01, min = 0, max = 0.4, step = 0.01),
                       sliderInput(ns("ini.juv.prev"), "Juv prevalence",
                                   value = 0.02, min = 0, max = 0.4, step = 0.01),
                       sliderInput(ns("ini.ad.f.prev"), "Doe prevalence",
                                   value = 0.03, min = 0, max = 0.4, step = 0.01),
                       sliderInput(ns("ini.ad.m.prev"), "Buck prevalence",
                                   value = 0.03, min = 0, max = 0.4, step = 0.01))
            )),
     column(width = 8, h4("Plots"),
            tabsetPanel(
              tabPanel("Totals", plotOutput(ns('TotalPlot')), hr(), hr(), 
                       h4(textOutput(ns('R0text1')))),
              
              tabPanel("Prevalence", plotOutput(ns('PrevPlot')), hr(), hr(), 
                       h4(textOutput(ns('R0text2')))),
              
              tabPanel("Deaths", plotOutput(ns('DeathPlot')), hr(), hr(), 
                       h4(textOutput(ns('R0text3')))),
              
              tabPanel("Ratios", plotOutput(ns('ClassPlot')), hr(), hr(), 
                       h4(textOutput(ns('R0text4')))),
              
              tabPanel("Age Distribution", plotOutput(ns('AgePlot')),hr(), hr(), 
                       h4(textOutput(ns('R0text5')))),
              
              tabPanel("Parameters", plotOutput(ns('ParamPlot')), hr(), hr(), 
                       h4(textOutput(ns('R0text6'))))
            )
          )
      )
)}
