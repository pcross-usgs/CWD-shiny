compare_modUI <- function(id) {
  ns <- NS(id)
  
wellPanel(tagList(
    fluidRow(h4("Parameters"), 
      column(3,
             numericInput(ns("fawn.an.sur"), "Fawn survival", 
                          value = 0.7, min = 0.3, max = .99, step = 0.05),
              numericInput(ns("juv.an.sur"), "Juvenile survival",
                          value = 0.7, min = 0.3, max = .99, step = 0.05),
              numericInput(ns("ad.an.f.sur"),  "Adult female survival",
                          value = 0.85,min = 0.5, max = .99, step = 0.01),
              numericInput(ns("ad.an.m.sur"), "Adult male survival",
                          value = 0.8, min = 0.5, max = .99, step = 0.01),
              numericInput(ns("juv.repro"), "Fawns per Juv",
                          value = 0.9, min = 0.2, max = 1.5, step = 0.1),
              numericInput(ns("ad.repro"), "Fawns per adult",
                          value = 1.6, min = 0.5, max = 2, step = 0.1)),
            column(3, 
              numericInput(ns("an.env.foi"), "Envi FOI",
                          value = 0, min = 0, max = 0.1, step = 0.01),
              numericInput(ns("r0_peryear"), "R0_per year",
                          value = 1, min = 0.1, max = 3, step = 0.1),
              numericInput(ns("theta"), "Theta",
                          value = 1, min = 0, max = 1, step = 0.1),
              numericInput(ns("beta.m"), "Relative male infection",
                          value = 1, min = 0.5, max = 3, step = 0.1),
              numericInput(ns("p"), "index of disease mortality",
                          value = 0.43, min = 0, max = .9, step = 0.01),
              numericInput(ns("rel.risk"), "Relative Risk hunting infecteds",
                          value = 1, min = .1, max = 4, step = 0.1)),
            column(3, 
              numericInput(ns("hunt.mort.fawn"),"% fawns hunted",
                          value = 0.01, min = 0.01, max = 0.1,step = 0.01),
              numericInput(ns("hunt.mort.juv.f"),"% female 1.5yr hunted",
                          value = 0.05, min = 0.01, max = 0.15,step = 0.01),
              numericInput(ns("hunt.mort.juv.m"),"% male 1.5yr hunted",
                          value = 0.2, min = 0.01, max = 0.6,step = 0.01),
              numericInput(ns("hunt.mort.ad.f"),"% females hunted",
                          value = 0.05, min = 0.01, max = 0.2,step = 0.01),
              numericInput(ns("hunt.mort.ad.m"),"% males hunted",
                          value = 0.4, min = 0.01, max = 0.6,step = 0.01)),
             column(3,
              numericInput(ns("n.years"), "# of years",
                          value = 10, min = 5, max = 30, step = 5),
              numericInput(ns("sims"), "# of sims",
                          value = 20, min = 5, max = 50, step = 5),
              numericInput(ns("n0"), "Initial population",
                          value = 2000, min = 100, max = 8000, step = 100),
              numericInput(ns("ini.fawn.prev"), "Fawn prevalence",
                          value = 0.01, min = 0, max = 0.1, step = 0.01),
              numericInput(ns("ini.juv.prev"), "Juvenile prevalence",
                          value = 0.02, min = 0, max = 0.2, step = 0.01),
              numericInput(ns("ini.ad.f.prev"), "Female prevalence",
                          value = 0.03, min = 0, max = 0.4, step = 0.01),
              numericInput(ns("ini.ad.m.prev"), "Male prevalence",
                          value = 0.03, min = 0, max = 0.4, step = 0.01)),
      actionButton(ns("go"), "Run simulation"), hr(),
    h4("Plots"),
    h4(textOutput(ns('R0text1'))),
    plotOutput(ns('PrevPlot')), hr(),
    plotOutput(ns('TotalPlot')), hr(),
    plotOutput(ns('DeathPlot')),hr(),
    plotOutput(ns('ClassPlot'))
    )
    )
)
}