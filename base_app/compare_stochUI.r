#' Shiny app UI function
#'
#' UI that defines the stochastic parameter sliders.
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

compare_stochUI <- function(id) {
  ns <- NS(id)
  wellPanel(
    tagList(
      column(3,
             numericInput(ns("fawn.an.sur"), "Fawn survival",
                          value = 0.6, min = 0.3, max = .99, step = 0.05),
             numericInput(ns("juv.an.sur"), "Juvenile survival",
                          value = 0.8, min = 0.3, max = .99, step = 0.05),
             numericInput(ns("ad.an.f.sur"),  "Adult female survival",
                          value = 0.95,min = 0.5, max = .99, step = 0.01),
             numericInput(ns("ad.an.m.sur"), "Adult male survival",
                          value = 0.9, min = 0.5, max = .99, step = 0.01),
             numericInput(ns("juv.repro"), "Fawns per Juv",
                          value = 0.6, min = 0.2, max = 1.5, step = 0.1),
             numericInput(ns("ad.repro"), "Fawns per adult",
                          value = 1, min = 0.5, max = 2, step = 0.1),
             numericInput(ns("repro.var"), "Reproduction variance",
                         value = .005, min = 0.001, max = .01, step = 0.001)),
      column(3,
             numericInput(ns("an.env.foi"), "Envi FOI",
                          value = 0, min = 0, max = 0.1, step = 0.01),
             numericInput(ns("r0_peryear"), "Direct R0 per year",
                          value = .8, min = 0.5, max = 1.5, step = 0.05),
             numericInput(ns("theta"), "Theta",
                          value = 1, min = 0, max = 1, step = 0.1),
             numericInput(ns("gamma.m"), "Relative male infection",
                          value = 2, min = 0.5, max = 5, step = 0.1),
             numericInput(ns("p"), "Disease mortality index",
                          value = 0.43, min = 0, max = .9, step = 0.01),
             numericInput(ns("rel.risk"), "Relative Risk hunting infecteds",
                          value = 1, min = .1, max = 3, step = 0.1),
             numericInput(ns("fawn.sur.var"), "Fawns survival variance",
                          value = .005, min = 0.001, max = .01, step = 0.001)),
      column(3,
             numericInput(ns("hunt.mort.fawn"),"% of fawns hunted",
                          value = 0.01, min = 0.01, max = 0.1,step = 0.01),
             numericInput(ns("hunt.mort.juv.f"),"% of female 1.5yr hunted",
                          value = 0.1, min = 0.01, max = 0.15,step = 0.01),
             numericInput(ns("hunt.mort.juv.m"),"% of male 1.5yr hunted",
                          value = 0.1, min = 0.01, max = 0.6,step = 0.01),
             numericInput(ns("hunt.mort.ad.f"),"% of females hunted",
                          value = 0.1, min = 0.01, max = 0.2,step = 0.01),
             numericInput(ns("hunt.mort.ad.m"),"% of males hunted",
                          value = 0.2, min = 0.01, max = 0.6,step = 0.01),
             numericInput(ns("hunt.var"),"Hunting variance",
                         value = 0.005, min = 0.001, max = 0.01, step = 0.001),
             numericInput(ns("sur.var"), "Survival variance",
                          value = .005, min = 0.001, max = .01, step = 0.001)),
      column(3,
             numericInput(ns("n.years"), "# of years",
                          value = 10, min = 5, max = 30, step = 5),
             numericInput(ns("sims"), "# of sims",
                          value = 30, min = 5, max = 100, step = 10),
             numericInput(ns("n0"), "Initial population",
                          value = 1000, min = 100, max = 5000, step = 100),
             numericInput(ns("ini.fawn.prev"), "Fawn prevalence",
                          value = 0.01, min = 0, max = 0.1, step = 0.01),
             numericInput(ns("ini.juv.prev"), "Juvenile prevalence",
                          value = 0.03, min = 0, max = 0.2, step = 0.01),
             numericInput(ns("ini.ad.f.prev"), "Female prevalence",
                          value = 0.04, min = 0, max = 0.4, step = 0.01),
             numericInput(ns("ini.ad.m.prev"), "Male prevalence",
                          value = 0.04, min = 0, max = 0.4, step = 0.01)),
      hr(),
      actionButton(ns("go"), "Run simulation")
    )
  )
}
