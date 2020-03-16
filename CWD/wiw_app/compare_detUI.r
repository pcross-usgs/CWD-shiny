#' Shiny UI function for the deterministic comparison
#'
#' UI function to define the sliders of the determinisitic comparison
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny


compare_detUI <- function(id) {
  ns <- NS(id)
  wellPanel(
    tagList(
      column(3,
             numericInput(ns("fawn.an.sur"), "Fawn survival",
                          value = 0.6, min = 0.3, max = .99, step = 0.05),
             numericInput(ns("juv.an.sur"), "Juvenile survival",
                          value = 0.8, min = 0.3, max = .99, step = 0.05),
             numericInput(ns("ad.an.f.sur"),  "Adult female survival",
                          value = 0.9,min = 0.5, max = .99, step = 0.01),
             numericInput(ns("ad.an.m.sur"), "Adult male survival",
                          value = 0.85, min = 0.5, max = .99, step = 0.01),
             numericInput(ns("juv.repro"), "Fawns per Juv",
                          value = 0.6, min = 0.2, max = 1.5, step = 0.1),
             numericInput(ns("ad.repro"), "Fawns per adult",
                          value = 1, min = 0.5, max = 2, step = 0.1),
             numericInput(ns("p"), "Disease mortality index",
                          value = 0.43, min = 0, max = .9, step = 0.01)),
      column(3,
             numericInput(ns("an.env.foi"), "Envi FOI",
                          value = 0, min = 0, max = 0.1, step = 0.01),
             numericInput(ns("r0_peryear"), "Direct R0 per year",
                          value = .9, min = 0.5, max = 1.5, step = 0.05),
             numericInput(ns("theta"), "Theta",
                          value = 1, min = 0, max = 1, step = 0.1),
             numericInput(ns("gamma.mm"), "Relative male-male transmission",
                          value = 2, min = 0.2, max = 5, step = 0.1),
             numericInput(ns("gamma.mf"), "Relative male-female transmission",
                          value = 1, min = 0.2, max = 5, step = 0.1),
             numericInput(ns("gamma.fm"), "Relative female-male transmission",
                          value = 1, min = 0.5, max = 5, step = 0.1)),
      column(3,
             numericInput(ns("hunt.mort.fawn"),"% fawns hunted",
                          value = 0.01, min = 0.01, max = 0.2,step = 0.01),
             numericInput(ns("hunt.mort.juv.f"),"% female 1.5yr hunted",
                          value = 0.03, min = 0.01, max = 0.2,step = 0.01),
             numericInput(ns("hunt.mort.juv.m"),"% male 1.5yr hunted",
                          value = 0.1, min = 0.01, max = 0.7,step = 0.05),
             numericInput(ns("hunt.mort.ad.f"),"% females hunted",
                          value = 0.1, min = 0.01, max = 0.7,step = 0.05),
             numericInput(ns("hunt.mort.ad.m"),"% males hunted",
                          value = 0.2, min = 0.01, max = 0.7,step = 0.05),
             numericInput(ns("rel.risk"), "Relative Risk hunting infecteds",
                          value = 1, min = .1, max = 3, step = 0.1)),
      column(3,
             numericInput(ns("n.years"), "# of years",
                          value = 10, min = 5, max = 30, step = 5),
             numericInput(ns("n0"), "Initial population",
                          value = 1000, min = 100, max = 5000, step = 100),
             numericInput(ns("ini.fawn.prev"), "Fawn prevalence",
                          value = 0.01, min = 0, max = 0.1, step = 0.01),
             numericInput(ns("ini.juv.prev"), "Juvenile prevalence",
                          value = 0.02, min = 0, max = 0.2, step = 0.01),
             numericInput(ns("ini.ad.f.prev"), "Female prevalence",
                          value = 0.03, min = 0, max = 0.4, step = 0.01),
             numericInput(ns("ini.ad.m.prev"), "Male prevalence",
                          value = 0.03, min = 0, max = 0.4, step = 0.01)),
      hr()
      )
    )
}
