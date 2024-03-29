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
             numericInput(ns("n.age.cats"),"Number of age categories",
                         value = 18, min = 10, max = 25, step = 1),
             # numericInput(ns("min.juvs"),"Min Juvenile Age",
             #             value = c(1), min = 1, max = 25, step = 1),
             # numericInput(ns("max.juvs"),"Max Juvenile Age",
             #              value = c(2), min = 1, max = 25, step = 1),
             # numericInput(ns("min.adults"),"Min Adult Age",
             #              value = c(3), min = 1, max = 25, step = 1),
             # numericInput(ns("max.adults"),"Max Adult Age",
             #              value = c(13), min = 1, max = 25, step = 1),
             # numericInput(ns("min.old"),"Min Scenecent Age",
             #              value = c(14), min = 1, max = 25, step = 1),
             # numericInput(ns("max.old"),"Max Scenecent Age",
             #              value = c(18), min = 1, max = 25, step = 1),
             numericInput(ns("fawn.an.sur"), "Fawn survival",
                         value = 0.6, min = 0.1, max = .99, step = 0.05),
             numericInput(ns("juv.an.sur"), "Juvenile survival",
                         value = 0.8, min = 0.3, max = .99, step = 0.05),
             numericInput(ns("ad.an.f.sur"),  "Adult female survival",
                         value = 0.95,min = 0.5, max = .99, step = 0.01),
             numericInput(ns("ad.an.m.sur"), "Adult male survival",
                         value = 0.9, min = 0.5, max = .99, step = 0.01),
             numericInput(ns("juv.repro"), "Fawns per Juv",
                         value = 0.6, min = 0.2, max = 1.5, step = 0.1),
             numericInput(ns("ad.repro"), "Fawns per adult",
                         value = 1, min = 0.7, max = 2, step = 0.1)),
      column(3,
             numericInput(ns("n.predators"), "Initial predator population",
                         value = 70, min = 0, max = 150, step = 1),
             numericInput(ns("min.predators"), "Min. predator population",
                         value = 20, min = 0, max = 150, step = 1),
             numericInput(ns("max.predators"), "Max. predator population",
                         value = 150, min = 0, max = 150, step = 1),
             numericInput(ns("k_max"), "Max. per capita kill rate",
                         value = 1, min = 0, max = 10, step = 0.01),
             # textInput(ns("functional.form"), "Functional Response Type",
             #             value = "Type3"),
             numericInput(ns("k_inflect"), "Inflection point for functional response",
                         value = 3000, min = 0, max = 10000, step = 100),
             # textInput(ns("numeric.form"), "Numeric Response Type",
             #             value = "Type2"),
             numericInput(ns("n_inflect"), "Inflection point for numerical response",
                         value = 2000, min = 0, max = 10000, step = 100),
             # textInput(ns("selection.form"), "Form of increase in prey selection as CWD progresses",
             #             value = "exponential"),
             numericInput(ns("r"), "Rate of increase in prey selection as CWD progresses",
                         value = 0.25, min = 0, max = 1, step = 0.01),
             numericInput(ns("base.juv"), "Base selection on juvenile",
                         value = 3, min = 0, max = 10, step = 0.1), 
             numericInput(ns("base.adult"), "Base selection on prime age",
                         value = 1, min = 0, max = 10, step = 0.1), 
             numericInput(ns("base.old"), "Base selection on old",
                         value = 2, min = 0, max = 10, step = 0.1) 
             # numericInput(ns("max.stages"), "Total number of CWD stages to include",
             #             value = c(10), min = 0, max = 10, step = 1)
             ),
      column(3,
             numericInput(ns("an.env.foi"), "Indirect transmission",
                         value = 0, min = 0, max = 0.1, step = 0.01),
             numericInput(ns("beta.f.low"), "Female direct transmission",
                         value = 0.025, min = 0, max = .1, step = 0.001),
             numericInput(ns("beta.m.low"), "Male direct transmission",
                         value = 0.05, min = 0, max = 0.1, step = 0.001),
             numericInput(ns("S"), "Transmission increase for I8-I10",
                         value = 5, min = 0, max = 10, step = 0.1),
             numericInput(ns("theta"), "Theta",
                         value = 1, min = 0, max = 1, step = 0.1),
             numericInput(ns("p"), "index of disease mortality",
                         value = 0.43, min = 0, max = .9, step = 0.01),
             numericInput(ns("rel.risk"),"Relative hunting risk",
                         value = 1, min = 0.1, max = 3, step = 0.1)),
      column(3,
             numericInput(ns("hunt.mort.fawn"),"% of fawns hunted",
                         value = 0.01, min = 0.01, max = 0.1,step = 0.01),
             numericInput(ns("hunt.mort.juv.f"),"% of female 1.5yr hunted",
                         value = 0.1, min = 0.01, max = 0.8,step = 0.05),
             numericInput(ns("hunt.mort.juv.m"),"% of male 1.5yr hunted",
                         value = 0.1, min = 0.01, max = 0.8,step = 0.05),
             numericInput(ns("hunt.mort.ad.f"),"% of females hunted",
                         value = 0.1, min = 0.01, max = 0.8,step = 0.05),
             numericInput(ns("hunt.mort.ad.m"),"% of males hunted",
                         value = 0.2, min = 0.01, max = 0.8,step = 0.05)),
      column(3,
             numericInput(ns("n0"),"Initial population size",
                         value = 1000, min = 100, max = 5000,step = 100),
             numericInput(ns("n.years"),"# of years",
                         value = 10, min = 5, max = 50, step = 1),
             numericInput(ns("ini.fawn.prev"), "Fawn prevalence",
                         value = 0.01, min = 0, max = 0.4, step = 0.01),
             numericInput(ns("ini.juv.prev"), "Juvenile prevalence",
                         value = 0.03, min = 0, max = 0.4, step = 0.01),
             numericInput(ns("ini.ad.f.prev"), "Male prevalence",
                         value = 0.04, min = 0, max = 0.4, step = 0.01),
             numericInput(ns("ini.ad.m.prev"), "Female prevalence",
                         value = 0.05, min = 0, max = 0.4, step = 0.01)),
      hr()
      )
    )
}
