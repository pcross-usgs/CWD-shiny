#' Shiny app server function
#'
#' Server function to run the deterministic model
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny


compare_det_server <- function(input, output, session){

 react.params <- reactive({
   list(sims = input$sims,
        n.age.cats= input$n.age.cats,
        
        n.predators= input$n.predators, 
        k_max = input$k_max, 
        min.predators = input$min.predators,
        max.predators = input$max.predators, 
        r = input$r, 
        k_inflect = input$k_inflect, 
        n_inflect = input$n_inflect,
        numeric.form = "Type2", 
        functional.form = "Type3",
        beta.f.low = input$beta.f.low, 
        beta.m.low = input$beta.m.low, 
        S = input$S, 
        base.juv = input$base.juv, 
        base.adult = input$base.adult, 
        base.old = input$base.old, 
        selection.form = "exponential", 
        stages = 0:10,
        juvs = 1:2, 
        adults = 3:13, 
        old = 14:18, 
        
        n0 = input$n0, # initial population size
        p = input$p, #probability of transitioning between infectious box cars;

        n.years = input$n.years,
        env.foi =  1 - ((1-input$an.env.foi)^(1/12)),
        theta = input$theta,

        ini.fawn.prev = input$ini.fawn.prev,
        ini.juv.prev = input$ini.juv.prev,
        ini.ad.f.prev = input$ini.ad.f.prev,
        ini.ad.m.prev = input$ini.ad.m.prev,
        rel.risk = input$rel.risk,

        fawn.an.sur = input$fawn.an.sur,
        juv.an.sur = input$juv.an.sur,
        ad.an.f.sur = input$ad.an.f.sur,
        ad.an.m.sur = input$ad.an.m.sur,

        fawn.repro = 0,
        juv.repro = input$juv.repro,
        ad.repro = input$ad.repro,

        hunt.mort.fawn = input$hunt.mort.fawn,
        hunt.mort.juv.f = input$hunt.mort.juv.f,
        hunt.mort.juv.m = input$hunt.mort.juv.m,
        hunt.mort.ad.f =  input$hunt.mort.ad.f,
        hunt.mort.ad.m = input$hunt.mort.ad.m)
 })

     #Run the model
 simout <- reactive({
  params <- react.params()
  out <- cwd_detmod_predation_late(params)
  out
  })
}
