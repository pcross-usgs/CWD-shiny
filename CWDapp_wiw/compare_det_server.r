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
        n.age.cats = 12,
        n0 = input$n0, # initial population size
        p = input$p, #probability of transitioning between infectious box cars;

        n.years = input$n.years,
        env.foi =  1 - ((1-input$an.env.foi)^(1/12)),

        #convert from r0_peryear to beta
        beta.ff = (input$r0_peryear  * input$n0 ^ (input$theta-1)) / 12,
        gamma.mm = input$gamma.mm, 
        gamma.mf = input$gamma.mf,
        gamma.fm = input$gamma.fm,
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

  out <- cwd_det_model_wiw(params)

  out <- list(counts = out$counts, deaths = out$deaths)
  out
  })

}
