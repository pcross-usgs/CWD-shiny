#' Shiny app server function
#'
#' Server to run the models for the stochastic comparison
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

compare_stoch_server <- function(input, output, session){

 react.params <- eventReactive(input$go,{
   list(sims = input$sims,
        n.age.cats = 12,
        n0 = input$n0, # initial population size
        p = input$p, #probability of transitioning between infectious box cars;

        fawn.sur.var = input$fawn.sur.var,
        sur.var = input$sur.var,
        hunt.var = input$hunt.var,
        repro.var = (1/2)^2 * input$repro.var, #check this

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
 simout <- eventReactive(input$go,{
   params <- react.params()
   counts.sims <- vector("list", input$sims)
   deaths.sims <- vector("list", input$sims)
   withProgress(message = "running simulation", value = 0, {

        for(i in 1:input$sims){
                   out <- cwd_stoch_model_wiw(params)
                   counts.sims[[i]] <- out$counts
                   deaths.sims[[i]] <- out$deaths
                   incProgress(i/input$sims, detail = paste("Run", i))
           }
    })

   counts.long <- melt(counts.sims,
                       id = c("age", "month", "population", "category",
                              "year", "sex", "disease")) %>% rename(sim = L1)

   deaths.long <- melt(deaths.sims,
                       id = c("age", "month", "population", "category",
                              "year", "sex")) %>% rename(sim = L1)

   out <- list(counts = counts.long, deaths = deaths.long, outputparams = params)
   return(out)
   })

}
