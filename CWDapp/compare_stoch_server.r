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
        beta.f = (input$r0_peryear  * input$n0 ^ (input$theta-1)) / 12,
        beta.m = input$gamma.m *(input$r0_peryear  * input$n0^(input$theta-1))/ 12,
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
                   out <- cwd_stoch_model(params)
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

   fem.R0 <-   (params$beta.f / params$n0 ^ (params$theta-1)) *
     mean(apply(cbind(rnbinom(1000, 1, (1 - input$ad.an.f.sur^(1/12))),
                      rnbinom(1000, 1, (1 - (1 - input$hunt.mort.ad.f)^(1/12))),
                      rgamma(1000, 10, input$p)), 1, FUN = min, na.rm = T))
   male.R0 <-   (params$beta.f * params$beta.m / params$n0 ^ (params$theta-1)) *
     mean(apply(cbind(rnbinom(1000, 1, (1 - input$ad.an.m.sur^(1/12))),
                      rnbinom(1000, 1, (1 - (1 - input$hunt.mort.ad.m)^(1/12))),
                      rgamma(1000, 10, input$p)), 1, FUN = min, na.rm = T))

   out <- list(counts = counts.long, deaths = deaths.long, fem.R0 = fem.R0,
               male.R0 = male.R0, outputparams = params)
   return(out)
   })

}
