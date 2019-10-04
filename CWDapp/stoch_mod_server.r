#' Shiny app server function
#'
#' Runs the model for the main stochastic model page
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

stoch_mod_server <- function(input, output, session){

  react.params <- reactive({
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

    withProgress(message = "running simulation", max = input$sims, {

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
                male.R0 = male.R0)
    out
  })

  output$R0text1 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$fem.R0, 1))
    str2 <- paste("Male direct transmission R0 = ", round(out$male.R0, 1))
    HTML(paste(str1, str2, sep="<br/>"))
  })
  output$R0text2 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$fem.R0, 1))
    str2 <- paste("Male direct transmission R0 = ", round(out$male.R0, 1))
    HTML(paste(str1, str2, sep="<br/>"))
  })
  output$R0text3 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$fem.R0, 1))
    str2 <- paste("Male direct transmission R0 = ", round(out$male.R0, 1))
    HTML(paste(str1, str2, sep="<br/>"))
  })
  output$R0text4 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$fem.R0, 1))
    str2 <- paste("Male direct transmission R0 = ", round(out$male.R0, 1))
    HTML(paste(str1, str2, sep="<br/>"))
  })
  output$R0text5 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$fem.R0, 1))
    str2 <- paste("Male direct transmission R0 = ", round(out$male.R0, 1))
    HTML(paste(str1, str2, sep="<br/>"))
  })
  output$R0text6 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$fem.R0,1))
    str2 <- paste("Male direct transmission R0 = ", round(out$male.R0, 1))
    HTML(paste(str1, str2, sep="<br/>"))
  })

  output$TotalPlot <- renderPlot({
    out <- simout()
    plot_stoch_disease(out$counts, error.bars = c(0.05, 0.95))
  })

  output$PrevPlot <- renderPlot({
    out <- simout()
    p1 <- plot_stoch_prev(out$counts, all.lines = T, error.bars = c(0.05, 0.95))
    p2 <- plot_stoch_prev_age_end(out$counts, error.bars = c(0.05, 0.95))
    plot_grid(p1, p2, nrow = 1)
  })

  output$ParamPlot <- renderPlot({
    params <- react.params()
    p1 <- plot_vitals(params)
    p2 <- plot_ttd(params$p)
    plot_grid(p1, p2, nrow = 2)
  })

  output$DeathPlot <- renderPlot({
    out <- simout()
    plot_stoch_perc_deaths(out$deaths, error.bars = c(0.05, 0.95))
  })

  output$AgePlot <- renderPlot({
    out <- simout()
    plot_stoch_age_dist(out$counts)
  })

  #plot fawn.doe and buck:doe
  output$ClassPlot <- renderPlot({
    out <- simout()
    p1 <- plot_stoch_fawn_doe(out$counts, all.lines = T, error.bars = c(0.05, 0.95))
    p2 <- plot_stoch_buck_doe(out$counts, all.lines = T, error.bars = c(0.05, 0.95))
    plot_grid(p1, p2)
  })

}
