# CWD shiny app stochastic model user interface
library(shiny)
library(popbio)
library(reshape2)
library(tidyverse)
library(cowplot)
library(ggridges)
library(magrittr)

source("stoch_model_fxn.r", local = T) #NOTE shinyapps.io is case-sensitive on the "R"
source("plot_stoch_fxns.r", local = T)
source("plot_params.r", local = T)
source("estBetaParams.r", local = T)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  react.params <- reactive({
    list(sims = input$sims,
         n.age.cats = 12,
         n0 = 2000, # initial population size
         p = input$p, #probability of transitioning between infectious box cars;

         fawn.an.sur.var = 0.005,
         an.sur.var = 0.005,

         hunt.mort.var = 0.005,
         fawn.repro.var = (1/2)^2 * 0,
         juv.repro.var = (1/2)^2 * 0.005,
         ad.repro.var = (1/2)^2 * 0.005,

         n.years = input$n.years,
         foi = 1 - ((1-input$an.foi)^(1/12)), # monthly infection probability
         foi.m = input$foi.m,

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
         hunt.mort.juv = input$hunt.mort.juv,
         hunt.mort.ad.f =  input$hunt.mort.ad.f,
         hunt.mort.ad.m = input$hunt.mort.ad.m)
  })

  #Run the model
  simout <- reactive({
    params <- react.params()

    counts.sims <- vector("list", input$sims)
    deaths.sims <- vector("list", input$sims)

    for(i in 1:input$sims){
      out <- stoch.pop.model(params)
      counts.sims[[i]] <- out$counts
      deaths.sims[[i]] <- out$deaths
    }

    counts.long <- melt(counts.sims,
                         id = c("age", "month", "population", "category",
                                "year", "sex", "disease")) %>% rename(sim = L1)

    deaths.long <- melt(deaths.sims,
                         id = c("age", "month", "population", "category",
                                "year", "sex")) %>% rename(sim = L1)

    out <- list(counts = counts.long, deaths = deaths.long)
    out
  })

  output$TotalsPlot <- renderPlot({
    out <- simout()
    p1 <- plot.stoch.tots.2(out$counts, error.bars = c(0.05, 0.95))
    p1
  })

  output$PrevPlot <- renderPlot({
    out <- simout()
    p1 <- plot.stoch.prev(out$counts, all.lines = T, error.bars = T,
                          cis <- c(0.05, 0.95))
    p2 <- plot.stoch.prev.age.2(out$counts, error.bars = c(0.05, 0.95))
    plot_grid(p1, p2, nrow = 1)
  })

  output$DeathsPlot <- renderPlot({
      out <- simout()
      p1 <- plot.stoch.deaths(out$deaths, error.bars = c(0.05, 0.95))
      p2 <- plot.stoch.perc.deaths(out$deaths, error.bars = c(0.05, 0.95))
      plot_grid(p1, p2, nrow = 2)
    })

   output$ParamsPlot <- renderPlot({
     params <- react.params()
     p1 <- plot.vitals(params)
     p2 <- plot.ttd(params$p)
     plot_grid(p1, p2, nrow = 2)
   })

   output$AgePlot <- renderPlot({
     out <- simout()
     plot.stoch.age.dist(out$counts)
   })

  output$ClassPlot <- renderPlot({
    out <- simout()
    #plot fawn.adult and buck:doe
    p1 <- plot.stoch.fawn.adult(out$counts, all.lines = T, error.bars = c(0.05, 0.95))
    p2 <- plot.stoch.buck.doe(out$counts, all.lines = T, error.bars = c(0.05, 0.95))
    plot_grid(p1, p2)
  })

})