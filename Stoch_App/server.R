# CWD shiny app stochastic model user interface
library(shiny)
#library(popbio)
library(reshape2)
library(tidyverse)
library(cowplot)

source("stoch_model_fxn_ver2.R", local = T) #NOTE shinyapps.io is case-sensitive on the "R"
source("plot_stoch_fxns.r", local = T)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  react.params <- reactive({
    list(sims = input$sims,
         n.age.cats = 12,
         n0 = 2000, # initial population size
         p = 0.43, #probability of transitioning between infectious box cars;

         fawn.an.sur.var = 0.005,
         an.sur.var = 0.005,

         hunt.mort.var = 0.005,
         fawn.repro.var = (1/2)^2 * 0,
         juv.repro.var = (1/2)^2 * 0.005,
         ad.repro.var = (1/2)^2 * 0.005,

         n.years = input$n.years,
         foi = 1 - ((1-input$an.foi)^(1/12)), # monthly infection probability

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

    out.sims <- vector("list", input$sims)

    for(i in 1:input$sims){
      out.sims[[i]] <- stoch.pop.model.2(params)
    }

    out.sims.long <- melt(out.sims) %>%
      rename(age = Var1, month = Var2, population = value,
             category = L2, sim = L1) %>%
      mutate(year = (month - 1) / 12, sex = as.factor(str_sub(category, -1)),
             disease = "no")
    out.sims.long$disease[str_sub(out.sims.long$category, 1,1) == "I"] = "yes"
    out.sims.long$disease <- as.factor(out.sims.long$disease)

    out.sims.long
  })

  output$TotalsPlot <- renderPlot({
    #plot the totals
   p1 <- plot.stoch.tots(simout(), all.lines = T, error.bars = c(0.25, 0.75),
                    by.sexage = F)
   p2 <- plot.stoch.prev(simout(), all.lines = T, error.bars = TRUE,
                         cis = c(0.25, 0.75))
   plot_grid(p1, p2)
   })

  #output$prevPlot <- renderPlot({})

  output$classPlot <- renderPlot({
    #plot fawn.adult and buck:doe
    p1 <- plot.stoch.fawn.adult(simout(), all.lines = T, error.bars = c(0.05, 0.95))
    p2 <- plot.stoch.buck.doe(simout(), all.lines = T, error.bars = c(0.05, 0.95))
    plot_grid(p1, p2)
  })

})
