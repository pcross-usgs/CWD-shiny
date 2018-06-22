# CWD shiny app stochastic model user interface
library(shiny)
library(popbio)
library(reshape2)
library(tidyverse)
library(cowplot)

source("stoch_model_fxn_ver2.r", local = T)
source("plot_stoch_fxns.r", local = T)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  react.params <- reactive({
    #Function to estimate alpha and beta for a beta distribution
    estBetaParams <- function(mu, var) {
      alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
      beta <- alpha * (1 / mu - 1)
      return(params = list(alpha = alpha, beta = beta))
    }

    fawn.rep <- 0
    preg.fawn = fawn.rep/2
    preg.juv = input$juv.rep/2
    preg.ad = input$ad.rep/2

    #Annual variance on reproduction; input by user?
    fawn.an.repro.var <- 0
    juv.an.repro.var <- 0.005
    ad.an.repro.var <- 0.005

    #Annual variance on survival; input by user?
    fawn.an.sur.var <- 0.005
    juv.an.sur.var <- 0.005
    ad.an.f.sur.var <- 0.005
    ad.an.m.sur.var <- 0.005

    #Variance of additive hunt mortality; user input?  Right now fixed across all age/sex classes
    hunt.mort.f.var <- 0.005
    hunt.mort.m.var <- 0.005
    hunt.mort.i.f.var <- 0.005
    hunt.mort.i.m.var <- 0.005

    #rescaling variance since can't just divide by 2....need to check this step
    fawn.repro.var <- (1/2)^2 * fawn.an.repro.var
    juv.repro.var <- (1/2)^2 * juv.an.repro.var
    ad.repro.var <- (1/2)^2 * ad.an.repro.var

    list(sims = input$sims,
         n.age.cats = 12,
         n0 = 2000, # initial population size
         p = 0.43, #probability of transitioning between infectious box cars; determines disease-induced mortality rate
         foi = 1 - ((1-input$an.foi)^(1/12)), # monthly probability of becoming infected
         n.years = input$n.years,

         ini.fawn.prev = input$ini.fawn.prev,
         ini.juv.prev = input$ini.juv.prev,
         ini.ad.f.prev = input$ini.ad.f.prev,
         ini.ad.m.prev = input$ini.ad.m.prev,

        #Estimate alpha & beta values for survival
        fawn.sur.alpha = estBetaParams(input$fawn.an.sur, fawn.an.sur.var)$alpha,
        fawn.sur.beta = estBetaParams(input$fawn.an.sur, fawn.an.sur.var)$beta,
        juv.sur.alpha = estBetaParams(input$juv.an.sur, juv.an.sur.var)$alpha,
        juv.sur.beta = estBetaParams(input$juv.an.sur, juv.an.sur.var)$beta,
        ad.f.sur.alpha = estBetaParams(input$ad.an.f.sur, ad.an.f.sur.var)$alpha,
        ad.f.sur.beta = estBetaParams(input$ad.an.f.sur, ad.an.f.sur.var)$beta,
        ad.m.sur.alpha = estBetaParams(input$ad.an.m.sur, ad.an.m.sur.var)$alpha,
        ad.m.sur.beta = estBetaParams(input$ad.an.m.sur, ad.an.m.sur.var)$beta,

        #Estimate alpha & beta values for the beta distribution of probability of reproducing
        juv.repro.alpha = estBetaParams(preg.juv, juv.repro.var)$alpha,
        juv.repro.beta = estBetaParams(preg.juv, juv.repro.var)$beta,
        ad.repro.alpha = estBetaParams(preg.ad, ad.repro.var)$alpha,
        ad.repro.beta = estBetaParams(preg.ad, ad.repro.var)$beta,

        #Estimate alpha and beta of beta distribution
        hunt.mort.f.alpha = estBetaParams(input$hunt.mort.f.mean, hunt.mort.f.var)$alpha,
        hunt.mort.f.beta = estBetaParams(input$hunt.mort.f.mean, hunt.mort.f.var)$beta,
        hunt.mort.m.alpha = estBetaParams(input$hunt.mort.m.mean, hunt.mort.m.var)$alpha,
        hunt.mort.m.beta = estBetaParams(input$hunt.mort.m.mean, hunt.mort.m.var)$beta,
        hunt.mort.i.f.alpha = estBetaParams(input$hunt.mort.i.f.mean, hunt.mort.i.f.var)$alpha,
        hunt.mort.i.f.beta = estBetaParams(input$hunt.mort.i.f.mean, hunt.mort.i.f.var)$beta,
        hunt.mort.i.m.alpha = estBetaParams(input$hunt.mort.i.m.mean, hunt.mort.i.m.var)$alpha,
        hunt.mort.i.m.beta = estBetaParams(input$hunt.mort.i.m.mean, hunt.mort.i.m.var)$beta
        )
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
    plot.stoch.tots(simout(), all.lines = T, error.bars = c(0.25, 0.75),
                    by.sexage = T)
  })

  output$prevPlot <- renderPlot({
    # prev by age
    plot.stoch.prev.age(simout(), by.sex = T)
  })

  output$classPlot <- renderPlot({
    #plot fawn.adult and buck:doe
    p1 <- plot.stoch.fawn.adult(simout(), all.lines = T, error.bars = c(0.05, 0.95))
    p2 <- plot.stoch.buck.doe(simout(), all.lines = T, error.bars = c(0.05, 0.95))
    plot_grid(p1, p2, labels = c("A", "B"))
  })

})
