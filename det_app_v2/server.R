#
# CWD shiny app deterministic model user interface
library(shiny)
library(popbio)
library(reshape2)
library(tidyverse)
library(ggridges)
library(cowplot)
source("det_pop_model_fxn_ver2.r", local = T)
source("plot_fxns.r", local = T)
source("plot_params.r", local = T)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #Create the reactive parameter set and run the model:
  react.params <- reactive({
    list(fawn.an.sur = input$fawn.an.sur,
         juv.an.sur = input$juv.an.sur,
         ad.an.f.sur = input$ad.an.f.sur,
         ad.an.m.sur = input$ad.an.m.sur,

         fawn.repro = 0,
         juv.repro = input$juv.repro,
         ad.repro = input$ad.repro,

         hunt.mort.fawn = input$hunt.mort.fawn,
         hunt.mort.juv = input$hunt.mort.juv,
         hunt.mort.ad.f = input$hunt.mort.ad.f,
         hunt.mort.ad.m = input$hunt.mort.ad.m,

         n.age.cats = 12,
         p = input$p,

         ini.fawn.prev = input$ini.fawn.prev,
         ini.juv.prev = input$ini.juv.prev,
         ini.ad.f.prev = input$ini.ad.f.prev,
         ini.ad.m.prev = input$ini.ad.m.prev,

         env.foi =  1 - ((1-input$an.env.foi)^(1/12)),

         #calculate beta from r0_female
         #R0 * n^theta-1 / (avg time to death)
         # for time to death first draw 1000 reps for each of the competing rates
         # take the min of the three rates, take the mean of the mins.
         beta = input$r0  * input$n0^(input$theta-1) /
                mean(apply(cbind(rexp(1000, (1-input$ad.an.f.sur^(1/12))),
                            rexp(1000, (1-(1-input$hunt.mort.ad.f)^(1/12))),
                            rgamma(1000, 10, input$p)), 1, FUN = min)),

         beta.m = input$beta.m,
         theta = input$theta,
         n0 = input$n0,
         n.years = input$n.years,
         rel.risk = input$rel.risk)
  })

  simout <- reactive({
    params <- react.params()
    out <- det.pop.model.v2(params)
    out
    })

  output$TotalsPlot <- renderPlot({
   out <- simout()
   par(cex = 1.5)
   plot.tots(out$counts)
  })

  output$prevPlot <- renderPlot({
    out <- simout()
    par(mar = c(6,6,1,1))
    plot.prev.2(out$counts,ylim = c(0, .7))
    })

  output$agePlot <- renderPlot({
    out <- simout()
    plot.age.dist(out$counts)
  })

  output$deathPlot <- renderPlot({
    out <- simout()
    p1 <- plot.deaths(out$deaths)
    p2 <- plot.perc.deaths(out$deaths)
    plot_grid(p1, p2, nrow = 2)
    })

  output$ParamsPlot <- renderPlot({
    params <- react.params()
    plot.ttd(params$p)
  })


  output$classPlot <- renderPlot({
    out <- simout()
    plot.fawn.buck(out$counts, ylim = c(0.2, 1), lwd = 3)
  })
})
