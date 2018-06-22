#
# CWD shiny app deterministic model user interface
library(shiny)
library(popbio)
library(reshape2)
library(tidyverse)
source("det_pop_model_fxn.r", local = T)
source("plot_fxns.r", local = T)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  #Create the reactive parameter set and run the model:
  react.params <- reactive({
    list(fawn.an.sur = input$fawn.an.sur,
         juv.an.sur = input$juv.an.sur,
         ad.an.f.sur = input$ad.an.f.sur,
         ad.an.m.sur = input$ad.an.m.sur,
         fawn.rep = 0,
         juv.rep = input$juv.rep,
         ad.rep = input$ad.rep,
         hunt.mort.f = input$hunt.mort.f,
         hunt.mort.m = input$hunt.mort.m,
         hunt.mort.i.f = input$hunt.mort.i.f,
         hunt.mort.i.m = input$hunt.mort.i.m,
         n.age.cats = 12,
         p =  0.43,
         ini.prev = input$ini.prev,
         foi =  1 - ((1-input$an.foi)^(1/12)),
         n0 = 2000,
         n.years = input$n.years)
  })

  simout <- reactive({det.pop.model(react.params())})

  output$TotalsPlot <- renderPlot({
   out <- simout()
   plot.tots(out, type = "l", ylab = "Total population", xlab = "Year",
              ylim = c(0, 2000), lwd = 3,
              cex = 1.25, cex.lab = 1.25, cex.axis = 1.25)
  })

  output$prevPlot <- renderPlot({
    out <- simout()
    plot.prev.age(out, by.sex = T)
    })

  output$classPlot <- renderPlot({
    out <- simout()
    par(mfrow = c(1,2))
    plot.fawn.adult(out, type = "l", xlab = "year", ylab = "fawn:adult")
    plot.buck.doe(out, type = "l", xlab = "year", ylab = "buck:doe")
  })
})
