#
# CWD shiny app deterministic model user interface
library(shiny)
library(popbio)
library(reshape2)
library(tidyverse)
source("det_pop_model_fxn_ver2.r", local = T)
source("plot_fxns.r", local = T)

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
         p =  0.43,

         ini.fawn.prev = input$ini.fawn.prev,
         ini.juv.prev = input$ini.juv.prev,
         ini.ad.f.prev = input$ini.ad.f.prev,
         ini.ad.m.prev = input$ini.ad.m.prev,

         env.foi =  1 - ((1-input$an.env.foi)^(1/12)),
         beta = input$beta,
         theta = input$theta,
         n0 = input$n0,
         n.years = input$n.years,
         rel.risk = input$rel.risk)
  })

  simout <- reactive({
    params <- react.params()
    out <- det.pop.model.v2(react.params())

    out.long <- melt(out) %>%
      rename(age = Var1, month = Var2, population = value,
             category = L1) %>%
      mutate(year = (month - 1) / 12, sex = as.factor(str_sub(category, -1)),
             disease = "no")
    out.long$disease[str_sub(out.long$category, 1,1) == "I"] = "yes"
    out.long$disease <- as.factor(out.long$disease)
    out.long
    })

  output$TotalsPlot <- renderPlot({
   out <- simout()
   par(mar = c(1,4,2,2))
   plot.tots(out, type = "l", ylab = "population", xlab = "year", lwd = 3,
             cex = 1.5, cex.lab = 1.5, cex.axis = 1.5,bty = "l")
  })

  output$prevPlot <- renderPlot({
    out <- simout()
    plot.prev.age(out, by.sex = T)
    })

  output$classPlot <- renderPlot({
    out <- simout()
    par(mfrow = c(1,2), mar = c(6,4,2,2))
    plot.fawn.adult(out, type = "l", xlab = "year", ylab = "fawn:adult", bty = "l",
                    cex = 1.5, lwd =  3, cex.lab = 1.5, cex.axis = 1.5)
    plot.buck.doe(out, type = "l", xlab = "year", ylab = "buck:doe", bty = "l",
                  cex = 1.5, lwd =  3, cex.lab = 1.5, cex.axis = 1.5)

  })
})
