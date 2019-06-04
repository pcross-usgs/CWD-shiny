det_mod <- function(input, output, session){

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
         hunt.mort.juv.f = input$hunt.mort.juv.f,
         hunt.mort.juv.m = input$hunt.mort.juv.m,
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
         #R0_per year * n^theta-1 / 12 months
         beta.f = (input$r0_peryear  * input$n0^(input$theta-1))/ 12,
         beta.m = input$beta.m,
         theta = input$theta,
         n0 = input$n0,
         n.years = input$n.years,
         rel.risk = input$rel.risk)
  })

  simout <- reactive({
    params <- react.params()
    out <- det.pop.model.v2(params)
    fem.R0 <-  ( params$beta.f / params$n0 ^ (params$theta-1) ) *
             mean(apply(cbind(rnbinom(1000, 1, (1 - input$ad.an.f.sur^(1/12))),
                       rnbinom(1000, 1, (1 - (1 - input$hunt.mort.ad.f)^(1/12))),
                       rgamma(1000, 10, input$p)), 1, FUN = min))
    male.R0 <-  ( params$beta.f * params$beta.m / params$n0 ^ (params$theta-1) ) *
      mean(apply(cbind(rnbinom(1000, 1, (1 - input$ad.an.m.sur^(1/12))),
                       rnbinom(1000, 1, (1 - (1 - input$hunt.mort.ad.m)^(1/12))),
                       rgamma(1000, 10, input$p)), 1, FUN = min))

    out <- list(counts = out$counts, deaths = out$deaths, fem.R0 = fem.R0,
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
    str1 <- paste("Female direct transmission R0 = ", round(out$fem.R0, 1))
    str2 <- paste("Male direct transmission R0 = ", round(out$male.R0, 1))
    HTML(paste(str1, str2, sep="<br/>"))
  })

  output$TotalPlot <- renderPlot({
    out <- simout()
    par(cex = 1.5)
    plot.tots(out$counts)
  })

  output$PrevPlot <- renderPlot({
    out <- simout()
    par(mar = c(6,6,1,1))
    plot.prev.2(out$counts, ylim = c(0, .7))
  })

  output$AgePlot <- renderPlot({
    out <- simout()
    plot.age.dist(out$counts)
  })

  output$DeathPlot <- renderPlot({
    out <- simout()
    p1 <- plot.deaths(out$deaths)
    p2 <- plot.perc.deaths(out$deaths)
    plot_grid(p1, p2, nrow = 2)
  })

  output$ParamPlot <- renderPlot({
    params <- react.params()
    plot.ttd(params$p)
  })

  output$ClassPlot <- renderPlot({
    out <- simout()
    plot.fawn.buck(out$counts, ylim = c(0.1, 1), lwd = 3)
  })
}