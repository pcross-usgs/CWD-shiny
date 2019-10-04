#' Shiny app server function
#'
#' Server function to run the deterministic model
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

det_mod_server <- function(input, output, session){

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
         beta.ff = (input$r0_peryear  * input$n0^(input$theta-1))/ 12,
         gamma.mm = input$gamma.mm, 
         gamma.mf = input$gamma.mf,
         gamma.fm = input$gamma.fm,
         theta = input$theta,
         n0 = input$n0,
         n.years = input$n.years,
         rel.risk = input$rel.risk)
  })

  simout <- reactive({
    params <- react.params()
    out <- cwd_det_model_wiw(params)
    out <- list(counts = out$counts, deaths = out$deaths)
    out
  })
  output$TotalPlot <- renderPlot({
    out <- simout()
    par(cex = 1.5)
    plot_tots(out$counts)
  })

  output$PrevPlot <- renderPlot({
    out <- simout()
    p1 <- plot_prev_time(out$counts)
    p2 <- plot_prev_age_end(out$counts)
    plot_grid(p1, p2, nrow = 1)
  })

  output$AgePlot <- renderPlot({
    out <- simout()
    plot_age_dist(out$counts)
  })

  output$DeathPlot <- renderPlot({
    out <- simout()
    p1 <- plot_deaths(out$deaths, percents = F)
    p2 <- plot_deaths(out$deaths, percents = T)
    plot_grid(p1, p2, nrow = 2)
  })

  output$ParamPlot <- renderPlot({
    params <- react.params()
    plot_ttd(params$p)
  })

  output$ClassPlot <- renderPlot({
    out <- simout()
    p1 <- plot_fawn_doe(out$counts)
    p2 <- plot_buck_doe(out$counts)
    p3 <- plot_grid(p1, p2, nrow = 1)
    p3
  })
}
