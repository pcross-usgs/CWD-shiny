#' Shiny app server function
#'
#' Runs the model for the main stochastic model page
#'
#' @import ggplot2
#' @import dplyr
#' @import shiny
#' @importFrom cowplot plot_grid
#'
#' @param input provided by shiny
#' @param output provided by shiny

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
                               "year", "sex", "disease")) %>%
      dplyr::rename(sim = L1)

    deaths.long <- melt(deaths.sims,
                        id = c("age", "month", "population", "category",
                               "year", "sex")) %>% dplyr::rename(sim = L1)

    out <- list(counts = counts.long, deaths = deaths.long, f.R0 = out$f.R0,
                m.R0 = out$m.R0)
    out
  })

  output$R0text1 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$f.R0, 1))
    str2 <- paste("Male direct transmission R0 = ", round(out$m.R0, 1))
    str3 <- paste("Next Generation R0 = ", round(R0_NGM(react.params(), stable.stage.pop = TRUE), 1))
    HTML(paste(str1, str2, str3, sep="<br/>"))
  })
  output$R0text2 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$f.R0, 1))
    str2 <- paste("Male direct transmission R0 = ", round(out$m.R0, 1))
    str3 <- paste("Next Generation R0 = ", round(R0_NGM(react.params(), stable.stage.pop = TRUE), 1))
    HTML(paste(str1, str2, str3,sep="<br/>"))
  })
  output$R0text3 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$f.R0, 1))
    str2 <- paste("Male direct transmission R0 = ", round(out$m.R0, 1))
    str3 <- paste("Next Generation R0 = ", round(R0_NGM(react.params(), stable.stage.pop = TRUE), 1))
    HTML(paste(str1, str2, str3,sep="<br/>"))
  })
  output$R0text4 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$f.R0, 1))
    str2 <- paste("Male direct transmission R0 = ", round(out$m.R0, 1))
    str3 <- paste("Next Generation R0 = ", round(R0_NGM(react.params(), stable.stage.pop = TRUE), 1))
    HTML(paste(str1, str2, str3,sep="<br/>"))
  })
  output$R0text5 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$f.R0, 1))
    str2 <- paste("Male direct transmission R0 = ", round(out$m.R0, 1))
    str3 <- paste("Next Generation R0 = ", round(R0_NGM(react.params(), stable.stage.pop = TRUE), 1))
    HTML(paste(str1, str2, str3,sep="<br/>"))
  })
  output$R0text6 <- renderUI({
    out <- simout()
    str1 <- paste("Female direct transmission R0 = ", round(out$f.R0,1))
    str2 <- paste("Male direct transmission R0 = ", round(out$m.R0, 1))
    str3 <- paste("Next Generation R0 = ", round(R0_NGM(react.params(), stable.stage.pop = TRUE), 1))
    HTML(paste(str1, str2, str3,sep="<br/>"))
  })

  output$TotalPlot <- renderPlot({
    out <- simout()
    plot_stoch_disease(out$counts, error.bars = c(0.05, 0.95))
  })

  output$PrevPlot <- renderPlot({
    out <- simout()
    p1 <- plot_stoch_prev(out$counts, all.lines = T, error.bars = c(0.05, 0.95))
    p2 <- plot_stoch_prev_age_end(out$counts, error.bars = c(0.05, 0.95))
    cowplot::plot_grid(p1, p2, nrow = 1)
  })

  output$ParamPlot <- renderPlot({
    params <- react.params()
    p1 <- plot_vitals(params)
    p2 <- plot_ttd(params$p)
    cowplot::plot_grid(p1, p2, nrow = 2)
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
    cowplot::plot_grid(p1, p2)
  })

}
