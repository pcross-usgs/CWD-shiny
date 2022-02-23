#' Shiny app server function
#'
#' Server function to run the deterministic model
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

det_mod_server <- function(input, output, session){
  
  observe({
    
    if(input$juvs[2] < 2) val = 2
    
    if(input$juvs[2] >= 2) val = input$juvs[2]
    
    updateSliderInput(session, "juvs", value = c(1,val),
                      min = 1, max = input$n.age.cats, step = 1)
    updateSliderInput(session, "adults", value = c(val+1,input$adults[2]),
                      min = 1, max = input$n.age.cats, step = 1)
    updateSliderInput(session, "old", value = c(input$adults[2]+1,input$n.age.cats),
                      min = 1, max = input$n.age.cats, step = 1)
    
    if(input$n.predators > input$max.predators) val.p = input$max.predators
    if(input$n.predators < input$min.predators) val.p = input$min.predators
    if(input$n.predators >= input$min.predators | 
       input$n.predators <= input$max.predators) val.p = input$n.predators
    
    updateSliderInput(session, "n.predators", value = val.p,
                      min = input$min.predators, max = input$max.predators, step = 1)
  })

  #Create the reactive parameter set and run the model:
  react.params <- reactive({
    list(n.age.cats= input$n.age.cats,
         juvs = input$juvs[1]:input$juvs[2], 
         adults = input$adults[1]:input$adults[2], 
         old = input$old[1]:input$old[2], 
         
         n.predators= input$n.predators, 
         k_max = input$k_max, 
         min.predators = input$min.predators,
         max.predators = input$max.predators, 
         r = input$r, 
         k_inflect = input$k_inflect, 
         n_inflect = input$n_inflect,
         numeric.form = input$numeric.form, 
         functional.form = input$functional.form,
         base.juv = input$base.juv, 
         base.adult = 1, 
         base.old = input$base.old, 
         selection.form = input$selection.form, 
         stages = input$stages[1]:input$stages[2],
        
         
         n0 = input$n0, # initial population size
         p = input$p, #probability of transitioning between infectious box cars;
         beta.f.low = input$beta.f.low, 
         beta.m.low = input$beta.m.low, 
         S = input$S, 
         n.years = input$n.years,
         env.foi =  1 - ((1-input$an.env.foi)^(1/12)),
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

  simout <- reactive({
    params <- react.params()
    out <- cwd_detmod_predation_late(params)
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
  
  output$PredPreyPlot <- renderPlot({
    out <- simout()
    plot_predator_prey_pop(out$counts, out$predation)
  })

  output$DeathPlot <- renderPlot({
    out <- simout()
    plot_predation_deaths(out$predation, out$deaths)
  })
  
  output$PredParamPlot <- renderPlot({
    out <- simout()
    plot_predation_params(out$params)
  })

  output$PredAge <- renderPlot({
    out <- simout()
    plot_predation_prop(out$counts, out$predation, out$params)
  })
  
  output$StagePlot <- renderPlot({
    out <- simout()
    plot_predation_stage_prop(out$counts, out$predation, out$params)
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
