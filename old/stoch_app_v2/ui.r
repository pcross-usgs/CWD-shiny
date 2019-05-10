# CWD shiny app stochastic model user interface
library(shiny)
shinyUI(fluidPage(
  navbarPage("Chronic Wasting Disease",
   tabPanel("Model",
    fluidRow(
      column(4, h4("Parameters"),
         tabsetPanel(

           tabPanel("Vital rates",
             sliderInput("fawn.an.sur", "Fawn survival",
                         value = 0.4, min = 0.1, max = .99, step = 0.05),
             sliderInput("juv.an.sur", "Juvenile survival",
                         value = 0.7, min = 0.3, max = .99, step = 0.05),
             sliderInput("ad.an.f.sur",  "Adult female survival",
                         value = 0.85,min = 0.5, max = .99, step = 0.01),
             sliderInput("ad.an.m.sur", "Adult male survival",
                         value = 0.8, min = 0.5, max = .99, step = 0.01),
             sliderInput("juv.repro", "Fawns per Juv",
                         value = 0.9, min = 0.2, max = 1.5, step = 0.1),
             sliderInput("ad.repro", "Fawns per adult",
                         value = 1.6, min = 0.5, max = 2, step = 0.1)),

           tabPanel("Disease",
              sliderInput("an.env.foi", "Envi FOI",
                         value = 0.02, min = 0, max = 0.1, step = 0.01),
              sliderInput("r0", "R0",
                         value = 1.5, min = 0.8, max = 5, step = 0.05),
              sliderInput("theta", "Theta",
                         value = 1, min = 0, max = 1, step = 0.1),
              sliderInput("beta.m", "Relative male infection",
                         value = 1, min = 0.8, max = 1.4, step = 0.05),
              sliderInput("p", "index of disease mortality",
                         value = 0.43, min = 0, max = .9, step = 0.01),
              sliderInput("rel.risk", "Relative Risk hunting infecteds",
                          value = 1, min = .1, max = 4, step = 0.1)),
           
          tabPanel("Hunting",
             sliderInput("hunt.mort.fawn","% fawns hunted",
                         value = 0.01, min = 0.01, max = 0.1,step = 0.01),
             sliderInput("hunt.mort.juv.f","% female 1.5yr hunted",
                         value = 0.05, min = 0.01, max = 0.15,step = 0.01),
             sliderInput("hunt.mort.juv.m","% male 1.5yr hunted",
                         value = 0.05, min = 0.01, max = 0.6,step = 0.01),
             sliderInput("hunt.mort.ad.f","% females hunted",
                         value = 0.05, min = 0.01, max = 0.2,step = 0.01),
             sliderInput("hunt.mort.ad.m","% males hunted",
                         value = 0.5, min = 0.01, max = 0.6,step = 0.01)),
          
           tabPanel("Simulation & Initial conditions",
                   sliderInput("n.years", "# of years",
                               value = 10, min = 5, max = 40, step = 5),
                   sliderInput("sims", "# of sims",
                               value = 20, min = 5, max = 100, step = 5),
                   sliderInput("n0", "Initial population",
                               value = 2000, min = 100, max = 8000, step = 100),
                   sliderInput("ini.fawn.prev", "Fawn prevalence",
                               value = 0.01, min = 0, max = 0.1, step = 0.01),
                   sliderInput("ini.juv.prev", "Juv prevalence",
                               value = 0.02, min = 0, max = 0.2, step = 0.01),
                   sliderInput("ini.ad.f.prev", "Doe prevalence",
                               value = 0.03, min = 0, max = 0.4, step = 0.01),
                   sliderInput("ini.ad.m.prev", "Buck prevalence",
                               value = 0.03, min = 0, max = 0.4, step = 0.01))
          )),
      column(8, h4("Plots"),
             tabsetPanel(
               tabPanel("Totals", plotOutput('TotalsPlot')),
               tabPanel("Prevalence", plotOutput('PrevPlot')),
               tabPanel("Deaths", plotOutput('DeathsPlot')),
               tabPanel("Classification", plotOutput('ClassPlot')),
               tabPanel("Age Distribution", plotOutput('AgePlot')),
               tabPanel("Parameters", plotOutput('ParamsPlot'))
             )
      )
    )
   ),
  tabPanel("Description",
   includeMarkdown("desc_stoch_app_ver2.md")
  )
  )
)
)