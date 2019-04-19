# CWD shiny app deterministic model user interface

library(shiny)
#library(markdown)
# Application title
shinyUI(fluidPage(
  titlePanel("CWD Model"),
   fluidRow(
      column(4, h4("Parameters"),
             tabsetPanel(
                tabPanel("Vital rates",
                   sliderInput("fawn.an.sur", "Fawn survival",
                               value = 0.4, min = 0.1, max = .99, step = 0.05),
                   sliderInput("juv.an.sur", "Juvenile survival",
                               value = 0.7, min = 0.3, max = .99, step = 0.05),
                   sliderInput("ad.an.f.sur",  "Adult female survival",
                               value = 0.8,min = 0.5, max = .99, step = 0.01),
                   sliderInput("ad.an.m.sur", "Adult male survival",
                               value = 0.7, min = 0.5, max = .99, step = 0.01),
                   sliderInput("juv.repro", "Fawns per Juv",
                               value = 0.7, min = 0.2, max = 1.5, step = 0.1),
                   sliderInput("ad.repro", "Fawns per adult",
                               value = 1.6, min = 0.7, max = 2, step = 0.1)),
                tabPanel("Disease",
                   sliderInput("an.env.foi", "Envi FOI",
                               value = 0.02, min = 0, max = 0.1, step = 0.01),
                   sliderInput("beta", "Direct transmission (beta)",
                               value = 0.003, min = 0, max = .2, step = 0.0001),
                   sliderInput("theta", "Theta",
                               value = 0.5, min = 0, max = 1, step = 0.1),
                   sliderInput("ini.fawn.prev", "Fawn prevalence",
                               value = 0.01, min = 0, max = 0.4, step = 0.01),
                   sliderInput("ini.juv.prev", "Juv prevalence",
                               value = 0.02, min = 0, max = 0.4, step = 0.01),
                   sliderInput("ini.ad.f.prev", "Doe prevalence",
                               value = 0.03, min = 0, max = 0.4, step = 0.01),
                   sliderInput("ini.ad.m.prev", "Buck prevalence",
                               value = 0.03, min = 0, max = 0.4, step = 0.01)),
                tabPanel("Simulation & Hunting",
                   sliderInput("n0","Initial population size",
                               value = 2000, min = 100, max = 6000,step = 10),
                   sliderInput("n.years","# of years",
                               value = 10, min = 5, max = 50, step = 1),
                   sliderInput("hunt.mort.fawn","% fawns hunted",
                               value = 0.03,min = 0.01, max = 0.1,step = 0.01),
                   sliderInput("hunt.mort.juv","% juv hunted",
                               value = 0.07,min = 0.01, max = 0.15,step = 0.01),
                   sliderInput("hunt.mort.ad.f","% Does hunted",
                               value = 0.1,min = 0.01, max = 0.3,step = 0.05),
                   sliderInput("rel.risk","Relative risk",
                               value = 1, min = 0.1, max = 2, step = 0.1),
                   sliderInput("hunt.mort.ad.m","% bucks hunted",
                               value = 0.2,min = 0.01, max = 0.3,step = 0.05))
             )
      ),
      column(width = 8, h4("Plots"),
        tabsetPanel(
          tabPanel("Totals", plotOutput('TotalsPlot'), width = "100%"),
          tabPanel("Prevalence", plotOutput('prevPlot'), width = "100%"),
          tabPanel("Deaths", plotOutput('deathPlot'), width = "100%"),
          tabPanel("Ratios", plotOutput('classPlot'), width = "100%")
          )
      )
    )
  )
)
