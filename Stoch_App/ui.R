# CWD shiny app stochastic model user interface

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("CWD Model"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(h4("Vital rates"),
                 sliderInput("fawn.an.sur",
                             "Fawn survival",
                             value = 0.4,
                             min = 0.1,
                             max = 1,
                             step = 0.05),
                 sliderInput("juv.an.sur",
                             "Juvenile survival",
                             value = 0.7,
                             min = 0.3,
                             max = 1,
                             step = 0.05),
                 sliderInput("ad.an.f.sur",
                             "Adult female survival",
                             value = 0.9,
                             min = 0.5,
                             max = 1,
                             step = 0.01),
                 sliderInput("ad.an.m.sur",
                             "Adult male survival",
                             value = 0.9,
                             min = 0.5,
                             max = 1,
                             step = 0.01),
                 sliderInput("juv.rep",
                             "Fawns per Juv",
                             value = 0.8,
                             min = 0.2,
                             max = 1.5,
                             step = 0.1),
                 sliderInput("ad.rep",
                             "Fawns per adult",
                             value = 1.7,
                             min = 0.7,
                             max = 2,
                             step = 0.1),
                 sliderInput("an.foi",
                             "Force of infection",
                             value = 0.02,
                             min = 0,
                             max = 0.1,
                             step = 0.01),
                 br(),
                 h4("Hunting"),
                 sliderInput("hunt.mort.fawn",
                             "% fawns hunted",
                             value = 0.03,
                             min = 0,
                             max = 0.1,
                             step = 0.01),
                 sliderInput("hunt.mort.juv",
                             "% juv hunted",
                             value = 0.07,
                             min = 0,
                             max = 0.15,
                             step = 0.01),
                 sliderInput("hunt.mort.ad.f",
                             "% Does hunted",
                             value = 0.1,
                             min = 0,
                             max = 0.3,
                             step = 0.05),
                 sliderInput("hunt.mort.ad.m",
                             "% bucks hunted",
                             value = 0.2,
                             min = 0,
                             max = 0.3,
                             step = 0.05),
                 br(),
                 h4("Simulation values:"),
                 sliderInput("n.years",
                             "# of years",
                             value = 10,
                             min = 3,
                             max = 15,
                             step = 1)
                 ),

    # Show a plot of results
    mainPanel(
      plotOutput('TotalsPlot')
    )
  )
))
