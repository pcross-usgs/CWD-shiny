library(shiny)
shinyUI(pageWithSidebar(
  headerPanel('SIS Model'),
  sidebarPanel(
    h4("Starting Populations:"),
    sliderInput("S0",
                "Number of Susceptibles (S):",
                value = 10,
                min = 3,
                max = 20,
                step = 1),
    sliderInput("I0",
                "Number of Infecteds (N):",
                value = 1,
                min = 1,
                max = 15,
                step = 1),
    br(),

    h4("Parameters:"),
    sliderInput("bVal",
                "Transmission coefficient (\\beta):",
                value = .06,
                min = 0,
                max = 1,
                step = 0.02),
    sliderInput("dVal",
                "Background mortality (\\delta):",
                value = .06,
                min = 0,
                max = 1,
                step = .02),
    sliderInput("rVal",
                "Reproduction rate (\\rho):",
                value = .06,
                min = 0,
                max = 1,
                step = .02),
    sliderInput("aVal",
                "Disease-induced mortality rate (\\alpha):",
                value = .06,
                min = 0,
                max = 1,
                step = .02),
    sliderInput("gVal",
                "Recovery rate (\\gamma):",
                value = .06,
                min = 0,
                max = 1,
                step = .02),
    br(),

    h4("Simulation parameters:"),
    sliderInput("endTime",
                "Duration (t):",
                value = 24,
                min = 1,
                max = 100,
                step = 1),
    br()),
  mainPanel(
    plotOutput('dynamics', height = 'auto')
    )
))