# CWD modeling application in Shiny
library(shiny)


# Source helpers ----
source("models.R")

###EVERYTHING BELOW WAS A COPY/PASTE FROM APP1
### NEEDS TO BE UPDATED......
# User interface ----
ui <- fluidPage(pageWithSidebar(
                  headerPanel('Deterministic CWD Model'),
                  sidebarPanel(h4("Starting Populations:"),
                               numericInput("S0", "Number of Susceptibles (S):", 1000),
                               numericInput("I0", "Number of Infecteds (I):", 1),
                               numericInput("endTime", "# of days:", 3000),
                               br(),
                               h4("Parameters:"),
                               sliderInput("bVal",
                                           "Transmission coefficient:",
                                           value = 0.01,
                                           min = 0,
                                           max = 0.03,
                                           step = 0.001),
                               sliderInput("dVal",
                                           "Background mortality:",
                                           value = .0005,
                                           min = 0,
                                           max = .003,
                                           step = .0001),
                               sliderInput("rVal",
                                           "Reproduction rate:",
                                           value = .9,
                                           min = 0.1,
                                           max = 1,
                                           step = .05),
                               sliderInput("a0Val",
                                           "Disease-induced mortality rate:",
                                           value = 0.0005,
                                           min = 0,
                                           max = .003,
                                           step = .0001),
                              numericInput("kVal",
                                            "Carrying capacity:", 6000),
                               br()),
                  mainPanel(plotOutput('dynamics')
                            )
                )
)

# Server logic
server <- function (input, output){

  data = reactive({

      times <- seq(1, input$endTime, 1)
      birth.t <- rep(0, input$endTime)
      birth.t[times%%365 == 153] <- 1 # births occur on June 1st every year

      return(list(bVal=input$bVal,
                  dVal=input$dVal,
                  a0Val=input$a0Val,
                  rVal=input$rVal,
                  kVal=input$kVal,
                  S0 = input$S0,
                  I0 = input$I0,
                  birth.t = birth.t,
                  times = times))
    })

  output$dynamics = renderPlot({
      #Starting conditions.
      xstart <- c(S = data()$S0, I = data()$I0)

      #Parameters
      parms <- list(b = data()$bVal,
                    d = data()$dVal,
                    a0 = data()$a0Val,
                    r = data()$rVal,
                    k = data()$kVal,
                    birth.t = data()$birth.t)

      #Main simulation here
      out <- suppressWarnings(
        lsoda(y = xstart, data()$times, model.1, parms)
        )

      # PLOTS
      par(mfrow = c(1, 1))
      # Dynamics over time
      plot(out[,1], out[,2], ylim = c(0, max(out[,1] + out[,2])),
           type="l", xlab="Time", ylab="Abundance",
           main="",
           lwd=2, lty=1, bty="l", col="blue",
           xlim=c(0, max(out[,1], na.rm=TRUE))
      )

      lines(out[,1],out[,3], col = "red", lwd = 2)
      lines(out[,1], apply(out[,2:3], 1,sum), col = "black", lwd = 2)
      legend(x="topright", col=c("blue", "red", "black"),
             legend=c("S", "I", "N"),
             lty=1, lwd = 2, bty="n")
    })

}

# Run the app
shinyApp(ui, server)

