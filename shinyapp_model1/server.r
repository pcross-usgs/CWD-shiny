library(shiny)
require(deSolve)

# function to do the SIS dynamics.
SIS_model <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    dS <- -b*S*(I/(S+I))- d*S  + r*(S+I) + g*I # susceptibles
    dI <- b*S*(I/(S+I)) - I*(d + a + g) # infecteds
    res <- c(dS, dI)
    list(res)
  })
}

shinyServer(function(input, output) {
  data = reactive({
    data.frame(bVal=input$bVal,
               dVal=input$dVal,
               aVal=input$aVal,
               gVal=input$gVal,
               rVal=input$rVal,
               endTime=input$endTime,
               S0 = input$S0,
               I0 = input$I0)
  })

  output$dynamics = renderPlot({
    D=data()
    bVal=D$bVal
    dVal=D$dVal
    aVal=D$aVal
    gVal=D$gVal
    rVal=D$rVal
    S0 = D$S0
    I0 = D$I0

    parms <- c(b = D$bVal, d = D$dVal, a = D$aVal, g = D$gVal, r= D$rVal)

    #Starting conditions.
    start.time <- 0
    nTimes <- 1000
    xstart <- c(S = S0, I = I0)
    times <- seq(start.time, D$endTime, length = nTimes)

    #Main simulation here
    out <- suppressWarnings(lsoda(xstart, times, SIS_model, parms))

    # PLOTS
    par(mfrow = c(2,1))
    # Dynamics over time
    matplot(out[,1],out[,2:3], ylim = c(0, max(out[,2:3])),
            type="l", xlab="Time", ylab="Abundance",
            main="",
            lwd=2, lty=1, bty="l", col=c("blue", "red"),
            xlim=c(0, max(out[,1], na.rm=TRUE)))
            legend(x="topright", col=c("blue", "red"), legend=c("S", "I"),
            lty=1, bty="n")
    # Total population
    plot(out[,1], apply(out[,2:3], 1,sum), ylim = c(0, max(apply(out[,2:3],1,sum))),
                  type="l", xlab="Time", ylab="Total abundance",
            main="", lwd=2, lty=1, bty="l", xlim=c(0, max(out[,1], na.rm=TRUE)))

    box()
    }, width = 600, height =800, res = 96)
})
