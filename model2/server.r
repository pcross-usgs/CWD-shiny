library(shiny)
require(deSolve)

# function to do the SIS dynamics with periodic birth.
SIS_model_4_pulsed <- function(t, y, parms) {
  dS <- -parms$b*y[1]*(y[2]/(y[1]+y[2])) - y[1]*(parms$d + parms$a2*y[2]) +
      parms$r*parms$birth.t[t]*(1-(y[1]+y[2])/parms$k) + parms$g*y[2] # susceptibles
  dI <- parms$b*y[1]*(y[2]/(y[1]+y[2])) -
        y[2]*(parms$d + parms$a0 + parms$a1*(y[1]+y[2]) + parms$a2*y[2] + parms$g) # infecteds
  list(c(dS, dI))
}

shinyServer(function(input, output) {
  data = reactive({

    times <- seq(1, input$endTime, 1)
    birth.t <- rep(0, input$endTime)
    birth.t[times%%365 == 153] <- 1 # births occur on June 1st every year

    return(list(bVal=input$bVal,
         dVal=input$dVal,
         a0Val=input$a0Val,
         a1Val=input$a1Val,
         a2Val=input$a2Val,
         gVal=input$gVal,
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
    parms <- list(b = data()$bVal, d = data()$dVal, a0 = data()$a0Val, a1 = data()$a1Val,a2 = data()$a2Val,
               g = data()$gVal, r = data()$rVal, k = data()$kVal, birth.t = data()$birth.t)

    #Main simulation here
    out <- suppressWarnings(lsoda(y = xstart, data()$times, SIS_model_4_pulsed, parms))

    # PLOTS
    par(mfrow = c(1, 1))
    # Dynamics over time
    plot(out[,1], out[,2], ylim = c(0, data()$kVal),
            type="l", xlab="Time", ylab="Abundance",
            main="",
            lwd=2, lty=1, bty="l", col="blue",
            xlim=c(0, max(out[,1], na.rm=TRUE)))
    lines(out[,1],out[,3], col = "red", lwd = 2)
    lines(out[,1], apply(out[,2:3], 1,sum), col = "black", lwd = 2)
    legend(x="topright", col=c("blue", "red", "black"), legend=c("S", "I", "N"),
            lty=1, lwd = 2, bty="n")
})
})

