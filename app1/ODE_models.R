#Model functions

# function to do the SIS dynamics with periodic birth.
model.1 <- function(t, y, parms) {
  # susceptibles
  dS <- -parms$b * y[1] * (y[2] / (y[1] + y[2])) - # transmission
    y[1] * parms$d + # death
    parms$r * parms$birth.t[t] * (y[1] + y[2]) * (1- (y[1] + y[2]) / parms$k) #birth

  dI <- parms$b * y[1] * (y[2] / (y[1] + y[2])) - # transmission
    y[2] * (parms$d + parms$a0) # death

  list(c(dS, dI))
}
