#Script to set up and run the Shiny SIS Models
rm(list = ls())
library(shiny)
library(popbio)
library(deSolve)

setwd("C:/Current Projects/CWD_shiny")
#SIS model
runApp("C:/Current Projects/CWD_shiny/model1")

# Pulsed SIS model
runApp("C:/Current Projects/CWD_shiny/model2")


#Deploy the app on the web.
#library(rsconnect)
#setwd("~/WorkPlay/MangeSIS/shinyapp_model4_pulsed")
#deployApp()


