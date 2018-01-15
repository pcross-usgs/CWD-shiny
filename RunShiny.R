#Script to set up and run the Shiny SIS Models
rm(list = ls())
library(shiny)
library(knitr)
library(markdown)
library(popbio)
library(deSolve)

setwd('~/WorkPlay/CWD_shiny/')
runApp("~/WorkPlay/CWD_shiny/shinyapp_model1") # straight-up SIS model.

#Deploy the app on the web.
#library(rsconnect)
#setwd("~/WorkPlay/MangeSIS/shinyapp_model4_pulsed")
#deployApp()


