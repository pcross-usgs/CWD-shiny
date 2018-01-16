#Script to set up and run the Shiny SIS Models
rm(list = ls())
library(shiny)
library(popbio)
library(deSolve)

runApp("app1")

#Deploy the app on the web.
library(rsconnect)
setwd("~/Current Projects/CWD_shiny/app1")
deployApp()


