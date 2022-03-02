# make sure everything is available to the app.
library(cowplot)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggridges)
library(knitr)
library(magrittr)
library(markdown)
library(plyr)
library(popbio)
library(reshape2)
library(shiny)
library(shinydashboard)
library(tidyr)
library(stringr)

source("allocate_deaths.r", local = T)
source("cwd_det_model_wiw_fxn.r", local = T)
source("cwd_stoch_model_wiw_fxn.r", local = T)
source("cwd_stoch_wiw_wrapper.r", local = T)
source("est_beta_params.r", local = T)
source("R0_NGM_wiw.r", local = T)

source("plot_age_dist.r", local = T)
source("plot_buck_doe.r", local = T)
source("plot_compare_all_det.r", local = T)
source("plot_compare_all_stoch.r", local = T)
source("plot_compare_hunted.r", local = T)
source("plot_compare_prev.r", local = T)
source("plot_compare_tots.r", local = T)
source("plot_deaths.r", local = T)
source("plot_fawn_doe.r", local = T)

source("plot_prev_age_end.r", local = T)
source("plot_prev_time.r", local = T)
source("plot_stoch_age_dist.r", local = T)
source("plot_stoch_buck_doe.r", local = T)
source("plot_stoch_deaths.r", local = T)
source("plot_stoch_disease.r", local = T)
source("plot_stoch_fawn_doe.r", local = T)
source("plot_stoch_perc_deaths.r", local = T)
source("plot_stoch_prev_age_end.r", local = T)
source("plot_stoch_prev_age.r", local = T)
source("plot_stoch_prev.r", local = T)
source("plot_stoch_tots.r", local = T)

source("plot_tots.r", local = T)
source("plot_ttd.r", local = T)
source("plot_vitals.r", local = T)

source("compare_det_plots_server.r", local = T)
source("compare_det_plots_server2.r", local = T)
source("compare_det_plotsUI.r", local = T)
source("compare_det_plotsUI2.r", local = T)
source("compare_det_server.r", local = T)
source("compare_detUI.r", local = T)

source("compare_stoch_plots_server.r", local = T)
source("compare_stoch_plots_server2.r", local = T)
source("compare_stoch_plotsUI.r", local = T)
source("compare_stoch_plotsUI2.r", local = T)
source("compare_stoch_server.r", local = T)
source("compare_stochUI.r", local = T)

source("det_mod_server.r", local = T)
source("det_modUI.r", local = T)

source("server.r", local = T)
source("stoch_modUI.r", local = T)
source("stoch_mod_server.r", local = T)
source("ui.r", local = T)
