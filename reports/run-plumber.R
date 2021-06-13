#!/usr/bin/env Rscript

library(plumber)
pr <- plumb('/opt/shiny-server/apps/SUCE_archive/reports/plumber.R')
pr$run(port=7000, host = "0.0.0.0")

