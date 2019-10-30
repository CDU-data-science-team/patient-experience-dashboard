#!/usr/bin/env Rscript

library(plumber)
pr <- plumb('plumber.R')
pr$run(port=7000, host = "0.0.0.0")

