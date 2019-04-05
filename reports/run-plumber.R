#!/usr/bin/env Rscript

library(plumber)
pr <- plumb('plumber.R')
pr$run(port=8000, host = "0.0.0.0")

