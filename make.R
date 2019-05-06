rm(list = ls())
source("R/packages.R")
source("R/document_parameters.R")
source("R/functions.R")
source("R/plan.R")

config <- drake_config(plan)
vis_drake_graph(config)

make(plan) # defined in R/plan.R

