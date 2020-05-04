source("r/packages.R")
source("r/functions.R")
source("r/plan.R")

config <- drake_config(plan, lock_envir = FALSE)
