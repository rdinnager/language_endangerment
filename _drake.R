try(file.remove("log_file.txt"))
source("R/packages.R")
source("R/functions.R")
source("R/plan.R")
# options(clustermq.scheduler = "multicore") # optional parallel computing
drake_config(plan, targets = c("final_data_csv",
                               "final_data_w_projections_csv",
                               NULL), 
             verbose = 1L, memory_strategy = "autoclean", garbage_collection = TRUE,
             log_make = "log_file.txt")
