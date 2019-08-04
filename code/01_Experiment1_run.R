# F. Campelo, E. Wanner: 
# "Sample size calculations for the experimental comparison of multiple 
# algorithms on multiple problem instances."
# Submitted, Journal of Heuristics, 2019

# Replication script for experiment number 1: Comparison of algorithm 
# configurations for the parallel machine scheduling problem with 
# setup times.
# ============================================================================ #

# Load packages
library(CAISEr)   # Version 1.0.6

# Prepare algorithm function to be used in run_experiment():
myalgo <- function(instance, time.limit = NA, seed = NA,
                   moves.to.remove = character(0)){
  
  run.args <- "java -jar upmsp-all.jar optimize"
  
  instance.args <- paste0("--instance ../data/Experiment1_Instances/", instance$FUN)
  
  time.args <- ""
  if (!is.na(time.limit)) time.args <- paste("--time-limit", time.limit)
  
  seed.args <- ""
  if (!is.na(seed)) seed.args <- paste("--seed", seed)
  
  type.args <- ""
  for (i in seq_along(moves.to.remove)){
    type.args   <- paste0(type.args,
                          " --param disable=",
                          moves.to.remove[i])
  }
  
  runcmd <- paste(run.args,
                  seed.args,
                  time.args,
                  instance.args,
                  type.args)
  
  output <- system(runcmd, intern = TRUE)
  
  myres <- as.numeric(unlist(strsplit(output, split = " ")))
  
  if (is.numeric(myres) && length(myres == 2)) {
    return(list(value = myres[1]))
  } else {
    return(list(value = 1e9))
  }
}


# Assemble instances list
instance.files <- dir("../data/Experiment1_Instances/", pattern = ".txt")

instances <- vector(length(instance.files), mode = "list")
for (i in 1:length(instances)){
  instances[[i]]$FUN   <- instance.files[i]
  instances[[i]]$alias <- gsub(".txt", "", instance.files[i])
}


# Assemble algorithms list. Notice that we need to provide an alias for each
# method, since all algorithms have the same '$FUN' argument.
move.names  <- c("shift", "direct-swap", "swap", 
                 "switch", "task-move", "two-shift")
move.alias  <- c("SHF", "DSW", "SWP", 
                 "SWT", "TSK", "2SH")

algorithms <- vector(mode = "list", length = 1 + length(move.names))
algorithms[[1]] <- list(FUN             = "myalgo",
                        alias           = "Full")
for (i in 1:length(move.names)){
  algorithms[[i+1]] <- list(FUN             = "myalgo",
                            alias           = paste0("no-", move.alias[i]),
                            moves.to.remove = move.names[i])
}

move.pairs  <- t(combn(move.names, 2))
pair.alias  <- t(combn(move.alias, 2))

for (i in 1:nrow(move.pairs)){
  indx  <- length(move.names) + i + 1
  alias <- paste0("no-", paste0(pair.alias[i, ], collapse = ".no-"))
  algorithms[[indx]] <- list(FUN             = "myalgo",
                             alias           = alias,
                             moves.to.remove = move.pairs[i, ])
}

# Define experimental parameters:
d      <- .5      # MRES for this experiment. 
se.max <- 0.05    # Measurement error: +-0.05 (5%)
power  <- 0.8     # Desired power.
sig.level <- 0.05 # Desired familywise significance level
dif = "perc"      # Use percent difference as the paired estimates of 
                  # differences in performance
nstart = 10       # Each algorithm starts with 10 runs on each instance                    
seed = 2019       # seed

comparisons = "all.vs.first"    # Desired comparisons: all vs. full algorithm
nmax = 50 * length(algorithms)  # Max number of runs/instance

ncpus = parallel::detectCores() - 1 # Number of CPUs to use - USE WITH CARE!

my.results <- run_experiment(instances = instances, 
                             algorithms = algorithms, 
                             d = d, 
                             se.max = se.max,
                             power = power, 
                             sig.level = sig.level,
                             dif = dif, 
                             comparisons = comparisons,
                             nstart = nstart, 
                             nmax = nmax,
                             seed = seed,
                             force.balanced = FALSE,
                             ncpus = ncpus,
                             save.partial.results ='../data/00-Exp01-results',
                             save.final.result = '../data/')