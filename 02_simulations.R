#set working directory and load packages
#setwd("~/Documents/Work/Spring 2022/Mourning Warblers/MourningWarblers")
setwd(system("pwd", intern = T))
source("functions.R")

#read in the regiolect data
data <- read.csv("data/regiolect_distributions.csv")

#identify unique types
types <- sort(unique(data$type))

#set total number of possible types: asymptote of the overall missing species richness model
total_possible_syls <- 312

#store years and regiolects for parsing
years <- c(1985, 2012, 2019)
regios <- c("west", "east", "newf", "nova")

#convert to object for modeling
regio_dists <- lapply(regios, function(x){
  #get frequencies from first year in that regiolect
  temp <- data[which(data$year == 1985 & data$regio == x), ]
  
  #match with existing types
  out <- temp$n[match(types, temp$type)]
  out[which(is.na(out))] <- 0
  
  #pad with zeroes to match number of total possible types and return
  out <- c(out, rep(0, total_possible_syls - length(out)))
  return(out)
})

#set number of simulations and priors for all four regions
n_sims <- 100000
priors <- data.frame(n_init = round(runif(n_sims, 5000, 50000)),
                     mu = rbeta(n_sims, 1, 40),
                     dems = round(KScorrect::rlunif(n_sims, 2, 8)),
                     frequency = truncnorm::rtruncnorm(n_sims, 0, Inf, 1, 0.2),
                     content = truncnorm::rtruncnorm(n_sims, 0, Inf, 0, 2))

#store parameter for splitting n_init into the four regiolects
all_squares <- 1000 + 1000 + 100 + 100

#create model with simplified input parameters for parallelization
simple_model <- function(params, regio_dists){
  model(n_init = c(round((params[1]/all_squares)*1000), round((params[1]/all_squares)*1000), 
                   round((params[1]/all_squares)*100), round((params[1]/all_squares)*100)), 
        regio_dists = regio_dists, total_possible_syls = total_possible_syls,
        mu = params[2], 
        dems = c(params[3], params[3], params[3], params[3]), 
        frequency = c(params[4], params[4], params[4], params[4]), 
        content = c(params[5], params[5], params[5], params[5]))
}

#run simulations
simulations <- parallel::mclapply(1:n_sims, function(x){simple_model(as.numeric(priors[x, ]), regio_dists)}, mc.cores = 7)

#combine with priors and save
priors_and_simulations <- list(priors, simulations)
save(priors_and_simulations, file = "data/priors_and_simulations.RData")
