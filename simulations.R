#POPULATION SIZES IN EACH YEAR AND LOCATION
#west = 60, 164, 181
#east = 76, 219, 227
#nova = 13, 24, 32
#newf = 47, 92, 100

#ESTIMATED ASYMPTOTIC RICHNESS IN EACH LOCATION
#west = 121
#east = 95
#nova = 38
#newf = 57

#set working directory and load packages
#setwd("~/Documents/Work/Spring 2022/Mourning Warblers/MourningWarblers")
setwd(system("pwd", intern = T))
source("functions.R")

#store required packages
pkgs <- unique(getParseData(parse("functions.R"))$text[getParseData(parse("functions.R"))$token == "SYMBOL_PACKAGE"])

#load regiolect distributions
regiolects <- read.csv("regiolect_distributions_repertoires.csv")
colnames(regiolects) <- gsub("X", "", colnames(regiolects))
west <- list(regiolects[,1][-which(is.na(regiolects[,1]))], regiolects[,2][-which(is.na(regiolects[,2]))], regiolects[,3][-which(is.na(regiolects[,3]))])
east <- list(regiolects[,4][-which(is.na(regiolects[,4]))], regiolects[,5][-which(is.na(regiolects[,5]))], regiolects[,6][-which(is.na(regiolects[,6]))])
nova <- list(regiolects[,7][-which(is.na(regiolects[,7]))], regiolects[,8][-which(is.na(regiolects[,8]))], regiolects[,9][-which(is.na(regiolects[,9]))])
newf <- list(regiolects[,10][-which(is.na(regiolects[,10]))], regiolects[,11][-which(is.na(regiolects[,11]))], regiolects[,12][-which(is.na(regiolects[,12]))])

#set number of simulations and priors for all four regions
n_sims <- 100000
priors <- data.frame(n_init = runif(n_sims, 250, 10000),
                     mu = rbeta(n_sims, 1, 40),
                     frequency = truncnorm::rtruncnorm(n_sims, 0, Inf, 1, 0.4),
                     content = truncnorm::rtruncnorm(n_sims, 0, Inf, 0, 4))

# WEST SIMULATIONS --------------------------------------------------------

#wrap simpler model for slurm
wrapped_model <- function(n_init, mu, frequency, content){
  model(n_init = n_init, syl_init = west[[1]], total_possible_syls = 121, inds_to_sample = c(60, 164, 181), mu = mu, frequency = frequency, content = content)
}

#run simulations
slurm <- rslurm::slurm_apply(wrapped_model, priors, jobname = "west",
                             nodes = 6, cpus_per_node = 20, pkgs = pkgs,
                             global_objects = objects(), slurm_options = list(mem = "230G"))

#get output and clean files
output <- rslurm::get_slurm_out(slurm)
rslurm::cleanup_files(slurm)

#save output
west_simulations <- list(priors = priors, sum_stats = output)
save(west_simulations, file = "west_simulations.RData")

rm(list = c("wrapped_model", "output", "west_simulations"))

# EAST SIMULATIONS --------------------------------------------------------

#wrap simpler model for slurm
wrapped_model <- function(n_init, mu, frequency, content){
  model(n_init = n_init, syl_init = east[[1]], total_possible_syls = 95, inds_to_sample = c(76, 219, 227), mu = mu, frequency = frequency, content = content)
}

#run simulations
slurm <- rslurm::slurm_apply(wrapped_model, priors, jobname = "east",
                             nodes = 6, cpus_per_node = 20, pkgs = pkgs,
                             global_objects = objects(), slurm_options = list(mem = "230G"))

#get output and clean files
output <- rslurm::get_slurm_out(slurm)
rslurm::cleanup_files(slurm)

#save output
east_simulations <- list(priors = priors, sum_stats = output)
save(east_simulations, file = "east_simulations.RData")

rm(list = c("wrapped_model", "output", "west_simulations"))

# NOVA SIMULATIONS --------------------------------------------------------

#wrap simpler model for slurm
wrapped_model <- function(n_init, mu, frequency, content){
  model(n_init = n_init, syl_init = nova[[1]], total_possible_syls = 38, inds_to_sample = c(13, 24, 32), mu = mu, frequency = frequency, content = content)
}

#run simulations
slurm <- rslurm::slurm_apply(wrapped_model, priors, jobname = "nova",
                             nodes = 6, cpus_per_node = 20, pkgs = pkgs,
                             global_objects = objects(), slurm_options = list(mem = "230G"))

#get output and clean files
output <- rslurm::get_slurm_out(slurm)
rslurm::cleanup_files(slurm)

#save output
nova_simulations <- list(priors = priors, sum_stats = output)
save(nova_simulations, file = "nova_simulations.RData")

rm(list = c("wrapped_model", "output", "west_simulations"))

# NEWF SIMULATIONS --------------------------------------------------------

#wrap simpler model for slurm
wrapped_model <- function(n_init, mu, frequency, content){
  model(n_init = n_init, syl_init = newf[[1]], total_possible_syls = 57, inds_to_sample = c(47, 92, 100), mu = mu, frequency = frequency, content = content)
}

#run simulations
slurm <- rslurm::slurm_apply(wrapped_model, priors, jobname = "newf",
                             nodes = 6, cpus_per_node = 20, pkgs = pkgs,
                             global_objects = objects(), slurm_options = list(mem = "230G"))

#get output and clean files
output <- rslurm::get_slurm_out(slurm)
rslurm::cleanup_files(slurm)

#save output
newf_simulations <- list(priors = priors, sum_stats = output)
save(newf_simulations, file = "newf_simulations.RData")
