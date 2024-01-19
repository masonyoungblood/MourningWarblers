#set working directory and load data
setwd("~/Documents/Work/Spring 2022/Mourning Warblers/MourningWarblers")
load("data/priors_and_simulations.RData")

#store number of simulations
n_sim <- nrow(priors_and_simulations[[1]])

#store max and min values for normalization before inference
sim_max <- max(unlist(priors_and_simulations[[2]]))
sim_min <- 0

#load in regiolect data
regiolects <- read.csv("data/regiolect_distributions.csv")
colnames(regiolects) <- gsub("X", "", colnames(regiolects))

#identify unique types
types <- sort(unique(regiolects$type))

#format simulations for bayesflow
formatted_simulations <- lapply(1:n_sim, function(x){
  lapply(1:4, function(y){
    a <- sort(priors_and_simulations[[2]][[x]][[1]][[y]], decreasing = TRUE)
    b <- sort(priors_and_simulations[[2]][[x]][[2]][[y]], decreasing = TRUE)
    a <- (a - sim_min)/(sim_max - sim_min)
    b <- (b - sim_min)/(sim_max - sim_min)
    return(list(a, b))
  })
})

#format observed data to match simulated data during inference
formatted_obs <- lapply(1:4, function(y){
  if(y == 1){reg <- "west"}
  if(y == 2){reg <- "east"}
  if(y == 3){reg <- "newf"}
  if(y == 4){reg <- "nova"}
  lapply(c(2005, 2019), function(x){
    temp <- regiolects$n[which(regiolects$regio == reg & regiolects$year == x)]
    temp <- (temp - sim_min)/(sim_max - sim_min)
    return(c(temp, rep(0, length(types) - length(temp))))
  })
})

#correct column names and scale
colnames(priors_and_simulations[[1]])[1] <- c("n_init")
scaled_priors <- scale(priors_and_simulations[[1]])

#restructure priors so it's compatible with json format
priors <- lapply(1:n_sim, function(x){as.numeric(scaled_priors[x, ])})

#save data for bayesflow
train_data <- list(priors[1:49000], formatted_simulations[1:49000])
test_data <- list(priors[49001:50000], formatted_simulations[49001:50000])
jsonlite::write_json(train_data, "04_bayesflow_analysis/train_data.json")
jsonlite::write_json(test_data, "04_bayesflow_analysis/test_data.json")
jsonlite::write_json(list(formatted_obs), "04_bayesflow_analysis/obs_data.json")
