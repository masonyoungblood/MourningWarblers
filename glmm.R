#set working directory
setwd("~/Documents/Work/Spring 2022/Mourning Warblers/MourningWarblers")

#load data and convert categorical variables to factors
data <- read.csv("syllable_params.csv")
data$regiolect <- factor(data$regiolect)
data$survive <- factor(data$survive)

#normalize concavity by duration
data$concavity <- data$concavity/data$duration

# #run full bayesian logistic model
# full_bayesian_model <- rstanarm::stan_glmer(survive ~ (1|regiolect) + scale(duration) + scale(max_freq) + scale(min_freq) + scale(concavity), data = data, family = binomial, iter = 4000)
# save(full_bayesian_model, file = "glmm.RData")
load("glmm.RData")

#print summaries
summary(full_bayesian_model, digits = 3)
rstanarm::posterior_interval(full_bayesian_model)
