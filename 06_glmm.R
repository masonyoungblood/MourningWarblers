# SYLLABLE SURVIVAL -------------------------------------------------------

#set working directory
setwd("~/Documents/Work/Spring 2022/Mourning Warblers/MourningWarblers")

#load data
survival_data <- read.csv("data/syllable_params.csv")[, -1]
survival_data$survive <- 0
survival_data$survive[which(survival_data$type %in% survival_data$type[which(survival_data$year == 2019)])] <- 1
survival_data <- survival_data[which(survival_data$year == 1985), ]
survival_data <- survival_data[-which(duplicated(survival_data$type)), ]
survival_data$survive <- factor(survival_data$survive)

#normalize concavity by duration
survival_data$concavity <- survival_data$concavity/survival_data$duration

#run full bayesian logistic model
survival_model <- rstanarm::stan_glmer(survive ~ (1|regio) + scale(duration) + scale(max_freq) + scale(min_freq) + scale(concavity), data = survival_data, family = binomial, iter = 4000)
save(survival_model, file = "data/survival_model.RData")
load("data/survival_model.RData")

#print summaries
summary(survival_model, digits = 3)
rstanarm::posterior_interval(survival_model, prob = 0.95)

# CONCAVITY TRENDS --------------------------------------------------------

#set working directory
setwd("~/Documents/Work/Spring 2022/Mourning Warblers/MourningWarblers")

#load data
syllable_data <- read.csv("data/syllable_params.csv")[, -1]
regiolects <- read.csv("data/regiolect_distributions.csv")

#get types that were novel in the last two years
new_2005 <- unique(regiolects$type[which(regiolects$year == 2005)][which(!(regiolects$type[which(regiolects$year == 2005)] %in% regiolects$type[which(regiolects$year == 1985)]))])
old_2005 <- unique(regiolects$type[which(regiolects$year == 2005)][which(regiolects$type[which(regiolects$year == 2005)] %in% regiolects$type[which(regiolects$year == 1985)])])
new_2019 <- unique(regiolects$type[which(regiolects$year == 2019)][which(!(regiolects$type[which(regiolects$year == 2019)] %in% regiolects$type[which(regiolects$year == 2005)]))])
old_2019 <- unique(regiolects$type[which(regiolects$year == 2019)][which(regiolects$type[which(regiolects$year == 2019)] %in% regiolects$type[which(regiolects$year == 2005)])])
syllable_data$new <- 0
syllable_data$new[match(new_2005, syllable_data$type)] <- 1
syllable_data$new[match(new_2019, syllable_data$type)] <- 1

#run t-tests
t.test(syllable_data$concavity[which(syllable_data$year == 2005)], syllable_data$concavity[which(syllable_data$year == 1985)])
t.test(syllable_data$concavity[which(syllable_data$year == 2019)], syllable_data$concavity[which(syllable_data$year == 2005)])
t.test(syllable_data$concavity[which(syllable_data$new == 1 & syllable_data$year %in% c(2005, 2019))],
       syllable_data$concavity[which(syllable_data$new == 0 & syllable_data$year %in% c(2005, 2019))])
