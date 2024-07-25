#set working directory and load packages
setwd("~/Documents/Work/Spring 2022/Mourning Warblers/MourningWarblers")
library(ggplot2)
library(cowplot)
load("data/priors_and_simulations.RData")

#load in posterior predictions and convert back to original scale for plotting
posts <- jsonlite::read_json("04_bayesflow_analysis/posterior_predictions.json")
posts <- data.frame(do.call(rbind, lapply(1:length(posts), function(x){unlist(posts[[x]])})))
colnames(posts) <- c("n_init", "mu", "dems", "frequency", "content")
posts$n_init <- (posts$n_init*sd(priors_and_simulations[[1]]$n_init)) + mean(priors_and_simulations[[1]]$n_init)
posts$mu <- (posts$mu*sd(priors_and_simulations[[1]]$mu)) + mean(priors_and_simulations[[1]]$mu)
posts$dems <- (posts$dems*sd(priors_and_simulations[[1]]$dems)) + mean(priors_and_simulations[[1]]$dems)
posts$frequency <- (posts$frequency*sd(priors_and_simulations[[1]]$frequency)) + mean(priors_and_simulations[[1]]$frequency)
posts$content <- (posts$content*sd(priors_and_simulations[[1]]$content)) + mean(priors_and_simulations[[1]]$content)

#write function for plotting
plot_abc <- function(posts, param, xlim, xlab){
  #specify probability distributions for each parameter
  if(param == "n_init"){
    param <- 1
    prior <- dunif(seq(xlim[1], xlim[2], length.out = 2^10), min = 5000, max = 50000)
  }
  if(param == "mu"){
    param <- 2
    prior <- dbeta(seq(xlim[1], xlim[2], length.out = 2^10), 1, 40)
  }
  if(param == "dems"){
    param <- 3
    prior <- dunif(seq(xlim[1], xlim[2], length.out = 2^10), min = 2, max = 5)
  }
  if(param == "frequency"){
    param <- 4
    prior <- truncnorm::dtruncnorm(seq(xlim[1], xlim[2], length.out = 2^10), a = 0, mean = 1, sd = 0.2)
  }
  if(param == "content"){
    param <- 5
    prior <- truncnorm::dtruncnorm(seq(xlim[1], xlim[2], length.out = 2^10), a = 0, mean = 0, sd = 2)
  }
  
  #structure data for plotting
  data <- data.frame(param = c(seq(xlim[1], xlim[2], length.out = 2^10),
                               density(posts[, param], n = 2^10, from = xlim[1], to = xlim[2])$x),
                     density = c(prior,
                                 density(posts[, param], n = 2^10, from = xlim[1], to = xlim[2])$y),
                     group = factor(c(rep(0, 2^10), rep(1, 2^10))))
  
  #create partial ggplot
  if(param %in% c(1, 3, 4, 5)){
    temp <- ggplot(data = data, aes(x = param, y = density, group = group)) + 
      geom_line(aes(linetype = group, color = group)) + 
      scale_x_continuous(expand = c(0, 0), breaks = seq(from = xlim[1], to = xlim[2], length.out = 4)) + 
      scale_color_manual(values = c("grey", "black")) + 
      scale_linetype_manual(values = c("solid", "solid")) + 
      xlab(xlab) + ylab("Density") + theme_linedraw() + 
      theme(legend.position = "none", plot.margin = margin(t = 5, r = 15, b = 5, l = 5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.y = element_text(angle = 90), axis.title.y = element_text(vjust = -4))
  } else{
    temp <- ggplot(data = data, aes(x = param, y = density, group = group)) + 
      geom_line(aes(linetype = group, color = group)) + 
      scale_x_continuous(expand = c(0, 0), breaks = seq(from = xlim[1], to = xlim[2], length.out = 4),
                         labels = scales::number_format(accuracy = 0.01, decimal.mark = '.')) + 
      scale_color_manual(values = c("grey", "black")) + 
      scale_linetype_manual(values = c("solid", "solid")) + 
      xlab(xlab) + ylab("Density") + theme_linedraw() + 
      theme(legend.position = "none", plot.margin = margin(t = 5, r = 15, b = 5, l = 5),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.y = element_text(angle = 90), axis.title.y = element_text(vjust = -4))
  }
  
  #add the relevant y axis labels for plotting
  if(param %in% c(1, 2)){
    temp <- temp + scale_y_continuous(expand = c(0, 0), breaks = c(max(data$density)), limits = c(0, max(data$density)), labels = scales::scientific_format(digits = 2))
  }
  if(param %in% c(4, 5)){
    temp <- temp + scale_y_continuous(expand = c(0, 0), breaks = c(max(data$density)), limits = c(0, max(data$density)), labels = round(max(data$density), digits = 1))
  }
  
  #return object
  return(temp)
}

# PLOT POSTERIORS ---------------------------------------------------------

#create plot for each parameter to be included in main panel
n_init_plot <- plot_abc(posts, "n_init", c(5000, 50000), "Population Size")
mu_plot <- plot_abc(posts, "mu", c(0, 0.1), "Innovation Rate")
content_plot <- plot_abc(posts, "content", c(0, 6), "Content Bias")
frequency_plot <- plot_abc(posts, "frequency", c(0, 3), "Frequency Bias")

#save figure
png(paste0("figures/main_posteriors.png"), width = 7, height = 5, units = "in", res = 600)
plot_grid(n_init_plot + theme(legend.position = "none"),
          mu_plot + theme(legend.position = "none"),
          frequency_plot + theme(legend.position = "none") + geom_vline(xintercept = 1, linetype = "dashed"),
          content_plot + theme(legend.position = "none"),
          nrow = 2, labels = c("A", "B", "C", "D"))
dev.off()

# POSTERIOR SIMULATIONS ---------------------------------------------------

#read in the regiolect data
data <- read.csv("data/regiolect_distributions.csv")
types <- sort(unique(data$type))
regios <- c("west", "east", "newf", "nova")

#convert to object for modeling
regio_dists <- lapply(regios, function(x){
  temp <- data[which(data$year == 2019 & data$regio == x), ]
  out <- temp$n[match(types, temp$type)]
  out[which(is.na(out))] <- 0
  out <- sort(out, decreasing = TRUE)
  out <- out[which(out > 0)]
  return(out)
})

#store observed values and scale priors to compute distance to estimated values
obs_vals <- c(median((posts$n_init - mean(priors_and_simulations[[1]]$n_init))/sd(priors_and_simulations[[1]]$n_init)),
              median((posts$mu - mean(priors_and_simulations[[1]]$mu))/sd(priors_and_simulations[[1]]$mu)),
              median((posts$dems - mean(priors_and_simulations[[1]]$dems))/sd(priors_and_simulations[[1]]$dems)),
              median((posts$frequency - mean(priors_and_simulations[[1]]$frequency))/sd(priors_and_simulations[[1]]$frequency)),
              median((posts$content - mean(priors_and_simulations[[1]]$content))/sd(priors_and_simulations[[1]]$content)))
scaled_priors <- scale(priors_and_simulations[[1]])

#compute distances between prior parameter values and estimated parameter values
dists <- sapply(1:nrow(scaled_priors), function(x){dist(rbind(obs_vals, scaled_priors[x, ]))})

#keep the 100 closest distributions to plot
closest <- order(dists)[1:100]
closest <- priors_and_simulations[[2]][closest]

#plot closest distributions for the west regiolect
png(paste0("figures/west_posts.png"), width = 5, height = 3.5, units = "in", res = 600)
par(mar = c(4, 4, 0.1, 0.1))
plot(regio_dists[[1]], type = "l", xlab = "Syllable Type (Ranked)", ylab = "Frequency", col = "white")
for(x in 1:length(priors_and_simulations[[2]])){
  vals <- sort(priors_and_simulations[[2]][[x]][[2]][[1]], decreasing = TRUE)
  vals <- vals[which(vals > 0)]
  lines(vals, col = scales::alpha("gray88", 1))
}
for(x in 1:length(closest)){
  vals <- sort(closest[[x]][[2]][[1]], decreasing = TRUE)
  vals <- vals[which(vals > 0)]
  lines(vals, col = scales::alpha("black", 1))
}
lines(regio_dists[[1]], col = "red", lwd = 2)
dev.off()

#plot closest distributions for the east regiolect
png(paste0("figures/east_posts.png"), width = 5, height = 3.5, units = "in", res = 600)
par(mar = c(4, 4, 0.1, 0.1))
plot(regio_dists[[2]], type = "l", xlab = "Syllable Type (Ranked)", ylab = "Frequency", col = "white")
for(x in 1:length(priors_and_simulations[[2]])){
  vals <- sort(priors_and_simulations[[2]][[x]][[2]][[2]], decreasing = TRUE)
  vals <- vals[which(vals > 0)]
  lines(vals, col = scales::alpha("gray88", 1))
}
for(x in 1:length(closest)){
  vals <- sort(closest[[x]][[2]][[2]], decreasing = TRUE)
  vals <- vals[which(vals > 0)]
  lines(vals, col = scales::alpha("black", 1))
}
lines(regio_dists[[2]], col = "red", lwd = 2)
dev.off()

#plot closest distributions for the newfoundland regiolect
png(paste0("figures/newf_posts.png"), width = 5, height = 3.5, units = "in", res = 600)
par(mar = c(4, 4, 0.1, 0.1))
plot(regio_dists[[3]], type = "l", xlab = "Syllable Type (Ranked)", ylab = "Frequency", col = "white")
for(x in 1:length(priors_and_simulations[[2]])){
  vals <- sort(priors_and_simulations[[2]][[x]][[2]][[3]], decreasing = TRUE)
  vals <- vals[which(vals > 0)]
  lines(vals, col = scales::alpha("gray88", 1))
}
for(x in 1:length(closest)){
  vals <- sort(closest[[x]][[2]][[3]], decreasing = TRUE)
  vals <- vals[which(vals > 0)]
  lines(vals, col = scales::alpha("black", 1))
}
lines(regio_dists[[3]], col = "red", lwd = 2)
dev.off()

#plot closest distributions for the nova scotia regiolect
png(paste0("figures/nova_posts.png"), width = 5, height = 3.5, units = "in", res = 600)
par(mar = c(4, 4, 0.1, 0.1))
plot(regio_dists[[4]], type = "l", xlab = "Syllable Type (Ranked)", ylab = "Frequency", col = "white")
for(x in 1:length(priors_and_simulations[[2]])){
  vals <- sort(priors_and_simulations[[2]][[x]][[2]][[4]], decreasing = TRUE)
  vals <- vals[which(vals > 0)]
  lines(vals, col = scales::alpha("gray88", 1))
}
for(x in 1:length(closest)){
  vals <- sort(closest[[x]][[2]][[4]], decreasing = TRUE)
  vals <- vals[which(vals > 0)]
  lines(vals, col = scales::alpha("black", 1))
}
lines(regio_dists[[4]], col = "red", lwd = 2)
dev.off()

#combine and save as panelled figure
png(paste0("figures/combined_posts.png"), width = 5.7, height = 4, units = "in", res = 600)
plot_grid(ggplot() + draw_image("figures/west_posts.png") + theme_void(),
          ggplot() + draw_image("figures/east_posts.png") + theme_void(),
          ggplot() + draw_image("figures/nova_posts.png") + theme_void(),
          ggplot() + draw_image("figures/newf_posts.png") + theme_void(),
          labels = c("A", "B", "C", "D"))
dev.off()

#delete individual panels
file.remove(c("figures/west_posts.png", "figures/east_posts.png", "figures/nova_posts.png", "figures/newf_posts.png"))

# LOSS CURVE --------------------------------------------------------------

#read in training and validation loss
loss <- read.csv("04_bayesflow_analysis/training_loss.csv")
loss <- loss$Loss
valid <- read.csv("04_bayesflow_analysis/validation_loss.csv")
valid <- valid$Loss

#training loss is raw while validation loss is the final value for each epoch, so divide raw loss values into the 200 epochs for averaging
to_avg <- 2969

#compute median training loss for each epoch and restructure for plotting
med_loss <- sapply(1:500, function(x){
  inds <- ((to_avg*(x-1))+1):(to_avg*x)
  median(loss[inds])
})
plot_data <- data.frame(loss = med_loss, valid = valid[1:500], epoch = c(1:500))

#save plot of loss curves
png(paste0("figures/loss_curve.png"), width = 7, height = 3, units = "in", res = 600)
ggplot(plot_data, aes(x = epoch)) + 
  geom_line(aes(y = valid), color = "gray60") + 
  geom_line(aes(y = loss), color = "black") + 
  scale_x_continuous(name = "Epoch") + 
  scale_y_continuous(name = "Median Training Loss", sec.axis = sec_axis(~., "Validation Loss"), 
                     limits = c(min(med_loss)-1, max(med_loss)+1)) + 
  theme_linedraw() + 
  theme(axis.title.y.right = element_text(color = "gray60"))
dev.off()

# STABILITY ANALYSIS ------------------------------------------------------

#set working directory and load packages
setwd("~/Documents/Work/Spring 2022/Mourning Warblers/MourningWarblers")
source("functions.R")
library(ggplot2)
library(cowplot)
load("data/priors_and_simulations.RData")

#load in posterior predictions and convert back to original scale for plotting
posts <- jsonlite::read_json("04_bayesflow_analysis/posterior_predictions.json")
posts <- data.frame(do.call(rbind, lapply(1:length(posts), function(x){unlist(posts[[x]])})))
colnames(posts) <- c("n_init", "mu", "dems", "frequency", "content")
posts$n_init <- (posts$n_init*sd(priors_and_simulations[[1]]$n_init)) + mean(priors_and_simulations[[1]]$n_init)
posts$mu <- (posts$mu*sd(priors_and_simulations[[1]]$mu)) + mean(priors_and_simulations[[1]]$mu)
posts$dems <- (posts$dems*sd(priors_and_simulations[[1]]$dems)) + mean(priors_and_simulations[[1]]$dems)
posts$frequency <- (posts$frequency*sd(priors_and_simulations[[1]]$frequency)) + mean(priors_and_simulations[[1]]$frequency)
posts$content <- (posts$content*sd(priors_and_simulations[[1]]$content)) + mean(priors_and_simulations[[1]]$content)

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

#store parameter for splitting n_init into the four regiolects
all_squares <- 1000 + 1000 + 100 + 100

#sum of the number of new types entering the population at each time point, as per acerbi and bentley (2014) [10.1016/j.evolhumbehav.2014.02.003]
turnover_rate <- function(regio_dists, output){
  freqs_over_time <- rbind(regio_dists[[1]] + regio_dists[[2]] + regio_dists[[3]] + regio_dists[[4]],
                           output[[1]][[1]] + output[[1]][[2]] + output[[1]][[3]] + output[[1]][[4]],
                           output[[2]][[1]] + output[[2]][[2]] + output[[2]][[3]] + output[[2]][[4]])
  length(which(freqs_over_time[1, ] == 0 & freqs_over_time[2, ] > 0)) + length(which(freqs_over_time[2, ] == 0 & freqs_over_time[3, ] > 0))
}

# #collect turnover rates from 100 simulations with the median point estimates of each parameter
# baseline_turnovers <- unlist(parallel::mclapply(1:100, function(x){
#   output <- model(n_init = c(round((median(posts$n_init)/all_squares)*1000), round((median(posts$n_init)/all_squares)*1000), 
#                              round((median(posts$n_init)/all_squares)*100), round((median(posts$n_init)/all_squares)*100)), 
#                   regio_dists = regio_dists, total_possible_syls = total_possible_syls,
#                   mu = median(posts$mu), 
#                   dems = c(round(median(posts$dems)), round(median(posts$dems)), round(median(posts$dems)), round(median(posts$dems))), 
#                   frequency = c(median(posts$frequency), median(posts$frequency), median(posts$frequency), median(posts$frequency)), 
#                   content = c(median(posts$content), median(posts$content), median(posts$content), median(posts$content)))
#   return(turnover_rate(regio_dists, output))
# }, mc.cores = 7))
# 
# #collect turnover rates from 100 simulations with the median point estimates of each parameter, but content set to 0
# no_content_turnovers <- unlist(parallel::mclapply(1:100, function(x){
#   output <- model(n_init = c(round((median(posts$n_init)/all_squares)*1000), round((median(posts$n_init)/all_squares)*1000), 
#                              round((median(posts$n_init)/all_squares)*100), round((median(posts$n_init)/all_squares)*100)), 
#                   regio_dists = regio_dists, total_possible_syls = total_possible_syls,
#                   mu = median(posts$mu), 
#                   dems = c(round(median(posts$dems)), round(median(posts$dems)), round(median(posts$dems)), round(median(posts$dems))), 
#                   frequency = c(median(posts$frequency), median(posts$frequency), median(posts$frequency), median(posts$frequency)), 
#                   content = c(0, 0, 0, 0))
#   return(turnover_rate(regio_dists, output))
# }, mc.cores = 7))
# 
# #save them
# turnovers <- list(baseline = baseline_turnovers, no_content = no_content_turnovers)
# save(turnovers, file = "data/turnovers.RData")
load("data/turnovers.RData")

#format data for plotting
plot_data <- data.frame(turnover = c(turnovers$baseline, turnovers$no_content), Condition = factor(rep(c("Posterior", "No Content Bias"), each = length(turnovers$baseline)), levels = c("Posterior", "No Content Bias")))

#create and export plot
png(paste0("figures/turnover.png"), width = 7, height = 3, units = "in", res = 600)
ggplot(data = plot_data, aes(x = turnover, fill = Condition)) + 
  geom_histogram() + 
  scale_fill_manual(values = c("grey", "black")) + 
  xlab("Turnover (# New Syllable Types in 2005 and 2019)") + ylab("Count") + theme_linedraw() + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()
