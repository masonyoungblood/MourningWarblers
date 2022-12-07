#set working directory and load packages
setwd("~/Documents/Work/Spring 2022/Mourning Warblers/MourningWarblers")

library(ggplot2)
library(cowplot)

models <- c("west", "east", "nova", "newf")
n_sims <- 100000
tols <- c(0.005, 0.001, 0.0005)

# abc_output <- list(west = list(tol005 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                tol001 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                tol0005 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                prior = NA,
#                                posterior = NA,
#                                observed = NA),
#                    east = list(tol005 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                tol001 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                tol0005 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                prior = NA,
#                                posterior = NA,
#                                observed = NA),
#                    nova = list(tol005 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                tol001 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                tol0005 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                prior = NA,
#                                posterior = NA,
#                                observed = NA),
#                    newf = list(tol005 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                tol001 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                tol0005 = list(n_init = NA, mu = NA, dems = NA, frequency = NA, content = NA),
#                                prior = NA,
#                                posterior = NA,
#                                observed = NA))
# 
# for(i in 1:length(models)){
#   target_model <- models[i]
# 
#   #load regiolect distributions
#   load(paste0(target_model, "_simulations.RData"))
# 
#   if(target_model == "west"){
#     simulations <- west_simulations
#     rm(west_simulations)
#   }
#   if(target_model == "east"){
#     simulations <- east_simulations
#     rm(east_simulations)
#   }
#   if(target_model == "nova"){
#     simulations <- nova_simulations
#     rm(nova_simulations)
#   }
#   if(target_model == "newf"){
#     simulations <- newf_simulations
#     rm(newf_simulations)
#   }
# 
#   regiolects <- read.csv("regiolect_distributions_repertoires.csv")
#   colnames(regiolects) <- gsub("X", "", colnames(regiolects))
# 
#   if(target_model == "west"){data <- list(regiolects[,1][-which(is.na(regiolects[,1]))], regiolects[,2][-which(is.na(regiolects[,2]))], regiolects[,3])}
#   if(target_model == "east"){data <- list(regiolects[,4][-which(is.na(regiolects[,4]))], regiolects[,5][-which(is.na(regiolects[,5]))], regiolects[,6][-which(is.na(regiolects[,6]))])}
#   if(target_model == "nova"){data <- list(regiolects[,7][-which(is.na(regiolects[,7]))], regiolects[,8][-which(is.na(regiolects[,8]))], regiolects[,9][-which(is.na(regiolects[,9]))])}
#   if(target_model == "newf"){data <- list(regiolects[,10][-which(is.na(regiolects[,10]))], regiolects[,11][-which(is.na(regiolects[,11]))], regiolects[,12][-which(is.na(regiolects[,12]))])}
# 
#   # obs_stats <- c(sapply(0:2, function(x){hillR::hill_taxa(data[[2]], q = x)}),
#   #                sapply(0:2, function(x){hillR::hill_taxa(data[[3]], q = x)}))
#   #
#   # sum_stats <- lapply(1:length(simulations$sum_stats), function(y){
#   #   c(sapply(0:2, function(x){hillR::hill_taxa(simulations$sum_stats[[y]][[1]], q = x)}),
#   #     sapply(0:2, function(x){hillR::hill_taxa(simulations$sum_stats[[y]][[2]], q = x)}))
#   # })
#   #
#   # dists <- sapply(1:length(sum_stats), function(x){dist(rbind(obs_stats, sum_stats[[x]]))})
# 
#   max_length <- max(unlist(lapply(1:length(simulations$sum_stats), function(x){lengths(simulations$sum_stats[[x]])})))
# 
#   #interpolated distributions from real data
#   obs_first_dists <- c(data[[2]], rep(0, max_length-length(data[[2]])))
#   obs_second_dists <- c(data[[3]], rep(0, max_length-length(data[[3]])))
# 
#   #interpolated distributions from first measured timepoint
#   first_dists <- lapply(1:length(simulations$sum_stats), function(x){
#     c(simulations$sum_stats[[x]][[1]], rep(0, max_length-length(simulations$sum_stats[[x]][[1]])))
#   })
# 
#   #interpolated distributions from second measured timepoint
#   second_dists <- lapply(1:length(simulations$sum_stats), function(x){
#     c(simulations$sum_stats[[x]][[2]], rep(0, max_length-length(simulations$sum_stats[[x]][[2]])))
#   })
# 
#   dists <- sapply(1:length(simulations$sum_stats), function(x){
#     dist(rbind(obs_first_dists, first_dists[[x]])) + dist(rbind(obs_second_dists, second_dists[[x]]))
#   })
# 
#   abc_output[[i]]$prior <- lapply(1:n_sims, function(x){simulations$sum_stats[[x]][[2]]})
#   abc_output[[i]]$observed <- data[[3]]
# 
#   for(j in 1:length(tols)){
#     posts <- order(dists, decreasing = FALSE)[1:(n_sims*tols[j])]
# 
#     if(j == 2){abc_output[[i]]$posterior <- lapply(posts, function(x){simulations$sum_stats[[x]][[2]]})}
# 
#     abc_output[[i]][[j]]$n_init <- simulations$priors$n_init[posts]
#     abc_output[[i]][[j]]$mu <- simulations$priors$mu[posts]
#     abc_output[[i]][[j]]$dems <- simulations$priors$dems[posts]
#     abc_output[[i]][[j]]$frequency <- simulations$priors$frequency[posts]
#     abc_output[[i]][[j]]$content <- simulations$priors$content[posts]
# 
#     rm(posts)
#   }
# 
#   # rm(list = c("target_model", "simulations", "regiolects", "data", "obs_stats", "sum_stats", "dists"))
#   rm(list = c("target_model", "simulations", "regiolects", "data", "max_length", "obs_first_dists", "obs_second_dists", "first_dists", "second_dists", "dists"))
# }
# 
# save(abc_output, file = "abc_output.RData")

load("abc_output.RData")

plot_abc <- function(abc_output, models, param, tol, xlim, xlab){
  if(param == "n_init"){
    param <- 1
    prior <- dunif(seq(xlim[1], xlim[2], length.out = 2^10), min = 250, max = 10000)
  }
  if(param == "mu"){
    param <- 2
    prior <- dbeta(seq(xlim[1], xlim[2], length.out = 2^10), 1, 40)
  }
  if(param == "frequency"){
    param <- 3
    prior <- truncnorm::dtruncnorm(seq(xlim[1], xlim[2], length.out = 2^10), a = 0, mean = 1, sd = 0.4)
  }
  if(param == "content"){
    param <- 4
    prior <- truncnorm::dtruncnorm(seq(xlim[1], xlim[2], length.out = 2^10), a = 0, mean = 0, sd = 4)
  }
  
  if(tol == 0.005){tol <- 1}
  if(tol == 0.001){tol <- 2}
  if(tol == 0.0005){tol <- 3}
  
  data <- data.frame(param = c(seq(xlim[1], xlim[2], length.out = 2^10),
                               density(abc_output$west[[tol]][[param]], n = 2^10, from = xlim[1], to = xlim[2])$x,
                               density(abc_output$east[[tol]][[param]], n = 2^10, from = xlim[1], to = xlim[2])$x,
                               density(abc_output$nova[[tol]][[param]], n = 2^10, from = xlim[1], to = xlim[2])$x,
                               density(abc_output$newf[[tol]][[param]], n = 2^10, from = xlim[1], to = xlim[2])$x),
                     density = c(prior,
                                 density(abc_output$west[[tol]][[param]], n = 2^10, from = xlim[1], to = xlim[2])$y,
                                 density(abc_output$east[[tol]][[param]], n = 2^10, from = xlim[1], to = xlim[2])$y,
                                 density(abc_output$nova[[tol]][[param]], n = 2^10, from = xlim[1], to = xlim[2])$y,
                                 density(abc_output$newf[[tol]][[param]], n = 2^10, from = xlim[1], to = xlim[2])$y),
                     Model = factor(rep(tools::toTitleCase(c("prior", models)), each = 2^10), levels = tools::toTitleCase(c("prior", models))))
  
  ggplot(data = data, aes(x = param, y = density, group = Model)) + 
    geom_line(aes(linetype = Model, color = Model, size = Model)) + 
    scale_linetype_manual(values = c("solid", rep("solid", 4))) + 
    scale_color_manual(values = c("black", "red", "blue", "green", "violet")) + 
    scale_size_manual(values = c(0.5, 0.5, 0.5, 0.5, 0.5)) + 
    xlab(xlab) + ylab("Density") + theme_linedraw() + 
    theme(legend.title = element_blank())
}

# PLOT POSTERIORS ---------------------------------------------------------

mu_plot <- plot_abc(abc_output, models, "mu", 0.001, c(0, 0.2), "Innovation Rate")
n_init_plot <- plot_abc(abc_output, models, "n_init", 0.001, c(250, 10000), "Population Size")
content_plot <- plot_abc(abc_output, models, "content", 0.001, c(0, 12), "Content Bias")
frequency_plot <- plot_abc(abc_output, models, "frequency", 0.001, c(0, 2), "Frequency Bias")

png(paste0("figures/main_posteriors.png"), width = 7, height = 3, units = "in", res = 300)
plot_grid(frequency_plot + theme(legend.position = "none") + geom_vline(xintercept = 1, linetype = "dashed"),
          content_plot + theme(legend.position = "none") + geom_vline(xintercept = 0, linetype = "dashed"),
          get_legend(frequency_plot), nrow = 1, rel_widths = c(1, 1, 0.2), labels = c("A", "B"))
dev.off()

png(paste0("figures/suppl_posteriors.png"), width = 7, height = 3, units = "in", res = 300)
plot_grid(n_init_plot + theme(legend.position = "none"),
          mu_plot + theme(legend.position = "none"),
          get_legend(frequency_plot), nrow = 1, rel_widths = c(1, 1, 0.2), labels = c("A", "B"))
dev.off()


# PLOT POSTERIORS AT ALTERNATE THRESHOLDS ---------------------------------

mu_plot_small <- plot_abc(abc_output, models, "mu", 0.0005, c(0, 0.2), "Innovation Rate")
n_init_plot_small <- plot_abc(abc_output, models, "n_init", 0.0005, c(250, 10000), "Population Size")
content_plot_small <- plot_abc(abc_output, models, "content", 0.0005, c(0, 12), "Content Bias")
frequency_plot_small <- plot_abc(abc_output, models, "frequency", 0.0005, c(0, 2), "Frequency Bias")

mu_plot_big <- plot_abc(abc_output, models, "mu", 0.005, c(0, 0.2), "Innovation Rate")
n_init_plot_big <- plot_abc(abc_output, models, "n_init", 0.005, c(250, 10000), "Population Size")
content_plot_big <- plot_abc(abc_output, models, "content", 0.005, c(0, 12), "Content Bias")
frequency_plot_big <- plot_abc(abc_output, models, "frequency", 0.005, c(0, 2), "Frequency Bias")

png(paste0("figures/all_posteriors_small.png"), width = 7, height = 6, units = "in", res = 300)
plot_grid(frequency_plot_small + theme(legend.position = "none") + geom_vline(xintercept = 1, linetype = "dashed"),
          content_plot_small + theme(legend.position = "none") + geom_vline(xintercept = 0, linetype = "dashed"),
          get_legend(frequency_plot_small), 
          n_init_plot_small + theme(legend.position = "none"),
          mu_plot_small + theme(legend.position = "none"),
          get_legend(frequency_plot_small) + theme(legend.position = "none"), 
          nrow = 2, ncol = 3, rel_widths = c(1, 1, 0.2), labels = c("A", "B", "", "C", "D", ""))
dev.off()

png(paste0("figures/all_posteriors_big.png"), width = 7, height = 6, units = "in", res = 300)
plot_grid(frequency_plot_big + theme(legend.position = "none") + geom_vline(xintercept = 1, linetype = "dashed"),
          content_plot_big + theme(legend.position = "none") + geom_vline(xintercept = 0, linetype = "dashed"),
          get_legend(frequency_plot_big), 
          n_init_plot_big + theme(legend.position = "none"),
          mu_plot_big + theme(legend.position = "none"),
          get_legend(frequency_plot_big) + theme(legend.position = "none"), 
          nrow = 2, ncol = 3, rel_widths = c(1, 1, 0.2), labels = c("A", "B", "", "C", "D", ""))
dev.off()

# POSTERIOR SIMULATIONS ---------------------------------------------------

png(paste0("figures/west_posts.png"), width = 5, height = 3, units = "in", res = 300)
par(mar = c(2, 4, 0.1, 0.1))
plot(abc_output$west$observed, type = "l", xlab = "", ylab = "Frequency", col = "white")
for(x in 1:length(abc_output$west$prior)){lines(abc_output$west$prior[[x]], col = scales::alpha("gray88", 1))}
for(x in 1:length(abc_output$west$posterior)){lines(abc_output$west$posterior[[x]], col = scales::alpha("red", 1))}
lines(abc_output$west$observed, col = "black", lwd = 2)
dev.off()

png(paste0("figures/east_posts.png"), width = 5, height = 3, units = "in", res = 300)
par(mar = c(2, 4, 0.1, 0.1))
plot(abc_output$east$observed, type = "l", xlab = "", ylab = "Frequency", col = "white")
for(x in 1:length(abc_output$east$prior)){lines(abc_output$east$prior[[x]], col = scales::alpha("gray88", 1))}
for(x in 1:length(abc_output$east$posterior)){lines(abc_output$east$posterior[[x]], col = scales::alpha("dodgerblue", 1))}
lines(abc_output$east$observed, col = "black", lwd = 2)
dev.off()

png(paste0("figures/nova_posts.png"), width = 5, height = 3, units = "in", res = 300)
par(mar = c(2, 4, 0.1, 0.1))
plot(abc_output$nova$observed, type = "l", xlab = "", ylab = "Frequency", col = "white")
for(x in 1:length(abc_output$nova$prior)){lines(abc_output$nova$prior[[x]], col = scales::alpha("gray88", 1))}
for(x in 1:length(abc_output$nova$posterior)){lines(abc_output$nova$posterior[[x]], col = scales::alpha("green2", 1))}
lines(abc_output$nova$observed, col = "black", lwd = 2)
dev.off()

png(paste0("figures/newf_posts.png"), width = 5, height = 3, units = "in", res = 300)
par(mar = c(2, 4, 0.1, 0.1))
plot(abc_output$newf$observed, type = "l", xlab = "", ylab = "Frequency", col = "white")
for(x in 1:length(abc_output$newf$prior)){lines(abc_output$newf$prior[[x]], col = scales::alpha("gray88", 1))}
for(x in 1:length(abc_output$newf$posterior)){lines(abc_output$newf$posterior[[x]], col = scales::alpha("violet", 1))}
lines(abc_output$newf$observed, col = "black", lwd = 2)
dev.off()

png(paste0("figures/combined_posts.png"), width = 6.5, height = 4, units = "in", res = 300)
plot_grid(ggplot() + draw_image("figures/west_posts.png") + theme_void(),
          ggplot() + draw_image("figures/east_posts.png") + theme_void(),
          ggplot() + draw_image("figures/nova_posts.png") + theme_void(),
          ggplot() + draw_image("figures/newf_posts.png") + theme_void(),
          labels = c("A", "B", "C", "D"))
dev.off()

file.remove(c("figures/west_posts.png", "figures/east_posts.png", "figures/nova_posts.png", "figures/newf_posts.png"))
