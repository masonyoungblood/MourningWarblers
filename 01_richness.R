#set working directory and load packages
setwd("~/Documents/Work/Spring 2022/Mourning Warblers/MourningWarblers")
library(iNEXT)
library(ggplot2)
library(cowplot)

#import data and convert to a list
data <- read.csv("data/regiolect_distributions.csv")[, -1]
combined_data <- sort(aggregate(n ~ ., FUN = sum, data = data[, c(1, 2)])$n, decreasing = TRUE)
data <- list(west = sort(aggregate(n ~ ., FUN = sum, data = data[which(data$regio == "west"), c(1, 2)])$n, decreasing = TRUE),
             east = sort(aggregate(n ~ ., FUN = sum, data = data[which(data$regio == "east"), c(1, 2)])$n, decreasing = TRUE),
             nova = sort(aggregate(n ~ ., FUN = sum, data = data[which(data$regio == "nova"), c(1, 2)])$n, decreasing = TRUE),
             newf = sort(aggregate(n ~ ., FUN = sum, data = data[which(data$regio == "newf"), c(1, 2)])$n, decreasing = TRUE))

#estimate richness values with unseen species model
all <- iNEXT(combined_data, q = 0)
west <- iNEXT(data[1], q = 0)
east <- iNEXT(data[2], q = 0)
nova <- iNEXT(data[3], q = 0)
newf <- iNEXT(data[4], q = 0)
all$AsyEst[1, 4] #312
west$AsyEst[1, 4]
east$AsyEst[1, 4]
nova$AsyEst[1, 4]
newf$AsyEst[1, 4]

#construct plots
west_plot <- ggiNEXT(west) + ggtitle("Western") + 
  xlab("Number of Sampled Syllables") + ylab("Richness (*q* = 0)") + 
  theme_linedraw() + theme(legend.position = "none", axis.title.y = ggtext::element_markdown()) + 
  guides(linetype = "none") + scale_color_manual(values = "black") + scale_fill_manual(values = "black")
east_plot <- ggiNEXT(east) + ggtitle("Eastern") + 
  xlab("Number of Sampled Syllables") + ylab("Richness (*q* = 0)") + 
  theme_linedraw() + theme(legend.position = "none", axis.title.y = ggtext::element_markdown()) + 
  guides(linetype = "none") + scale_color_manual(values = "black") + scale_fill_manual(values = "black")
nova_plot <- ggiNEXT(nova) + ggtitle("Nova Scotia") + 
  xlab("Number of Sampled Syllables") + ylab("Richness (*q* = 0)") + 
  theme_linedraw() + theme(legend.position = "none", axis.title.y = ggtext::element_markdown()) + 
  guides(linetype = "none") + scale_color_manual(values = "black") + scale_fill_manual(values = "black")
newf_plot <- ggiNEXT(newf) + ggtitle("Newfoundland") + 
  xlab("Number of Sampled Syllables") + ylab("Richness (*q* = 0)") + 
  theme_linedraw() + theme(legend.position = "none", axis.title.y = ggtext::element_markdown()) + 
  guides(linetype = "none") + scale_color_manual(values = "black") + scale_fill_manual(values = "black")

#export plot
png("figures/richness.png", units = "in", width = 8, height = 6, res = 600)
plot_grid(west_plot, east_plot, nova_plot, newf_plot)
dev.off()
