#set working directory and load packages
setwd("~/Documents/Work/Spring 2022/Mourning Warblers/MourningWarblers")
library(iNEXT)
library(ggplot2)
library(cowplot)

#import data and convert to a list
data <- read.csv("regiolect_distributions_all_years.csv")
names <- gsub("X", "", colnames(data))
data <- lapply(1:ncol(data), function(x){data[, x]})
for(i in 1:length(data)){
  if(length(which(is.na(data[[i]]))) > 0){
    data[[i]] <- data[[i]][-which(is.na(data[[i]]))]
  }
}
names(data) <- c("west", "east", "nova", "newf")

#estimate richness values with unseen species model
west <- iNEXT(data[1], q = 0)
east <- iNEXT(data[2], q = 0)
nova <- iNEXT(data[3], q = 0)
newf <- iNEXT(data[4], q = 0)
west$AsyEst[1, 4]
east$AsyEst[1, 4]
nova$AsyEst[1, 4]
newf$AsyEst[1, 4]

#construct plots
west_plot <- ggiNEXT(west) + ggtitle("Western") + 
  xlab("Number of sampled syllables") + ylab("Richness (*q* = 0)") + 
  theme_linedraw() + theme(legend.position = "none", axis.title.y = ggtext::element_markdown()) + 
  guides(linetype = "none") + scale_color_manual(values = "red") + scale_fill_manual(values = "red")
east_plot <- ggiNEXT(east) + ggtitle("Eastern") + 
  xlab("Number of sampled syllables") + ylab("Richness (*q* = 0)") + 
  theme_linedraw() + theme(legend.position = "none", axis.title.y = ggtext::element_markdown()) + 
  guides(linetype = "none") + scale_color_manual(values = "blue") + scale_fill_manual(values = "blue")
nova_plot <- ggiNEXT(nova) + ggtitle("Nova Scotia") + 
  xlab("Number of sampled syllables") + ylab("Richness (*q* = 0)") + 
  theme_linedraw() + theme(legend.position = "none", axis.title.y = ggtext::element_markdown()) + 
  guides(linetype = "none") + scale_color_manual(values = "green") + scale_fill_manual(values = "green")
newf_plot <- ggiNEXT(newf) + ggtitle("Newfoundland") + 
  xlab("Number of sampled syllables") + ylab("Richness (*q* = 0)") + 
  theme_linedraw() + theme(legend.position = "none", axis.title.y = ggtext::element_markdown()) + 
  guides(linetype = "none") + scale_color_manual(values = "violet") + scale_fill_manual(values = "violet")

#export plot
png("figures/richness.png", units = "in", width = 8, height = 6, res = 300)
plot_grid(west_plot, east_plot, nova_plot, newf_plot)
dev.off()
