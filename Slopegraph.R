library(ggplot2)
library(scales)
library(dplyr)
library(magrittr)
library(ggthemes)
library(RColorBrewer)

# Slopegraph Example: http://rpubs.com/walkerke/slopegraph

rm(list=ls())
setwd("D:/My Work/Cal Poly/2017 MSBA Winter Quarter/GSB 503/Oracle")

# --------- Load Data --------- #

load("Data/RegionReport.Rdata")
load("Data/CountryReport.Rdata")
load("Data/IndustryReport.Rdata")


# Region --------------------------------------------

# Set Index of Regions to Subset Data
locs_to_adjust1 <- c("APAC","EMEA", "LAD", "NORTHAMERICA")

# Define Label Names
label_locs <- c("NA","EMEA","LAD","APAC","NA","EMEA","LAD","APAC")

# Calculate Number of Regions
n <- length(RegionReport$Region)

# Create Data Frame for Region Slopegraph
y <- c(RegionReport$Proportion,RegionReport$WinPercent)
x <- c(rep("Proportion of Customers", n), rep("W/L Percentage", n))
reg <- c(as.character(RegionReport$Region),as.character(RegionReport$Region))
coord <- as.data.frame(cbind(x,y,reg))
colnames(coord) <- c("x","y", "region")
coord$region <- gsub(" ","",as.character(coord$region))
coord$label <- paste0(label_locs, ": ", as.character(round(y*100,0)), "%   ")

# Alternative Color Palettes (doesn't work properly)
# pal <- c('#fee5d9','#fcae91','#fb6a4a', '#cb181d')
# pal <- c('#1b9e77', '#d95f02', '#e7298a', '#66a61e')
my_palette <- c(brewer.pal(9, "Reds")[c(3,5,7,9)])
my_palette2 <- c("#CB181D", "#FCBBA1", "#FB6A4A", "#67000D")
my_palette3 <- c("#CB181D", "#bdbdbd", "#bdbdbd", "#bdbdbd")

# Create Slopegraph
reg_sg <- ggplot(coord) + 
  geom_line(aes(x = as.factor(x), y = y, group = region, color = region), size = 2) + 
  geom_point(aes(x = as.factor(x), y = y, color = region), size = 5) + 
  theme_minimal(base_size = 18) + 
  # scale_color_brewer(palette = "Reds") + 
  scale_colour_manual(values = my_palette3) +
  xlab("") + 
  geom_text(data = subset(coord, x == "Proportion of Customers" & region %in% locs_to_adjust1), 
            aes(x = as.factor(x), y = y, color=region, label = label), 
            size = 6, hjust = 1, vjust = 0.5) + 
  geom_text(data = subset(coord, x == "W/L Percentage" & region %in% locs_to_adjust1),
            aes(x = as.factor(x), y = y, color = region, label = label),
            size = 6, hjust = -.12, vjust = 0.5) +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        plot.title = element_text(color="#666666", face="bold", size=18, hjust=0),
        plot.subtitle = element_text(color="darkgrey",size=12, vjust=2)) +
  geom_hline(aes(yintercept= .4), color="darkgrey", linetype="solid") +
  labs(title = "Proportion of Customers vs. Win/Loss Percentage", subtitle = "Comparison of Opportunity Between Regions")

# Save Slopegraph
ggsave("region_slopegraph.png", reg_sg, dpi = 600, bg = "transparent")