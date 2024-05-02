#clean work environment
rm(list = ls())

#load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

#read in file
#function to read file and calculate mean, median, and percentiles
calc_mean_percent <- function(file_path){
    file <- read.csv(file_path) %>% 
        select(Year, L, A, D, HP, state) %>% 
        group_by(Year, state) %>% 
        summarise( L_mean = mean(L), A_median = median(A), D_median = median(D), HP_median = median(HP),
                   L_p25 = quantile(L, 0.05), L_p75 = quantile(L, 0.95),
                   A_p25 = quantile(A, 0.25), A_p75 = quantile(A, 0.75),
                   D_p25 = quantile(D, 0.25), D_p75 = quantile(D, 0.75),
                   HP_p25 = quantile(HP, 0.25), HP_p75 = quantile(HP, 0.75))
}

#read scenario 245 forecasted data
fin2 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/original surge/s2_merged_mean.csv")
fin2$scenario = "s2"
fin2$surge = "Surge Threshold"

fin2S1 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 1/s2_merged_mean.csv")
fin2S1$scenario = "s2"
fin2S1$surge = "Surge Threshold + 1m"

fin2S2 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 2/s2_merged_mean.csv")
fin2S2$scenario = "s2"
fin2S2$surge = "Surge Threshold + 2m"

fin2S3 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 100/s2_merged_mean.csv")
fin2S3$scenario = "s2"
fin2S3$surge = "Surge Threshold + 100m"

#read scenario 585 forecasted data
fin5 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/original surge/s5_merged_mean.csv")
fin5$scenario = "s5"
fin5$surge = "Surge Threshold"

fin5S1 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 1/s5_merged_mean.csv")
fin5S1$scenario = "s5"
fin5S1$surge = "Surge Threshold + 1m"

fin5S2 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 2/s5_merged_mean.csv")
fin5S2$scenario = "s5"
fin5S2$surge = "Surge Threshold + 2m"

fin5S3 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 100/s5_merged_mean.csv")
fin5S3$scenario = "s5"
fin5S3$surge = "Surge Threshold + 100m"

#combines files 
fall <- rbind(fin2, fin2S1, fin2S2, fin2S3, fin5, fin5S1, fin5S2,  fin5S3)

#arrange states from west to east
#order of states from west to east in US
state_order <- c("Washington", "Oregon", "California", "Texas", "Mississippi", "Alabama",
                 "Florida", "Georgia", "South Carolina", "North Carolina", "Virginia", "Maryland",
                 "Delaware", "New Jersey", "New York", "Connecticut", "Rhode Island",
                 "Massachusetts", "New Hampshire", "Maine")
#reorder the factor level of svis
fall$state <- factor(fall$state, levels = state_order)

#filter out uncalibrated states
fall <- fall %>% filter(!state %in% c("Alabama", "Delaware", "Georgia", "Mississippi", "New Hampshire"))
fall$surge <- factor(fall$surge, levels = c("Surge Threshold", "Surge Threshold + 1m", "Surge Threshold + 2m", "Surge Threshold + 100m"))

#make plots
my_col <- brewer.pal(n = 3, name = "RdBu")
my_colors <- c(my_col[3], my_col[1])

#For Housing Price

#For Flood Awareness

#For Cumulative Loss

#For Housing Density
pl3 <- fall %>% ggplot(aes(x = Year, y = D_median))+
    geom_ribbon(aes(ymin = D_p25, ymax = D_p75, fill = scenario), alpha = 0.2)+
    #geom_line(aes(color = scenario), linewidth = 1.2)+
    geom_smooth(aes(color = scenario), se = F, span = 0.2, linewidth = 0.8)+
    facet_grid(surge~state, scales = "free_y")+
    scale_fill_manual(values = my_colors)+
    scale_color_manual(values = my_colors)+
    theme_bw()+
    ylab("Multi-model median housing density")+
    coord_cartesian(ylim = c(0,1))+
    #theme(panel.grid = element_blank())+
    #scale_y_continuous(breaks = c(0.0, 0.8, 1))+
    scale_x_continuous(breaks = c(2040, 2080))+
    theme(legend.position = "none")

#save final plot to local storage
ggsave(plot = pl3, "C:/SANDEEP/Shmodel_US/Projection/DensityByStates_MultiThreshold.png", width = 15, height = 8, dpi = 300)


