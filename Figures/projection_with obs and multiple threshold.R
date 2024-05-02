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
        select(Year, L, A, D, HP) %>% 
        group_by(Year) %>% 
        summarise( L_mean = mean(L), A_median = median(A), D_median = median(D), HP_median = median(HP),
                   L_p25 = quantile(L, 0.05), L_p75 = quantile(L, 0.95),
                   A_p25 = quantile(A, 0.25), A_p75 = quantile(A, 0.75),
                   D_p25 = quantile(D, 0.25), D_p75 = quantile(D, 0.75),
                   HP_p25 = quantile(HP, 0.25), HP_p75 = quantile(HP, 0.75))
}

#read scenario 2 data forecasted by model, SSP245
fin2 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/original surge/s2_merged_mean.csv")
fin2$scenario = "s2"
fin2$surge = "surge_orig"

fin2S1 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 1/s2_merged_mean.csv")
fin2S1$scenario = "s2"
fin2S1$surge = "surge_1"

fin2S2 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 2/s2_merged_mean.csv")
fin2S2$scenario = "s2"
fin2S2$surge = "surge_2"

fin2S3 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 100/s2_merged_mean.csv")
fin2S3$scenario = "s2"
fin2S3$surge = "surge_100"

#read scenario 5 data forecasted by model, SSP585
fin5 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/original surge/s5_merged_mean.csv")
fin5$scenario = "s5"
fin5$surge = "surge_orig"

fin5S1 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 1/s5_merged_mean.csv")
fin5S1$scenario = "s5"
fin5S1$surge = "surge_1"

fin5S2 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 2/s5_merged_mean.csv")
fin5S2$scenario = "s5"
fin5S2$surge = "surge_2"

fin5S3 <- calc_mean_percent("C:/SANDEEP/Shmodel_US/Projection/surge plus 100/s5_merged_mean.csv")
fin5S3$scenario = "s5"
fin5S3$surge = "surge_100"

#combines all the files
fall <- rbind(fin2, fin2S1, fin2S2, fin2S3, fin5, fin5S1, fin5S2,  fin5S3)
fall$surge <- factor(fall$surge, levels = c("surge_orig", "surge_1", "surge_2", "surge_100"))

#make plots
my_col <- brewer.pal(n = 3, name = "RdBu")
my_colors <- c(my_col[3], my_col[1])

#For Housing Price
pl1 <- fall %>% ggplot(aes(x = Year, y = HP_median))+
    geom_ribbon(aes(ymin = HP_p25, ymax = HP_p75, fill = scenario), alpha = 0.3)+
    #geom_line(aes(color = scenario), linewidth = 1.2)+
    geom_smooth(aes(color = scenario), se = F, span = 0.2, linewidth = 0.75)+
    facet_wrap(~surge, ncol = 1, scales = "free_y")+
    scale_fill_manual(values = my_colors)+
    scale_color_manual(values = my_colors)+
    theme_minimal()+
    ylab("Housing Price")+
    theme(panel.grid = element_blank())+
    scale_y_continuous(breaks = c(0.0, 0.5, 1))+
    scale_x_continuous(breaks = c(2020, 2060, 2100))+
    geom_vline(xintercept =2020)+
    geom_hline(yintercept = 0)+
    theme(legend.position = "none")


#For Flood Awareness/ NFIP policy
pl2 <- fall %>% ggplot(aes(x = Year, y = A_median))+
    geom_ribbon(aes(ymin = A_p25, ymax = A_p75, fill = scenario), alpha = 0.3)+
    #geom_line(aes(color = scenario), linewidth = 1.2)+
    geom_smooth(aes(color = scenario), se = F, span = 0.2, linewidth = 0.75)+
    facet_wrap(~surge, ncol = 1)+
    coord_cartesian(ylim = c(0,0.4))+
    scale_fill_manual(values = my_colors)+
    scale_color_manual(values = my_colors)+
    theme_minimal()+
    ylab("Active Policies")+
    theme(panel.grid = element_blank())+
    scale_y_continuous(breaks = c(0.0, 0.2, 0.4))+
    scale_x_continuous(breaks = c(2020, 2060, 2100))+
    geom_vline(xintercept =2020)+
    geom_hline(yintercept = 0)+
    theme(legend.position = "none")

#For Housing Density
pl3 <- fall %>% ggplot(aes(x = Year, y = D_median))+
    geom_ribbon(aes(ymin = D_p25, ymax = D_p75, fill = scenario), alpha = 0.3)+
    #geom_line(aes(color = scenario), linewidth = 1.2)+
    geom_smooth(aes(color = scenario), se = F, span = 0.2, linewidth = 0.75)+
    facet_wrap(~surge, ncol = 1)+
    scale_fill_manual(values = my_colors)+
    scale_color_manual(values = my_colors)+
    theme_minimal()+
    ylab("Housing Density")+
    theme(panel.grid = element_blank())+
    scale_y_continuous(breaks = c(0.0, 0.3, 1))+
    scale_x_continuous(breaks = c(2020, 2060, 2100))+
    geom_vline(xintercept =2020)+
    geom_hline(yintercept = 0.3)+
    coord_cartesian(ylim = c(0.3,1))+
    theme(legend.position = "none")

#For Loss
pl4 <- fall %>% 
    group_by(scenario, surge) %>% 
    mutate(cum_L_mean = cumsum(L_mean), cumulative_L25 = cumsum(L_p25), cumulative_L75 = cumsum(L_p75)) %>% 
    ggplot(aes(x = Year, y = cum_L_mean))+
    geom_ribbon(aes(ymin = cumulative_L25, ymax = cumulative_L75, fill = scenario), alpha = 0.3)+
    #geom_line(aes(color = scenario), linewidth = 1)+
    geom_smooth(aes(color = scenario), se = F, span = 0.2, linewidth = 0.75)+
    facet_wrap(~surge, ncol = 1)+
    scale_fill_manual(values = my_colors)+
    scale_color_manual(values = my_colors)+
    theme_minimal()+
    ylab("Cumulative Flood Loss")+
    theme(panel.grid = element_blank())+
    #coord_cartesian(ylim = c(0,0.01))+
    scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3))+
    scale_x_continuous(breaks = c(2020, 2060, 2100))+
    geom_vline(xintercept =2020)+
    geom_hline(yintercept = 0)+
    theme(legend.position = "none")

#arrange all plots
ggarrange(pl4, pl2, pl3, pl1, nrow = 1)

#save final plot to local storage
ggsave("C:/SANDEEP/Shmodel_US/Projection/HP_A_L for different surge.png", width = 10, height = 6, dpi =300, bg = "white")
ggsave("C:/SANDEEP/Shmodel_US/Projection/HP_A_L for different surge.pdf", width = 10, height = 6)
