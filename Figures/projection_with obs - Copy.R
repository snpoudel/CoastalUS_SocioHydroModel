#clean work environment
rm(list = ls())

#load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

#order of states from west to east in US
state_order <- c("Washington", "Oregon", "California", "Texas", "Mississippi", "Alabama",
                 "Florida", "Georgia", "South Carolina", "North Carolina", "Virginia", "Maryland",
                 "Delaware", "New Jersey", "New York", "Connecticut", "Rhode Island",
                 "Massachusetts", "New Hampshire", "Maine")

#read in file
#read data generated during model calibration period (HIstorical)
hist <- read.csv("C:/SANDEEP/Shmodel_US/Output/historical/only_good_kge/SH_Results_withstate.csv")
hist <- hist %>% select(c("Year", "L", "HP", "A", "D", "state"))
hist <- hist %>% 
    group_by(Year, state) %>%
    summarise( L_mean = mean(L), A_median = median(A), D_median = median(D), HP_median = median(HP),
               L_p25 = quantile(L, 0.05), L_p75 = quantile(L, 0.95),
               A_p25 = quantile(A, 0.25), A_p75 = quantile(A, 0.75),
               D_p25 = quantile(D, 0.25), D_p75 = quantile(D, 0.75),
               HP_p25 = quantile(HP, 0.25), HP_p75 = quantile(HP, 0.75))
hist$scenario = "hist"

#read scenario ssp245 data forecasted by model
fin2 <- read.csv("C:/SANDEEP/Shmodel_US/Projection/original surge/s2_merged_mean.csv")
fin2 <- fin2 %>% 
    group_by(Year, state) %>%
    summarise( L_mean = mean(L), A_median = median(A), D_median = median(D), HP_median = median(HP),
               L_p25 = quantile(L, 0.05), L_p75 = quantile(L, 0.95),
               A_p25 = quantile(A, 0.25), A_p75 = quantile(A, 0.75),
               D_p25 = quantile(D, 0.25), D_p75 = quantile(D, 0.75),
               HP_p25 = quantile(HP, 0.25), HP_p75 = quantile(HP, 0.75))
fin2$scenario = "s2"

#read scenario ssp585 data forecasted by model
fin5 <- read.csv("C:/SANDEEP/Shmodel_US/Projection/original surge/s5_merged_mean.csv")
fin5 <- fin5 %>% 
    group_by(Year, state) %>%
    summarise( L_mean = mean(L), A_median = median(A), D_median = median(D), HP_median = median(HP),
               L_p25 = quantile(L, 0.05), L_p75 = quantile(L, 0.95),
               A_p25 = quantile(A, 0.25), A_p75 = quantile(A, 0.75),
               D_p25 = quantile(D, 0.25), D_p75 = quantile(D, 0.75),
               HP_p25 = quantile(HP, 0.25), HP_p75 = quantile(HP, 0.75))
fin5$scenario = "s5"

#read model validation data/ observed data
valid <- read.csv("C:/SANDEEP/Shmodel_US/Data/scaled_validation_data.csv")
names(valid) <- c("Year", "L", "D", "HP", "A", "state")
valid <- valid %>% 
    group_by(Year, state) %>%
    summarise( L_mean = mean(L, na.rm = T), A_median = median(A, na.rm = T), D_median = median(D, na.rm = T), HP_median = median(HP, na.rm = T),
               L_p25 = quantile(L, 0.05, na.rm = T), L_p75 = quantile(L, 0.95, na.rm = T),
               A_p25 = quantile(A, 0.25, na.rm = T), A_p75 = quantile(A, 0.75, na.rm = T),
               D_p25 = quantile(D, 0.25, na.rm = T), D_p75 = quantile(D, 0.75, na.rm = T),
               HP_p25 = quantile(HP, 0.25, na.rm = T), HP_p75 = quantile(HP, 0.75, na.rm = T))
valid$scenario = "Obs"

#combines files except validation data
fall <- rbind(hist, fin2, fin5)

#reorder the factor level of states based on the state_order
fall$state <- factor(fall$state, levels = state_order)
valid$state <- factor(valid$state, levels = state_order)

#filter out states that has fewer than 10 calibrated tracts
fall <- fall %>% filter(!(state %in% c("Mississippi", "Alabama", "Georgia", "New Hampshire", "Delaware")))
valid <- valid %>% filter(!(state %in% c("Mississippi", "Alabama", "Georgia", "New Hampshire", "Delaware")))

#make plots
my_col <- brewer.pal(n = 3, name = "RdBu")
my_colors <- c("grey", my_col[3], my_col[1])

#For Housing Price

#For Flood Awareness

#For Flood Loss

#For Housing Density
pl3 <- fall %>% ggplot(aes(x = Year, y = D_median))+
    geom_ribbon(aes(ymin = D_p25, ymax = D_p75, fill = scenario), alpha = 0.4)+
    #geom_line(aes(color = scenario), linewidth = 1.2)+
    geom_smooth(aes(color = scenario), se = F, span = 0.1, linewidth = 0.8)+
    facet_wrap(~state, nrow = 5, ncol = 3)+
    scale_fill_manual(values = my_colors)+
    scale_color_manual(values = my_colors)+
    theme_minimal()+
    ylab("Multi-model median housing density")+
    theme(panel.grid = element_blank())+
    scale_y_continuous(breaks = c(0.0, 0.5, 1))+
    scale_x_continuous(breaks = c(1980, 2020, 2060, 2100))+
    geom_vline(xintercept =1970)+
    geom_hline(yintercept = 0)+
    geom_point(data = valid, aes(x = Year, y = D_median), alpha = 0.5, shape = 16, size = 1, color = "black")

#save final plot to local storage
ggsave(plot = pl3, "C:/SANDEEP/Shmodel_US/Projection/Density10x6.png", width = 10, height = 6, dpi = 300, bg = "white")
ggsave(plot = pl3, "C:/SANDEEP/Shmodel_US/Projection/Density10x6.pdf", width = 10, height = 6)

