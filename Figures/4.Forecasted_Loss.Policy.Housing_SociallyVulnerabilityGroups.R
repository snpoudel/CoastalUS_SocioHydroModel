#clean work environment
rm(list = ls())

#load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

#read in file
#read svi(social vulnerability) for all us coastal tracts with category(high, medium, low)
svi <- read.csv("C:/SANDEEP/Shmodel_US/SVI/svi_3category.csv")
colnames(svi)[1] = "census_tract"

#read data generated during model calibration period (Historical)
hist <- read.csv("C:/SANDEEP/Shmodel_US/Output/historical/only_good_kge/SH_Results_withstate.csv") 
hist <- hist %>%
    filter(hist$census_tract %in% svi$census_tract) %>%
    select(c("Year", "L", "HP", "A", "D", "state", "census_tract"))

#two erroneous loss for texas in 2008, above 1 value delete that
hist$L[hist$L > 0.99] = NA

#append svi column 
hist <- merge(hist, svi, by = "census_tract") 
hist <- hist %>% select(-census_tract, -state) #now remove census tract column
hist <- hist %>%  #group by and summarize
    group_by(Year, svis) %>%
    summarise( L_mean = mean(L, na.rm = T), A_median = median(A), D_median = median(D), HP_median = median(HP),
               L_p25 = quantile(L, 0.05, na.rm = T), L_p75 = quantile(L, 0.95, na.rm = T),
               A_p25 = quantile(A, 0.25), A_p75 = quantile(A, 0.75),
               D_p25 = quantile(D, 0.25), D_p75 = quantile(D, 0.75),
               HP_p25 = quantile(HP, 0.25), HP_p75 = quantile(HP, 0.75))
hist$scenario = "hist"

#read scenario 5 data, SSP585
fin5 <- read.csv("C:/SANDEEP/Shmodel_US/Projection/original surge/s5_merged_mean.csv") 
fin5 <- fin5 %>%
    filter(fin5$census_tract %in% svi$census_tract) 
fin5 <- merge(fin5, svi, by = "census_tract")
fin5 <- fin5 %>% select(-census_tract, -state)
fin5 <- fin5 %>% 
    group_by(Year, svis) %>% 
    summarise( L_mean = mean(L), A_median = median(A), D_median = median(D), HP_median = median(HP),
               L_p25 = quantile(L, 0.05), L_p75 = quantile(L, 0.95),
               A_p25 = quantile(A, 0.25), A_p75 = quantile(A, 0.75),
               D_p25 = quantile(D, 0.25), D_p75 = quantile(D, 0.75),
               HP_p25 = quantile(HP, 0.25), HP_p75 = quantile(HP, 0.75))
fin5$scenario = "s5"


######################################################
#combines files for scenario ssp585
fall <- rbind(hist, fin5)

#reorder the factor level of svis
fall$svis <- factor(fall$svis, levels = c("low svi", "medium svi", "high svi"))

#make plots
my_colors <- c("grey50", "red")
my_col1 <- brewer.pal(n = 3, name = "Greys")
my_col2 <- brewer.pal(n = 3, name = "YlOrRd")
my_col <- c(my_col1, my_col2)

#For Housing Price
pl1 <- fall %>% ggplot(aes(x = Year, y = HP_median))+
    geom_ribbon(aes(ymin = HP_p25, ymax = HP_p75, fill = interaction(svis, scenario)), color = "grey90", alpha = 0.2, linewidth = 0.3)+
    geom_smooth(aes(color = scenario, linetype = svis), se = F, span = 0.2,  linewidth = 0.75)+
    scale_fill_manual(values = my_col)+
    scale_color_manual(values = my_colors)+
    scale_linetype_manual(values = c("dashed", "dotted", "solid"))+
    scale_x_continuous(breaks = c(1980, 2020, 2060, 2100))+
    lims( y = c(0,1))+
    labs(y = "Housing Price")+
    theme_pubr()

#For Awareness
pl2 <- fall %>% ggplot(aes(x = Year, y = A_median))+
    geom_ribbon(aes(ymin = A_p25, ymax = A_p75, fill = interaction(svis, scenario)), color = "grey90", alpha = 0.2, linewidth = 0.3)+
    geom_smooth(aes(color = scenario, linetype = svis), se = F, span = 0.2,  linewidth = 0.75)+
    scale_fill_manual(values = my_col)+
    scale_color_manual(values = my_colors)+
    scale_linetype_manual(values = c("dashed", "dotted", "solid"))+
    scale_x_continuous(breaks = c(1980, 2020, 2060, 2100))+
    coord_cartesian( ylim = c(0,0.6))+
    labs(y = "Active Policies")+
    theme_pubr()


#For Density
pl3 <- fall %>% ggplot(aes(x = Year, y = D_median))+
    geom_ribbon(aes(ymin = D_p25, ymax = D_p75, fill = interaction(svis, scenario)), color = "grey90", alpha = 0.2, linewidth = 0.3)+
    geom_smooth(aes(color = scenario, linetype = svis), se = F, span = 0.2,  linewidth = 0.75)+
    scale_fill_manual(values = my_col)+
    scale_color_manual(values = my_colors)+
    scale_linetype_manual(values = c("dashed", "dotted", "solid"))+
    scale_x_continuous(breaks = c(1980, 2020, 2060, 2100))+
    coord_cartesian( ylim = c(0,1))+
    labs(y = "Housing Density")+
    theme_pubr()

#For Cumulative Loss
#Find maximum cumulative value for historical and start projecction with that value
fall_cum <- fall
fall_cumulative <- fall %>% 
    group_by(scenario, svis) %>%
    mutate(cumulative_L_mean = cumsum(L_mean), cumulative_L25 = cumsum(L_p25), cumulative_L75 = cumsum(L_p75) )
max_lowsvi =  max(fall_cumulative$cumulative_L_mean[fall_cumulative$scenario == "hist"& fall_cumulative$svis == "low svi"])
max_mediumsvi =  max(fall_cumulative$cumulative_L_mean[fall_cumulative$scenario == "hist"&fall_cumulative$svis == "medium svi"])
max_highsvi =  max(fall_cumulative$cumulative_L_mean[fall_cumulative$scenario == "hist"&fall_cumulative$svis == "high svi"])

max_lowsvi25 =  max(fall_cumulative$cumulative_L25[fall_cumulative$scenario == "hist"& fall_cumulative$svis == "low svi"])
max_mediumsvi25 =  max(fall_cumulative$cumulative_L25[fall_cumulative$scenario == "hist"&fall_cumulative$svis == "medium svi"])
max_highsvi25 =  max(fall_cumulative$cumulative_L25[fall_cumulative$scenario == "hist"&fall_cumulative$svis == "high svi"])

max_lowsvi75 =  max(fall_cumulative$cumulative_L75[fall_cumulative$scenario == "hist"& fall_cumulative$svis == "low svi"])
max_mediumsvi75 =  max(fall_cumulative$cumulative_L75[fall_cumulative$scenario == "hist"&fall_cumulative$svis == "medium svi"])
max_highsvi75 =  max(fall_cumulative$cumulative_L75[fall_cumulative$scenario == "hist"&fall_cumulative$svis == "high svi"])

#now add this max cum values to start the projection cum series
fall_cum$L_mean[fall_cum$Year == 2021 & fall_cum$scenario == "s5" & fall_cum$svis == "low svi"] =  max_lowsvi
fall_cum$L_mean[fall_cum$Year == 2021 & fall_cum$scenario == "s5" & fall_cum$svis == "medium svi"] =  max_mediumsvi
fall_cum$L_mean[fall_cum$Year == 2021 & fall_cum$scenario == "s5" & fall_cum$svis == "high svi"] =  max_highsvi

fall_cum$L_p25[fall_cum$Year == 2021 & fall_cum$scenario == "s5" & fall_cum$svis == "low svi"] =  max_lowsvi25
fall_cum$L_p25[fall_cum$Year == 2021 & fall_cum$scenario == "s5" & fall_cum$svis == "medium svi"] =  max_mediumsvi25
fall_cum$L_p25[fall_cum$Year == 2021 & fall_cum$scenario == "s5" & fall_cum$svis == "high svi"] =  max_highsvi25

fall_cum$L_p75[fall_cum$Year == 2021 & fall_cum$scenario == "s5" & fall_cum$svis == "low svi"] =  max_lowsvi75
fall_cum$L_p75[fall_cum$Year == 2021 & fall_cum$scenario == "s5" & fall_cum$svis == "medium svi"] =  max_mediumsvi75
fall_cum$L_p75[fall_cum$Year == 2021 & fall_cum$scenario == "s5" & fall_cum$svis == "high svi"] =  max_highsvi75


pl4 <- fall_cum %>%
    group_by(scenario, svis) %>%
    mutate(cumulative_L_mean = cumsum(L_mean), cumulative_L25 = cumsum(L_p25), cumulative_L75 = cumsum(L_p75)) %>%
    ggplot(aes(x = Year, y = cumulative_L_mean)) +
    geom_ribbon(aes(ymin = cumulative_L25, ymax = cumulative_L75, fill = interaction(svis, scenario)), color = "grey90", alpha = 0.2, linewidth = 0.8)+
    geom_smooth(aes(color = scenario, linetype = svis), span = 0.1, se = FALSE, linewidth = 0.75) +
    scale_fill_manual(values = my_col) +
    scale_color_manual(values = my_colors) +
    scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
    scale_x_continuous(breaks = c(1980, 2020, 2060, 2100))+
    coord_cartesian( ylim = c(0,0.6))+
    labs(y = "Cumulative Flood Loss") +
    theme_pubr()

#arrange plots
ggarrange(pl4, pl2, pl3, pl1, nrow=2, ncol = 2)

#save final plot to local storage
ggsave("C:/SANDEEP/Shmodel_US/Projection/LADHP for 3svis 10x6 SSP585.png", width = 8, height = 6, dpi = 300, bg = "white")
ggsave("C:/SANDEEP/Shmodel_US/Projection/LADHP for 3svis 10x6 SSP585.pdf", width = 8, height = 6)
