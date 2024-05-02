#clean environment
rm(list = ls())

#load libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(boot)

# function to Calculate standard error
se_lower <- function(data){ #lower limit
    data = na.omit(data)
    se_lower = mean(data)-  (sd(data)/sqrt(length(data)))
}
se_upper <- function(data){ #upper limit
    data = na.omit(data)
    se_upper = mean(data)+  (sd(data)/sqrt(length(data)))
}

#read in data
#read historical validation data for loss, density, housing price, policy purchases
hist <- read.csv("C:/SANDEEP/Shmodel_US/Data/scaled_validation_data.csv")
colnames(hist) <- c("Year", "L", "D", "HP", "A", "state")

#convert to percentage
hist$L <- hist$L * 100
hist$A <- hist$A * 100


#read model forecasted data
s2 <- read.csv("C:/SANDEEP/Shmodel_US/Projection/original surge/s2_merged_mean.csv") #for scenario ssp245
s5 <- read.csv("C:/SANDEEP/Shmodel_US/Projection/original surge/s5_merged_mean.csv") #for scenario ssp585

#convert to pecentage
s2$L <- s2$L * 100
s5$L <- s5$L * 100

s2$A <- s2$A * 100
s5$A <- s5$A * 100

#remove states that didn't calibrate well
hist <- hist %>% filter(!state %in% c("Alabama", "Mississippi", "Delaware", "New Hampshire", "Georgia") )
s2 <- s2 %>% filter(!state %in% c("Alabama", "Mississippi", "Delaware", "New Hampshire", "Georgia") )
s5 <- s5 %>% filter(!state %in% c("Alabama", "Mississippi", "Delaware", "New Hampshire", "Georgia") )

#convert states to abbreviated form
# Lookup table for state abbreviations
state_abbreviations <- c(
    "Washington" = "WA", "Oregon" = "OR", "California" = "CA", "Texas" = "TX", 
    "Mississippi" = "MS", "Alabama" = "AL", "Florida" = "FL", "Georgia" = "GA", 
    "South Carolina" = "SC", "North Carolina" = "NC", "Virginia" = "VA", 
    "Maryland" = "MD", "Delaware" = "DE", "New Jersey" = "NJ", "New York" = "NY", 
    "Connecticut" = "CT", "Rhode Island" = "RI", "Massachusetts" = "MA", 
    "New Hampshire" = "NH", "Maine" = "ME"
)

hist$state <- state_abbreviations[hist$state]
s2$state <- state_abbreviations[s2$state]
s5$state <- state_abbreviations[s5$state]

#For loss and policy for historical case, group data
hist_loss <- hist %>% select(state, L, A) %>% 
    group_by(state) %>% 
    summarise(annual_loss = mean(L, na.rm =T), annual_loss25 = se_lower(L), annual_loss75 = se_upper(L), 
              annual_policy = mean(A, na.rm =T), annual_policy25 = se_lower(A), annual_policy75 = se_upper(A))
hist_loss$tag = "hist"

#order the states based on descending order of flood loss, this is used in the order of plots
loss_desc = arrange(hist_loss, desc(annual_loss))
state_order_desc = loss_desc$state

#For scenario ssp245, group data for period 2021-2050
s2_loss2050 <- s2 %>% filter(Year <= 2050) %>% 
    select(state, L, A) %>% 
    group_by(state) %>% 
    summarise(annual_loss = mean(L, na.rm =T), annual_loss25 = se_lower(L), annual_loss75 = se_upper(L), 
              annual_policy = mean(A, na.rm =T), annual_policy25 = se_lower(A), annual_policy75 = se_upper(A))
s2_loss2050$tag <- "2050"

#For scenario ssp245, group data for period 2050-2100
s2_loss2100 <- s2 %>% filter(Year > 2050) %>% 
    select(state, L, A) %>% 
    group_by(state) %>% 
    summarise(annual_loss = mean(L, na.rm =T), annual_loss25 = se_lower(L), annual_loss75 = se_upper(L), 
              annual_policy = mean(A, na.rm =T), annual_policy25 = se_lower(A), annual_policy75 = se_upper(A))
s2_loss2100$tag <- "2100"

#merge all grouped datasets into a single dataframe
df <- rbind(hist_loss, s2_loss2050, s2_loss2100)
df$tag <- factor(df$tag, levels = c("hist", "2050", "2100"))
df$state <- factor(df$state, levels = state_order_desc)

#add the ratio of loss to policy to the dataframe
df$lossTOpolicy <- df$annual_loss/df$annual_policy
df$lossTOpolicy25 <- df$annual_loss25/df$annual_policy25
df$lossTOpolicy75 <- df$annual_loss75/df$annual_policy75

#make histogram plot of loss, policy, loss to policy for three different time periods
pl1 <- df %>% ggplot(aes(x = state, y = annual_loss, fill = tag))+
    geom_bar(stat = "identity", position = position_dodge())+
    geom_errorbar(aes(ymin = annual_loss25, ymax = annual_loss75),position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5, color = "grey30" )+
    #geom_text(aes(label = round(annual_loss, 3), vjust = -0.5),position = position_dodge(width = 0.9))+
    labs(y = "Annual average flood loss claims\nto housing price ratio (%)")+
    theme_pubr()+
    scale_fill_brewer(palette = "Reds")

pl2 <- df %>% ggplot(aes(x = state, y = annual_policy, fill = tag))+
    geom_bar(stat = "identity", position = position_dodge())+
    geom_errorbar(aes(ymin = annual_policy25, ymax = annual_policy75),position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5, color = "grey30" )+
    #geom_text(aes(label = round(annual_policy, 0), vjust = -0.5),position = position_dodge(width = 0.9))+
    labs(y = "Annual average active flood insurace policy\nto number of housing ratio (%)")+
    theme_pubr()+
    scale_fill_brewer(palette = "Blues")

pl3 <- df %>% ggplot(aes(x = state, y = lossTOpolicy, fill = tag))+
    geom_bar(stat = "identity", position = position_dodge())+
    geom_errorbar(aes(ymin = lossTOpolicy25, ymax = lossTOpolicy75),position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5, color = "grey30" )+
    #geom_text(aes(label = round(annual_policy, 0), vjust = -0.5),position = position_dodge(width = 0.9))+
    labs(y = "Annual average Flood loss\nover active policy")+
    theme_pubr()+
    scale_fill_brewer(palette = "Greens")

plot1 <- ggarrange(pl1, pl2,pl3, nrow = 3)

#save plot for ssp245 scenario
ggsave(plot = plot1, "C:/SANDEEP/Shmodel_US/Loss barplot/loss&policySSP2 8x8.png", width = 8, height = 9, dpi = 300, bg = "white")
ggsave(plot = plot1, "C:/SANDEEP/Shmodel_US/Loss barplot/loss&policySSP2 8x8.pdf", width = 8, height = 9)


##################################################
#For scenario ssp585
s5_loss2050 <- s5 %>% filter(Year <= 2050) %>% 
    select(state, L, A) %>% 
    group_by(state) %>% 
    summarise(annual_loss = mean(L, na.rm =T), annual_loss25 = se_lower(L), annual_loss75 = se_upper(L), 
              annual_policy = mean(A, na.rm =T), annual_policy25 = se_lower(A), annual_policy75 = se_upper(A))
s5_loss2050$tag <- "2050"

s5_loss2100 <- s5 %>% filter(Year > 2050) %>% 
    select(state, L, A) %>% 
    group_by(state) %>% 
    summarise(annual_loss = mean(L, na.rm =T), annual_loss25 = se_lower(L), annual_loss75 = se_upper(L), 
              annual_policy = mean(A, na.rm =T), annual_policy25 = se_lower(A), annual_policy75 = se_upper(A))
s5_loss2100$tag <- "2100"

#merge all datasets
df5 <- rbind(hist_loss, s5_loss2050, s5_loss2100)
df5$tag <- factor(df$tag, levels = c("hist", "2050", "2100"))
df5$state <- factor(df$state, levels = state_order_desc)

#add the ratio of loss to policy
df5$lossTOpolicy <- df5$annual_loss/df5$annual_policy
df5$lossTOpolicy25 <- df5$annual_loss25/df5$annual_policy25
df5$lossTOpolicy75 <- df5$annual_loss75/df5$annual_policy75

#make histogram plot of loss and policy for three different time periods
pl4 <- df5 %>% ggplot(aes(x = state, y = annual_loss, fill = tag))+
    geom_bar(stat = "identity", position = position_dodge())+
    geom_errorbar(aes(ymin = annual_loss25, ymax = annual_loss75),position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5, color = "grey30" )+
    #geom_text(aes(label = round(annual_loss, 3), vjust = -0.5),position = position_dodge(width = 0.9))+
    labs(y = "Annual average flood loss claims\nto housing value ratio (%)")+
    theme_pubr()+
    scale_fill_brewer(palette = "Reds")


pl5 <- df5 %>% ggplot(aes(x = state, y = annual_policy, fill = tag))+
    geom_bar(stat = "identity", position = position_dodge())+
    geom_errorbar(aes(ymin = annual_policy25, ymax = annual_policy75),position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5, color = "grey30" )+
    #geom_text(aes(label = round(annual_policy, 0), vjust = -0.5),position = position_dodge(width = 0.9))+
    labs(y = "Annual average active flood insurace policy\nto number of housing ratio (%)")+
    theme_pubr()+
    scale_fill_brewer(palette = "Blues")

pl6 <- df5 %>% ggplot(aes(x = state, y = lossTOpolicy, fill = tag))+
    geom_bar(stat = "identity", position = position_dodge())+
    geom_errorbar(aes(ymin = lossTOpolicy25, ymax = lossTOpolicy75),position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5, color = "grey30" )+
    #geom_text(aes(label = round(annual_policy, 0), vjust = -0.5),position = position_dodge(width = 0.9))+
    labs(y = "Annual average Flood loss\nover active policy")+
    theme_pubr()+
    scale_fill_brewer(palette = "Greens")

plot2 <- ggarrange(pl4, pl5,pl6, nrow = 3)

#save plot for ssp245 scenario
ggsave(plot = plot2, "C:/SANDEEP/Shmodel_US/Loss barplot/loss&policySSP5 8x8.png", width = 8, height = 9, dpi = 300, bg = "white")
ggsave(plot = plot2, "C:/SANDEEP/Shmodel_US/Loss barplot/loss&policySSP5 8x8.pdf", width = 8, height = 9)
