#Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(RColorBrewer)
library(gridExtra)
library(cowplot)
library(patchwork)
#read in data
#read the calibarted model parameters across census tracts under study
df <- read.csv("C:/SANDEEP/Shmodel_US/Output/historical/only_good_kge/SH_Parameters_withstate.csv") %>% 
    select(c(-ID, -census_tract, - POT_S_max, -POT_R_max, -alpha_r, -alpha_p, -mew_p))

#states with less than 10 well calibrated census_tracts are filtered out, as it may not be representative
frequency <- as.data.frame( table(df$state) ) #calculate frequency
df <- df %>% filter(!state %in% c("Mississippi", "Alabama", "Georgia", "New Hampshire", "Delaware")) #based on frequency

#Group states by coastal region: pacific, gulf, atlantic
df$region <- NA

PC <- c("Washington", "Oregon", "California")
GC <- c("Texas", "Florida")
EC <- c("South Carolina", "North Carolina", "Virginia", "Maryland",
        "Delaware", "New Jersey", "New York", "Connecticut", "Rhode Island",
        "Massachusetts", "New Hampshire", "Maine")
df$region[df$state %in% PC] = "Pacific Coast"
df$region[df$state %in% GC] = "Golf Coast"
df$region[df$state %in% EC] = "Atlantic Coast"
df$region <- factor(df$region, levels = c("Pacific Coast", "Golf Coast", "Atlantic Coast"))

#function to transform model parameter values into factors (low, medium, high..)
transform_to_factors <- function(column) {
    percentiles <- quantile(column, probs = c(0, 0.25, 0.5, 0.75, 1))
    return(cut(column, breaks = percentiles, labels = c("0-25th", "25-50th", "50-75th", "75-100th"), include.lowest = TRUE))
}

# Apply the transformation function to all columns of the parameter dataframe
#first exclude the x axis column
new_df <- df[, setdiff(names(df), "state")]
new_df <- new_df[, setdiff(names(new_df), "region")]
new_df <- as.data.frame(lapply(new_df , transform_to_factors))
new_df$region <- df$region



# Function to return the most repeated value, calculate mode
calculate_mode <- function(x) {
    table_x <- table(x)
    mode_value <- as.character(names(table_x)[which.max(table_x)])
    return(mode_value)
}

#For each coastal region find the modal value for all parameters
new_df <- new_df %>% group_by(region) %>% 
    summarise( across(everything(), calculate_mode) )


# Set colors
my_colors <- brewer.pal(n = 4, name = "Reds")
factor_colors <- c("0-25th" = my_colors[1], "25-50th" = my_colors[2], "50-75th" = my_colors[3],"75-100th" = my_colors[4] )

# Reshape data for ggplot plotting
df_long <- pivot_longer(new_df, names_to = "factor_column", values_to = "value", -region)

#order of sh parameters
param_order <- c("Surge_threshold", "Rain_threshold", "U_rate", "mew_a", "alpha_a", "alpha_d",
                 "duration", "b3", "b2", "b1")
df_long$factor_column = factor(df_long$factor_column, levels = param_order)

# GridPlot code
pl1 <- ggplot(df_long, aes(x = region, y = factor_column, color = value, fill = value)) +
    geom_point(size = 10, shape = 22) +
    scale_color_manual(values = factor_colors)+
    scale_fill_manual(values = factor_colors) +
    theme_minimal()+
    theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank())

#Code to make density plot for parameters, keep them next to grid plot for reference!
get_density <- function(cname) {
    
    ggplot(df, aes(x = !!rlang::sym(cname))) +
        geom_density(color = "lightgrey", fill = "grey", alpha = 0.2) +
        geom_vline(xintercept = c(quantile(df[[cname]], 0.25), quantile(df[[cname]], 0.5), quantile(df[[cname]], 0.75)),
                   linetype = "solid", color = "grey", size = 0.3) +
        theme_classic() +
        theme(axis.line.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())+
        scale_x_continuous(labels = function(x) ifelse(x %% 1 == 0, as.integer(x), scales::number_format(scale = 1, accuracy = 0.1)(x)))
}

# generate density plot for all parameters
pl2 <- ggarrange( get_density("b1"), get_density("b2"), get_density("b3"), get_density("duration"),
                  get_density("alpha_d"), get_density("alpha_a"), get_density("mew_a"),get_density("U_rate"),
                  get_density("Rain_threshold"), get_density("Surge_threshold"),
                  ncol = 1)

#Arrange both plots
wrap_plots(pl1, pl2, ncol = 2, widths = c(0.8, 0.2))

#Save final plot to local storage
ggsave("C:/SANDEEP/Shmodel_US/Parallel Plot/parameters_gridplot6 bycoasts.5x5.png", width = 6.5, height = 5, dpi =300)
ggsave("C:/SANDEEP/Shmodel_US/Parallel Plot/parameters_gridplot6 bycoasts.5x5.pdf", width = 6.5, height = 5)
