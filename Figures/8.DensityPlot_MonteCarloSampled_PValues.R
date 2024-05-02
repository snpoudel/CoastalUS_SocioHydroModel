#clean environment
rm(list = ls())

#load libraries
library(tidyverse)
library(gridExtra)

#set working directory
setwd("C:/SANDEEP/Shmodel_US/montecarlo-svis")

#read files, read model parameters for all well calibrated census tracts
param <- read.csv("SH_Parameters_withstate.csv")
#only select desired parameters
param <- param %>% select(b1, b2, b3, duration, alpha_d, alpha_a, mew_a, census_tract)

#read social vulnerability data with moe
sv_moe <- read.csv("SVCensusMOE.csv")
colnames(sv_moe)[2] <- "census_tract"
#SV Without moe
sv <- select(sv_moe, -starts_with("M_")) #remove columns of moe 

#merge param with sv
fmerge <- merge(param, sv_moe, by = "census_tract")


#make a function 
pval_func <- function(sv, sv_moe, socio){ #start of function
    #sv is the name of social vulnerability metrics
    #sv_moe is the name of social vulnerability metrics MOE
    #socio is the name of SE model parameter
    #Run monte carlo simulation
    cor_val <- numeric(10000)
    p_val <- numeric(10000)
    
    for (sim in 1:10000) { #start of monte carlo sampling
        # Choose census variable and its MOE
        census_var <- fmerge[[sv]]
        census_var_moe <- fmerge[[sv_moe]]
        se <- census_var_moe/1.645 #standard error from ACS documentation
        
        # Generate new set of SV measures for all tracts using normal distribution
        sv_est <- census_var + rnorm(length(census_var), 0, se)
        
        # Choose SE parameter
        se_par <- fmerge[[socio]]
        
        # Calculate Spearman's correlation
        sp_cor <- cor.test(sv_est, se_par, method = "spearman")
        cor_val[sim] <- sp_cor$estimate
        p_val[sim] <- sp_cor$p.value
    } #end of monte carlo sampling
    
    #plots
    df = as.data.frame(p_val)
    ggplot(df, aes(x = p_val))+
        geom_density()+
        labs(x = "P Value", y = "Density", title = paste0(sv, " ~ ", socio))+
        geom_vline(xintercept = quantile(df$p_val, 0.5), linetype = "dashed", color = "red")+
        theme_minimal()+
        theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))
    
} #end of function

#make plots
#column names for parameters and sv
param_name <- colnames(param)[1:7]
sv_names <- colnames( select(fmerge, starts_with("E_")) )
sv_moe_names <- colnames( select(fmerge, starts_with("M_")) )

for(vars in 1:length(param_name)){
    plot = list() #empty list to store ggplots
    for(iter in 1:length(sv_names)){
        
        plot[[iter]] <-  pval_func(sv_names[iter],sv_moe_names[iter],param_name[vars])
    }
    #arrange the plots
    pl <- grid.arrange(grobs = plot)
    #save the plot
    ggsave(plot = pl, paste0("pvalue_", param_name[vars],".jpg"), height = 8, width = 12, dpi = 300, bg = "white")
}
