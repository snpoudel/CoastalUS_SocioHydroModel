#clean environment
rm(list = ls())

#Load libraries
.libPaths("~/rlibs")
library(evd)
library(tidyverse)
library(ggpubr)
library(rslurm)
     
#write a function to loop through each stations one at a time
#This script is run in HPC
function_HPC <- function(id){ #1 

    #set working directory
    setwd("/gpfs/scratchfs1/jok20001/sap22021/Shmodel_US/Data/projection_data/")
    #Source the sociohydro model for projection
    source("SocioHydroModel_forProjection.R")
    
    #Read parameters for each tract
    param <- read.csv("SH_Parameters_withstate.csv") 
    
    #Read the result for year 2021 whose values will be initialized for future simulations
    sh_result_2021 <- read.csv("SH_Results_2021.csv")
    
    #read the tidal stations
    stations <- read.csv("selected_stations.csv")
    
    #read gumbel extreme value parameters
    gumbel <- read.csv("gumbel_parameters.csv")
    
    
    #Read sea level rise data
    slr <- read.csv(paste0("slr/slr_", id, ".csv"))[17:96,]
    rain <- read.csv(paste0("precip/cnrm/cnrm2_", id, ".csv"))[7:86,] #This reading is for cnrm Global climate model, change accordingly for other models
    #Read precipitation ### ✅CHANGE FOR EACH MODEL### 
    
    #generate storm surge
    #The simulation randomly samples surge values from gumbel distribution
    for (sim in 1:10000) { #number of monte carlo surge simulaiton 
    #empty variables to assign values later
    results_sim = double() #to store results of all simulation for all tracts within a station
        surge = double()
        counter = 1
        
        for (yr in 2021:2100) { #surge generation for one random simulation event
            sample = runif(1, 0, 1) 
            para1 = gumbel$loc[gumbel$ID == id]
            para2 = gumbel$scale[gumbel$ID == id]
            surge[counter] = qgumbel(sample, loc = para1 , scale = para2) #location and scale parameter from processing of gumbel paper data
            counter = counter + 1
        }
        surge.df <- data.frame(year = 2021:2100, surge.ht = surge)
        surge.df$slr <- slr$s2 #This is for SSP245 SLE condition ####### ✅✅✅CHANGE SLR CONDITION HERE ############
        surge.df$total.wl <- surge.df$surge.ht + surge.df$slr
        surge.df <- cbind(simulation = sim, surge.df)
        
        #select param for only one station at a time
        param_s <- param %>% filter(ID == id)
        
        #run the Socio-hydrological model 
        for (i in 1:length(param_s$census_tract)) {
            #result_2021 <- sh_result_2021 %>% filter(census_tract == param_s$census_tract[i])
            results = SocHydModel_SurgePrecipHousing(result_2021 = sh_result_2021 %>% filter(census_tract == param_s$census_tract[i]),
                                                     Year = 2021:2100, W = surge.df$total.wl , Precip = rain$max.precip ,
                                                     param_s$Surge_threshold[i] +100 , param_s$Rain_threshold[i], param_s$alpha_d[i] , #1,2,3 m are respectively added to base surge threshold to see it's impact on future loss
                                                     param_s$alpha_a[i], param_s$alpha_p[i], param_s$alpha_r[i],
                                                     1, param_s$mew_a[i], param_s$mew_p[i],
                                                     1, 1, 1, 1, param_s$U_rate[i] ,param_s$POT_S_max[i], param_s$POT_R_max[i],
                                                     param_s$b1[i],param_s$b2[i],param_s$b3[i],param_s$duration[i],1)
            
            results <- cbind(simulation = sim, census_tract = param_s$census_tract[i], results, state = param_s$state[i] )
            results_sim <- rbind(results_sim,results)
            #cat(paste("🏃.. Station:",id, " sim:" ,sim, i, "\n"))
        } #end of sociohydro loop
        
    #save csv file of result
    write.csv(results_sim,paste0("/gpfs/scratchfs1/jok20001/sap22021/Shmodel_US/Output/projection/cnrm2/result_", id, "_", sim, ".csv"), row.names = F)
    ##✅✅✅Change file location here
        
    } #end of monte carlo surge simulation
    
    
}#1 End of function

#Use rslurm to run in HPC
#read the tidal stations
stations <- read.csv("/gpfs/scratchfs1/jok20001/sap22021/Shmodel_US/Data/projection_data/selected_stations.csv")
pars <- data.frame(id = stations$ID)
sopt <- list(time = '00:60:00', partition = 'hi-core', ntasks = '1')
sjob <- slurm_apply(function_HPC, pars, nodes = 60, cpus_per_node = 1, jobname = 'cnrm2_test', slurm_options = sopt, libPaths = "~/rlibs" )