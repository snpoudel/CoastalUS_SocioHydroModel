#clear working environment
rm(list = ls())

#load libraries
.libPaths("~/rlibs")
library(tidyverse)
library(rslurm)

#This script is run using HPC
slurm_function <- function(ct){ #start of HPC function
    
    #set working directory
    setwd("/gpfs/scratchfs1/jok20001/sap22021/Shmodel_US/Data/")
    source("SocioHydroModel_forCalibration.R") #sociohydro model
    source("rmse.R") #find rmse score, (you can use the built in R package for this as well, At the moment the Package was not available in CRAN to load in HPC)

    #Function that Returns numIter length list of entries to be peturbed for DDS 
    probPeturb<-function(x, numIter)
    {
        # Input is xBounds & numIter.  
        # Returns numIter entry list with the indices which will be peturbed
        xDims<-nrow(x)
        probabilityVector<-1-log(1:numIter)/log(numIter)
        peturbIdx<-apply(matrix(unlist(lapply(probabilityVector, function(x) as.logical(rbinom(xDims, 1, x)))), byrow=TRUE, ncol=xDims), 1, which)
        return(peturbIdx)
    }
    
    #DDS parameters
    r= 0.2
    numIter =200000
    #running code for all census tracts of interest
    Coeff_SH_mat <- data.frame(Doubles=double())
    Results_SH_mat <- data.frame(Doubles=double())
    KGE_best_mat <- data.frame(Doubles=double())
    
    
    #Read in index
    #Read all the hydrological stations from which data will be retreived
    stn <- read.csv("/gpfs/scratchfs1/jok20001/sap22021/Shmodel_US/Data/nonmissing_tract_tidalstationsRMSE.csv")

    #extract only census tracts column
    fips <- data.frame(FIPS = stn$FIPS)
    
    #Find station ID corresponding to this tract
    st <- stn$ID[fips$FIPS[ct] == stn$FIPS]
    
    #read storm surge and precipitation corresponding to this station
    Battery = read.csv(paste0("WL&Precip/WL_data/wl_", st, ".csv") ) #Battery is wl reading, it reads wl belonging to this station
    precip = read.csv(paste0("WL&Precip/Precip_data/precip", st, ".csv")) #read precip belonging to this station
    
    fema_claims = read.csv(paste0("FEMA/claims/FEMA_claims",fips$FIPS[ct],".csv")) #NFIP claims
    fema_policies = read.csv(paste0("FEMA/policies/FEMA_policies",fips$FIPS[ct],".csv")) #NFIP policy purchases
    nhouses <- read.csv(paste0("Housing/number_with.hindcasting/nhousing_",fips$FIPS[ct],".csv")) #aggregate housing density
    vhouses <- read.csv(paste0("Housing/aggregate_housing_value/vhousing_",fips$FIPS[ct],".csv")) #aggregate housing value
    norm_vhouses <- read.csv(paste0("Housing/value_median_normalized/vhousing_",fips$FIPS[ct],".csv")) # median housing value
    
    #missing values of nhouses and vhouses are assigned NA
    nhouses$norm.hnum[nhouses$norm.hnum==0 | nhouses$norm.hnum=="null" | nhouses$norm.hnum=="-"] <- NA
    vhouses$value[vhouses$value==0 | vhouses$value=="null" | vhouses$value=="-"] <- NA
    norm_vhouses$norm_vhouses.hp[norm_vhouses$norm_vhouses==0 | norm_vhouses$norm_vhouses=="null" | norm_vhouses$norm_vhouses=="-"] <- NA
    
    #Rescaling values for the sociohydro model input
    scaled_claims =  fema_claims$V3/mean(as.numeric(vhouses$value), na.rm = T)
    scaled_policy =  fema_policies$V2/mean(nhouses$value, na.rm = T)
    scaled_nhouses = nhouses$norm.hnum / max(nhouses$norm.hnum, na.rm = T)
    scaled_vhouses = norm_vhouses$norm_vhouses / max( norm_vhouses$norm_vhouses, na.rm = T)
    
    #store the scaled values in dataframe
    df_dummy = data.frame(year = 1970:2021) #a dummy dataframe to set years for all calib period
    df_claims = data.frame(year = fema_claims$V1, claims = scaled_claims)
    df_nhouses = data.frame(year = nhouses$year, nhouses = scaled_nhouses)
    df_vhouses = data.frame(year = norm_vhouses$year, vhouses = scaled_vhouses)
    df_policy = data.frame(year = fema_policies$V1, policy = scaled_policy)
    
    #merge every columns into one dataframe, this dataframe would then have complete list of observed data used for calibration
    list_df = list(df_dummy, df_claims, df_nhouses, df_vhouses, df_policy)
    dff = Reduce( function(x, y) merge(x, y, by = "year", all = T), list_df )
    
    #Set calibration parameter boundaries for DDS
    xBounds.df = data.frame(matrix(ncol=2,nrow=19))
    colnames(xBounds.df)<-c("min", "max")
    
    #Surge threshold
    xBounds.df$min[1] = 0.001 
    xBounds.df$max[1] = 6 
    
    #Rain threshold
    xBounds.df$min[2] = 0.001 
    xBounds.df$max[2] = 0.5 
    
    #alphad
    xBounds.df$min[3] = 0.001 
    xBounds.df$max[3] = 15 
    
    #alphaa
    xBounds.df$min[4] = 0.001
    xBounds.df$max[4] = 150 
    
    #alphap
    xBounds.df$min[5] = 0.001
    xBounds.df$max[5] = 100 
    
    #alphar
    xBounds.df$min[6] = 0.001
    xBounds.df$max[6] = 1 
    
    #mewa
    xBounds.df$min[7] = 0.001
    xBounds.df$max[7] = 0.25 
    
    #mewp
    xBounds.df$min[8] = 0.001
    xBounds.df$max[8] = 1 
    
    #U_rate
    xBounds.df$min[9] = 0.001
    xBounds.df$max[9] = 3 
    
    #POT_S_max
    xBounds.df$min[10] = 0.001 
    xBounds.df$max[10] = 6 
    
    #POT_R_max
    xBounds.df$min[11] = 0.001
    xBounds.df$max[11] = 0.5
    
    #housing
    #b1
    xBounds.df$min[12] = 0.001
    xBounds.df$max[12] = 0.3  
    
    #b2
    xBounds.df$min[13] = 0.001
    xBounds.df$max[13] = 300  
    
    #b3
    xBounds.df$min[14] = 0.001
    xBounds.df$max[14] = 5   
    
    #duration
    xBounds.df$min[15] = 0.001
    xBounds.df$max[15] = 15
    
    #Initial HP
    xBounds.df$min[16] = 0.1
    xBounds.df$max[16] = 0.5
    
    #Initial D
    xBounds.df$min[17] = 0.5
    xBounds.df$max[17] = 1
    
    #Initial A
    xBounds.df$min[18] = 0.01
    xBounds.df$max[18] = 0.2
    
    #Initial P
    xBounds.df$min[19] = 0.01
    xBounds.df$max[19] = 0.2
    
    # Generate initial first guess
    x_init<-c(2, 0.125, 20, 50, 50, 0.5, 0.125, 0.05, 1, 3, 0.125, 0.05, 50, 0.5, 5, 0.5, 0.5, 0.2, 0.2)
    x_best = data.frame(x_init)
    KGE_best <- 100000000
    
    peturbIdx<-probPeturb(xBounds.df, numIter)
    # Peturb each entry by N(0,1)*r(x_max - x_min) reflecting if beyond boundaries
    sigma<-xBounds.df$max - xBounds.df$min
    
    for (i in 1:numIter) #start of dds iteration
    {
        # Set up test parameter values as x_test
        x_test<-as.matrix(x_best)
        
        # Get entries we will peturb
        idx<-peturbIdx[[i]]
        if (sum(idx) == 0) {idx = round(runif(1,1,19))}
        
        # Initialize vector of peturbations initially zeros with same length of x so we will add this vector to peturb x
        peturbVec<-rep(0, length(x_test))
        # Generate the required number of random normal variables
        N<-rnorm(length(x_test), mean=0, sd=1)
        
        # Set up vector of peturbations
        peturbVec[idx]<-r*N[idx]*sigma[idx]
        
        # Temporary resulting x value if we peturbed it
        testPeturb<-x_test + peturbVec  
        # Find the values in testPeturb that have boundary violations.  Store the indices in boundaryViolationsIdx
        boundaryViolationIdx<-which(testPeturb<xBounds.df$min | testPeturb > xBounds.df$max)
        
        # Reset those violated indices to the opposite peturbation direction
        peturbVec[boundaryViolationIdx]<-(-1*r*N[boundaryViolationIdx]*sigma[boundaryViolationIdx])
        
        # Find values still at violations of min or max and set them to the minimum or maximum values
        x_test<-x_test + peturbVec
        minViolationIdx<-which(x_test<xBounds.df$min)
        maxViolationIdx<-which(x_test>xBounds.df$max)
        x_test[minViolationIdx]<-xBounds.df$min[minViolationIdx]
        x_test[maxViolationIdx]<-xBounds.df$max[maxViolationIdx]
        
        #Socio-hydrological model    
        Results = SocHydModel_SurgePrecipHousing(Year = 1970:2021 , W = Battery$wl[1:52], Precip = precip$precip_max[1:52],
                                                 x_test[1], x_test[2], x_test[3], x_test[4], x_test[5], x_test[6],
                                                 1, x_test[7], x_test[8], 
                                                 1, 1, 1, 1, x_test[9], x_test[10], x_test[11], x_test[12],
                                                 x_test[13], x_test[14], x_test[15], 1, x_test[16], x_test[17], x_test[18], x_test[19])
        
        #Objective Function calculation
        
        #make a new dataframe that only has years where all the calibration data exists
        #extract sh modeling results values for cells/position where calibration data exists otherwise assign NA
        Results_sub <- Results %>% select(c(Year, L, D, HP, A))
        Results_sub$L <- ifelse(is.na(dff$claims), NA, Results_sub$L)
        Results_sub$D <- ifelse(is.na(dff$nhouses), NA, Results_sub$D)
        Results_sub$HP <- ifelse(is.na(dff$vhouses), NA, Results_sub$HP)
        Results_sub$A <- ifelse(is.na(dff$policy), NA, Results_sub$A)
        
        #multi objective optimization
        #calculate mean normalized root mean square efficiency for each set of calibration dataset
        #We tested for KGE, NSE, RMSE, and NRMSE
        #Even though following indication says KGE but they are calculating NRMSE 
        
        #Loss with nfip claims
        if(mean(dff$claims, na.rm = TRUE) == 0){
            KGE_Test_V = rmse(Results_sub$L, dff$claims)
        }else{KGE_Test_V = rmse(Results_sub$L, dff$claims) / mean(dff$claims, na.rm = TRUE) }
        #awareness with nfip policy
        if(mean(dff$policy, na.rm = TRUE) == 0){
            KGE_Test_A = rmse(Results_sub$A, dff$policy)
        }else{KGE_Test_A = rmse(Results_sub$A, dff$policy) / mean(dff$policy, na.rm = TRUE) }
        #density with observed housing density
        if(mean(dff$nhouses, na.rm = TRUE) == 0){
            KGE_Test_D = rmse(Results_sub$D, dff$nhouses)
        }else{KGE_Test_D = rmse(Results_sub$D, dff$nhouses) / mean(dff$nhouses, na.rm = TRUE) }
        #housing price with observed housing price
        if(mean(dff$vhouses, na.rm = TRUE) == 0){
            KGE_Test_HP = rmse(Results_sub$HP, dff$vhouses)
        }else{KGE_Test_HP = rmse(Results_sub$HP, dff$vhouses) / mean(dff$vhouses, na.rm = TRUE)}
        
        #Calculate global calibration score
        KGE_Test <- sqrt(KGE_Test_V^2 + KGE_Test_A^2 + KGE_Test_D^2 + KGE_Test_HP^2)
        #Check if this simulation is better  
        if (KGE_Test < KGE_best) 
        {
            x_best = x_test
            KGE_best = KGE_Test
            KGE_best_V = KGE_Test_V
            KGE_best_A = KGE_Test_A
            KGE_best_D = KGE_Test_D
            KGE_best_HP = KGE_Test_HP
        }
        
        #Print to console
        #print_str = paste(ct, "Tract:", fips$FIPS[ct],"KGE BEST",i,":",KGE_best)
        #print(print_str)
        
    } #end of dds iteration
    
    ###write results
    ##Result of SH coefficients for all census tracts
    x_bestt=t(x_best)
    colnames(x_bestt) <- c("Surge_threshold","Rain_threshold","alpha_d","alpha_a","alpha_p","alpha_r","mew_a","mew_p","U_rate","POT_S_max","POT_R_max", "b1", "b2", "b3", "duration", "HP1", "D1", "A1", "P1")
    x_bestt <- cbind(census_tract = fips$FIPS[ct] , x_bestt)
    Coeff_SH_mat <- rbind(Coeff_SH_mat,x_bestt)
    
    ##Result of SH variables for all census tracts
    Results_SH <- cbind(census_tract = fips$FIPS[ct], Results)
    Results_SH_mat <-  rbind(Results_SH_mat,Results_SH)
    
    ##Results of KGE for all census tracts
    KGE_best <- cbind( census_tract = fips$FIPS[ct], KGE_best, KGE_best_V, KGE_best_A, KGE_best_D, KGE_best_HP)
    KGE_best_mat <- rbind(KGE_best_mat,KGE_best)
    
    #write the outputs
    write.csv(Coeff_SH_mat,paste0("/gpfs/scratchfs1/jok20001/sap22021/Shmodel_US/Output/parameter/SH_Parameters",fips$FIPS[ct],".csv"),row.names = F)
    write.csv(Results_SH_mat,paste0("/gpfs/scratchfs1/jok20001/sap22021/Shmodel_US/Output/results/SH_Results",fips$FIPS[ct],".csv"),row.names = F)
    write.csv(KGE_best_mat,paste0("/gpfs/scratchfs1/jok20001/sap22021/Shmodel_US/Output/kge/SH_KGE",fips$FIPS[ct],".csv"),row.names = F)
    
} #end of HPC function


#The following code is for HPC to decide no of parallel jobs to run
stn <- read.csv("/gpfs/scratchfs1/jok20001/sap22021/Shmodel_US/Data/nonmissing_tract_tidalstationsRMSE.csv")
pars <- data.frame(ct = 1:length(stn$FIPS)) #number of jobs you want to run, decided by a parameter in the HPC function
sopt <- list(time = '00:40:00', partition = 'hi-core', ntasks = '1')
sjob <- slurm_apply(slurm_function, pars, nodes = 1000, cpus_per_node = 1, jobname = 'DDStest', slurm_options = sopt, libPaths = "~/rlibs" )