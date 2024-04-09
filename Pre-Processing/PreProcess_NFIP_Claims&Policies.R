#clean working environment
rm(list = ls())

#load libraries
library(tidyr)
library(data.table)
library(bit64)

#Read in the FIPS numbers for the census tract
A = 'C:/SANDEEP/Shmodel_US/Data/fips_svi.csv'
FIPS_file <-  read.csv(A)
FIPS <- FIPS_file %>% select(FIPS)
#Read in FEMA Open Claims Data
Claims <-  read.csv("C:/SANDEEP/Shmodel_US/Data/FEMA/FimaNfipClaims.csv")

#Change all the null values in these two columns to 0.00
Claims$amountPaidOnBuildingClaim[is.na(Claims$amountPaidOnBuildingClaim)] <- 0
Claims$amountPaidOnContentsClaim[is.na(Claims$amountPaidOnContentsClaim)] <- 0

for (i in 1:nrow(FIPS)){
    
    ClaimsSub <-  Claims[which(Claims$censusTract == FIPS[i,]),]
    #ClaimsSub is where the entries from the census column of Claims is the same as the i row of FIPS
    Output <-  matrix(nrow=(2022-1979),ncol=3)
    #Output is a matrix with a row for each year and three columns
    counter = 1
    #counter is necessary for the nested for loop
    for (year in 1979:2021) {
        
        ClaimsSubYear <-  ClaimsSub[which(ClaimsSub$yearOfLoss == year),]
        Output[counter,1]  <-   year 
        Output[counter,2] <-  nrow(ClaimsSubYear)
        Output[counter,3] <-  sum(ClaimsSubYear$amountPaidOnBuildingClaim,ClaimsSubYear$amountPaidOnContentsClaim) 
        counter = counter + 1
        
    }#2
    
    #write the output to a .csv file
    write.csv(Output,paste("C:/SANDEEP/Shmodel_US/Data/FEMA/claims/FEMA_claims",FIPS[i,],".csv",sep=""))
    
}#1


#Process policies now
#Read in only these three columns of the massive policy file
Policies <-  fread("C:/SANDEEP/Shmodel_US/Data/FEMA/FimaNfipPolicies.csv",
                   select = c("totalBuildingInsuranceCoverage","censusTract","policyEffectiveDate"))
#Change all the null values in the BuildingInsuranceCoverage column to zero
Policies$totalBuildingInsuranceCoverage[is.na(Policies$totalBuildingInsuranceCoverage)] <- 0

for (i in 1:nrow(FIPS)){
    PolicySub <-  Policies[which(Policies$censusTract == FIPS[i,]),]
    PolicySub$year = as.numeric(format(as.Date(PolicySub$policyEffectiveDate),"%Y"))
    Output <-  matrix(nrow=(13),ncol=3)
    counter = 1
    PolicySubYear=0
    for (yearval in 2009:2021) {
        PolicySubYear <-  PolicySub[which(PolicySub$year == yearval),]
        Output[counter,1]  <-   yearval
        Output[counter,2] <-  nrow(PolicySubYear)
        Output[counter,3] <-  sum(PolicySubYear$totalBuildingInsuranceCoverage)
        counter = counter + 1
    }#2
    write.csv(Output,paste("C:/SANDEEP/Shmodel_US/Data/FEMA/policies/FEMA_policies",FIPS[i,],".csv",sep = ""))
}#1
