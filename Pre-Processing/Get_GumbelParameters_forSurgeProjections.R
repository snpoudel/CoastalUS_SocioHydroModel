#clean working environment
rm(list = ls())
#load libraries
library(ncdf4)
library(tidyverse)

#set working directory
setwd("C:/SANDEEP/Shmodel_US/Data/Climprojection/wl")

#read tidal stations
stations <- read.csv("C:/SANDEEP/Shmodel_US/Data/selected_stations.csv")
#read nc file
nc_data <- nc_open("CODEC_amax_ERA5_1979_2017_coor_mask_GUM_RPS.nc")
#read all the variables of the nc file
lon <- ncvar_get(nc_data, "station_y_coordinate")
lat <- ncvar_get(nc_data, "station_x_coordinate")

return_period <- ncvar_get(nc_data, "RPS")
gumbel_para <- ncvar_get(nc_data, "GUM")

#initialize a empty dataframe
df <- data.frame("id" = double(), "loc" = double(), "scale" = double())

#Loop through each stations and find gumbel parameters
for (i in 1:length(stations$ID)) {
    
    lat_lis = stations$Longitude[i] #lat lon are reversed
    lon_lis = stations$Latitude[i]
    
    dist = sqrt((lat_lis - lat)^2 + (lon_lis - lon)^2) #Find relevant lat and lon of data based on minimum distance
    index = which.min(dist)
    value = gumbel_para[,index] #these are the gumbel parameters
    temp_df = data.frame("id" = stations$ID[i], "loc" = value[1], "scale" = value[2] )
    df <- rbind(df, temp_df)
}

#save the result
write.csv(df, "gumbel_parameters.csv", row.names = F)


