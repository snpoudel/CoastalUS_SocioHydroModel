#clear working environment
rm(list = ls())
#load libraries
library(ncdf4)
library(tidyverse)
#set working directory
setwd("C:/SANDEEP/Clim_projection/Globus Loca_down CMIP6")

#read stations file
stn = read.csv("C:/SANDEEP/Shmodel_US/Data/WL&Precip/selected_stations.csv")
#read nc file
#These file are the loca downcaled CMIP6 precipitation datasets
nc_44 <- nc_open("CNRM-CM6-1/ssp245/pr.CNRM-CM6-1.ssp245.r1i1p1f2.2015-2044.LOCA_16thdeg_v20220519.nc")
nc_74 <- nc_open("CNRM-CM6-1/ssp245/pr.CNRM-CM6-1.ssp245.r1i1p1f2.2045-2074.LOCA_16thdeg_v20220519.nc")
nc_100 <- nc_open("CNRM-CM6-1/ssp245/pr.CNRM-CM6-1.ssp245.r1i1p1f2.2075-2100.LOCA_16thdeg_v20220519.nc")

#The nc file has four dimensions(lon,lat,time, and pr). The first three dimension are used to organize pr.
lon_44 <- ncvar_get(nc_44, "lon")
lat_44 <- ncvar_get(nc_44, "lat")
time_44 = ncvar_get(nc_44, 'time')

lon_74 <- ncvar_get(nc_74, "lon")
lat_74 <- ncvar_get(nc_74, "lat")
time_74 = ncvar_get(nc_74, 'time')

lon_100 <- ncvar_get(nc_100, "lon")
lat_100 <- ncvar_get(nc_100, "lat")
time_100 = ncvar_get(nc_100, 'time')
#convert time to readable format
time_44 = as.POSIXct(floor(unclass(time_44)*86400), origin = "1900-01-01", tz = "GMT") #see the help section of posixct for detail info, multiplied by 86400 cause the data are in days
time_74 = as.POSIXct(floor(unclass(time_74)*86400), origin = "1900-01-01", tz = "GMT")
time_100 = as.POSIXct(floor(unclass(time_100)*86400), origin = "1900-01-01", tz = "GMT")

#head(time_44) #look at the first few entries
#dim(lon_44) #look at the dimension


for (i in 1:length(stn$ID)) { #loop through each stations
    
    #watershed_lon and lat
    watershed_lon = (360 + stn$Longitude[i] )  ##Wlat is measured in E so had to subtract from 360
    watershed_lat = stn$Latitude[i]
    #lon and lat for watershed
    lon_index_44 = which.min(abs(lon_44 - watershed_lon))
    lat_index_44 = which.min(abs(lat_44 - watershed_lat))
    
    lon_index_74 = which.min(abs(lon_74 - watershed_lon))
    lat_index_74 = which.min(abs(lat_74 - watershed_lat))
    
    lon_index_100 = which.min(abs(lon_100 - watershed_lon))
    lat_index_100 = which.min(abs(lat_100 - watershed_lat))
    
    #Extract values for your watershed
    watershed_precip_44 = ncvar_get(nc_44, "pr", start = c(lon_index_44, lat_index_44, 1),
                                    count = c(1, 1, length(time_44))) * (86400/1000) #Convert precip into m/day
    
    watershed_precip_74 = ncvar_get(nc_74, "pr", start = c(lon_index_74, lat_index_74, 1),
                                    count = c(1, 1, length(time_74))) * (86400/1000) #Convert precip into m/day
    watershed_precip_100 = ncvar_get(nc_100, "pr", start = c(lon_index_100, lat_index_100, 1),
                                     count = c(1, 1, length(time_100))) * (86400/1000) #Convert precip into m/day
    
    #make dataframe with date and precipitation
    df_44 <- data.frame(date = time_44, precip = watershed_precip_44)
    df_74 <- data.frame(date = time_74, precip = watershed_precip_74)
    df_100 <- data.frame(date = time_100, precip = watershed_precip_100)
    
    #combine dataframes
    df <- rbind(df_44, df_74, df_100)
    
    #convert daily precip to annual maximum precip time series
    #First, disect date into years
    df$date <- as.numeric(format(as.Date(df$date), format = "%Y"))
    #group by year and calculate max of each year
    df_final = df %>% group_by(date) %>% 
        summarise(max.precip = max(precip))
    #write the final annual precip time series file
    write.csv(df_final, paste0("C:/SANDEEP/Shmodel_US/Data/Climprojection/precip/cnrm/cnrm2_",stn$ID[i], ".csv"))
}



