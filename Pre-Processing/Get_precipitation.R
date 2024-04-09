#load libraries
library(ncdf4)

#read file containing all stations where we want to retreive data
stn = read.csv("C:/SANDEEP/Shmodel_US/Data/WL&Precip/selected_stations.csv")


for (i in 1:length(stn$ID)) {
    
    #Find time series of precipitation for each stations 
    Output=data.frame(yr=numeric(0),precip_max=numeric(0))
    
    watershed_lon = (360 + stn$Longitude[i] )
    watershed_lat = stn$Latitude[i]
    
    # precip data 
    for (year in 1970:2022)
    {
        nc = nc_open(paste("C:/SANDEEP/shmodel_US/Data/WL&Precip/Precip_data/ncfile/precip.V1.0.",year,".nc",sep=""))
        
        precip = ncvar_get(nc)
        
        lon = ncvar_get(nc, 'lon' )
        lat = ncvar_get(nc, 'lat' )
        time = ncvar_get(nc, 'time' )
        
        lat_index = which.min(abs(lat - watershed_lat))
        lon_index = which.min(abs(lon - watershed_lon))
        
        watershed_precip = precip[lon_index,lat_index,]
        watershed_precip_max = (max(watershed_precip,na.rm = T))/1000 #to convert precipitation in mm to m
        max_prep = data.frame(yr=year,precip_max=watershed_precip_max)
        Output = rbind(Output, max_prep)
        
    }
    
    #create a folder to save results for each station
    path = paste0("C:/SANDEEP/Shmodel_US/Data/WL&Precip/Precip_data/precip",stn$ID[i],".csv")
    #save the timeseries of one station inside this folder
    write.csv(Output,path, row.names = F)
}
