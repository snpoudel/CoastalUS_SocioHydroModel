#Load libraries
library(RCurl)
library(tidyverse)

#get the file containing all tidal stations
stn = read.csv("C:/SANDEEP/Shmodel_US/Data/WL&Precip/selected_stations.csv")
sid = stn %>% select(ID) #station ID

#Get Water Level
for (id in 25:length(sid$ID)){ #The simulation may break sometimes, just run again from when it breaks!
    
    #The code below get hourly tide, the limit to download hourly tide is 1 year
    for (year in 1970:2022)
    {
        url1 = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date="
        begindate = paste(year,"01","01",sep = "") 
        url2 = "&end_date="
        enddate = paste(year,"12","31",sep = "")
        url3 = "&station="
        stationid = sid$ID[id]
        url4 = "&product=hourly_height&datum=msl&units=metric&time_zone=gmt&application=web_services&format=csv"
        
        #Assemble the URL
        urltotal = paste(url1,begindate,url2,enddate,url3,stationid,url4,sep ="")
        
        #Download the data
        dat = getURL(urltotal) #use RCurl to retrieve text into a vector 'dat'

        #create a folder to save results for each station
        path = paste0("C:/SANDEEP/Shmodel_US/Data/WL&Precip/WL_data/",sid$ID[id])
        dir.create(path, recursive = T)
        #save the timeseries of one station inside this folder
        filename = paste(path,"/wl_",year,".csv",sep="")
        write.table(dat,filename)
    }
    #Process Water level to get yearly maximum
    #initialize a empty dataframe
    df = double()
    setwd(path)
    #get max of each year
    for (year in 1970:2022) {
        filename = paste("wl_",year,".csv",sep = "")
        fn = read.csv(filename, skip = 2, header = F) 
        max_wl = max(fn$V2, na.rm = T)
        temp_df = data.frame(yr = year, wl = max_wl)
        df = rbind(df, temp_df)
    }
    filename = paste("C:/SANDEEP/Shmodel_US/Data/WL&Precip/WL_data/wl_",sid$ID[id],".csv",sep="")
    write.csv(df, filename, row.names = F)
    
}
