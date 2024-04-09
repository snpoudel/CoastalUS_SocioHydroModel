#load libraries
library(tidyverse)
library(xml2)
#read in data
stn = read.csv("C:/SANDEEP/Shmodel_US/Data/WL&Precip/selected_stations.csv")

#function that takes station ID as input and gives sea level projection up to 2100
for (id in stn$ID) {
    
    #read url
    url = paste0("https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/slr_projections.xml?units=metric&station=",id,"&report_year=2022&affil=US" )
    #extract year and slr data from xml file
    file = read_xml(url)
    year = xml_text(xml_find_all(file, "//projectionYear"))
    slr = xml_text(xml_find_all(file, "//projectionRsl"))
    scenario = xml_text(xml_find_all(file, "//scenario"))
    df = data.frame(year, slr, scenario)
    
    #only extract for s2 and s5
    df <- df %>% filter(scenario %in% c("Intermediate-Low", "High"))
    df <- pivot_wider(df, names_from = scenario, values_from = slr)
    colnames(df) <- c("year", "s2", "s5")
    
    #interpolate values to have annual timeseries
    idf <- data.frame( year = 2005:2100, s2 = approx(df$year, df$s2, xout = 2005:2100)$y,
                       s5 = approx(df$year, df$s5, xout = 2005:2100)$y)
    idf$s2 <- idf$s2/100 #convert slr from cm to m
    idf$s5 <- idf$s5/100 
    
    #write the final idf dataframe as a csv file
    write.csv(idf, paste0("C:/SANDEEP/Shmodel_US/Data/Climprojection/slr/slr_",id,".csv"))
}

