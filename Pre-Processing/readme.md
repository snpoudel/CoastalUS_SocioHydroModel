This folder contains R scripts for pre-processing of historical and projection datasets

* PreProcess_NFIP_Claims&Policies.R  This script reads in National Flood Insurance Program(NFIP) claims and policies database and generates time series of claims made and policy purchases for each census tracts of the study region
* Preprocess_Census_HousingData.py  This script reads in the housing data obtained from US Census and generates timeseries of housind data for each census tracts. This script can be modified to preprocess aggregate housing values, median housing value, and median housing density.
* Get_precipitation.R  This script gets time series of precipitation across the US coast
* Get_SLR_Projections.R  This script gets Sea level rise projection across the US coast from the NOAA tides and currents API
* Get_water_elevation.R  This script gets time series of water surface elevation across the US coastal gauging stations
* Get_Precip_Projections.R  This script gets precipitation projections from the LOCA downcaled CMIP6 GCMs. The script can be modified to obtain precipitation for different GCMs and different climate scenarios.
* Get_GumbelParameters_forSurgeProjection.R  This script gets gumbel extreme value parameters for all the tidal gauging station location across US coast. The parameters are then used in the SH model projection to generate time series of storm surges with monte-carlo simulations.
