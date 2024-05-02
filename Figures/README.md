# This folder contains Rscript that generates the following different figures.
## 1. Grid Plot of Model Parameters across US Coasts
GridPlot can be used as an alternative of parallel coordinate plot for a neat and descriptive plot. Grid plot here shows the distribution of model parameters values across the US Coasts (Pacific, Gulf, Atlantic). Lighter color means lower parameter value and darker color means higher parameter value.
![parameters_gridplot6 bycoasts 5x5](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/55970f79-19ff-4f43-9678-f7de23b5ea41)

## 2. Correlation Plot between Model Parameters and Social Vulnerability Measures
This is a Spearman's Correlation plot between model parameters and measures of social vulnerability, with stars indicating the p-value significance level. This correlation plot is designed to not show redundant variables on either x or y axis.
![corplot](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/c4c6531b-1e77-4547-af3d-c355d8214e62)

## 3. BarPlot of NFIP Claims, Policy, Claims/Policy across US Coastal States
This is a barplot with three dodged position for each x axis variable along with error bar showing the standard error. The plots here are for NFIP Claims, NFIP policy, and NFIP claims/policy respectively for climate change emission scenario SSP585.
![loss policySSP5 8x8](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/cc6af17a-31fc-4bbb-b94f-703da64ac6a6)

## 4. Line Plot of NFIP Claims, Policy, Population density, and housing values for three different social vulnerability groups  
This is a lineplot with different linetype indicating different social vulnerability groups and different color representing different climate change emission scenarios. The plots are for Cumulative NFIP loss/claims, NFIP policy, Population density, and housing values respectively.
![LADHP for 3svis 10x6 SSP585](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/3829ec98-053f-4184-8f0d-9384ff1e6b91)

## 5. Line Plot of NFIP Claims, Policy, Population density, and housing values for existing/modeled and increasing threshold level by +1, +2, +100m  
This is a lineplot showing the forecasted NFIP claims, policy, density, and housing values for low and high emission climate change scenarios for modeled surge threshold level and threshold level increased by +1, +2, and +100m respectively. The first row is for modeled surge threshold level, and the consecutive rows are for +1, +2 and +100m incrased surge threshold levels respectively.
![HP_A_L for different surge](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/6c2f6cc5-e6e7-4d11-80b5-986a5307f926)

## 6. Line Plot of historical and forecasted Population Density across US coastal states
This is a lineplot showing the historical and forecasted population density across US coastal states. Different colors are used to represent historical and forecasted results and the black dots represent the observed values during the historical period. Similarly, lineplot for NFIP claims/loss, policy, and housing values can be produced.
![Density10x6](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/31ef8825-6bf7-4128-ab74-ecbee38d16ca)

## 7. Line Plot of forecasted Population Density across US coastal states for modeled and increased surge threshold levels by +1, +2, +100m
This is a lineplot showing forecasted population density across US coastal states for modeled/existing surge threhosld level and threshold level increased by +1, +2, and +100m respectively. Similarly, plot for NFIP claims/loss, policy, and housing values can be produced.
![DensityByStates_MultiThreshold](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/dbc99103-8236-4fb7-85b1-9bbb0978e531)

## 8. Density plot of MonteCarlo sampled P-values
This is the density plot showing the distribution of p-values calculated between model parameter vs 10,000 monte carlo sampled social vulnerability measures. This figure is for one model parameters (alpha_a), the script can generate similar plots for all model parameters.
![pvalue_alpha_a](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/761a9b6b-5a78-46f1-b58b-ee1d87484a34)
