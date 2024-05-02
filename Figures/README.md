# This folder contains R scripts that generates following different figures.
## 1. Grid Plot of Model Parameters across US Coasts
GridPlot can be used as an alternative of parallel coordinate plot for a neat and descriptive visulaization. Grid plot here shows the distribution of model parameters values across the US Coasts (Pacific, Gulf, Atlantic). Lighter color means lower parameter value and darker color means higher parameter value.
![parameters_gridplot6 bycoasts 5x5](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/55970f79-19ff-4f43-9678-f7de23b5ea41)

## 2. Correlation Plot between Model Parameters and Social Vulnerability Measures
This is a Spearman's Correlation plot between model parameters and measures of social vulnerability. Stars indicates the p-values at different significance level. The plot is intentionally designed to avoid redundant variables on both the x and y axes.
![corplot](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/c4c6531b-1e77-4547-af3d-c355d8214e62)

## 3. BarPlot of NFIP Claims, Policy, Claims/Policy across US Coastal States
This barplot illustrates NFIP (National Flood Insurance Program) claims, policies, and the ratio of claims to policies across US coastal states respectively for high climate change emission scenario (SSP585). Each x-axis variable is presented with three dodged positions, accompanied by error bars indicating the standard error. Plot for lower emission scenario (SSP245) can be produced similarly.
![loss policySSP5 8x8](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/cc6af17a-31fc-4bbb-b94f-703da64ac6a6)

## 4. Line Plot of NFIP Claims, Policy, Population density, and housing values for three different social vulnerability groups  
This line plot depicts cumulative NFIP (National Flood Insurance Program) loss/claims, NFIP policies, population density, and housing values for three distinct social vulnerability groups across US coasts respectively. Different linetypes distinguish between the social vulnerability groups, while two different colors represent historical and higher climate change emission scenario (SSP585). Plot for lower emission scenario (SSP245) can be produced similarly.
![LADHP for 3svis 10x6 SSP585](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/3829ec98-053f-4184-8f0d-9384ff1e6b91)

## 5. Line Plot of NFIP Claims, Policy, Population density, and housing values for existing/modeled and increased threshold level by +1, +2, +100m  
This lineplot shows the forecasted NFIP claims, policy, density, and housing values for low and high climate change emission scenarios for modeled surge threshold level and threshold level increased by +1, +2, and +100m respectively. The first row presents data for the modeled surge threshold level, while subsequent rows correspond to increased surge threshold levels of +1m, +2m, and +100m, respectively. Different color indicates different climate change emission scenarios (SSP245 & SSP585).
![HP_A_L for different surge](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/6c2f6cc5-e6e7-4d11-80b5-986a5307f926)

## 6. Line Plot of historical and forecasted Population Density across US coastal states
This  lineplot shows the historical and forecasted population density across US coastal states. Different colors are used to represent historical and forecasted results and the black dots represents the observed values during the historical period. Similarly, lineplot for NFIP claims/loss, policy, and housing values can be produced.
![Density10x6](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/31ef8825-6bf7-4128-ab74-ecbee38d16ca)

## 7. Line Plot of forecasted Population Density across US coastal states for modeled and increased surge threshold levels by +1, +2, +100m
This lineplot shows forecasted population density across US coastal states for modeled/existing surge threhosld level and threshold level increased by +1, +2, and +100m respectively. Similarly, plot for NFIP claims/loss, policy, and housing values can be produced.
![DensityByStates_MultiThreshold](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/dbc99103-8236-4fb7-85b1-9bbb0978e531)

## 8. Density plot of MonteCarlo sampled P-values
This is a density plot showing the distribution of p-values calculated for Spearman's ranked correlation between a model parameter (alpha_a) and 10,000 Monte Carlo sampled social vulnerability measures. This figure represents one model parameter, but the script is capable of generating similar plots for all model parameters.
![pvalue_alpha_a](https://github.com/snpoudel/CoastalUS_SocioHydroModel/assets/71980576/761a9b6b-5a78-46f1-b58b-ee1d87484a34)
