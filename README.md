# SocioHydrological(SH) model to study Human-Flood Dynamics across US Coast

This repo contains the following files:

*SocioHydroModel_forCalibration.R   This is script of SH model tailored for model calibration

*SocioHydroModel_forProjection.R    This is script of SH model tailored for model projection

*Calibration_HPC.R                  This script simulates SH variables using SocioHydroModel_forCalibration.R and compares with observed SH datasets to calibrate SH parameters using DDS algorithm

*Projection_HPC.R                   This script reads in climate projection datasets and forecasts SH variables using SocioHydroModel_forProjection.R


References:

*Poudel, S., Caridad, C., Elliott, R., & Knighton, J. (2023). Housing market dynamics of the post-Sandy Hudson estuary, Long Island Sound, and New Jersey coastline are explained by NFIP participation. Environmental Research Letters, 18(9), 094009.

*Knighton, J., Hondula, K., Sharkus, C., Guzman, C., & Elliott, R. (2021). Flood risk behaviors of United States riverine metropolitan areas are driven by local hydrology and shaped by race. Proceedings of the National Academy of Sciences, 118(13), e2016839118.

*Barendrecht, M. H., Viglione, A., Kreibich, H., Merz, B., Vorogushyn, S., & Blöschl, G. (2019). The value of empirical data for estimating the parameters of a sociohydrological flood risk model. Water resources research, 55(2), 1312-1336.

*Tolson, B. A., & Shoemaker, C. A. (2007). Dynamically dimensioned search algorithm for computationally efficient watershed model calibration. Water Resources Research, 43(1).
