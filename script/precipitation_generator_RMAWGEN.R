# file precipitation-generator.R
# 
# This file contains a script example with daily temperature  stochastic generations 
#
#
# author: Emanuele Cordano on 12-01-2012
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################




rm(list=ls())
library(RMAWGEN)
library(RMRAINGEN)

set.seed(1222)

data(trentino)
## path where to plot the output figures

wpath <-  "/Users/ecor/Dropbox/iasma/RMAWGENdev/RMAWGEN_article/RMAWGEN_simulations/data" ## wpath <-  "/home/idroclima/github/RMAWGENCodeCorner/output_cassiopea/temperature_generator_multi_station"

## ADJUST DATASET 


year_min <- 1961
year_max <- 1990

period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max
station_prec <- names(PRECIPITATION)[!(names(PRECIPITATION) %in% c("day","month","year"))]
station_temp <- names(TEMPERATURE_MAX)[!(names(TEMPERATURE_MAX) %in% c("day","month","year"))]
prec_mesx <- PRECIPITATION[period,station_prec]
temp_mesx <- TEMPERATURE_MAX[period,station_temp]

names <- intersect(station_prec,station_temp)

## removing nonworking stations (e.g. time series with NA)


accepted <- array(TRUE,length(names))
names(accepted) <- names
for (it in names) {
	cond  <- (length(which(!is.na(prec_mesx[,it])))==length(prec_mesx[,it]))

##	accepted[it]  <- (length(which(!is.na(temp_mesx[,it])))==length(temp_mesx[,it])) & cond 
	accepted[it] <- cond
}


station <- names[accepted]
print(station)



# MONTHLY CLIMATOLOGY

TX_CLIMATE <- NULL #Tx_1961_1990[,station]
TN_CLIMATE <- NULL #Tn_1961_1990[,station]
PREC_CLIMATE <- NULL #prec_1961_1990[,station] # NULL # Adjusts prec_1961_1990 with days!!!! 

# Calibration period 
year_max <- 1990
year_min <- 1961
origin <- paste(year_min,1,1,sep="-")

# Simulation period (Stochastic Generation)
# MONTHLY CLIMATOLOGY


# specific parameter for model calibration 

n_GPCA_iter <- 5
n_GPCA_iteration_residuals <- 5

p_test <- 1
p_prec <- 3
#
exogen <- NULL
exogen_sim <- exogen


## ADDED PRECIPITATION GENERATION with RMRAINGEN 
set.seed(3456)
#### Preparing precipitation data frame
period <- PRECIPITATION$year>=year_min & PRECIPITATION$year<=year_max

prec_mes <- PRECIPITATION[period,station]

## Fitting of Probability Distribution of Precipitation Amount with WEIBULL DISTRIBUTION
fit_monthly   <- fitdistrForPrecipitation(data=prec_mes,dname="weibull",start=NULL,sample="monthly",origin=origin)
fit_monthly_exp   <- fitdistrForPrecipitation(data=prec_mes,dname="exp",start=NULL,sample="monthly",origin=origin)
## Estimate parameters for Precipitation Occurence Modeling (using generate.YuleWalkerCoefficientBlockmatrices)
#coeff_monthly <- CoeffYWeq(data=prec_mes,p=1,tolerance=0.0001,sample="monthly",origin=origin)
coeff_monthly <- NULL
#generation_monthly <- generate(coeff_monthly,year_min=year_min,year_max=year_max,names=names(prec_mes),precipitation.indicator=FALSE) ## fff

##  Estimate parameters for Precipitation Occurence Modeling (using generate.CCGammaObject)
CCGamma_monthly <- CCGamma(data=prec_mes,lag=0,tolerance=0.001,only.matrix=FALSE,sample="monthly",origin=origin)
set.seed(3456) 
generation_monthly <- generate(CCGamma_monthly,year_min=year_min,year_max=year_max,names=names(prec_mes),precipitation.indicator=TRUE)

prec_gen_RMRAINGEN <- generatePrecipitationAmount(x=generation_monthly,origin=origin,par=fit_monthly)
#prec_gen_RMRAINGEN_exp <- generatePrecipitationAmount(x=generation_monthly,origin=origin,par=fit_monthly_exp)
### end  ADDED PRECIPITATION GENERATION with RMRAINGEN 

### RMAWGEN GENERATIONS
set.seed(3456)
generationP03GPCA_prec <- ComprehensivePrecipitationGenerator(station=station,prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,p=p_prec,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_prec=PREC_CLIMATE,no_spline=FALSE)
set.seed(3456)
generationP01GPCA_prec <- ComprehensivePrecipitationGenerator(station=station,prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,p=p_test,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_prec=PREC_CLIMATE,no_spline=FALSE)
#
#
set.seed(3456)
generationP03_prec <- ComprehensivePrecipitationGenerator(station=station,prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,p=p_prec,n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_prec=PREC_CLIMATE,no_spline=FALSE)
set.seed(3456)
generationP01_prec <- ComprehensivePrecipitationGenerator(station=station,prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,p=p_test,n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_prec=PREC_CLIMATE,no_spline=FALSE)

prec_mes <- generationP01GPCA_prec$prec_mes

prec_gen <- list(P03GPCA=generationP03GPCA_prec$prec_gen,
		P01GPCA=generationP01GPCA_prec$prec_gen,
		P03=generationP03_prec$prec_gen,
		P01=generationP01_prec$prec_gen,
		RMRAINGEN=prec_gen_RMRAINGEN)#,RMRAINGENEXP=prec_gen_RMRAINGEN_exp)


## save results
results_prec <- list(prec_mes=prec_mes,
					 prec_gen=prec_gen,
					 year_min=year_min,
					 year_max=year_max,
				     data_GPCA=generationP03GPCA_prec$var@GPCA_data$final_results,
				data_temp=generationP01_prec$data_for_var,
				RMRAINGEN_fit_monthly=fit_monthly,#,RMRAINGEN_fit_monthly_exp=fit_monthly_exp,
				RMRAINGEN_coeff_monthly=coeff_monthly,RMRAINGEN_CCGamma_monthly=CCGamma_monthly)
				
results_prec_var <- list(				
				varP01=generationP01_prec$var,
				varP03=generationP03_prec$var,
				varP01GPCA=generationP01GPCA_prec$var,
				varP03GPCA=generationP03GPCA_prec$var)
filename <- paste(wpath,"results_precipitation_generator_plural_station.rda",sep="/")
filename_var <- paste(wpath,"vars_precipitation_generator_plural_station.rda",sep="/")

save(results_prec,file=filename)

##save(results_prec_var,file=filename_var)











# COMMENTED STUFF
#
#
#> VARselect(generationP03GPCA_prec$var@GPCA_data$final_results)
#$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#1      1      1      1 
#
#$criteria
#1           2           3          4          5          6          7           8           9           10
#AIC(n) -0.28729307 -0.25772251 -0.22594215 -0.1978698 -0.1696424 -0.1365381 -0.1052445 -0.07492719 -0.04038317 -0.004937949
#HQ(n)  -0.20189269 -0.09119176  0.02171897  0.1309217  0.2402794  0.3545141  0.4669381  0.57838575  0.69406014  0.810635725
#SC(n)  -0.03386182  0.23646843  0.50900847  0.7778405  1.0468275  1.3206916  1.5927449  1.86382186  2.13912556  2.415330472
#FPE(n)  0.75029186  0.77281009  0.79776576  0.8204803  0.8439737  0.8723859  0.9001253  0.92784313  0.96046856  0.995141184
#
#> 
#		VARselect(generationP01_prec$data_prec)
#$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#1      1      1      1 
#
#$criteria
#1             2             3             4             5             6             7             8             9            10
#AIC(n) -9.862662e+00 -9.835481e+00 -9.803721e+00 -9.768119e+00 -9.737060e+00 -9.7067454354 -9.672093e+00 -9.639415e+00 -9.609915e+00 -9.582051e+00
#HQ(n)  -9.777261e+00 -9.668950e+00 -9.556060e+00 -9.439328e+00 -9.327138e+00 -9.2156932235 -9.099911e+00 -8.986102e+00 -8.875471e+00 -8.766477e+00
#SC(n)  -9.609230e+00 -9.341290e+00 -9.068771e+00 -8.792409e+00 -8.520590e+00 -8.2495157575 -7.974104e+00 -7.700666e+00 -7.430406e+00 -7.161782e+00
#FPE(n)  5.208355e-05  5.351865e-05  5.524572e-05  5.724817e-05  5.905438e-05  0.0000608724  6.301928e-05  6.511338e-05  6.706383e-05  6.896005e-05
#
#> 
#
#
#
#
#
#DIAGNOSTICS TEST 
#
#
#> 
#		> serial.test(generationP01_prec$var)
#Error in serial.test(generationP01_prec$var) : 
#		Please provide an object of class 'varest', generated by 'var()', or an object of class 'vec2var' generated by 'vec2var()'.
#> serial_test(generationP01_prec$var)
#
#Portmanteau Test (asymptotic)
#
#data:  Residuals of VAR object temp
#Chi-squared = 5651.298, df = 5415, p-value = 0.01239
#
#> serial_test(generationP03_prec$var)
#
#Portmanteau Test (asymptotic)
#
#data:  Residuals of VAR object temp
#Chi-squared = 4723.656, df = 4693, p-value = 0.3735
#
#> serial_test(generationP01GPCA_prec$var)
#
#Portmanteau Test (asymptotic)
#
#data:  Residuals of VAR object temp
#Chi-squared = 5707.74, df = 5776, p-value = 0.7361
#
#> serial_test(generationP03GPCA_prec$var)
#
#Portmanteau Test (asymptotic)
#
#data:  Residuals of VAR object temp
#Chi-squared = 4794.501, df = 5776, p-value = 1
#
#> normality_test(generationP03GPCA_prec$var)
#$JB
#
#JB-Test (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 3.1073, df = 38, p-value = 1
#
#
#$Skewness
#
#Skewness only (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 0.0694, df = 19, p-value = 1
#
#
#$Kurtosis
#
#Kurtosis only (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 3.0379, df = 19, p-value = 1
#
#
#> serial_test(generationP01GPCA_prec$var)
#
#Portmanteau Test (asymptotic)
#
#data:  Residuals of VAR object temp
#Chi-squared = 5707.74, df = 5776, p-value = 0.7361
#
#> serial_test(generationP01_prec$var)
#
#Portmanteau Test (asymptotic)
#
#data:  Residuals of VAR object temp
#Chi-squared = 5651.298, df = 5415, p-value = 0.01239
#
#> serial_test(generationP03_prec$var)
#
#Portmanteau Test (asymptotic)
#
#data:  Residuals of VAR object temp
#Chi-squared = 4723.656, df = 4693, p-value = 0.3735
#
#> normality_test(generationP01GPCA_prec$var)
#$JB
#
#JB-Test (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 3.2877, df = 38, p-value = 1
#
#
#$Skewness
#
#Skewness only (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 0.264, df = 19, p-value = 1
#
#
#$Kurtosis
#
#Kurtosis only (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 3.0237, df = 19, p-value = 1
#
#
#> normality_test(generationP01_prec$var)
#$JB
#
#JB-Test (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 26008.35, df = 38, p-value < 2.2e-16
#
#
#$Skewness
#
#Skewness only (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 18810.11, df = 19, p-value < 2.2e-16
#
#
#$Kurtosis
#
#Kurtosis only (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 7198.238, df = 19, p-value < 2.2e-16
#
#
#> normality_test(generationP03_prec$var)
#$JB
#
#JB-Test (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 26082.45, df = 38, p-value < 2.2e-16
#
#
#$Skewness
#
#Skewness only (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 19116.08, df = 19, p-value < 2.2e-16
#
#
#$Kurtosis
#
#Kurtosis only (multivariate)
#
#data:  Residuals of VAR object temp
#Chi-squared = 6966.368, df = 19, p-value < 2.2e-16
#
#
#> ARCH_test(generationP03_prec$var)
#Error: could not find function "ARCH_test"
#> arch_test(generationP03_prec$var)
#
#ARCH (multivariate)
#
#data:  Residuals of VAR object var_in_arch_test
#Chi-squared = 187458.5, df = 180500, p-value < 2.2e-16
#
#> arch_test(generationP01_prec$var)
#
#ARCH (multivariate)
#
#data:  Residuals of VAR object var_in_arch_test
#Chi-squared = 189195.5, df = 180500, p-value < 2.2e-16
#
#> arch_test(generationP01GPCA_prec$var)
#
#ARCH (multivariate)
#
#data:  Residuals of VAR object var_in_arch_test
#Chi-squared = 187775.2, df = 180500, p-value < 2.2e-16
#
#> arch_test(generationP03GPCA_prec$var)
#
#ARCH (multivariate)
#
#data:  Residuals of VAR object var_in_arch_test
#Chi-squared = 188210.8, df = 180500, p-value < 2.2e-16
#
#> 
