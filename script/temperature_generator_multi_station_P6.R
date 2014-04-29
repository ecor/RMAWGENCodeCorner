# file precipitation-generator.R
# 
# This file contains a script example with daily temperature  stochastic generations 
#
#
# author: Emanuele Cordano on 2014-04-01
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
set.seed(1222)

data(trentino)
## path where to plot the output figures

## wpath <-  "/Users/ecor/dev/github/RMAWGENCodeCorner/data"
wpath <- paste(getwd(),"data",sep="/") ##  set setwd() function to RMAWGENCodeCorner directory
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

	accepted[it]  <- (length(which(!is.na(temp_mesx[,it])))==length(temp_mesx[,it])) 
}


station <- names[accepted]
print(station)
###
#station
#[1] "T0001" "T0014" "T0064" "T0083" "T0090" "T0129" "T0139" "T0211" "T0367"
##
#
#
####

## END ADJUST DATASET

##station <- c("T0090","T0083") #,"T0099","T0001")
# Calibration period 
# MONTHLY CLIMATOLOGY

TX_CLIMATE <- NULL #Tx_1961_1990[,station]
TN_CLIMATE <- NULL #Tn_1961_1990[,station]
PREC_CLIMATE <- NULL #prec_1961_1990[,station] # NULL # Adjusts prec_1961_1990 with days!!!! 

# Calibration period 
year_max <- 1990
year_min <- 1961
origin <- "1961-1-1"

# Simulation period (Stochastic Generation)
# MONTHLY CLIMATOLOGY

# specific parameter for model calibration 

n_GPCA_iter <- 5
n_GPCA_iteration_residuals <- 5


# GENERATION OF DAILY TEMPERATURE COUPLED WITH PRECIPITATION WITH PREVIOUSLY GENERATED


n_GPCA_iter <- 5
n_GPCA_iteration_residuals <- 5

p_test <- 1
p_temp <- 6

# Exogenous variables : Daily Precipitation (Observed and P03GPCA)  

##exogen <- normalizeGaussian_severalstations(x=prec_mes,data=prec_mes,sample="monthly",origin_x=origin,origin_data=origin,step=0)
##exogen_sim <- normalizeGaussian_severalstations(x=prec_gen$P03GPCA,data=prec_gen$P03GPCA,sample="monthly",origin_x=origin,origin_data=origin,step=0)

exogen <- NULL
exogen_sim <- NULL
#exogen <- prec_mes 
#exogen_sim <- prec_gen$P03GPCA
seed <- 1224


set.seed(seed)
generationP06GPCA_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_temp,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)
set.seed(seed)
generationP01GPCA_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_test,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)

set.seed(seed)
generationP06_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_temp,n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)

set.seed(seed)
generationP01_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_test,n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)

# VAR select 

VARselect(generationP01_temp$input$data_for_var,lag.max=20) 
VARselect(generationP01GPCA_temp$var@GPCA_data$final_results,lag.max=20)


normality_test(generationP01_temp$var)
normality_test(generationP06_temp$var)
normality_test(generationP01GPCA_temp$var)
normality_test(generationP06GPCA_temp$var)


serial_test(generationP01_temp$var)
serial_test(generationP06_temp$var)
serial_test(generationP01GPCA_temp$var)
serial_test(generationP06GPCA_temp$var)


# Collecting the measured and generated time series 

Tn_mes <- generationP01_temp$input$Tn_mes
Tx_mes <- generationP01_temp$input$Tx_mes
Tx_spline <- generationP01_temp$input$Tx_spline
Tn_spline <- generationP01_temp$input$Tn_spline

Tx_gen <- list(P06GPCA=generationP06GPCA_temp$output$Tx_gen,
		P01GPCA=generationP01GPCA_temp$output$Tx_gen,
		P06=generationP06_temp$output$Tx_gen,
		P01=generationP01_temp$output$Tx_gen)



Tn_gen <- list(P06GPCA=generationP06GPCA_temp$output$Tn_gen,
		P01GPCA=generationP01GPCA_temp$output$Tn_gen,
		P06=generationP06_temp$output$Tn_gen,
		P01=generationP01_temp$output$Tn_gen)



prec_mes <- NULL
prec_gen <- NULL
## save inpts 
results <- list(Tx_gen=Tx_gen,Tn_gen=Tn_gen,Tx_mes=Tx_mes,Tn_mes=Tn_mes,Tn_spline=Tn_spline,Tx_spline=Tx_spline,prec_mes=prec_mes,prec_gen=prec_gen,year_min=year_min,year_max=year_max,
				data_tempGPCA=generationP06GPCA_temp$var@GPCA_data$final_results,
				data_temp=generationP01_temp$input$data_for_var)
filename <- paste(wpath,"results_uncoupled_temperature_generator_P06.rda",sep="/")
save(results,file=filename)




#
#s
#
#$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#6      2      1      6 
#
#$criteria
#1             2             3             4             5             6             7             8             9            10
#AIC(n) -1.154384e+01 -1.234313e+01 -1.260051e+01 -1.271298e+01 -1.275324e+01 -1.276251e+01 -1.274319e+01 -1.270848e+01 -1.265840e+01 -1.259008e+01
#HQ(n)  -1.124448e+01 -1.175252e+01 -1.171864e+01 -1.153985e+01 -1.128885e+01 -1.100686e+01 -1.069628e+01 -1.037031e+01 -1.002897e+01 -9.669389e+00
#SC(n)  -1.065549e+01 -1.059046e+01 -9.983502e+00 -9.231639e+00 -8.407566e+00 -7.552499e+00 -6.668845e+00 -5.769802e+00 -4.855385e+00 -3.922733e+00
#FPE(n)  9.695640e-06  4.359622e-06  3.370367e-06  3.011934e-06  2.893232e-06  2.866770e-06  2.923014e-06  3.026690e-06  3.182730e-06  3.408560e-06
#
#
#VARselect(generationP10GPCA_temp$var@GPCA_data$final_results)
#$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#6      2      1      6 
#
#$criteria
#1             2             3             4             5             6             7             8             9            10
#AIC(n) -1.154384e+01 -1.234313e+01 -1.260051e+01 -1.271298e+01 -1.275324e+01 -1.276251e+01 -1.274319e+01 -1.270848e+01 -1.265840e+01 -1.259008e+01
#HQ(n)  -1.124448e+01 -1.175252e+01 -1.171864e+01 -1.153985e+01 -1.128885e+01 -1.100686e+01 -1.069628e+01 -1.037031e+01 -1.002897e+01 -9.669389e+00
#SC(n)  -1.065549e+01 -1.059046e+01 -9.983502e+00 -9.231639e+00 -8.407566e+00 -7.552499e+00 -6.668845e+00 -5.769802e+00 -4.855385e+00 -3.922733e+00
#FPE(n)  9.695640e-06  4.359622e-06  3.370367e-06  3.011934e-06  2.893232e-06  2.866770e-06  2.923014e-06  3.026690e-06  3.182730e-06  3.408560e-06
#
#> VARselect(generationP01_temp$input$data_for_var)
#$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#6      2      1      6 
#
#$criteria
#1             2             3             4             5             6             7             8             9            10
#AIC(n) -4.780245e+01 -4.864611e+01 -4.890469e+01 -4.901659e+01 -4.905368e+01 -4.905952e+01 -4.903500e+01 -4.900086e+01 -4.894643e+01 -4.887712e+01
#HQ(n)  -4.750310e+01 -4.805550e+01 -4.802282e+01 -4.784346e+01 -4.758929e+01 -4.730387e+01 -4.698809e+01 -4.666269e+01 -4.631700e+01 -4.595642e+01
#SC(n)  -4.691411e+01 -4.689343e+01 -4.628768e+01 -4.553524e+01 -4.470800e+01 -4.384951e+01 -4.296065e+01 -4.206218e+01 -4.114341e+01 -4.020977e+01
#FPE(n)  1.736434e-21  7.469056e-22  5.767297e-22  5.156920e-22  4.969410e-22  4.940852e-22  5.064070e-22  5.240676e-22  5.534892e-22  5.933512e-22
##
