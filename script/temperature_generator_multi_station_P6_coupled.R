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
##wpath <-  "/Users/ecor/dev/github/RMAWGENCodeCorner/data"
wpath <-  "/Users/ecor/R-packages/RMAWGENCodeCorner/data"
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
	##cond  <- (length(which(!is.na(prec_mesx[,it])))==length(prec_mesx[,it]))
	cond <- TRUE
	accepted[it]  <- (length(which(!is.na(temp_mesx[,it])))==length(temp_mesx[,it])) & cond 
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

#p_test <- 1
#p_prec <- 3
#
#exogen <- NULL
#exogen_sim <- exogen
#
#generationP03GPCA_prec <- ComprehensivePrecipitationGenerator(station=station,prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,p=p_prec,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_prec=PREC_CLIMATE,no_spline=FALSE)
#generationP01GPCA_prec <- ComprehensivePrecipitationGenerator(station=station,prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,p=p_test,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_prec=PREC_CLIMATE,no_spline=FALSE)
#
#
#generationP03_prec <- ComprehensivePrecipitationGenerator(station=station,prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,p=p_prec,n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_prec=PREC_CLIMATE,no_spline=FALSE)
#generationP01_prec <- ComprehensivePrecipitationGenerator(station=station,prec_all=PRECIPITATION,year_min=year_min,year_max=year_max,p=p_test,n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_prec=PREC_CLIMATE,no_spline=FALSE)
#
## VAR select 
#
#VARselect(generationP03_prec$data_prec,lag.max=20) 
#VARselect(generationP03GPCA_prec$var@GPCA_data$final_results,lag.max=20)
#
#
#normality_test(generationP01_prec$var)
#normality_test(generationP03_prec$var)
#normality_test(generationP01GPCA_prec$var)
#normality_test(generationP03GPCA_prec$var)
#
#
#serial_test(generationP01_prec$var)
#serial_test(generationP03_prec$var)
#serial_test(generationP01GPCA_prec$var)
#serial_test(generationP03GPCA_prec$var)
#
## ORGANIZZARE PRECIPITAZIONI COME PER LE TEMPERATURE!!! 
#
#
## Collecting the measured and generated time series 
#
#prec_mes <- generationP01_prec$prec_mes
#
#
#prec_gen <- list(P03GPCA=generationP03GPCA_prec$prec_gen,
#		P01GPCA=generationP01GPCA_prec$prec_gen,
#		P03=generationP03GPCA_prec$prec_gen,
#		P01=generationP01GPCA_prec$prec_gen)
## season 
#
#
#NDAY <- nrow(prec_mes)	
#days <- list()
#days$DJF <-  extractmonths(data=1:NDAY,when=c("Dec","Jan","Feb"),origin=origin)
#days$MAM <- extractmonths(data=1:NDAY,when=c("Mar","Apr","May"),origin=origin)
#days$JJA <- extractmonths(data=1:NDAY,when=c("Jun","Jul","Aug"),origin=origin)
#days$SON <- extractmonths(data=1:NDAY,when=c("Sep","Oct","Nov"),origin=origin)	  
#
#
## SET THE CORRECT PATH WHERE TO PLOT THE FIGURES 
#wpath <-  "/Users/ecor/Dropbox/iasma/RMAWGENdev/RMAWGEN/inst/doc/private/elsevier/article/Rscript_2/images_plural_stations" ##  "./"
#station00 <- "T0090"
#CEX <- 1.4
#
#for (it in names(days)) {
#	
#	str(it)
#	name <- it
#	season <- days[[it]]
#	lag <- 1
#	pdf_prec <- paste(wpath,"/prec_qqplot_",lag,"_",year_min,"_",year_max,"_",it,".pdf",sep="")
#	main_prec  <- paste("prec",names(prec_gen),station00,lag,"days",it,sep=" ")
#	qqplot_RMAWGEN_prec(prec_mes=prec_mes,prec_gen=prec_gen,main=main_prec,station=station00,when=season,pdf=pdf_prec,lag=lag,cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
#
#	lag <- 2
#	pdf_prec <- paste(wpath,"/prec_qqplot_",lag,"_",year_min,"_",year_max,"_",it,".pdf",sep="")
#	main_prec  <- paste("prec",names(prec_gen),station00,lag,"days",it,sep=" ")
#	qqplot_RMAWGEN_prec(prec_mes=prec_mes,prec_gen=prec_gen,main=main_prec,station=station00,when=season,pdf=pdf_prec,lag=lag,xlim=range(prec_mes)*lag,cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
#	
#	
#	
#}
#
#
#
## ACF Function 
#pdf(paste(wpath,"acf_prec_P03GPCA.pdf",sep="/"))
#plot(acf(prec_gen$P03GPCA,lag=10),xlab="lag [day]")
#dev.off()
#pdf(paste(wpath,"acf_prec_mes.pdf",sep="/"))
#plot(acf(prec_mes,lag=10))
#dev.off()

# GENERATION OF DAILY TEMPERATURE COUPLED WITH PRECIPITATION WITH PREVIOUSLY GENERATED


n_GPCA_iter <- 5
n_GPCA_iteration_residuals <- 5

p_test <- 1
p_temp <- 6

# Exogenous variables : Daily Precipitation (Observed and P03GPCA)  
## Precipitation value 

rdarchive <- paste(wpath,"results_precipitation_generator_plural_station.rda",sep="/")
load(rdarchive)

prec_mes <- results_prec$prec_mes
prec_gen <- results_prec$prec_gen


exogen <- normalizeGaussian_severalstations(x=prec_mes,data=prec_mes,sample="monthly",origin_x=origin,origin_data=origin,step=0)
exogen_sim <- normalizeGaussian_severalstations(x=prec_gen$P03GPCA,data=prec_gen$P03GPCA,sample="monthly",origin_x=origin,origin_data=origin,step=0)
exogen_sim_RMRAINGEN <- normalizeGaussian_severalstations(x=prec_gen$RMRAINGEN,data=prec_gen$RMRAINGEN,sample="monthly",origin_x=origin,origin_data=origin,step=0)


seed <- 1224
stop()

set.seed(seed)
generationP06GPCA_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_temp,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE,seed=seed)



set.seed(seed)
generationP01GPCA_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_test,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE,seed=seed)



set.seed(seed)
generationP06GPCA_temp_RMRAINGEN <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_temp,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim_RMRAINGEN,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE,seed=seed)


set.seed(seed)
generationP01GPCA_temp_RMRAINGEN <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_test,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim_RMRAINGEN,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE,seed=seed)


#generationP06_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_temp,n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)
#generationP01_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_test,n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)

# VAR select 

VARselect(generationP06GPCA_temp$var@GPCA_data$final_results,lag.max=20)
VARselect(generationP06GPCA_temp_RMRAINGEN$var@GPCA_data$final_results,lag.max=20)



normality_test(generationP01GPCA_temp$var)
normality_test(generationP06GPCA_temp$var)
normality_test(generationP01GPCA_temp_RMRAINGEN$var)
normality_test(generationP06GPCA_temp_RMRAINGEN$var)


serial_test(generationP01GPCA_temp$var)
serial_test(generationP06GPCA_temp$var)
serial_test(generationP01GPCA_temp_RMRAINGEN$var)
serial_test(generationP06GPCA_temp_RMRAINGEN$var)



# Collecting the measured and generated time series 

Tn_mes <- generationP01GPCA_temp$input$Tn_mes
Tx_mes <- generationP01GPCA_temp$input$Tx_mes
Tx_spline <- generationP01GPCA_temp$input$Tx_spline
Tn_spline <- generationP01GPCA_temp$input$Tn_spline

Tx_gen <- list(P06GPCA=generationP06GPCA_temp$output$Tx_gen,
		P01GPCA=generationP01GPCA_temp$output$Tx_gen,
		P06GPCA_RMRAINGEN=generationP06GPCA_temp_RMRAINGEN$output$Tx_gen,
		P01GPCA_RMRAINGEN=generationP01GPCA_temp_RMRAINGEN$output$Tx_gen)



Tn_gen <- list(P06GPCA=generationP06GPCA_temp$output$Tn_gen,
		P01GPCA=generationP01GPCA_temp$output$Tn_gen,
		P06GPCA_RMRAINGEN=generationP06GPCA_temp_RMRAINGEN$output$Tn_gen,
		P01GPCA_RMRAINGEN=generationP01GPCA_temp_RMRAINGEN$output$Tn_gen)



#prec_mes <- NULL
#prec_gen <- NULL
## save inpts 
results <- list(Tx_gen=Tx_gen,Tn_gen=Tn_gen,Tx_mes=Tx_mes,Tn_mes=Tn_mes,Tn_spline=Tn_spline,Tx_spline=Tx_spline,prec_mes=prec_mes,prec_gen=prec_gen,year_min=year_min,year_max=year_max,
				data_tempGPCA=generationP06GPCA_temp$var@GPCA_data$final_results,
				data_tempGPCA_RMRAINGEN=generationP06GPCA_temp_RMRAINGEN$var@GPCA_data$final_results)
filename <- paste(wpath,"results_coupled_temperature_generator_P06.rda",sep="/")
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
