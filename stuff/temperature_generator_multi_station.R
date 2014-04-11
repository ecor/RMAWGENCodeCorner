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
set.seed(1222)

data(trentino)
## path where to plot the output figures
wpath <- "/Users/ecor/dev/github/RMAWGENCodeCorner/output/temperature_generator_multi_station"
		
### trash		"/Users/ecor/R-packages/RMAWGENCodeCorner/output/temperature_generator_multi_station"
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
p_temp <- 5

# Exogenous variables : Daily Precipitation (Observed and P03GPCA)  

##exogen <- normalizeGaussian_severalstations(x=prec_mes,data=prec_mes,sample="monthly",origin_x=origin,origin_data=origin,step=0)
##exogen_sim <- normalizeGaussian_severalstations(x=prec_gen$P03GPCA,data=prec_gen$P03GPCA,sample="monthly",origin_x=origin,origin_data=origin,step=0)

exogen <- NULL
exogen_sim <- NULL
#exogen <- prec_mes 
#exogen_sim <- prec_gen$P03GPCA

generationP10GPCA_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_temp,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)
generationP01GPCA_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_test,n_GPCA_iteration=n_GPCA_iter,n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)


generationP10_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_temp,n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)
generationP01_temp <- ComprehensiveTemperatureGenerator(station=station,Tx_all=TEMPERATURE_MAX,Tn_all=TEMPERATURE_MIN,year_min=year_min,year_max=year_max,p=p_test,n_GPCA_iteration=0,n_GPCA_iteration_residuals=0,exogen=exogen,exogen_sim=exogen_sim,sample="monthly",mean_climate_Tn=TN_CLIMATE,mean_climate_Tx=TX_CLIMATE)

# VAR select 

VARselect(generationP01_temp$input$data_for_var,lag.max=20) 
VARselect(generationP01GPCA_temp$var@GPCA_data$final_results,lag.max=20)


normality_test(generationP01_temp$var)
normality_test(generationP10_temp$var)
normality_test(generationP01GPCA_temp$var)
normality_test(generationP10GPCA_temp$var)


serial_test(generationP01_temp$var)
serial_test(generationP10_temp$var)
serial_test(generationP01GPCA_temp$var)
serial_test(generationP10GPCA_temp$var)


# Collecting the measured and generated time series 

Tn_mes <- generationP01_temp$input$Tn_mes
Tx_mes <- generationP01_temp$input$Tx_mes
Tx_spline <- generationP01_temp$input$Tx_spline
Tn_spline <- generationP01_temp$input$Tn_spline

Tx_gen <- list(P10GPCA=generationP10GPCA_temp$output$Tx_gen,
		P01GPCA=generationP01GPCA_temp$output$Tx_gen,
		P10=generationP10_temp$output$Tx_gen,
		P01=generationP01_temp$output$Tx_gen)



Tn_gen <- list(P10GPCA=generationP10GPCA_temp$output$Tn_gen,
		P01GPCA=generationP01GPCA_temp$output$Tn_gen,
		P10=generationP10_temp$output$Tn_gen,
		P01=generationP01_temp$output$Tn_gen)



for (it in names(days)) {
	
	str(it)
	name <- it
	season <- days[[it]]
	pdf_Tx <- paste(wpath,"/tx_qqplot_uncoupled_",year_min,"_",year_max,"_",it,".pdf",sep="")
	pdf_Tn <- paste(wpath,"/tn_qqplot_uncoupled_",year_min,"_",year_max,"_",it,".pdf",sep="")
	pdf_deltaT <- paste(wpath,"/dt_qqplot_uncoupled_",year_min,"_",year_max,"_",it,".pdf",sep="")
	pdf_Tx_anom <- paste(wpath,"/tx_anom_qqplot_uncoupled_",year_min,"_",year_max,"_",it,".pdf",sep="")
	pdf_Tn_anom <- paste(wpath,"/tn_anom_qqplot_uncoupled_",year_min,"_",year_max,"_",it,".pdf",sep="")
	main_tx  <- paste("Tx",names(Tx_gen),station00,it,sep=" ")
	main_tn  <- paste("Tn",names(Tn_gen),station00,it,sep=" ")
	main_deltat  <- paste("dT",names(Tx_gen),station00,it,sep=" ")
	main_tx_anom  <- paste("Tx_anom",names(Tx_gen),station00,it,sep=" ")
	main_tn_anom  <- paste("Tn_anom",names(Tn_gen),station00,it,sep=" ")
	
	qqplot_RMAWGEN_Tx(Tx_mes=Tx_mes,Tn_mes=Tn_mes,Tx_gen=Tx_gen,Tn_gen=Tn_gen,main=main_tx,station=station00,when=season,pdf=pdf_Tx,cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
	qqplot_RMAWGEN_Tn(Tx_mes=Tx_mes,Tn_mes=Tn_mes,Tx_gen=Tx_gen,Tn_gen=Tn_gen,main=main_tn,station=station00,when=season,pdf=pdf_Tn,cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
	qqplot_RMAWGEN_deltaT(Tx_mes=Tx_mes,Tn_mes=Tn_mes,Tx_gen=Tx_gen,Tn_gen=Tn_gen,main=main_deltat,station=station00,when=season,pdf=pdf_deltaT,cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
	qqplot_RMAWGEN_Tx(Tx_mes=Tx_mes,Tn_mes=Tn_mes,Tx_gen=Tx_gen,Tn_gen=Tn_gen,Tx_spline=Tx_spline,Tn_spline=Tn_spline,main=main_tx_anom,station=station00,when=season,pdf=pdf_Tx_anom,cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
	qqplot_RMAWGEN_Tn(Tx_mes=Tx_mes,Tn_mes=Tn_mes,Tx_gen=Tx_gen,Tn_gen=Tn_gen,Tx_spline=Tx_spline,Tn_spline=Tn_spline,main=main_tn_anom,station=station00,when=season,pdf=pdf_Tn_anom,cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
	
}

# ACF Function 
pdf(paste(wpath,"acf_uncoupled_tx_anom_P10GPCA.pdf",sep="/"))

plot(acf(Tx_gen$P10GPCA-Tx_spline,lag=50),xlab="lag [day]",cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
dev.off()
pdf(paste(wpath,"acf_uncoupled_tx_anom_mes.pdf",sep="/"))
plot(acf(Tx_mes-Tx_spline,lag=50))
dev.off()

pdf(paste(wpath,"acf_uncoupled_tn_anom_P10GPCA.pdf",sep="/"))
plot(acf(Tn_gen$P10GPCA-Tn_spline,lag=50),xlab="lag [day]")
dev.off()
pdf(paste(wpath,"acf_uncoupled_tn_anom_mes.pdf",sep="/"))
plot(acf(Tn_mes-Tn_spline,lag=50),cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
dev.off()

pdf(paste(wpath,"acf_uncoupled_deltat_P10GPCA.pdf",sep="/"))
plot(acf(Tx_gen$P10GPCA-Tn_gen$P10GPCA,lag=50),xlab="lag [day]")
dev.off()
pdf(paste(wpath,"acf_uncoupled_deltat_mes.pdf",sep="/"))
plot(acf(Tx_mes-Tn_mes,lag=50),cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
dev.off()

print("acf PUT PRECIPITAION!!!!")
dT_mes <- Tx_mes - Tn_mes 
dT_gen <- Tx_gen$P10GPCA-Tn_gen$P10GPCA



names(Tx_gen$P10GPCA) <- paste("Tx",names(Tx_gen$P10GPCA),sep="_")
names(Tx_spline)      <- paste("Tx",names(Tx_spline),sep="_")
names(Tx_mes)         <- paste("Tx",names(Tx_mes),sep="_")


names(Tn_gen$P10GPCA) <- paste("Tn",names(Tn_gen$P10GPCA),sep="_")
names(Tn_spline)      <- paste("Tn",names(Tn_spline),sep="_")
names(Tn_mes)         <- paste("Tn",names(Tn_mes),sep="_")

# plottare auto-covarianze !!!! (VEDI SOPRA) ACF Function 
names(prec_mes)         <- paste("prec",names(prec_mes),sep="_")
names(prec_gen$P03GPCA) <- paste("prec",names(prec_gen$P03GPCA),sep="_")


names(dT_mes)         <- paste("dT",names(dT_mes),sep="_")
names(dT_gen)         <- paste("dT",names(dT_gen),sep="_")

# Auto-Covariance daily thermal range/precipitation 


dT_station <- "dT_T0090"
prec_station <- "prec_T0090"
val_gen <- as.data.frame(cbind(prec_gen$P03GPCA[,prec_station],dT_gen[,dT_station]))		
val_mes <- as.data.frame(cbind(prec_mes[,prec_station],dT_mes[,dT_station]))

names(val_mes) <- c(prec_station,dT_station)
names(val_gen) <- c(prec_station,dT_station)
pdf(paste(wpath,"acf_uncoupled_prec_dt_anom_P10GPCA.pdf",sep="/"))
plot(acf(val_gen,lag=50),xlab="lag [day]",cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
dev.off()

pdf(paste(wpath,"acf_uncoupled_prec_dt_anom_mes.pdf",sep="/"))
plot(acf(val_mes,lag=50),xlab="lag [day]",cex.main=CEX,cex.lab=CEX,cex.axis=CEX)
dev.off()

#> str(generationP10GPCA_temp$var@GPCA_data$final_results)
#'data.frame':	10957 obs. of  18 variables:
#		$ V1 : num  -0.469 -0.943 -1.409 -1.63 1.321 ...
#$ V2 : num  1.073 -1.726 0.758 -2.836 -0.445 ...
#$ V3 : num  0.294 -1.002 -0.261 -2.123 -0.383 ...
#$ V4 : num  0.318 -0.672 -0.419 0.718 0.975 ...
#$ V5 : num  1.728 -0.143 0.991 1.454 0.219 ...
#$ V6 : num  0.287 0.175 0.796 -1.154 -1.047 ...
#$ V7 : num  -0.9675 -2.0541 -0.0391 -1.4197 0.4037 ...
#$ V8 : num  -0.78 -1.073 -1.554 0.397 -0.356 ...
#$ V9 : num  2.49 0.986 -0.824 1.796 0.329 ...
#$ V10: num  -1.136 0.806 -0.508 1.126 -0.148 ...
#$ V11: num  -0.335 0.433 1.642 -0.169 -1.725 ...
#$ V12: num  -0.381 -2.032 0.121 -2.485 -1.563 ...
#$ V13: num  0.3504 0.0774 -0.099 -0.4151 -0.057 ...
#$ V14: num  -2.433 -0.627 -0.216 -0.28 -1.55 ...
#$ V15: num  -0.545 0.401 -0.686 0.31 0.962 ...
#$ V16: num  2.145 -0.792 1.241 -0.112 1.101 ...
#$ V17: num  0.44 -0.334 1.697 -1.09 -1.114 ...
#$ V18: num  -0.582 -1.414 -1.513 -1.194 -0.604 ...
#> VARselect(generationP10GPCA_temp$var@GPCA_data$final_results)
#$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#9      4      2      9 
#
#$criteria
#1            2             3             4             5             6             7             8             9            10
#AIC(n) -6.344669707 -6.904821420 -7.1151460468 -7.2244749331 -7.2877819519 -7.3297752668 -7.3523440603 -7.3667617586 -7.3786893263 -7.3698300709
#HQ(n)  -6.267809360 -6.755146009 -6.8926555709 -6.9291693924 -6.9196613463 -6.8888395964 -6.8385933250 -6.7801959585 -6.7193084614 -6.6376341411
#SC(n)  -6.116581583 -6.460649811 -6.4548909524 -6.3481363533 -6.1953598867 -6.0212697162 -5.8277550242 -5.6260892371 -5.4219333194 -5.1969905786
#FPE(n)  0.001756083  0.001002939  0.0008127032  0.0007285376  0.0006838479  0.0006557287  0.0006410999  0.0006319288  0.0006244435  0.0006300094
#
# 
#> str(generationP01_temp$input$data_for_var)
#'data.frame':	10957 obs. of  18 variables:
#		$ T0001: num  -0.7643 -0.0148 -0.9468 1.5626 0.0202 ...
#$ T0014: num  -0.786 -0.732 -0.914 -0.51 -0.66 ...
#$ T0064: num  -0.846 -1.058 -0.455 -0.294 -0.274 ...
#$ T0083: num  -0.691 -0.376 -0.926 -0.359 -0.213 ...
#$ T0090: num  -0.311 0.246 -0.117 0.282 -0.461 ...
#$ T0129: num  -0.232 -0.134 -0.708 0.039 0.031 ...
#$ T0139: num  -1.008 -1.003 -1.19 -0.654 -0.246 ...
#$ T0211: num  -0.644 0.066 -0.353 0.66 -0.147 ...
#$ T0367: num  -0.7182 -0.5008 -0.8083 -0.0795 -1.0166 ...
#$ T0001: num  -0.6502 -1.1471 0.0606 -0.6502 -0.1147 ...
#$ T0014: num  -0.513 -0.746 -0.764 -0.271 0.221 ...
#$ T0064: num  -1.61 -1.61 -0.611 -1.373 1.082 ...
#$ T0083: num  -0.808 -1.581 -1.116 -1.116 0.951 ...
#$ T0090: num  -0.739 -2.024 -0.972 -2.024 0.161 ...
#$ T0129: num  -3.069 -1.132 -1.132 -1.591 -0.382 ...
#$ T0139: num  -0.595 -1.258 -2.198 -0.889 1.121 ...
#$ T0211: num  -0.458 0.232 0.232 -0.746 0.66 ...
#$ T0367: num  -1.16 0.91 -1.85 2.2 1.3 ...
#> VARselect(generationP01_temp$input$data_for_var)
#$selection
#AIC(n)  HQ(n)  SC(n) FPE(n) 
#9      4      2      9 
#
#$criteria
#1             2             3             4             5             6             7             8             9
#AIC(n) -2.137185e+01 -2.192806e+01 -2.213576e+01 -2.224250e+01 -2.230644e+01 -2.234717e+01 -2.237013e+01 -2.238477e+01 -2.239684e+01
#HQ(n)  -2.129499e+01 -2.177838e+01 -2.191327e+01 -2.194719e+01 -2.193832e+01 -2.190624e+01 -2.185638e+01 -2.179820e+01 -2.173746e+01
#SC(n)  -2.114376e+01 -2.148389e+01 -2.147551e+01 -2.136616e+01 -2.121402e+01 -2.103867e+01 -2.084554e+01 -2.064409e+01 -2.044008e+01
#FPE(n)  5.227847e-10  2.997545e-10  2.435347e-10  2.188803e-10  2.053243e-10  1.971294e-10  1.926563e-10  1.898592e-10  1.875836e-10
#10
#AIC(n) -2.238755e+01
#HQ(n)  -2.165536e+01
#SC(n)  -2.021471e+01
#FPE(n)  1.893358e-10
#
#


## save inpts 
results <- list(Tx_gen=Tx_gen,Tn_gen=Tn_gen,Tx_mes=Tx_mes,Tn_mes=Tn_mes,Tn_spline=Tn_spline,Tx_spline=Tx_spline,prec_mes=prec_mes,prec_gen=prec_gen,year_min=year_min,year_max=year_max,
				data_tempGPCA=generationP10GPCA_temp$var@GPCA_data$final_results,
				data_temp=generationP01_temp$input$data_for_var)
filename <- paste(wpath,"results_uncoupled_temperature_generator_plural_station.rda",sep="/")
save(results,file=filename)










