# TODO: Add comment
# 
# Author: ecor
###############################################################################
# file corplot.R
#
# This file ...
#
# author: Emanuele Cordano on 17-01-2014
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
library(ggplot2)
library(reshape2)
library(RMAWGEN)
library(stringr)
## load additional function 



####

additional_function_path  <- "/Users/ecor/Dropbox/iasma/RMAWGENdev/RMAWGEN_article/plottingscripts/additional_functions"
list.files <- list.files(additional_function_path,pattern=".R",full.names=TRUE)
for (it in list.files) source(it)
#####

## PLOT SIZE

width = 480
height = width
height_unseas=height/2
height_lags=height



# http://stackoverflow.com/questions/11800212/write-plots-to-png-file ### LOOK AT HERE ... 

wpath <- '/Users/ecor/Dropbox/iasma/RMAWGENdev/RMAWGEN_article/plottingscripts/ems_article'

#wpath <- '/Users/ecor/Dropbox/iasma/RMAWGENdev/RMAWGEN_article/plottingscripts/ems_article' ##/data/results_uncoupled_temperature_generator_plural_station.rda
#rdaname <-  'data/results_uncoupled_temperature_generator_plural_station.rda'

#rdaname <- paste(wpath,rdaname,sep='/')
# From RMAWGENCodeCorner
rdaname <- '/Users/ecor/R-packages/RMAWGENCodeCorner/data/results_uncoupled_temperature_generator_P06.rda' 


load(rdaname)
## 
 ## to be adjusted!!! in rda
##
winter <- c("Dec","Jan","Feb")
spring <- c("Mar","Apr","May")
summer <- c("Jun","Jul","Aug")
autumn <- c("Sep","Oct","Nov")





year_min <- results$year_min
year_max <- results$year_max

origin <- paste(year_min,1,1,sep="-")

Tx_mes <- results$Tx_mes ##-results$Tx_spline
Tn_mes <- results$Tn_mes ### -results$Tn_spline

Tx_gen <- results$Tx_gen
Tn_gen <- results$Tn_gen

Tx_mes_s <- results$Tx_mes-results$Tx_spline
Tn_mes_s <- results$Tn_mes-results$Tn_spline
Tx_gen_s <- lapply(X=results$Tx_gen,FUN=function(x,y){x-y},y=results$Tx_spline)
Tn_gen_s <- lapply(X=results$Tn_gen,FUN=function(x,y){x-y},y=results$Tn_spline)

spatialCorrelation <- FALSE
if (spatialCorrelation) {
	
	method <- "kendall"
	## plot Temparature Correletion 
	png <- paste(wpath,'plot/spatial_correlation_tmin_uncoupled.png',sep='/')
	
	
	corplot_tn <- corplot(x=Tn_mes,y=Tn_gen,method=method,season=TRUE,title="Spatial Correlation: daily minimum temperature anom.",origin=origin)
	
	png(png,width=width,height=height)
	print(corplot_tn)
	dev.off()
	
	png <- paste(wpath,'plot/spatial_correlation_tmax_uncoupled.png',sep='/')
	
	
	corplot_tx <- corplot(x=Tx_mes,y=Tx_gen,method=method,season=TRUE,title="Spatial Correlation: daily maximum temperature anom.",origin=origin)
	
	png(png,width=width,height=height)
	print(corplot_tx)
	dev.off()
	
	
	png <- paste(wpath,'plot/spatial_correlation_tmin_uncoupled_unseas.png',sep='/')
	
	
	corplot_tn_unseas <- corplot(x=Tn_mes,y=Tn_gen,method=method,season=FALSE,title="Spatial Correlation: daily minimum temperature anom.",origin=origin)
	
	png(png,width=width,height=height_unseas)
	print(corplot_tn_unseas)
	dev.off()
	
	png <- paste(wpath,'plot/spatial_correlation_tmax_uncoupled_unseas.png',sep='/')
	
	
	corplot_tx_unseas <- corplot(x=Tx_mes,y=Tx_gen,method=method,season=FALSE,title="Spatial Correlation: daily maximum temperature anom.",origin=origin)
	
	png(png,width=width,height=height_unseas)
	print(corplot_tx_unseas)
	dev.off()
}
## http://stackoverflow.com/questions/11800212/write-plots-to-png-file

stations <- names(Tx_mes)[1:4]

## Chech the correcteness of the station!! 
qqplot=TRUE ## TRUE
if (qqplot) {
	for (it in stations) { 
	
		png <- paste(wpath,'plot/qqplot_tmin_uncoupled_XXX.png',sep='/')  
		title <- "Quantile-Quantile daily minimum temperature at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_tn <- QuantileQuantilePlot(x=Tn_mes,y=Tn_gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
			
		png(png,width=width,height=height)
		print(qqplot_tn)
		dev.off()	
			
		png <- paste(wpath,'plot/qqplot_tmax_uncoupled_XXX.png',sep='/')  
		title <- "Quantile-Quantile daily maximum temperature at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_tx <- QuantileQuantilePlot(x=Tx_mes,y=Tx_gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
		
		png(png,width=width,height=height)
		print(qqplot_tx)
		dev.off()		
		
		
		png <- paste(wpath,'plot/qqplot_tmin_uncoupled_XXX_unseas.png',sep='/')  
		title <- "Quantile-Quantile daily minimum temperature at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_tn_unseas <- QuantileQuantilePlot(x=Tn_mes,y=Tn_gen,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,station=it)
		
		png(png,width=width,height=height_unseas)
		print(qqplot_tn_unseas)
		dev.off()	
		
		png <- paste(wpath,'plot/qqplot_tmax_uncoupled_XXX_unseas.png',sep='/')  
		title <- "Quantile-Quantile daily maximum temperature at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_tx_unseas <- QuantileQuantilePlot(x=Tx_mes,y=Tx_gen,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,station=it)
		
		png(png,width=width,height=height_unseas)
		print(qqplot_tx_unseas)
		dev.off()		
		
		
		
		
		
			
		
		
		
		
		
		
		
		
	
	}
}
###stop("Temporary Stop")
lags <- 0:5 ###0:5

for (l in lags) { 

	# TMIN 
	title <- "XXX-lag spatial Autocorrelation: daily minimum temperature anom."
	png <- paste(wpath,'plot/spatial_XXX-lag_autocorrelation_tmin_uncoupled.png',sep='/')
	
	png <- str_replace(png,"XXX",l)
	title <- str_replace(title,"XXX",l)
	
	corplot  <- corplotlag(x=Tn_mes,y=Tn_gen,corx=NULL,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,lag=l)
	
	png(png,width=width,height=height)
	print(corplot)
	dev.off()	
	# TMAX 
	
	title <- "XXX-lag spatial Autocorrelation: daily maximum temperature anom."
	png <- paste(wpath,'plot/spatial_XXX-lag_autocorrelation_tmax_uncoupled.png',sep='/')
	
	png <- str_replace(png,"XXX",l)
	title <- str_replace(title,"XXX",l)
	
	corplot  <- corplotlag(x=Tx_mes,y=Tx_gen,corx=NULL,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,lag=l)
	
	png(png,width=width,height=height)
	print(corplot)
	dev.off()		
	
	## UNSEASON 
	
	
	# TMIN 
	title <- "XXX-lag spatial Autocorrelation: daily minimum temperature anom."
	png <- paste(wpath,'plot/spatial_XXX-lag_autocorrelation_tmin_uncoupled_unseas.png',sep='/')
	
	png <- str_replace(png,"XXX",l)
	title <- str_replace(title,"XXX",l)
	
	corplot  <- corplotlag(x=Tn_mes,y=Tn_gen,corx=NULL,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,lag=l)
	
	png(png,width=width,height=height_unseas)
	print(corplot)
	dev.off()	
	# TMAX 
	
	title <- "XXX-lag spatial Autocorrelation: daily maximum temperature anom."
	png <- paste(wpath,'plot/spatial_XXX-lag_autocorrelation_tmax_uncoupled_unseas.png',sep='/')
	
	png <- str_replace(png,"XXX",l)
	title <- str_replace(title,"XXX",l)
	
	corplot  <- corplotlag(x=Tx_mes,y=Tx_gen,corx=NULL,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,lag=l)
	
	png(png,width=width,height=height_unseas)
	print(corplot)
	dev.off()		
	
	## UNSEASON 
	
	
	
	
}


#### 

## Plot of comprehensive 

lag <- c(0:2,7)


corplotlags(lag=lag)
title <- " Auto-cross-correlation: daily maximum temperature anom."
png <- paste(wpath,'plot/spatial_autocrosscorrelation_tmax_uncoupled_lags.png',sep='/')

corplotl  <- corplotlags(x=Tx_mes_s,y=Tx_gen_s,corx=NULL,xlab="observed",ylab="generated",title=title,origin=origin,lag=lag)

png(png,width=width,height=height_lags)
print(corplotl)
dev.off()		


corplotlags(lag=lag)
title <- " Auto-cross-correlation: daily minimum temperature anom."
png <- paste(wpath,'plot/spatial_autocrosscorrelation_tmin_uncoupled_lags.png',sep='/')

corplotl  <- corplotlags(x=Tn_mes_s,y=Tn_gen_s,corx=NULL,xlab="observed",ylab="generated",title=title,origin=origin,lag=lag)

png(png,width=width,height=height_lags)
print(corplotl)
dev.off()		











