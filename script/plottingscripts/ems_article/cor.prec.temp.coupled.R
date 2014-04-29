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
library(RWEATHER)
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

#wpath <- '/Users/ecor/Dropbox/iasma/RMAWGENdev/RMAWGEN_article/plottingscripts/ems_article'
wpath <- '/Users/ecor/Dropbox/iasma/RMAWGENdev/RMAWGEN_article/plottingscripts/ems_article' ##/data/results_coupled_temperature_generator_plural_station.rda
rdaname <-  'data/results_coupled_temperature_generator_P06.rda'
rdaname <- paste(wpath,rdaname,sep='/')
rdaname_uncoupled <- 'data/results_uncoupled_temperature_generator_P06.rda'
rdaname_uncoupled <- paste(wpath,rdaname_uncoupled,sep='/')


# From RMAWGENCodeCorner

## WE iGNORED "UNCOUPLED" DATA 

#rdaname <- '/Users/ecor/R-packages/RMAWGENCodeCorner/data/results_coupled_temperature_generator_P06.rda' 
#load(rdaname_uncoupled)
#
#Tx_gen_uncoupled <- results$Tx_gen
#Tn_gen_uncoupled <- results$Tn_gen

load(rdaname)



year_min <- results$year_min
year_max <- results$year_max

origin <- paste(year_min,1,1,sep="-")

Tx_mes <- results$Tx_mes ##-results$Tx_spline
Tn_mes <- results$Tn_mes ### -results$Tn_spline
prec_mes <- results$prec_mes
prec_gen <- results$prec_gen


Tx_gen <- results$Tx_gen
Tn_gen <- results$Tn_gen
 



names_for_prec <- c("P03GPCA","RMRAINGEN")
names(Tx_gen)
names <- c("P06GPCA-P03GPCA","P01GPCA-P03GPCA","P06GPCA-RMRAINGEN","P01GPCA-RMRAINGEN")
names_for_prec <- unlist(lapply(X=str_split(names,"-"),FUN=function(x){x[2]}))

prec_gen <- prec_gen[names_for_prec]

names(Tx_gen) <- names
names(Tn_gen) <- names
names(prec_gen)   <- names

Tx_gen$obs <- Tx_mes
Tn_gen$obs <- Tn_mes
prec_gen$obs <- prec_mes

#### 

station <- c("T0083","T0090","T0129") ###unique(intersect(names(Tx_mes),names(prec_mes)))[1:5]

title <- "Correlation log(prec)~Daily Thermal Range"
ylab <- "value"
xlab <- "Model Configuration"
cors <- logcor.plot(prec=prec_gen,Tx=Tx_gen,Tn=Tn_gen,station=station,origin=origin,fun=applyCor,valmin=0.5,applyCorfun=extract_cor_value,xlab=xlab,ylab=ylab,title=title)
## save the plot here!!! 
png <- paste(wpath,'plot/cor_prec_temp.png',sep='/')
png(png,width=width,height=height)
print(cors)
dev.off()	


stations <- unique(intersect(names(Tx_mes),names(prec_mes)))
print(stations)

for (it in stations) { 
	
	station <- it
	title_tn <- paste("Minimum Temperature at ",station,sep="")
	title_tx <- paste("Maximum Temperature at ",station,sep="")
	title_dt <- paste("Thermal Range at ",station,sep="")
	conf_xlab <- "Model Configuration"

	temp_min <- temperature.wetdry.barplot(prec=prec_gen,Tx=Tx_gen,Tn=Tn_gen,station=station,valmin_prec=0.5,variable="temperature_min",title=title_tn,ylab=title_tn,xlab=conf_xlab)
	temp_max <- temperature.wetdry.barplot(prec=prec_gen,Tx=Tx_gen,Tn=Tn_gen,station=station,valmin_prec=0.5,variable="temperature_max",title=title_tx,ylab=title_tx,xlab=conf_xlab)
	thermal_range <- temperature.wetdry.barplot(prec=prec_gen,Tx=Tx_gen,Tn=Tn_gen,station=station,valmin_prec=0.5,variable="thermal_range",title=title_dt,ylab=title_dt,xlab=conf_xlab)

	

	png_tmin <- paste(wpath,'plot/boxplot_wetdry_tmin_STATION.png',sep='/')
	png_tmax <- paste(wpath,'plot/boxplot_wetdry_tmax_STATION.png',sep='/')
	png_dt <- paste(wpath,'plot/boxplot_wetdry_dt_STATION.png',sep='/')

	
	png_tmin <- str_replace(png_tmin,"STATION",station)
	png_tmax <- str_replace(png_tmax,"STATION",station)
	png_dt <- str_replace(png_dt,"STATION",station)
	
	
	png(png_tmin,width=width,height=height)
	print(temp_min)
	dev.off()	
	
	png(png_tmax,width=width,height=height)
	print(temp_max)
	dev.off()	
	
	png(png_dt,width=width,height=height)
	print(thermal_range)
	dev.off()	
	
	
}