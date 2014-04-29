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
library(RMRAINGEN)
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
wpath <-  '/Users/ecor/Dropbox/iasma/RMAWGENdev/RMAWGEN_article/plottingscripts/ems_article' 
rdaname <-  'data/results_precipitation_generator_plural_station.rda'
rdaname <- paste(wpath,rdaname,sep='/')


load(rdaname)
## 
 ## to be adjusted!!! in rda
##
winter <- c("Dec","Jan","Feb")
spring <- c("Mar","Apr","May")
summer <- c("Jun","Jul","Aug")
autumn <- c("Sep","Oct","Nov")





year_min <- results_prec$year_min
year_max <- results_prec$year_max

origin <- paste(year_min,1,1,sep="-")

prec_mes <- results_prec$prec_mes

prec_gen <- results_prec$prec_gen



## http://stackoverflow.com/questions/11800212/write-plots-to-png-file

stations <- names(prec_mes)

## added on ec 20140327 
#
#extract <- "dry"
#dw.spell.dry.mes <- dw.spell(prec_mes,origin=origin,extract=extract)
#dw.spell.dry.gen <- lapply(X=prec_gen,FUN=dw.spell,origin=origin,extract=extract)
#
#station <- "T0129"
###names(dw.spell.dry.gen)
#type <- "P03GPCA" 
#type <- "RMRAINGEN"
#obs <- dw.spell.dry.mes[[station]]$spell_length
#gen <- dw.spell.dry.gen[[type]][[station]]$spell_length
#qqplot(obs,gen)
#abline(0,1)
#stop()
## end added on ec 20140327 



## Chech the correcteness of the station!! 
qqplot=FALSE ## TRUE
if (qqplot) {
	for (it in stations) { 
	
		png <- paste(wpath,'plot/qqplot_prec_RMAWGEN_XXX.png',sep='/')  
		title <- "Quantile-Quantile daily precipitation at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec <- QuantileQuantilePlot(x=prec_mes,y=prec_gen,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,station=it)
			
		png(png,width=width,height=height)
		print(qqplot_prec)
		dev.off()	
			
	
		png <- paste(wpath,'plot/qqplot_prec_RMAWGEN_XXX_unseas.png',sep='/')  
		title <- "Quantile-Quantile daily precipitation at XXX"		
		
		png <- str_replace(png,"XXX",it)
		title <- str_replace(title,"XXX",it)
		qqplot_prec_unseas <- QuantileQuantilePlot(x=prec_mes,y=prec_gen,xlab="observed",ylab="generated",title=title,season=FALSE,origin=origin,station=it)
		
		png(png,width=width,height=height_unseas)
		print(qqplot_prec_unseas)
		dev.off()	
		
	
		
		
		
	
	}
}


return.values=c("nooccurence","occurence","continuity_ratio","nooccurence_gcorrelation")
titles <- c("Joint probabilities that station pairs are both dry", "Joint probabilities that station pairs are both wet","Continuity Ratio","Wilks Gaussian Correlation that station pairs are both dry")
names(return.values) <- titles
for (it in titles) {
	for (lag in c(0,1)) {
	
		title <- paste(it,"(Lag Y)",sep=" ")
		png <- paste(wpath,'plot/XXX_lagY_prec_RMAWGEN.png',sep='/')
		
		png <- str_replace(png,"XXX",return.values[it])
		png <- str_replace(png,"Y",lag)
		
		title <- str_replace(title,"Y",lag)
		
		ccgammaplot_out <- ccgammaplot(x=prec_mes,y=prec_gen,corx=NULL,xlab="observed",ylab="generated",title=title,season=TRUE,origin=origin,lag=lag,return.value=return.values[it])
	
		png(png,width=width,height=height)
	    print(ccgammaplot_out)
	    dev.off()	
		
		
	}
}




