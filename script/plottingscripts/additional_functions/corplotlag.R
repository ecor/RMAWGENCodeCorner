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


library(ggplot2)
library(reshape2)
library(RMAWGEN)
library(cocor) ## http://www.philippsinger.info/?p=347  http://comparingcorrelations.org/

NULL
#'
#' @param x data.frame 
#' @param lag lag-correletion
#' @param ... further arguments for log 
#' 
#' 
#' 
#' 
#' 

corlag <- function(x,lag=1,...) {
	
	
	out <- array(NA,c(ncol(x),ncol(x)))
	######
	
	out <- acf(x,lag.max=lag,plot=FALSE)$acf[lag+1,,]
#	i <- 1:(nrow(x)-lag)+lag
#	j <- i-lag 
#	
#	
#	for (r in 1:nrow(out)) {
#		for (c in 1:ncol(out)) {
#			
#			out[r,c] <- cor(x=x[i,r],y=x[j,c],...)
#			
#		}
		
#	}
	print(out)
	str(out)

	return(out)
	
	
}

#
#df <- data.frame(x=rnorm(100),y=rnorm(100)+2,z=rexp(100))
#> x <- acf(df)
#> x
#
#Autocorrelations of series ÔdfÕ, by lag
#
#, , x
#
#x            y            z           
#1.000 (  0)  0.004 (  0)  0.149 (  0)
#-0.127 (  1) -0.058 ( -1)  0.132 ( -1)
#-0.047 (  2)  0.042 ( -2)  0.039 ( -2)
#0.011 (  3)  0.029 ( -3)  0.136 ( -3)
#-0.116 (  4)  0.030 ( -4) -0.072 ( -4)
#-0.006 (  5)  0.075 ( -5) -0.147 ( -5)
#0.039 (  6)  0.105 ( -6)  0.031 ( -6)
#0.051 (  7) -0.118 ( -7) -0.152 ( -7)
#-0.121 (  8) -0.066 ( -8)  0.111 ( -8)
#0.101 (  9) -0.118 ( -9) -0.071 ( -9)
#-0.013 ( 10) -0.132 (-10)  0.034 (-10)
#-0.008 ( 11)  0.044 (-11)  0.076 (-11)
#0.050 ( 12) -0.037 (-12) -0.120 (-12)
#0.178 ( 13) -0.105 (-13)  0.069 (-13)
#-0.055 ( 14) -0.195 (-14) -0.141 (-14)
#-0.084 ( 15) -0.035 (-15)  0.078 (-15)
#
#, , y
#
#x            y            z           
#0.004 (  0)  1.000 (  0)  0.034 (  0)
#-0.203 (  1)  0.042 (  1)  0.009 ( -1)
#0.040 (  2) -0.034 (  2) -0.133 ( -2)
#0.143 (  3) -0.065 (  3) -0.076 ( -3)
#-0.175 (  4)  0.018 (  4) -0.050 ( -4)
#-0.027 (  5) -0.016 (  5) -0.013 ( -5)
#-0.038 (  6)  0.016 (  6)  0.045 ( -6)
#0.140 (  7)  0.074 (  7) -0.007 ( -7)
#-0.018 (  8) -0.093 (  8)  0.073 ( -8)
#0.193 (  9) -0.072 (  9)  0.008 ( -9)
#0.078 ( 10) -0.066 ( 10)  0.053 (-10)
#-0.018 ( 11)  0.014 ( 11)  0.036 (-11)
#0.066 ( 12) -0.040 ( 12) -0.037 (-12)
#-0.014 ( 13)  0.019 ( 13)  0.184 (-13)
#-0.062 ( 14)  0.061 ( 14)  0.050 (-14)
#-0.008 ( 15)  0.272 ( 15)  0.107 (-15)
#
#, , z
#
#x            y            z           
#0.149 (  0)  0.034 (  0)  1.000 (  0)
#0.009 (  1) -0.042 (  1) -0.101 (  1)
#0.109 (  2)  0.094 (  2)  0.048 (  2)
#-0.201 (  3) -0.026 (  3)  0.033 (  3)
#0.028 (  4)  0.137 (  4) -0.011 (  4)
#-0.035 (  5)  0.089 (  5) -0.126 (  5)
#-0.011 (  6) -0.106 (  6) -0.008 (  6)
#0.050 (  7) -0.013 (  7) -0.120 (  7)
#0.050 (  8) -0.007 (  8)  0.182 (  8)
#-0.007 (  9) -0.018 (  9) -0.045 (  9)
#0.112 ( 10) -0.136 ( 10)  0.031 ( 10)
#-0.031 ( 11) -0.078 ( 11) -0.145 ( 11)
#0.046 ( 12) -0.255 ( 12)  0.096 ( 12)
#0.106 ( 13) -0.078 ( 13)  0.002 ( 13)
#-0.026 ( 14) -0.021 ( 14)  0.010 ( 14)
#-0.085 ( 15) -0.029 ( 15) -0.025 ( 15)
#
#> str(df)
#'data.frame':	100 obs. of  3 variables:
#		$ x: num  -0.423 -0.234 -0.709 0.355 1.851 ...
#$ y: num  0.415 2.218 1.57 2.377 1.408 ...
#$ z: num  0.348 0.108 1.565 1.486 1.288 ...
#> str(acf(df))
#List of 6
#$ acf   : num [1:16, 1:3, 1:3] 1 -0.1274 -0.0468 0.011 -0.1162 ...
#$ type  : chr "correlation"
#$ n.used: int 100
#$ lag   : num [1:16, 1:3, 1:3] 0 1 2 3 4 5 6 7 8 9 ...
#$ series: chr "df"
#$ snames: chr [1:3] "x" "y" "z"
#- attr(*, "class")= chr "acf"
#> str(acf(df,lag=1))
#List of 6
#$ acf   : num [1:2, 1:3, 1:3] 1 -0.1274 0.004 -0.0582 0.1486 ...
#$ type  : chr "correlation"
#$ n.used: int 100
#$ lag   : num [1:2, 1:3, 1:3] 0 1 0 -1 0 -1 0 1 0 1 ...
#$ series: chr "df"
#$ snames: chr [1:3] "x" "y" "z"
#- attr(*, "class")= chr "acf"
#> str(acf(df,lag=1))$acf
#List of 6
#$ acf   : num [1:2, 1:3, 1:3] 1 -0.1274 0.004 -0.0582 0.1486 ...
#$ type  : chr "correlation"
#$ n.used: int 100
#$ lag   : num [1:2, 1:3, 1:3] 0 1 0 -1 0 -1 0 1 0 1 ...
#$ series: chr "df"
#$ snames: chr [1:3] "x" "y" "z"
#- attr(*, "class")= chr "acf"
#NULL
#> (acf(df,lag=1))$acf
#, , 1
#
#[,1]        [,2]      [,3]
#[1,]  1.0000000  0.00399543 0.1485907
#[2,] -0.1274151 -0.05816535 0.1315205
#
#, , 2
#
#[,1]       [,2]        [,3]
#[1,]  0.00399543 1.00000000 0.034079782
#[2,] -0.20346186 0.04199617 0.008673619
#
#, , 3
#
#[,1]        [,2]       [,3]
#[1,] 0.148590734  0.03407978  1.0000000
#[2,] 0.008819997 -0.04191461 -0.1013051
#
#> help(acf)
#starting httpd help server ... done
#> 




#'
#' Function which plots the correlation among observed and generated variables
#'
#' @param x oberserved variable
#' @param y generated variable
#' @param corx correlation of obervations. It must be a \code{NULL} object or a single correlation matrix or a list of correlation matrices.
#' @param use,method see \code{\link{cor}} 
#' @param lag lag for autocorrelation 
#' @param return.just.data.frame logical value. If \code{TRUE} functions returns no plot but just the date frame between observed and modeled autocrosscorrelations. Default is \code{FALSE}. 
#' @param cor.null.value null value for correlation corresponc test. See \code{\link{cocor.indep.groups}} or \url{http://comparingcorrelations.org/}.
#' @param useGPCA integer value. If it is greater than 0, GPCA interations are made in preprocessing. See \link{\code{GPCA}}. Default is 0, no GPCA are preprocessed.
#' @param signif singificance used to plot confindence interval. Default is 0.05 . 
#' @param ... further arguments for eastetics. See \code{\link{aes}} 
#' 
#' 
#' 


corplotlag <- function(x=Tn_mes,y=Tn_gen,corx=NULL,
		xlab="observed",ylab="generated",title="Spatial Correlation",season=FALSE,origin="1960-01-01",return.just.data.frame=FALSE,lag=1,cor.null.value=0,useGPCA=0,signif=0.05,...) {
	
	if (is.data.frame(y)) {
		
		y <- list(y=y)
		names(y) <- ylab[1]
	}
	
	if (season) {
		
		if (is.null(corx)) x <- addseason(x,origin=origin,nodata=TRUE)
		y <- lapply(X=y,FUN=addseason,origin=origin,nodata=TRUE)
		
		
		
		factor <- "season"
		
		factors <- unique(as.character(x[,factor]))
		nobs <- tapply(X=x[,1],INDEX=as.character(x[,factor]),FUN=length)
		
	} else {
		
		factor <- "season"
		nobs <- nrow(x)
		
		factors <- 1
		
##		facors <- factor
		
		
		
	}
	
	if ((useGPCA>0) & season) {
		
		if (is.null(corx))  {
			
			for (it in factors) {
				
				c <- which(as.character(x[,factor])==it)
				
				xg <- GPCA(x_prev=x[c,names(x)!=factor],n=useGPCA,extremes=TRUE)
				x[c,names(x)!=factor] <- xg$final_results[,]
			}
			
		}
		
		y <- lapply(X=y,FUN=function(x,useGPCA,it,factors,factor) {
					
					
					for (it in factors) {
						
						c <- which(as.character(x[,factor])==it)
						
						xg <- GPCA(x_prev=x[c,names(x)!=factor],n=useGPCA,extremes=TRUE)
						x[c,names(x)!=factor] <- xg$final_results[,]
					}
					return(x)
					
					
					
				},useGPCA=useGPCA,it=it,factors=factors,factor=factor)
		
		
		
		
		
	} 
	#### PUT GPCA ON X AND Y !!!!  
	####
	
#	factors <- unique(as.character(x[,factor]))
	
	
#	nobs <- tapply(X=x[,1],INDEX=as.character(x[,factor]),FUN=length)
	
	
	
	
	
	if (is.null(corx)) {
		corx <- fun_factor(data=x,factor=factor,fun=corlag,lag=lag,...)
		
	} else if (is.matrix(corx) | is.list(corx)) {
		
		if (is.matrix(corx) | is.data.frame(corx)) {
			corx <- list(corx=corx)
			names(corx) <- factor
		}
		## corx must be a NULL object or a single correlation matrix or a list of correlation matrices 
		out <- lapply(X=corx,FUN=as.vector)
		
		cnt <- unlist(lapply(X=out,FUN=length))
		names(cnt) <- names(out)
		
		names <- rep(names(out),times=cnt)
		
		out <- unlist(out)
		names(out) <- names
		
		n_out <- data.frame(nfactor=names(out),value=out)
		names(n_out) <- c(factor,"value")
		
		corx <- n_out
		
		
	}
	str(corx)
	cory <- lapply(X=y,FUN=fun_factor,factor=factor,fun=corlag,lag=lag,...)
 	str(cory)
	str(corx)
	for (i in names(cory)) {
		
		cory[[i]][,xlab] <- corx$value
	
	}
	
	str(cory)
	cory <- melt(cory,id=c(xlab,factor))
	cory <- cory[,names(cory)!="variable"]
	

	names(cory)[names(cory)=="L1"] <- "level"

#	str(cory)

	names(cory)[names(cory)=="value"] <- "generated"
	names(cory)[names(cory)==xlab] <- "observed"

#	str(cory)
#	str(names(cory))
	
	df <- cory
	df <- df[!is.na(df$observed),]
	
	df$value_max <- df$observed
	df$value_min <- df$observed
	
	### INSERT HERE 
	for (level in unique(as.character(df$level))) {
		
		print(level)
		condlevel  <- as.character(df$level)==level 
		
		for (ifactor in factors) {
	
			no <- nobs[ifactor]
			print(ifactor)
			if (season)  {
				no <- nobs[ifactor]
				print(ifactor)
				condfactor  <- as.character(df[,factor])==ifactor
			} else {
				
				condfactor  <- TRUE
			} 
			
			cond <- condlevel & condfactor
			cond <- which(cond)
			print(cond)
			
			corint <- cor.confidence.interval(r1=df$observed[cond],r2=NA,n=no,signif=signif) 
			
			df$value_max[cond] <- corint$rmax
			df$value_min[cond] <- corint$rmin
			
			
		}
	}
	
	
	df <- df[df$observed!=1 | df$generated!=1,]
	if (return.just.data.frame) return(df)
	asp <- length(unique(df$season))/length(unique(df$level))
############	aes <- aes(x=observed,y=generated,shape=level,group=season,col=level,...)
	out <- qplot(observed,generated, data = df, geom = "point", group = level,method=lm,asp=1) +
			facet_grid(season ~ level, scale = "fixed")+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 

	if (cor.null.value>=0) out <- out+geom_ribbon(mapping=aes(x=observed,ymax=value_max,ymin=value_min),data=df,alpha=0.4)    ### cor.null.value
####  


	
	#	out <- ggplot()+geom_point(mapping=aes,data=df)+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 
#	if (season) out <- out+facet_grid(season~season,scale="fixed")
	return(out)

}
NULL 
#'
#' Function which plots the correlation among observed and generated variables for more lags
#' 
#' 
#' @seealso \code{\link{corplotlag}}
corplotlags <- function(x=Tn_mes,y=Tn_gen,lag=c(0,1,4),return.just.data.frame=FALSE,
	xlab="observed",ylab="generated",title="Spatial Correlation",...) {
	
	df <- corplotlag(x=x,y=y,lag=lag[1],return.just.data.frame=TRUE,season=FALSE,...)
	df$lag <- lag[1]
	
	for (i in 2:length(lag)) {
		
		temp <- corplotlag(x=x,y=y,lag=lag[i],return.just.data.frame=TRUE,season=FALSE,...)
		temp$lag <- lag[i]
		
		df <- rbind(df,temp)
		

		
	}
	df$lag <- paste(df$lag,"-day lag",sep="")
	out <- qplot(observed,generated, data = df, geom = "point", group = level,method=lm,asp=1) +
			facet_grid(lag ~ level, scale = "fixed")+xlab(xlab)+ylab(ylab)+ggtitle(title)+geom_abline() 
	str(df)
	out <- out+geom_ribbon(mapping=aes(x=observed,ymax=value_max,ymin=value_min),data=df,alpha=0.4)    ### cor.null.value
	
	return(out)
}






#### UNCOUPLED GENERATION
#
## http://stackoverflow.com/questions/11800212/write-plots-to-png-file ### LOOK AT HERE ... 
#wpath <-  '/Users/ecor/Dropbox/iasma/RMAWGENdev/RMAWGEN/inst/doc/private/elsevier/article/Rscript_3/RMAWGENCodeCorner/output_cassiopea/temperature_generator_multi_station'
#rdaname <-  'results_uncoupled_temperature_generator_plural_station.rda'
#
#rdaname <- paste(wpath,rdaname,sep='/')
#
#
#load(rdaname)
#
#winter <- c("Dec","Jan","Feb")
#spring <- c("Mar","Apr","May")
#summer <- c("Jun","Jul","Aug")
#autumn <- c("Sep","Oct","Nov")
#
#
#
#
#
#year_min <- results$Tx_mes
#year_min <- results$Tx_mes
#
#
#Tx_mes <- results$Tx_mes-results$Tn_spline
#Tn_mes <- results$Tn_mes-results$Tx_spline
#
#
#Tx_gen <- lapply(X=results$Tx_gen,FUN=function(x,y){x-y},y=results$Tn_spline)
#Tn_gen <- lapply(X=results$Tn_gen,FUN=function(x,y){x-y},y=results$Tx_spline)
#
#method <- "kendall"
#
### plot Temparature Correletion 
#pdf <- paste(wpath,'spatial_correlation_tmin_uncoupled.pdf',sep='/')
#
#
#corplot_tn <- corplot(x=Tn_mes,y=Tn_gen,method=method,season=TRUE,title="Spatial Correlation: daily minimum temperature anom.")
#
#pdf(pdf)
#print(corplot_tn)
#dev.off()
#
#pdf <- paste(wpath,'spatial_correlation_tmax_uncoupled.pdf',sep='/')
#
#
#corplot_tx <- corplot(x=Tx_mes,y=Tx_gen,method=method,season=TRUE,title="Spatial Correlation: daily maximum temperature anom.")
#
#pdf(pdf)
#print(corplot_tx)
#dev.off()
#
#
#pdf <- paste(wpath,'spatial_correlation_tmin_uncoupled_unseas.pdf',sep='/')
#
#
#corplot_tn_unseas <- corplot(x=Tn_mes,y=Tn_gen,method=method,season=FALSE,title="Spatial Correlation: daily minimum temperature anom.")
#
#pdf(pdf)
#print(corplot_tn_unseas)
#dev.off()
#
#pdf <- paste(wpath,'spatial_correlation_tmax_uncoupled_unseas.pdf',sep='/')
#
#
#corplot_tx_unseas <- corplot(x=Tx_mes,y=Tx_gen,method=method,season=FALSE,title="Spatial Correlation: daily maximum temperature anom.")
#
#pdf(pdf)
#print(corplot_tx_unseas)
#dev.off()
#
### http://stackoverflow.com/questions/11800212/write-plots-to-png-file

