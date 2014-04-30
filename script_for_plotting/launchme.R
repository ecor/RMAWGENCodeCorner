
###############################################################################
# file launchme.R
#
# This file launches all the plotting script on RMAWGENCodeCorner subfolder 
#
# author: Emanuele Cordano on 2014-04-30
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
library(stringr)
rm(list=ls())
wpath <- '/Users/ecor/R-packages/RMAWGENCodeCorner' ### set wpath to RMAWGENCodeCoredern directiory


wpathf <- paste(wpath,"script_for_plotting",sep="/")
print(wpathf)
files <- list.files(wpathf,full.names=TRUE,pattern=".R")

thisFile <- "launchme.R"
files <- files[which(!str_detect(files,thisFile))]

#### for (it in files) source(it)
