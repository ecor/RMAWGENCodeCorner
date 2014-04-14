#
# Author Emanuele Cordano
#
# file generate_all.R
# 
# 
#
#
# author: Emanuele Cordano on 12-04-2012
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


wpath_script <- paste(getwd(),"script",sep="/")

source('script/temperature_generator_multi_station_P6.R', chdir = TRUE)
source('script/precipitation_generator_RMAWGEN.R', chdir = TRUE)
source('script/temperature_generator_multi_station_P6_coupled.R', chdir = TRUE)