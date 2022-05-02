# MSFD: indicators computation.
#
# Copyright 2018 Guillaume Wacquet, Duflos Marie, Mialet Benoit, Rombouts Isabelle
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' meanPC computes the mean per month and per year for each physico-chemical parameter
#' @title Mean of physico-chemical parameters
#' @description Compute the mean per month and per year for each physico-chemical parameter.
#' @param dat data.frame containing the data to process.
#' @param mode character vector specifying the mode for mean computation. Must be 'month' (default) or 'year'.
#' @return dat data.frame containing the mean of each physico-chemical parameter.
#' 
#' @export
#' 
meanPC <- function(dat, mode="month") {
  dat$date <- as.Date(dat$date, format="%d/%m/%Y")
  if (mode == "month") {
    dat$month <- format(dat$date, "%m")
    dat <- aggregate(values ~ month+param+station, dat, mean)
    dat <- dat[c("station","param","month","values")]
  }
  if (mode == "year") {
    dat$year <- format(dat$date, "%Y")
    dat <- aggregate(values ~ year+param+station, dat, mean)
    dat <- dat[c("station","param","year","values")]
  }
  dat
}

##################################
###   PC parameters plotting   ###
##################################

#' plotPC plots evolution of physico-chemical parameters
#' @title Evolution of physico-chemical parameters
#' @description Plot evolution of physico-chemical parameters.
#' @param dat data.frame containing the data to process.
#' @param station character vector specifying the station to consider. Can be 'all' to consider all stations. 
#' @param dir character vector specifying the path of the directory in which the figures are saved. 
#' 
#' @export
#' 
plotPC <- function(dat, station="all", dir) {
  if (station == "all") 
    station <- unique(dat$station)
  
  for (p in unique(dat$param)) {
    cat("\n", p, "plots computation...")
    dirp <- file.path(dir, p)
    dir.create(dirp)
    datTemp <- dat[which(dat$param == p), ]
    datTemp <- datTemp[order(datTemp$date), ]
    for (s in unique(datTemp$station)) {
      years <- as.numeric(format(datTemp$date[which(datTemp$station==s)], "%Y"))
      datTemp2 <- data.frame(date=seq(from = as.Date(paste(min(years), 
                                                           "-01-15", sep="")), 
            by="month", length.out=12*(max(years)-min(years)+1)), values=NA)
      datTemp2$values[which(datTemp2$date %in% datTemp$date[which(datTemp$station==s)])] <- datTemp$values[which(datTemp$station==s)]
      jpeg(file.path(dirp, paste(s, ".jpg", sep="")))
      plot(datTemp2, main=s, xlab="Year", ylab=p, las=2, type="l", cex=2, col="blue")
      box()
      dev.off()
    }
  }
}

#' pcBoxplot plots boxplots of physico-chemical parameters
#' @title Boxplots of physico-chemical parameters
#' @description Plot boxplots of physico-chemical parameters.
#' @param dat data.frame containing the data to process.
#' @param station character vector specifying the station to consider. Can be 'all' to consider all stations. 
#' @param mode character vector specifying the mode for boxplot computation. Must be 'month' or 'year' (default).
#' @param dir character vector specifying the path of the directory in which the figures are saved. 
#' 
#' @export
#' 
pcBoxplot <- function(dat, station="all", mode="year", dir) {
  print(paste("\nBoxplots computation (", mode, ")...", sep=""))
  if (station == "all") 
    station <- unique(dat$station)
  
  months <- 1:12
  years <- min(format(dat$date, "%Y")):max(format(dat$date, "%Y"))
  
  if (mode == "year") {
    xlab <- "Year"
    lasb <- 2
  } else if (mode == "month") {
    xlab <- "Month"
    lasb <- 1
  } else {
    xlab <- ""
    lasb <- 1
  }
  
  for (s in station) {
    print(s)
    datTemp <- dat[which(dat$station == s), ]
    datTemp$date <- as.Date(datTemp$date, format="%d/%m/%Y")
    if (mode == "month")
      datTemp$month <- format(datTemp$date, "%m")
    if (mode == "year")
      datTemp$year <- format(datTemp$date, "%Y")
    datTemp <- datTemp[order(datTemp[[mode]]), ]
    pc <- unique(datTemp$param)
    
    for (i in 1:length(pc)) {
      datTemp2 <- datTemp[which(datTemp$param==pc[i]), ]
      jpeg(file.path(dir, paste(s, " - ", pc[i], ".jpg", sep="")))
      with(datTemp2, boxplot(datTemp2[["values"]] ~ factor(as.numeric(datTemp2[[mode]]), 
                                                           levels=get(paste(mode, "s", sep=""))),
                        main=paste("Station", s, "-", pc[i]), 
                        xlab=xlab, ylab="Values", las=2, col="red", 
                        outline=FALSE, axes=FALSE))
      axis(1, at=1:length(get(paste(mode, "s", sep=""))), 
           lab=get(paste(mode, "s", sep="")), las=lasb)
      axis(2, las=1)
      dev.off()
    }
  }
}