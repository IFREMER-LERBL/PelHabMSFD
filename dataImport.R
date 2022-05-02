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

##############################
###   Import/Export data   ###
##############################

#' dataImport reads a csv file containing the data to process
#' @title Data importation
#' @description Read a csv file containing the data to process.
#' @param file character vector specifying the path and the name of the csv file.
#' @return df data.frame containing the data to process.
#' 
#' @examples 
#' dat <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' colnames(dat) <- c("x","y")
#' tf <- tempfile()
#' write.table(dat, tf, sep=";", dec=",")
#'
#' df <- dataImport(tf)
#'   
#' @export 
#' 
dataImport <- function(file) {
  df <- read.table(file, quote="", fill=TRUE, sep=";", dec=",", header=TRUE, )
  df
}

#' saveResults saves obtained results in a csv file
#' @title Results saving
#' @description Save obtained results in a csv file.
#' @param dat data.frame containing the results to save.
#' @param filename character vector specifying the path and the name of the csv file.
#' @param sep character specifying the field separator in the csv file.
#' @param col.names boolean: if TRUE (default), the names of columns are written.
#' @param row.names boolean: if TRUE (default), the names of rows are written.
#' 
#' @examples 
#' res <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 2, sd = 0.3), ncol = 2), 
#'              matrix(rnorm(100, mean = 4, sd = 0.3), ncol = 2))
#' colnames(res) <- c("x","y")
#' 
#' tf <- tempfile()
#' saveResults(res, tf)
#'
#' @export 
#' 
saveResults <- function(dat, filename, sep=";", col.names=TRUE, row.names=TRUE) {
  write.table(x=dat, file=filename, sep=sep, col.names=col.names, row.names=row.names)
}

## Display sampling information
#' indexSampling plots index sampling
#' @title Index sampling
#' @description Plot index sampling.
#' @param res data.frame containing metadata and indices values.
#' @param station character vector specifying the name of station to consider.
#' @param dir character vector specifying the name of directory in which the figures are saved.
#' 
#' @export
#'
indexSampling <- function(res, station=1, mainTitle="", dir) {
  if (!("year" %in% names(res))) {
    res$month <- as.numeric(format(res$date, "%m"))
    res$year <- as.numeric(format(res$date, "%Y"))
  }
  
  months <- 1:12
  years <- min(res$year):max(res$year)
  
  if (station == "all") 
    station <- unique(res$station)
  
  cpt <- 1
  for (s in station) {
    dat <- res[which(res$station == s), ]
    mat <- matrix(NA, nrow=length(months), ncol=length(years))
    colnames(mat) <- years
    rownames(mat) <- months
    
    for (j in 1:dim(dat)[1])
      mat[which(rownames(mat)==dat$month[j]), which(colnames(mat)==dat$year[j])] <- 255
    jpeg(file.path(dir, paste(s, ".jpg", sep="")))
    image(years, months, t(mat), col=c("black","white"), 
          main=paste(mainTitle, "-", s), xlab="Year", ylab="Month", axes=FALSE)
    axis(1, at=years, lab=years, las=2)
    axis(2, las=1)
    box()
    dev.off()
    
    progress(cpt, length(station), progress.bar=TRUE)
    cpt <- cpt+1
  }
  message("Done!!!")
}

############################################
###   PH2 data creation/transformation   ###
############################################

#' ph2DataCleaning cleans data to compute PH2 indicator, according to minimal number of months in each year and consistency condition
#' @title PH2 data cleaning
#' @description Clean data to compute PH2 indicator, according to minimal number of months in each year and consistency condition.
#' @param dat data.frame containing the data to process.
#' @return datClean data.frame containing the cleaned data to process.
#' 
#' @export 
#' 
ph2DataCleaning <- function(dat, check.consistency="0", check.nbMonths="1", 
                            minMonths=6, check.nbYears="1", minYears=5, 
                            nLowerRef, nUpperRef) {
  dat$month <- format(dat$date, "%m")
  dat$year <- format(dat$date, "%Y")
  stations <- unique(dat$station)
  
  datClean <- NULL
  for (s in stations) {
    datTemp <- dat[which(dat$station==s), ]
    datTemp <- datTemp[order(datTemp$station, datTemp$year), ]
    
    # Remove years with less than minMonths
    if (check.nbMonths == "1") {
      if (dim(datTemp)[1] > 0) {
        yearsToRemove <- names(table(datTemp$year)[which(table(datTemp$year)<minMonths)])
        if (length(yearsToRemove) > 0)
          datTemp <- datTemp[-which(datTemp$year %in% yearsToRemove), ]
      }
    }
    
    # Check if consistent time serie (if not, remove years before)
    if (check.consistency == "1") {
      if (dim(datTemp)[1] > 0) {
        diffYear <- which(diff(as.numeric(unique(datTemp$year))) > 1)
        if (length(diffYear) > 0) {
          yearsToRemove <- unique(datTemp$year)[1:max(diffYear)]
          datTemp <- datTemp[-which(datTemp$year %in% yearsToRemove), ]
        }
      }
    }
    
    # Minimal number of reference years
    if (check.nbYears == "1") {
      if (dim(datTemp)[1] > 0) {
        datRef <- datTemp[which(datTemp$year >= nLowerRef), ]
        datRef <- datRef[which(datRef$year <= nUpperRef), ]
        yearsRef <- unique(datRef$year)
        if (length(yearsRef) < minYears)
          datTemp <- datTemp[-which(datTemp$station == s), ]
      }
    }
    
    # Check the beginning month 
    if (dim(datTemp)[1] > 0) {
      startMonth <- as.numeric(datTemp$month[1])
      if (!(startMonth == 1)) {
        for (i in 1:(startMonth-1)) {
          datTemp <- rbind(data.frame(station=s, values=NA, 
                                  # date=as.Date(paste("15/0", as.character(i), "/", datTemp$year[1], sep=""), 
                                  #              "%d/%m/%Y"),
                                  date=as.Date(paste(datTemp$year[1], "-0", as.character(i), "-", "15", sep=""), 
                                               "%Y-%m-%d"),
                                  month=paste("0", as.character(i), sep=""), 
                                  year=datTemp$year[1]), datTemp)
        }
      }
    }
    datTemp <- datTemp[order(datTemp$station, datTemp$date), ]
    datClean <- rbind(datClean, datTemp)
  }
  datClean <- datClean[, -which(names(datClean) %in% c("month","year"))]
  datClean
}

#' ph2DataSelection selects the years for reference period and evaluation period
#' @title Data selection
#' @description Select the years for reference period and evaluation period.
#' @param dat data.frame containing the data to process.
#' @param nLowerRef numeric number specifying the lower year for reference period. 
#' @param nUpperRef numeric number specifying the upper year for reference period. 
#' @param nLower numeric number specifying the lower year for evaluation period. 
#' @param nUpper numeric number specifying the upper year for evaluation period. 
#' @return dat data.frame containing the selected data to process.
#' 
#' @export
#' 
ph2DataSelection <- function(dat, nLowerRef, nUpperRef, nLower, nUpper) {
  dat$year <- format(dat$date, "%Y")
  stations <- unique(dat$station)
  
  for (s in stations) {
    idx <- which(dat$station==s)
    idxLowerRef <- idx[which(dat$year[idx]<nLowerRef)]
    idxUpperRef <- idx[which((dat$year[idx]>nUpperRef) & (dat$year[idx]<nLower))]
    idxUpper <- idx[which(dat$year[idx]>nUpper)]
    idx <- c(idxLowerRef, idxUpperRef, idxUpper)
    # Remove years not selected
    if (length(idx) > 0)
      dat <- dat[-idx,]
  }
  dat <- dat[, -which(names(dat) == "year")]
  dat
}

#' ph2MonthlyPool computes a monthly pool of data to compute PH2 indicator
#' @title PH2 monthly pool computation
#' @description Compute a monthly pool of data to compute PH2 indicator.
#' @param dat data.frame containing the data to process.
#' @param func function to compute monthly pool. Must be 'mean' (default) or 'sum'.
#' @return datMonthly data.frame containing the monthly pool of data.
#' 
#' @export 
#' 
ph2MonthlyPool <- function(dat, func=mean) {
  #dat$date <- as.Date(dat$date, format="%d/%m/%Y")
  dat$date <- as.Date(dat$date, format="%Y-%m-%d")
  dat$month <- format(dat$date, "%m")
  dat$year <- format(dat$date, "%Y")
  datMonthly <- aggregate(values ~ station+month+year, dat, func)
  # datMonthly$date <- as.Date(paste("15/", datMonthly$month, "/", datMonthly$year, sep=""), 
  #                            "%d/%m/%Y")
  datMonthly$date <- as.Date(paste(datMonthly$year, "-", datMonthly$month, "-", "15", sep=""), 
               "%Y-%m-%d")
  datMonthly <- datMonthly[, -which(names(datMonthly) %in% c("month","year"))]
  datMonthly <- datMonthly[order(datMonthly$station), ]
  datMonthly
}

############################################
###   PH3 data creation/transformation   ###
############################################

#' ph3DataCleaning cleans data to compute PH3 indicator, according to minimal number of months in each year and minimal number of years
#' @title PH3 data cleaning
#' @description Cleans data to compute PH3 indicator, according to minimal number of months in each year and minimal number of years.
#' @param dat data.frame containing the data to process.
#' @return stationsClean vector of character specifying the names of the stations to keep.
#' 
#' @export 
#' 
ph3DataCleaning <- function(dat, check.nbMonths="1", minMonths=6, 
                            check.nbYears="1", minYears=5) {
  stations <- unique(dat$station)
  stationsClean <- stations
  
  for (s in stations) {
    datTemp <- dat[which(dat$station == s), ]
    
    if (check.nbMonths == "1") {
      yearsToRemove <- names(table(datTemp$year)[which(table(datTemp$year)<minMonths)])
      if (length(yearsToRemove) > 0)
        datTemp <- datTemp[-which(datTemp$year %in% yearsToRemove), ]
    }
    
    if (check.nbYears == "1") {
      if (length(unique(datTemp$year)) < minYears)
        stationsClean <- stationsClean[-which(stationsClean == s)]
    }
  }
  stationsClean
}

#' ph3DataTransform computes data transformation (all taxa in columns)
#' @title PH3 data transformation
#' @description Compute data transformation (all taxa in columns).
#' @param dat data.frame containing the data to process.
#' @param origin character vector specifying the origin of data file ('rephy' by default).
#' @return dat2 data.frame containing the transformed data to process.
#' 
#' @export 
#' 
ph3DataTransform <- function(dat, origin="rephy") {
  # Extract all taxa
  taxa <- sort(unique(dat$taxon))
  # Extract all stations/dates
  dates <- unique(dat[c("station","day","month","year","longitude","latitude")])
  
  # Data transformation (RESOMAR format)
  dat2 <- matrix(0, dim(dates)[1], length(taxa))
  colnames(dat2) <- as.character(taxa)
  for (i in 1:dim(dates)[1]) {
    datTemp <- dat[(dat$station==dates[i,"station"]) & (dat$day==dates[i,"day"]) 
                   & (dat$month==dates[i,"month"]) & (dat$year==dates[i,"year"])
                   & (dat$longitude==dates[i,"longitude"]) & (dat$latitude==dates[i,"latitude"]), ]
    datTemp <- aggregate(abundance ~ station+day+month+year+longitude+latitude+taxon, 
                         data=datTemp, FUN=sum)
    dat2[i, as.character(datTemp$taxon)] <- as.numeric(as.character(datTemp$abundance))
  }
  dat2 <- data.frame(dat2, check.names=FALSE, stringsAsFactors=FALSE)
  cbind(dates, dat2)
}

#' ph3MonthlyPool computes a monthly pool of data to compute PH3 indicator
#' @title PH3 monthly pool computation
#' @description Compute a monthly pool of data to compute PH3 indicator.
#' @param dat data.frame containing the data to process.
#' @param taxa vector of character specifying the names of all taxa.
#' @param func function to compute monthly pool. Must be 'mean' (default) or 'sum'.
#' @return datMonthly data.frame containing the monthly pool of data.
#' 
#' @export 
#' 
ph3MonthlyPool <- function(dat, taxa, func=mean) {
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  datMonthly <- aggregate(x, list(station=dat$station, month=dat$month, 
                                  year=dat$year), func)
  datMonthly <- datMonthly[order(datMonthly$station), ]
  datMonthly
}

#' ph3RandomPool computes a random pool of data to compute PH3 indicator
#' @title PH3 random pool computation
#' @description Compute a random pool of data to compute PH3 indicator.
#' @param dat data.frame containing the data to process.
#' @param taxa vector of character specifying the names of all taxa.
#' @return datMonthly data.frame containing the random pool of data.
#' 
#' @export 
#' 
ph3RandomPool <- function(dat, taxa) {
  datMonthly <- dat[!duplicated(dat[c("station","month","year")]), ]
  datMonthly <- datMonthly[, which(names(datMonthly) %in% 
                                     c("station","month","year", taxa))]  # Select the taxa columns
  datMonthly <- datMonthly[order(datMonthly$station), ]
  datMonthly
}

#' ph3YearlyPool computes a yearly pool of data to compute PH3 indicator
#' @title PH3 yearly pool computation
#' @description Compute a yearly pool of data to compute PH3 indicator.
#' @param dat data.frame containing the data to process.
#' @param taxa vector of character specifying the names of all taxa.
#' @param func function to compute yearly pool. Must be 'mean' (default) or 'sum'.
#' @return datYearly data.frame containing the yearly pool of data.
#' 
#' @export 
#' 
ph3YearlyPool <- function(dat, taxa, func=mean) {
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  datYearly <- aggregate(x, list(station=dat$station, year=dat$year), func)
  datYearly <- datYearly[order(datYearly$station), ]
  datYearly
}

#########################################################
###   Physico-Chemical data creation/transformation   ###
#########################################################

#' pcMonthlyPool computes a monthly pool of data to process physico-chemical data
#' @title Monthly pool computation for physico-chemical data
#' @description Compute a monthly pool of data to process physico-chemical data.
#' @param dat data.frame containing the data to process.
#' @param func function to compute monthly pool. Must be 'mean' (default) or 'sum'.
#' @return datMonthly data.frame containing the monthly pool of data.
#' 
#' @export 
#' 
pcMonthlyPool <- function(dat, func=mean) {
  dat$date <- as.Date(dat$date, format="%Y-%m-%d")
  dat$month <- format(dat$date, "%m")
  dat$year <- format(dat$date, "%Y")
  datMonthly <- aggregate(values ~ station+month+year+param, dat, func)
  # datMonthly$date <- as.Date(paste("15/", datMonthly$month, "/", datMonthly$year, sep=""), 
  #                            "%d/%m/%Y")
  datMonthly$date <- as.Date(paste(datMonthly$year, "-", datMonthly$month, "-15", sep=""), 
                             "%Y-%m-%d")
  datMonthly <- datMonthly[, -which(names(datMonthly) %in% c("month","year"))]
  datMonthly <- datMonthly[order(datMonthly$station), ]
  datMonthly
}