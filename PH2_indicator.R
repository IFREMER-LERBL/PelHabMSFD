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

#####################################
###   PH2 indicator computation   ###
#####################################

#' anomalies computes anomalies in time series, according to the specification of a reference period and an evaluation period
#' @title Anomalies computation
#' @description Compute anomalies in time series, according to the specification of a reference period and an evaluation period.
#' @param dat data.frame containing the data to process.
#' @param nLower numeric number specifying the lower year for evaluation period. 
#' @param nUpper numeric number specifying the upper year for evaluation period. 
#' @param na.rm boolean: if TRUE (default), NA values are removed.
#' @param dir character vector specifying the path of the directory in which the figures are saved. 
#' @return The function returns a list containing:
#' \item{anomonthly}{data.frame containing the anomalies values.}
#' \item{rDatamonthly}{data.frame containing the values of time series.}
#' \item{repCyclemonthly}{data.frame containing the values of the cycle.}
#' \item{ResultFin}{data.frame containing the number of strong and extreme anomalies.}
#' \item{LCS}{numeric value specifying the uppest superior limit.}
#' \item{LCI}{numeric value specifying the lowest inferior limit.}
#' \item{LSS}{numeric value specifying the first superior limit.}
#' \item{LSI}{numeric value specifying the first inferior limit.}
#' \item{lim_sup}{numeric value specifying the superior quantile.}
#' \item{lim_inf}{numeric value specifying the inferior quantile.}
#' 
#' @export
#' 
anomalies <- function(dat, nLower=min(format(dat$date, "%Y")), 
                      nUpper=max(format(dat$date, "%Y")), na.rm=TRUE) {
  # Total number of months (without missing values)
  startDate <- min(dat$date)
  endDate <- max(dat$date)
  startYear <- as.numeric(format(startDate, "%Y"))
  endYear <- as.numeric(format(endDate, "%Y"))
  LmonthSerie <- (endYear-startYear+1)*12
  
  # Serie regularization (avec log)
  DATAOK <- log10(dat$values+1)
  t <- as.numeric(julian(dat$date)) # convert dates as text to R date format
  newdata <- regul(t, DATAOK, units="daystoyears", frequency=12,
                   methods="area", tol=3, n=LmonthSerie, rule=2) 
  rData <- tseries(newdata)
  rDataRef <- rData
  if (nLower!=startYear)
    rDataRef <- rData[c(1:(12*(nLower-startYear)))]
  
  # Start of the decomposition
  # Decompose time series using seasonal difference method
  # Compute mean annual cycle
  ab <- as.matrix(rData)
  abRef <- as.matrix(rDataRef)
  mmean <- rep(0,12) # monthly mean array
  for (m in seq(1,12)) {
    aa <- seq(m, nrow(abRef), 12)
    mmean[m] <- mean(abRef[aa,], na.rm=na.rm)
  }
  # Remove seasonal cycle
  nt <- dim(abRef)[1]
  nyear <- floor(nt/12)
  repCycle <- rep(mmean, nyear) # repeat mean cycle
  repCycle <- repCycle[1:nt]
  if (nLower != startYear)
    repCycle <- c(repCycle, rep(repCycle[1:12], endYear-as.numeric(nLower)+1))
  ano <- ab-repCycle # monthly anomalies

  # save mensual anomalies
  anodate <- seq(as.Date(startDate), by="month", length.out=LmonthSerie)
  anomonthly <- cbind.data.frame(anodate, ano)
  rDatamonthly <- cbind.data.frame(anodate, rData)
  repCyclemonthly <- cbind.data.frame(anodate, repCycle)
 
  # To calculate the final part of the indicator (% of anomalies)
  # Create the categorization based on quantiles
  # and for creating the limit for creating the final graph
  # we fix the limits based on quantiles
  LCS <- quantile(anomonthly[,2], c(0.975), na.rm=na.rm) # uppest superior limit
  LCI <- quantile(anomonthly[,2], c(0.025), na.rm=na.rm) # lowest inferior limit
  LSS <- quantile(anomonthly[,2], c(0.75), na.rm=na.rm) # 1st superior limit
  LSI <- quantile(anomonthly[,2], c(0.25), na.rm=na.rm) # 1st inferior limit
  lim_sup <- quantile(anomonthly[,2], c(1), na.rm=na.rm)
  lim_inf <- quantile(anomonthly[,2], c(0), na.rm=na.rm)
  
  anomonthly$change <- "small"
  anomonthly$change[which(is.na(anomonthly$ano))] <- NA
  anomonthly$change[which(anomonthly$ano >= LSS)] <- "important +"
  anomonthly$change[which(anomonthly$ano <= LSI)] <- "important -"
  anomonthly$change[which(anomonthly$ano >= LCS)] <- "extreme +"
  anomonthly$change[which(anomonthly$ano <= LCI)] <- "extreme -"
  
  # Calculation of final results concerning the evaluation period
  temps <- anomonthly$anodate # you have to adapt this line with the years of your time-serie
  evalPH2 <- anomonthly # subset of the results (anomalies) for the evaluation period
  anomEval <- evalPH2$ano # select of the anomalies values for the evaluation period
  TotAnomValuesEval <- length(which(!is.na(evalPH2$ano))) # Total number of values present in the evaluation period
  NbBothPosi <- length(which(evalPH2$ano >= as.numeric(LSS))) # Number of strong positive anomalies (in important and extreme changes) during the evaluation period
  NbBothNega <- length(which(evalPH2$ano <= as.numeric(LSI))) # Number of strong negative anomalies (in important and extreme changes) during the evaluation period
  NbToTBothAnom <- NbBothPosi+NbBothNega # Number of strong positive anomalies (in important and extreme changes) during the evaluation period
  PerBoth <- (NbToTBothAnom/TotAnomValuesEval)*100 # percentage of anomalies withing the "Strong ones" (important and extreme) during the evaluation period
  NbExtremPosi <- length(which(evalPH2$ano >= as.numeric(LCS)))
  NbExtremNega <- length(which(evalPH2$ano <= as.numeric(LCI)))
  NbToTExtremAnom <- NbExtremPosi+NbExtremNega
  PerExtrem <- (NbToTExtremAnom/TotAnomValuesEval)*100
  NbStrongPosi <- length(which((evalPH2$ano >= as.numeric(LSS)) & 
                                 (evalPH2$ano < as.numeric(LCS))))
  NbStrongNega <- length(which((evalPH2$ano <= as.numeric(LSI)) & 
                                 (evalPH2$ano > as.numeric(LCI))))
  NbToTStrongAnom <- NbStrongPosi+NbStrongNega
  PerStrong <- (NbToTStrongAnom/TotAnomValuesEval)*100
  
  ResultFin <- c(TotAnomValuesEval, NbToTBothAnom, NbBothPosi, NbBothNega, 
                 PerBoth, NbToTExtremAnom, NbExtremPosi, NbExtremNega, PerExtrem, 
                 NbToTStrongAnom, NbStrongPosi, NbStrongNega, PerStrong)
  names(ResultFin) <- c("Tot Nb","Tot Nb Anom","Nb Anom +", "Nb Anom -", 
                        "Percentage Anom", "Tot Nb Extremes Anom",
                        "Nb Extremes Anom +", "Nb Extremes Anom -", 
                        "Percentage Extremes Anom", "Tot Nb High Anom",
                        "Nb High Anom +", "Nb High Anom -", 
                        "Percentage High Anom")
  
  list(anomonthly=anomonthly, rDatamonthly=rDatamonthly, 
       repCyclemonthly=repCyclemonthly, ResultFin=ResultFin, LCS=LCS, LCI=LCI, 
       LSS=LSS, LSI=LSI, lim_sup=lim_sup, lim_inf=lim_inf)
}

#' trendDetection tests if the series has an increasing or decreasing trend
#' @title Trend detection
#' @description Test if the series has an increasing or decreasing trend.
#' @param dat data.frame containing the data to process.
#' @param nLower numeric number specifying the lower year for evaluation period. 
#' @param nUpper numeric number specifying the upper year for evaluation period. 
#' @param station character vector specifying the name of station to consider. 
#' @return trend numeric value specifying the trend estimation.
#' 
#' @export
#' 
trendDetection <- function(dat, date, nLower=min(format(date, "%Y")), 
                           nUpper=max(format(date, "%Y")), station=NULL) {
  dat <- dat[which((format(date, "%Y")>=nLower) & (format(date, "%Y")<=nUpper))]
  if (has_warning(trend <- trend.test(dat, R=1))) {
    trend <- trend.test(dat, R=100)
    trend$estimate <- trend$t0
  }
  trend
}

#' testStudent compares means between anomalies in reference period and anomalies in evaluation period
#' @title Student test
#' @description Compare means between anomalies in reference period and anomalies in evaluation period.
#' @param dat data.frame containing the data to process.
#' @param nLower numeric number specifying the lower year for evaluation period. 
#' @return res numeric value specifying the value of Student test.
#' 
#' @export
#' 
testStudent <- function(dat, nLower) {
  dat1 <- dat$ano[which(format(dat$anodate, "%Y") < nLower)]
  dat2 <- dat$ano[which(format(dat$anodate, "%Y") >= nLower)]
  res <- t.test(dat1, dat2)
  res
}

#' varTest tests the homogeneity of variances
#' @title Homogeneity of variances
#' @description Test the homogeneity of variances.
#' @param dat data.frame containing the data to process.
#' @param nLower numeric number specifying the lower year for evaluation period. 
#' @return res numeric value.
#' 
#' @export
#' 
varTest <- function(dat, nLower) {
  dat1 <- dat$ano[which(format(dat$anodate, "%Y") < nLower)]
  dat2 <- dat$ano[which(format(dat$anodate, "%Y") >= nLower)]
  res <- var.test(dat1, dat2)
  res
}

#' SDevolution computes the percentage of increase/decrease of anomalies SD between the reference period and the evaluation period
#' @title Percentage of increase/decrease of SD
#' @description Compute the percentage of increase/decrease of anomalies SD.
#' @param dat data.frame containing the data to process.
#' @param nLower numeric number specifying the lower year for evaluation period. 
#' @return res numeric value.
#' 
#' @export
#' 
SDevolution <- function(dat, nLower) {
  dat1 <- dat$ano[which(format(dat$anodate, "%Y") < nLower)]
  dat2 <- dat$ano[which(format(dat$anodate, "%Y") >= nLower)]
  res <- -(1-(sd(dat2))/(sd(dat1)))*100
  res
}

##################################
###   PH2 indicator plotting   ###
##################################

#' plotAnomalies plots anomalies obtained by 'anomalies' function
#' @title Anomalies plotting
#' @description Plot anomalies obtained by 'anomalies' function.
#' @param anomonthly data.frame containing the anomalies.
#' @param LCS numeric value specifying the uppest superior limit.
#' @param LCI numeric value specifying the lowest inferior limit.
#' @param LSS numeric value specifying the first superior limit.
#' @param LSI numeric value specifying the first inferior limit.
#' @param lim_sup numeric value specifying the superior quantile.
#' @param lim_inf numeric value specifying the inferior quantile. 
#' @param station character vector specifying the name of station to consider. 
#' 
#' @export
#' 
plotAnomalies <- function(anomonthly, LCS, LCI, LSS, LSI, lim_sup, lim_inf, 
                          station=NULL, mainTitle="") {
  temps <- anomonthly$anodate # you have to adapt this line with the years of your time-serie
  MAXtime <- as.numeric(max(temps))
  MINtime <- as.numeric(min(temps))
  
  # Make the final result graph
  plot(anomonthly$anodate, anomonthly$ano, xlab="Year", ylab="Anomalies", bty="n", las=2)
  # To trace polygons zones
  polygon(x=c(min(anomonthly$anodate)-20000, max(anomonthly$anodate)+20000, 
              max(anomonthly$anodate)+20000, min(anomonthly$anodate)-20000), 
          y=c(lim_inf, lim_inf, lim_sup, lim_sup), col="#6495ED", border=NA)
  polygon(x=c(min(anomonthly$anodate)-20000, max(anomonthly$anodate)+20000, 
              max(anomonthly$anodate)+20000, min(anomonthly$anodate)-20000),
          y=c(LCI, LCI, LCS, LCS), col="#87CEFA", border=NA)
  polygon(x=c(min(anomonthly$anodate)-20000, max(anomonthly$anodate)+20000, 
              max(anomonthly$anodate)+20000, min(anomonthly$anodate)-20000),
          y=c(LSI, LSI, LSS, LSS), col="#E0FFFF", border=NA)
  # Add the lines which limit the different zones
  abline(h=c(LSI, LSS), col="blue", lty=3)
  abline(h=c(LCI, LCS), col="blue", lty=2)
  text(MAXtime-100, LCI+0.004, "2.5nd percentiles", col="blue", cex=0.9, font=2) 
  text(MAXtime-100, LSI+0.004, "25th percentiles", col="blue", cex=0.9, font=2)
  text(MAXtime-100, LSS+0.004, "75th percentiles", col="blue", cex=0.9, font=2)
  text(MAXtime-100, LCS+0.004, "97.5th percentiles", col="blue", cex=0.9, font=2)
  text((MAXtime+MINtime)/2, LSI+((LSS+abs(LSI))/2), "", col="lightblue", cex=3, font=2)
  text((MAXtime+MINtime)/2, (LSS+((LCS-abs(LSS))/2)), "", col="#1E7FCB", cex=2.5, font=2)
  text((MAXtime+MINtime)/2, -(abs(LSI)+((abs(LCI)-abs(LSI))/2)), "", col="#1E7FCB", cex=2.5, font=2)
  text((MAXtime+MINtime)/2, (LCS+((lim_sup-LCS)/2)), "", col="#5472AE", cex=2.5, font=2)
  text((MAXtime+MINtime)/2, -(abs(LCI)+((abs(lim_inf)-abs(LCI))/2)), "", col="#5472AE", cex=2.5, font=2)
  points(anomonthly$anodate, anomonthly$ano, pch=4, cex=1.2, lwd=1) # it replaces the points above the polygons
  box()
  col <- c("#6495ED","#87CEFA","#E0FFFF")
  title(paste(mainTitle, "-", station))
}

#' plotTimeSeries plots time series
#' @title Time series plotting
#' @description Plot time series.
#' @param dat data.frame containing the data to process.
#' @param station character vector specifying the name of station to consider. 
#' @param na.rm boolean: if TRUE (default), NA values are removed. 
#' 
#' @export
#' 
plotTimeSeries <- function(dat, station=NULL, mainTitle="", na.rm=TRUE) {
  tse <- NULL
  startDate <- min(dat$date)
  endDate <- max(dat$date)
  startYear <- as.numeric(format(startDate, "%Y"))
  endYear <- as.numeric(format(endDate, "%Y"))
  LmonthSerie <- (endYear-startYear+1)*12
  tseDates <- seq(as.Date(startDate), by="month", length.out=LmonthSerie)
  tse <- data.frame(date=tseDates, values=NA)
  tse$values[which(tse$date %in% dat$date)] <- dat$values
  
  temps <- tse$date
  MAXtime <- as.numeric(max(temps))
  MINtime <- as.numeric(min(temps))
  LCS <- quantile(tse$values, c(0.975), na.rm=na.rm) # uppest superior limit
  LCI <- quantile(tse$values, c(0.025), na.rm=na.rm) # lowest inferior limit
  LSS <- quantile(tse$values, c(0.75), na.rm=na.rm) # 1st superior limit
  LSI <- quantile(tse$values, c(0.25), na.rm=na.rm) # 1st inferior limit
  lim_sup <- quantile(tse$values, c(1), na.rm=na.rm)
  lim_inf <- quantile(tse$values, c(0), na.rm=na.rm)
  
  tse$change <- "small"
  tse$change[which(is.na(tse$values))] <- NA
  tse$change[which(tse$values >= LSS)] <- "important +"
  tse$change[which(tse$values <= LSI)] <- "important -"
  tse$change[which(tse$values >= LCS)] <- "extreme +"
  tse$change[which(tse$values <= LCI)] <- "extreme -"
  
  # Make the final result graph
  plot(tse$date, tse$values, xlab="Year", ylab="Values", bty="n", las=2)
  # To trace polygons zones
  polygon(x=c(min(tse$date)-20000, max(tse$date)+20000, 
              max(tse$date)+20000, min(tse$date)-20000), 
          y=c(lim_inf, lim_inf, lim_sup, lim_sup), col="#6495ED", border=NA)
  polygon(x=c(min(tse$date)-20000, max(tse$date)+20000, 
              max(tse$date)+20000, min(tse$date)-20000),
          y=c(LCI, LCI, LCS, LCS), col="#87CEFA", border=NA)
  polygon(x=c(min(tse$date)-20000, max(tse$date)+20000, 
              max(tse$date)+20000, min(tse$date)-20000),
          y=c(LSI, LSI, LSS, LSS), col="#E0FFFF", border=NA)
  # Add the lines which limit the different zones
  abline(h=c(LSI, LSS), col="blue", lty=3)
  abline(h=c(LCI, LCS), col="blue", lty=2)
  text(MAXtime-100, LCI+0.004, "2.5nd percentiles", col="blue", cex=0.9, font=2) 
  text(MAXtime-100, LSI+0.004, "25th percentiles", col="blue", cex=0.9, font=2)
  text(MAXtime-100, LSS+0.004, "75th percentiles", col="blue", cex=0.9, font=2)
  text(MAXtime-100, LCS+0.004, "97.5th percentiles", col="blue", cex=0.9, font=2)
  text((MAXtime+MINtime)/2, LSI+((LSS+abs(LSI))/2), "", col="lightblue", cex=3, font=2)
  text((MAXtime+MINtime)/2, (LSS+((LCS-abs(LSS))/2)), "", col="#1E7FCB", cex=2.5, font=2)
  text((MAXtime+MINtime)/2, -(abs(LSI)+((abs(LCI)-abs(LSI))/2)), "", col="#1E7FCB", cex=2.5, font=2)
  text((MAXtime+MINtime)/2, (LCS+((lim_sup-LCS)/2)), "", col="#5472AE", cex=2.5, font=2)
  text((MAXtime+MINtime)/2, -(abs(LCI)+((abs(lim_inf)-abs(LCI))/2)), "", col="#5472AE", cex=2.5, font=2)
  points(tse$date, tse$values, pch=4, cex=1.2, lwd=1) # it replaces the points above the polygons
  box()
  col <- c("#6495ED","#87CEFA","#E0FFFF")
  title(paste(mainTitle, "-", station))
  tse
}

#' plotDecomposition plots decomposition of time series (cycle, anomalies)
#' @title Decomposition plotting
#' @description Plot decomposition of time series (cycle, anomalies).
#' @param dat data.frame containing the data to process.
#' @param station character vector specifying the name of station to consider. 
#' 
#' @export
#' 
plotDecomposition <- function(dat, station=NULL, mainTitle="") {
  par(mfrow=c(3,1), mar=c(0.4,5,0,2), oma=c(3,0,2,0), mgp=c(2,.6,0))
  plot(dat$rDatamonthly, type="l", col="blue", ylab="Time series (log10)",
       xaxt="n", cex.lab=1.2, cex.axis=1.2)
  title(paste(mainTitle, "-", station), outer=TRUE)
  plot(dat$repCyclemonthly, type="l", col="blue", ylab="Mean annual cycle", 
       xaxt="n", cex.lab=1.2, cex.axis=1.2)
  plot(dat$anomonthly[, c("anodate","ano")], type="l", col="blue", 
       ylab="Anomalies", xlab="Year", las=2, cex.lab=1.2, cex.axis=1.2)
}

#' plotAnomaliesBarplot plots a barplot of anomalies
#' @title Anomalies barplot
#' @description Plot a barplot of anomalies.
#' @param anomonthly data.frame containing the anomalies values.
#' @param rDatamonthly data.frame containing the values of time series.
#' @param nLower numeric number specifying the lower year for evaluation period. 
#' @param nUpper numeric number specifying the upper year for evaluation period. 
#' @param station character vector specifying the name of station to consider.
#' 
#' @export
#' 
plotAnomaliesBarplot <- function(anomonthly, rDatamonthly, 
                                 nLower=min(format(anomonthly$anodate, "%Y")),
                                 nUpper=max(format(anomonthly$anodate, "%Y")), 
                                 station=NULL, mainTitle="") {
  # Graph anomalies
  anomonthly$ano[which(is.na(anomonthly$ano))] <- 0
  bp <- barplot(anomonthly$ano, xlab="Year", ylab = "Anomalies",
                ylim=c(-max(c(-min(anomonthly$ano),max(anomonthly$ano))),
                       max(c(-min(anomonthly$ano),max(anomonthly$ano)))),
                col=ifelse(anomonthly$ano>0, "blue", "red"), border=NA, las=2)
  v <- bp[which(format(anomonthly$anodate, "%Y") == nLower)[1]]
  rect(v, -max(abs(min(anomonthly$ano)),abs(max(anomonthly$ano))), 
       bp[length(bp)]+.6, max(abs(min(anomonthly$ano)),abs(max(anomonthly$ano))), 
       col=rgb(1,1,0,alpha=.4), lty=0)
  par(new=TRUE)
  barplot(anomonthly$ano, xlab="Year", ylab="Anomalies",
          ylim=c(-max(c(-min(anomonthly$ano),max(anomonthly$ano))),
                 max(c(-min(anomonthly$ano),max(anomonthly$ano)))),
          col=ifelse(anomonthly$ano>0, "blue", "red"), border=NA, las=2)
  axis(1, at=bp[seq(1, length(anomonthly$anodate), 
                    by=length(anomonthly$anodate)/length(unique(format(anomonthly$anodate, "%Y"))))],
       labels=unique(format(anomonthly$anodate, "%Y")), las=2)  
  box()
  legend("bottom", c("Positive","Negative"), fill=c("blue","red"), horiz=TRUE, 
         cex=1, box.lty=0, bg="transparent")
  title(paste(mainTitle, "-", station))
  
  abline(v=v, col="black", lwd=2)
  text(x=v+.5, y=max(anomonthly$ano)-5*max(anomonthly$ano)/100, 
       "Eval", srt=-90, pos=4, cex=.8, col="dimgrey")
}

#' plotAllAnomaliesBarplot plots a barplot of anomalies on whole time series
#' @title All anomalies barplot
#' @description Plot a barplot of anomalies.
#' @param anomonthly data.frame containing the anomalies values.
#' @param rDatamonthly data.frame containing the values of time series.
#' @param nLower numeric number specifying the lower year for evaluation period. 
#' @param nUpper numeric number specifying the upper year for evaluation period. 
#' @param station character vector specifying the name of station to consider.
#' 
#' @export
#' 
plotAllAnomaliesBarplot <- function(anomonthly, rDatamonthly, 
                                    nLower=min(format(anomonthly$anodate, "%Y")), 
                                    nUpper=max(format(anomonthly$anodate, "%Y")),
                                    station=NULL, mainTitle="") {
  # Graph anomalies
  anomonthly$ano[which(is.na(anomonthly$ano))] <- 0
  bp <- barplot(anomonthly$ano, xlab = "Year", ylab = "Anomalies",
                ylim=c(-max(c(-min(anomonthly$ano),max(anomonthly$ano))),
                       max(c(-min(anomonthly$ano),max(anomonthly$ano)))),
                col=ifelse(anomonthly$ano>0, "blue", "red"), border=NA, las=2)
  par(new=TRUE)
  barplot(anomonthly$ano, xlab="Year", ylab="Anomalies",
          ylim=c(-max(c(-min(anomonthly$ano),max(anomonthly$ano))),
                 max(c(-min(anomonthly$ano),max(anomonthly$ano)))),
          col=ifelse(anomonthly$ano>0, "blue", "red"), border=NA, las=2)
  axis(1, at=bp[seq(1, length(anomonthly$anodate), 
                    by=length(anomonthly$anodate)/length(unique(format(anomonthly$anodate, "%Y"))))],
       labels=unique(format(anomonthly$anodate, "%Y")), las=2)
  box()
  legend("bottom", c("Positive","Negative"), fill=c("blue","red"), 
         horiz=TRUE, cex=1, box.lty=0, bg="transparent")
  title(paste(mainTitle, "-", station))
}

#' plotCumSum plots cumulative sum
#' @title Cumulative sum plotting
#' @description Plot cumulative sum.
#' @param dat data.frame containing the data to process.
#' @param date character vector specifying the date to consider.
#' @param k numeric number specifying the reference value to substract from cumulated sums.. 
#' @param nLower numeric number specifying the lower year for evaluation period. 
#' @param nUpper numeric number specifying the upper year for evaluation period. 
#' @param station character vector specifying the name of station to consider.
#' @return lo local.trend object returned by 'local.trend' function.
#' 
#' @export
#' 
plotCumSum <- function(dat, date, k=mean(dat), nLower=min(format(date, "%Y")), 
                       nUpper=max(format(date, "%Y")), station=NULL, mainTitle="") {
  # Graph CumSum
  lo <- local.trend(dat, k=k, axes=FALSE, plotit=FALSE)
  
  ## Plot line
  plot(as.vector(lo), type="n", axes=FALSE, xlab="Year", ylab="Cumulative sum")
  v <- which(format(date, "%Y") == nLower)[1]
  rect(v, min(as.vector(lo))-.5, length(dat), max(as.vector(lo))+.5, 
       col=rgb(1,1,0,alpha=.4), lty=0)
  lines(as.vector(lo), type="l", col="blue", lwd=2)
  abline(h=0, lty=2)
  box()
  axis(1, at=seq(1, length(date), by=length(date)/length(unique(format(date, "%Y")))), 
       labels=unique(format(date, "%Y")), las=2)
  axis(2, las=2)
  title(paste(mainTitle, "-", station))
  
  abline(v=v, col="black",lwd=2)
  text(x=v+.05, y=max(lo)-5*max(lo)/100, "Eval", srt=-90, pos=4, cex=0.8, col="dimgrey")
  lo
}