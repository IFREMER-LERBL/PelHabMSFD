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
###   PH3 indicator computation   ###
#####################################

#' richnessIndex computes species richness index
#' @title Species richness index
#' @description Compute species richness index.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res numeric value for index result.
#' 
#' @export
#' 
richnessIndex <- function(dat, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  res <- rowSums(x != 0) # How many taxa upper than 0
  res
}

#' menhinickIndex computes Menhinick index
#' @title Menhinick index
#' @description Compute Menhinick index.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res numeric value for index result.
#' 
#' @export
#' 
menhinickIndex <- function(dat, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  res <- rowSums(x != 0) / (rowSums(x)^(1/2))
  res
}

#' margalefIndex computes Margalef index
#' @title Margalef index
#' @description Compute Margalef index.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res numeric value for index result.
#' 
#' @export
#' 
margalefIndex <- function(dat, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  res <- (rowSums(x != 0)-1)/log(rowSums(x))
  res
}

#' hulburtIndex computes Hulburt index
#' @title Hulburt index
#' @description Compute Hulburt index.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res numeric value for index result.
#' 
#' @export
#' 
hulburtIndex <- function(dat, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  # Formule I.Rombouts
  xTemp <- t(apply(x, 1, sort))
  n1 <- xTemp[,dim(x)[2]]
  n2 <- xTemp[,dim(x)[2]-1]
  n <- n1 + n2
  N <- rowSums(x)
  res <- 100*(n/N)
  res
}

#' shannonIndex computes Shannon index
#' @title Shannon index
#' @description Compute Shannon index.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res numeric value for index result.
#' 
#' @export
#' 
shannonIndex <- function(dat, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  res <- diversity(x, index="shannon")
  res
}

#' pattenIndex computes Patten index
#' @title Patten index
#' @description Compute Patten index.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res numeric value for index result.
#' 
#' @export
#' 
pattenIndex <- function(dat, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  shannon <- diversity(x, index="shannon")
  maxShannon <- max(na.omit(shannon))
  minShannon <- min(na.omit(shannon))
  patten <- (maxShannon-shannon)/(maxShannon-minShannon)
  patten
}

#' simpsonIndex computes Simpson index
#' @title Simpson index
#' @description Compute Simpson index.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res numeric value for index result.
#' 
#' @export
#' 
simpsonIndex <- function(dat, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  res <- diversity(x, index="simpson")
  res
}

#' giniIndex computes Gini index
#' @title Gini index
#' @description Compute Gini index.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res numeric value for index result.
#' 
#' @export
#'
giniIndex <- function(dat, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  res <- 1-diversity(x, index="simpson")
  res
}

#' fisherIndex computes Fisher index
#' @title Fisher index
#' @description Compute Fisher index.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res numeric value for index result.
#' 
#' @export
#'
fisherAlpha <- function(dat, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  res <- fisher.alpha(x)
  res
}

#' pielouIndex computes Pielou index
#' @title Pielou index
#' @description Compute Pielou index.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res numeric value for index result.
#' 
#' @export
#'
pielouIndex <- function(dat, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  x <- dat[, which(names(dat) %in% taxa)]  # Select the taxa columns
  # Formule I.Rombouts
  even <- rep(NA, dim(x)[1])
  abon <- rowSums(x)
  hmax <- rowSums(x != 0)
  sha <- -rowSums((x/abon)*log2((x+1)/abon))
  res <- sha/hmax
  res
}

#' bloom computes bloom frequency
#' @title Bloom frequency
#' @description Compute bloom frequency.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res data.frame containing the bloom frequencies.
#' 
#' @export
#'
bloom <- function(dat,taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  stations <- unique(dat$station)
  
  res <- NULL
  for (s in stations) {
    x <- datMonthly[which(datMonthly$station == s), ]
    for (y in unique(x$year)) {
      xTemp <- x[which(x$year == y), ]
      xTemp <- xTemp[, which(names(xTemp) %in% taxa)] 
      maxRow <- apply(xTemp, 1, max)
      sumRow <- rowSums(xTemp)
      res <- rbind(res, c(station=s, year=y, 
                          bloom=100-length(which(maxRow>=sumRow/2))))
    }
  }
  as.data.frame(res)
}

#' missingMonths computes the missing months
#' @title Missing months
#' @description Compute missing months.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res data.frame containing the missing months.
#' 
#' @export
#'
missingMonths <- function(dat,taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  stations <- unique(dat$station)
  
  res <- NULL
  for (s in stations) {
    x <- dat[which(dat$station == s), ]
    for (y in unique(x$year)) {
      xTemp <- x[which(x$year == y), which(!names(x) %in% taxa)]
      if (dim(xTemp)[1]==12) {
        res <- rbind(res, c(station=s, year=y, missingMonths="No"))
      } else {
        res <- rbind(res, c(station=s, year=y, missingMonths="Yes"))
      }
    }
  }
  as.data.frame(res)
}

## EQR for indices (100-Hulburt, Menhinick, 100-bloom frequency; valRef = max)
#' eqrIndices computes the EQR indices (100-Hulburt, Menhinick, 100-bloom frequency; valRef = max)
#' @title EQR computation
#' @description Compute EQR indices (100-Hulburt, Menhinick, 100-bloom frequency; valRef = max).
#' @param dat data.frame containing the data to process.
#' @return dat data.frame containing the EQR results.
#' 
#' @export
#'
eqrIndices <- function(dat) {
  stations <- unique(dat$station)
  
  Refcenthulburt <- NULL
  Refmenhinick <- NULL
  Refcentbloom <- NULL
  EQRcenthulburt <- NULL
  EQRmenhinick <- NULL
  EQRcentbloom <- NULL
  res <- NULL
  for (s in stations) {
    x <- dat[which(dat$station == s), ]
    x$centhulburt <- 100 - x$hulburt
    # if (any(x$missingMonths == "No")) {
    #   centhulburtRef <- max(as.numeric(as.character(x$centhulburt[which(x$missingMonths == "No")])))
    #   menhinickRef <- max(as.numeric(as.character(x$menhinick[which(x$missingMonths == "No")])))
    #   centbloomRef <- max(as.numeric(as.character(x$centbloom[which(x$missingMonths == "No")])))
      centhulburtRef <- max(as.numeric(as.character(x$centhulburt)))
      menhinickRef <- max(as.numeric(as.character(x$menhinick)))
      centbloomRef <- max(as.numeric(as.character(x$centbloom)))
        
      Refcenthulburt <- c(Refcenthulburt, rep(centhulburtRef, dim(x)[1]))
      Refmenhinick <- c(Refmenhinick, rep(menhinickRef, dim(x)[1]))
      Refcentbloom <- c(Refcentbloom, rep(centbloomRef, dim(x)[1]))
      EQRcenthulburt <- c(EQRcenthulburt, 
                          as.numeric(as.character(x$centhulburt))/centhulburtRef)
      EQRmenhinick <- c(EQRmenhinick, 
                        as.numeric(as.character(x$menhinick))/menhinickRef)
      EQRcentbloom <- c(EQRcentbloom, 
                        as.numeric(as.character(x$centbloom))/centbloomRef)
    # } else {
    #   Refcenthulburt <- c(Refcenthulburt, rep(NA,  dim(x)[1]))
    #   Refmenhinick <- c(Refmenhinick, rep(NA,  dim(x)[1]))
    #   Refcentbloom <- c(Refcentbloom, rep(NA,  dim(x)[1]))
    #   EQRcenthulburt <- c(EQRcenthulburt, rep(NA,  dim(x)[1]))
    #   EQRmenhinick <- c(EQRmenhinick, rep(NA,  dim(x)[1]))
    #   EQRcentbloom <- c(EQRcentbloom, rep(NA,  dim(x)[1]))
    # }
  }
  res <- data.frame(Refcenthulburt=Refcenthulburt, EQRcenthulburt=EQRcenthulburt,
                    Refmenhinick=Refmenhinick, EQRmenhinick=EQRmenhinick,
                    Refcentbloom=Refcentbloom, EQRcentbloom=EQRcentbloom)
  dat <- cbind(dat, res)
  dat
}

## EQR for chla (chlaRef = min)
#' eqrChl computes the EQR indices for Chla (chlaRef = min)
#' @title EQR computation for Chla
#' @description Compute the EQR indices for Chla (chlaRef = min).
#' @param resIndexMean data.frame containing the mean of indices.
#' @param chlFile character vector specifying the path and the name of the csv file containing the chla values.
#' @return resIndexMean dat data.frame containing the mean of indices and the EQR results.
#' 
#' @export
#'
eqrChl <- function(resIndexMean, chlFile) {
  datChl <- dataImport(chlFile)
  if (length(which(is.na(datChl$values))) > 0)
    datChl <- datChl[-which(is.na(datChl$values)), ]
  datChlMonthly <- ph2MonthlyPool(datChl, func=mean)
  datChlMonthly$year <- format(datChlMonthly$date, "%Y")
  datChlMonthly <- merge(datChlMonthly, resIndexMean[, c("station","year")], 
                         by.x=c("station","year"), by.y=c("station","year"))
  stations <- unique(datChlMonthly$station)
  
  resChlMean <- NULL
  for (s in stations) {
    x <- datChlMonthly[which(datChlMonthly$station == s), ]
    x$logValues <- log10(x$values+1)
    idxToDrop <- which(abs(x$logValues-mean(x$logValues))/sd(x$logValues)>2.5)
    x <- x[which(!(abs(x$logValues-mean(x$logValues))/sd(x$logValues)>2.5)), ]
    years <- unique(x$year)
    
    for (y in years) {
      xTemp <- x[which(format(x$date, "%Y") == y), ]
      if (dim(xTemp)[1]==12) {
        missingMonthsChl <- "No"
      } else {
        missingMonthsChl <- "Yes"
      }
      resChlMean <- rbind(resChlMean, c(station=s, year=y, 
                          chl=10^(mean(xTemp$logValues))-1, 
                          missingMonths=missingMonthsChl))
    }
  }
  resChlMean <- as.data.frame(resChlMean)
  resChlMean <- resChlMean[which(!(resChlMean$chl == "NaN")), ]
  
  Refchla <- NULL
  EQRchla <- NULL
  options(warn=2)
  for (s in stations) {
    print(s)
    x <- resChlMean[which(resChlMean$station == s), ]
    # if (any(x$missingMonths == "No")) {
      chlaRef <- min(as.numeric(as.character(x$chl)))
      # chlaRef <- min(as.numeric(as.character(x$chl[which(x$missingMonths == "No")])))
      Refchla <- c(Refchla, rep(chlaRef, dim(x)[1]))
      EQRchla <- c(EQRchla, chlaRef/as.numeric(as.character(x$chl)))
    # } else {
    #   Refchla <- c(Refchla, rep(NA, dim(x)[1]))
    #   EQRchla <- c(EQRchla, rep(NA, dim(x)[1]))
    # }
  }
  resChlMean$EQRchla <- EQRchla
  resChlMean$Refchla <- Refchla
  resChlMean <- resChlMean[which(!(resChlMean$chl == "NaN")), ]
  resChlMean <- merge(resChlMean, resIndexMean[,c("station","year")], 
                      by.x=c("station","year"), by.y=c("station","year"))
  resIndexMean <- merge(resIndexMean, resChlMean[,c("station","year")], 
                        by.x=c("station","year"), by.y=c("station","year"))
  resIndexMean$chla <- resChlMean$chl
  resIndexMean$missingMonthsChla <- resChlMean$missingMonths
  resIndexMean$Refchla <- resChlMean$Refchla
  resIndexMean$EQRchla <- resChlMean$EQRchla
  resIndexMean
}

## MPI (mean of EQR(100-Hulburt), EQR(Menhinick), EQR(100-bloom frequency), EQR(chla))
#' mpi computes the MPI index (mean of EQR(100-Hulburt), EQR(Menhinick), EQR(100-bloom frequency), EQR(chla))
#' @title MPI computation
#' @description Compute the MPI index (mean of EQR(100-Hulburt), EQR(Menhinick), EQR(100-bloom frequency), EQR(chla)).
#' @param dat data.frame containing the EQR indices.
#' @return dat dat data.frame containing the EQR indices and the MPI results.
#' 
#' @export
#'
mpi <- function(dat) {
  dat$mpi <- rowMeans(cbind(dat$EQRcenthulburt, dat$EQRmenhinick, 
                            dat$EQRcentbloom, dat$EQRchla))
  dat
}

## LCBD indices per month
#' lcdbIndex computes the LCBD indices
#' @title LCBD computation
#' @description Compute the LCBD indices.
#' @param dat data.frame containing the data to process.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res dat data.frame containing the LCBD results.
#' 
#' @export
#'
lcdbIndex <- function(dat, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  cat("LCBD computation... \n")
  res <- list()
  stations <- unique(dat$station)
  cpt <- 1
  
  for (s in stations) {
    xTemp <- dat[which(dat$station == s), ]
    x <- xTemp[, (names(xTemp) %in% taxa)]  # Select the taxa columns
    
    if (length(which(colSums(x)==0)) > 0)
      x <- x[,-which(colSums(x)==0)]
    G <- sqrt(rowSums(x)*2)
    Y <- x/G
    M <- colMeans(Y)
    D <- t(t(Y)-M)
    S2 <- D^2
    SS <- colSums(S2)
    SStot <- sum(SS)
    BS <- rowSums(S2)
    finalRes <- BS/SStot
    
    res[[cpt]] <- finalRes
    names(res[[cpt]]) <- xTemp$year
    #names(res[[cpt]]) <- as.Date(paste(xTemp$year, xTemp$month, "01", sep = "-"), "%Y-%m-%d")
    progress(cpt, length(stations), progress.bar=TRUE)
    cpt <- cpt+1
  }
  names(res) <- stations
  message("Done!!!")
  res
}

## LCBD permutations
#' lcdbPerm computes the LCBD permutations
#' @title LCBD permutations
#' @description Compute the LCBD permutations.
#' @param dat data.frame containing the data to process.
#' @param resLCBD data.frame containing the LCBD results.
#' @param taxa character vector specifying all taxa names to consider.
#' @param nperm numeric value specifying the number of permutations to compute.
#' @return res dat data.frame containing the results of LCBD permutations.
#' 
#' @export
#'
lcdbPerm <- function(dat, resLCBD, taxa=NULL, nperm=999) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  cat("LCBD permutations... \n")
  res <- list()
  stations <- as.character(unique(dat$station))
  cpt <- 1
  
  for (s in stations) {
    xTemp <- dat[which(dat$station == s), ]
    x <- xTemp[, (names(xTemp) %in% taxa)]  # Select the taxa columns
    
    if (length(which(colSums(x)==0)) > 0)
      x <- x[,-which(colSums(x)==0)]
    
    res.LCBD <- resLCBD[[s]]
    p <- ncol(x)
    nGE.L <- rep(1,nrow(x))
    for (n in 1:nperm) {
      x.perm <- apply(x, 2, sample)
      G <- sqrt(rowSums(x.perm)*2)
      Y <- x.perm/G
      M <- colMeans(Y)
      D <- t(t(Y)-M)
      S2 <- D^2
      SS <- colSums(S2)
      SStot <- sum(SS)
      BS <- rowSums(S2)
      res.p <- BS/SStot
      ge <- which(res.p >= res.LCBD)
      nGE.L[ge] <- nGE.L[ge] + 1
    }
    p.LCBD <- nGE.L / (nperm+1)
    
    res[[cpt]] <- p.LCBD
    names(res[[cpt]]) <- xTemp$year
    #names(res[[cpt]]) <- as.Date(paste(xTemp$year, xTemp$month, "01", sep = "-"), "%Y-%m-%d")
    progress(cpt, length(stations), progress.bar=TRUE)
    cpt <- cpt+1
  }
  names(res) <- stations
  message("Done!!!")
  res
}

##IVI indices (per period: 2 months before and 2 months after)
#' iviIndex computes the IVI indices
#' @title IVI computation
#' @description Compute the IVI indices.
#' @param dat data.frame containing the data to process.
#' @param resLCBD data.frame containing the LCBD results.
#' @param resLCBDPerm data.frame containing the results of LCBD permutations.
#' @param nb numeric value specifying the number of species to consider.
#' @param taxa character vector specifying all taxa names to consider.
#' @return res dat data.frame containing the results of IVI indices.
#' 
#' @export
#'
iviIndex <- function(dat, resLCBD, resLCBDPerm, nb=10, 
                     signif.test=0.05, taxa=NULL) {
  if (is.null(taxa)) 
    taxa <- names(dat)
  cat("IVI computation... \n")
  res <- list()
  stations <- as.character(unique(dat$station))
  cpt <- 1
  cptStations <- 1
  namesList <- NULL
  
  for (s in stations) {
    resLCBDSorted <- sort(resLCBD[[s]], decreasing=TRUE, index.return=TRUE)
    resLCBDPermSorted <- resLCBDPerm[[s]][resLCBDSorted$ix]
    datesSorted <- which(resLCBDPermSorted <= signif.test)
    if (length(datesSorted) > 0) {
      for (i in 1:min(3,length(datesSorted))) {
        year <- names(datesSorted)[i]
        xTemp <- dat[which(dat$station == s), ]
        rangeIdx <- which(xTemp$year==as.numeric(year))
        xTemp2 <- xTemp[rangeIdx[(rangeIdx>0) & (rangeIdx<=(dim(xTemp)[1]))], ]
        x <- xTemp2[, (names(xTemp2) %in% taxa)]  # Select the taxa columns
        d <- colSums(x)
        d2 <- sum(d)
        RD <- (d/d2)*100
        a <- colSums(x != 0)
        a2 <- sum(a)
        RF <- (a/a2)*100
        Y1 <- (RD + RF)
        Y1 <- sort(Y1, decreasing = TRUE)
        Y1 <- Y1[1:nb]
        res[[cpt]] <- Y1
        cpt <- cpt+1
        namesList <- c(namesList, paste(year, s, sep="_"))
      }
    }
    progress(cptStations, length(stations), progress.bar=TRUE)
    cptStations <- cptStations+1
  }
  names(res) <- namesList
  message("Done!!!")
  res
}

## Correlation between indices
#' corrIndices computes the correlation between indices
#' @title Indices correlation
#' @description Compute the correlation between indices.
#' @param dat data.frame containing the data to process.
#' @param index character vector specifying all indices names to consider.
#' @return corrInd data.frame containing the results of correlations between indices.
#' 
#' @export
#'
corrIndices <- function(dat, index) {
  dat <- dat[,which(names(dat) %in% index)]
  corrInd <- cor(dat)
  corrInd
}

##################################
###   PH3 indicator plotting   ###
##################################

## Display results (boxplots)
#' indexBoxplot plots boxplot of indices
#' @title Indices boxplots
#' @description Plot boxplot of indices.
#' @param res data.frame containing metadata and indices values.
#' @param index character vector specifying the name of indices to consider.
#' @param station character vector specifying the name of station to consider.
#' @param mode character vector specifying the mode of computation. Must be 'month' or 'year' (default).
#' @param dir character vector specifying the name of directory in which the figures are saved.
#' 
#' @export
#'
indexBoxplot <- function(res, index="richness", station=1, mode="year", 
                         mainTitle="", dir) {
  if (station == "all") 
    station <- unique(res$station)
  
  years <- min(res$year):max(res$year)
  months <- 1:12
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
    dat <- res[which(res$station == s), ]
    dat <- dat[order(dat[[mode]]), ]
    
    for (i in 1:length(index)) {
      jpeg(file.path(dir, paste(s, " - ", index[i], ".jpg", sep="")))
      with(dat, boxplot(dat[[index[i]]] ~ factor(dat[[mode]], 
                                                 levels=get(paste(mode, "s", sep=""))),
                        main=paste(mainTitle, "-", s, "-", index[i]), 
                        xlab=xlab, ylab="Values", las=2, col="red", 
                        outline=FALSE, axes=FALSE))
      axis(1, at=1:length(get(paste(mode, "s", sep=""))), 
           lab=get(paste(mode, "s", sep="")), las=lasb)
      axis(2, las=1)
      dev.off()
    }
  }
}

## Display results (contours)
#' indexContour plots contours of indices
#' @title Indices contours
#' @description Plot contours of indices.
#' @param res data.frame containing metadata and indices values.
#' @param index character vector specifying the name of indices to consider.
#' @param station character vector specifying the name of station to consider.
#' @param dir character vector specifying the name of directory in which the figures are saved.
#' 
#' @export
#'
indexContour <- function(res, index="richness", station=1,
                         mainTitle="", dir) {
  months <- 1:12
  years <- min(res$year):max(res$year)
  
  if (station == "all") 
    station <- unique(res$station)
  
  for (s in station) {
    dat <- res[which(res$station == s), ]
    
    if (length(years) > 1) {
      mat <- matrix(NA, nrow=length(months), ncol=length(years))
      colnames(mat) <- years
      rownames(mat) <- months
    
      for (i in 1:length(index)) {
        for (j in 1:dim(dat)[1])
          mat[which(rownames(mat) == dat$month[j]), 
              which(colnames(mat) == dat$year[j])] <- dat[[index[i]]][j]
        jpeg(file.path(dir, paste(s, " - ", index[i], ".jpg", sep="")))
        filled.contour(years, months, t(mat), nlevels=5, 
                       color.palette=viridis, 
                       main=paste(mainTitle, "-", s, "-", index[i]),
                       xlab="Year", ylab="Month")
        dev.off()
      }
    }
  }
}

## Display results (LCBD per month)
#' lcbdGraph plots LCBD graphs
#' @title LCBD graphs
#' @description Plot LCBD graphs.
#' @param res data.frame containing metadata and indices values.
#' @param station character vector specifying the name of station to consider.
#' @param years numeric value specifying the year to consider.
#' @param dir character vector specifying the name of directory in which the figures are saved.
#' 
#' @export
#'
lcbdGraph <- function(res, station=1, years, mainTitle="", dir) {
  cat("LCBD plots computation... \n")
  if (station == "all") 
    station <- unique(names(res))
  
  cpt <- 1
  for (s in station) {
    if (!(all(is.na(res[[s]])))) {
      missedYears <- setdiff(as.character(years), names(res[[s]]))
      missedValues <- setNames(rep(NA, length(missedYears)), missedYears)
      res[[s]] <- c(res[[s]], missedValues)
      res[[s]] <- res[[s]][order(names(res[[s]]))]
      jpeg(file.path(dir, paste("lcbd_station ", s, ".jpg", sep="")), 
           width=1000, height=600)
      plot(years, res[[s]], main=paste(mainTitle, "-", s), 
           xlab="Year", ylab="LCBD", las=2, type="o", pch=18, cex=2, col="blue")
      axis(2, las=1)
      box()
      dev.off()
    }
    progress(cpt, length(station), progress.bar=TRUE)
    cpt <- cpt+1
  }
  message("Done!!!")
}

## Display results (IVI per period)
#' iviGraph plots IVI graphs
#' @title IVI graphs
#' @description Plot IVI graphs.
#' @param res data.frame containing metadata and indices values.
#' @param datMonthly data.frame containing the monthly pool of data.
#' @param station character vector specifying the name of station to consider.
#' @param dir character vector specifying the name of directory in which the figures are saved.
#' 
#' @export
#'
iviGraph <- function(res, datMonthly, station=1, nb=10, mainTitle="", dir) {
  cat("IVI plots computation... \n")
  if (station == "all") 
    station <- unique(names(res))
  
  cpt <- 1
  for (s in station) {
    smpName <- gsub("^.*\\_", "", s)
    smpDate <- gsub("^(.*?)_.*", "\\1", s)
    grps <- sort(names(res[[s]]))
    
    year <- smpDate
    xTemp <- datMonthly[which(datMonthly$station == smpName), ]
    rangeIdx <- which(xTemp$year==as.numeric(year))
    xTemp <- xTemp[rangeIdx[(rangeIdx > 0) & (rangeIdx <= (dim(xTemp)[1]))], ]
    xTemp <- xTemp[, which(names(xTemp) %in% c("station","month","year",grps))]
    xTemp <- xTemp[order(xTemp$year), ]
    datRes <- data.frame(date=as.Date(paste(xTemp$year, xTemp$month, "15", sep="-"), 
                                      "%Y-%m-%d"),
                         month=rep(xTemp$month, each=nb), 
                         year=rep(xTemp$year, each=nb),
                         taxon=rep(grps, dim(xTemp)[1]), 
                         abundance=rep(0, (dim(xTemp)[1]*nb)))
    for (m in xTemp$month)
      datRes$abundance[datRes$month == m] <- unname(xTemp[xTemp$month==m, 
                                                          which(names(xTemp) %in% grps)])
    datRes$abundance <- unlist(datRes$abundance)
    datRes$month <- factor(datRes$month, levels=xTemp$month)
    jpeg(file.path(dir, paste("ivi_", s, ".jpg", sep="")))
    gg <- ggplot(datRes, aes(x=month, y=abundance, group=taxon, fill=taxon)) 
    gg <- gg + geom_area(position="stack") 
    gg <- gg + theme(panel.grid.major=element_blank(), 
                     panel.grid.minor=element_blank(), 
                     panel.border=element_blank(), 
                     panel.background=element_blank(), 
                     axis.line=element_line(colour="black"))
    gg <- gg + scale_x_discrete(name="", breaks=xTemp$month, 
                                labels=paste(xTemp$month, "/", xTemp$year, sep=""), 
                                position="bottom")
    gg <- gg + labs(x="Month", y="Abundance", title=paste(mainTitle, "-", smpName)) 
    gg <- gg + theme(axis.text.x=element_text(angle=90))
    print(gg)
    dev.off()
    
    progress(cpt, length(station), progress.bar=TRUE)
    cpt <- cpt+1
  }
  message("Done!!!")
}

## Plot indices evolution
#' indicesGraph plots indices evolution
#' @title Indices evolution
#' @description Plot indices evolution.
#' @param dat data.frame containing metadata and indices values.
#' @param index character vector specifying the names of indices to consider.
#' @param dir character vector specifying the name of directory in which the figures are saved.
#' 
#' @export
#'
indicesGraph <- function(dat, index, mainTitle="", dir) {
  if (length(index > 1)) {
    dat$date <- as.Date(paste(dat$year, dat$month, "01", sep="-"), "%Y-%m-%d")
    stations <- unique(dat$station)
    axisCol <- c(1,4,7,10,13)
    linesCol <- c(3,6,9,12,15)
    colsCol <- c("red","green","black","yellow","blue")

    for (s in stations) {
      ind <- dat[which(dat$station == s), ]
      ind <- ind[order(ind$date), ]
      ind <- ind[, which(names(ind) %in% c(index,"date"))]
    
      datTemp <- data.frame(date=seq.Date(from=min(ind$date), 
                                          to=max(ind$date), by="month"), 
                            values=NA)
      datTemp$values[which(datTemp$date %in% ind$date)] <- ind[[index[1]]][which(ind$date %in% datTemp$date)]
      
      jpeg(file.path(dir, paste(s, " - Indices.jpg", sep="")), 
           width=1000, height=600)
      par(las=3, mar=c(5,13,2,1)+0.1)
      plot(datTemp$date, datTemp$values, type="o", ylab="", xlab="Year", 
           col=colsCol[1], lwd=2, axes=FALSE)
      axis(2, col=colsCol[1], col.axis=colsCol[1], line=axisCol[1], 
           cex.axis=1, mgp=c(3,.5,0))
      mtext(2, text=index[1], line=linesCol[1], col=colsCol[1], cex=1)
      
      for (i in 2:length(index)) {
        datTemp <- data.frame(date=seq.Date(from=min(ind$date), 
                                            to=max(ind$date), by="month"),
                              values=NA)
        datTemp$values[which(datTemp$date %in% ind$date)] <- ind[[index[i]]][which(ind$date %in% datTemp$date)]
        par(new=TRUE)
        plot(datTemp$date, datTemp$values, type="o", ylab="", xlab="Year", 
             col=colsCol[i], lwd=2, axes=FALSE)
        axis(2, col=colsCol[i], col.axis=colsCol[i], line=axisCol[i], 
             cex.axis=1, mgp=c(3,.5,0))
        mtext(2, text=index[i], line=linesCol[i], col=colsCol[i], cex=1)
      }
      axis.Date(1, datTemp$date, format="%Y")
      title(paste(mainTitle, "-", s))
      box()
      dev.off()
    }
  }
}

## Plot correlation between indices
#' corrIndicesGraph plots indices correlations
#' @title Indices correlation
#' @description Plot indices correlations.
#' @param dat data.frame containing metadata and indices values.
#' @param index character vector specifying the names of indices to consider.
#' @param dir character vector specifying the name of directory in which the figures are saved.
#' 
#' @export
#'
corrIndicesGraph <- function(dat, index, dir) {
  stations <- unique(dat$station)
  for (s in stations) {
    datTemp <- dat[which(dat$station == s), ]
    if (dim(datTemp)[1] > 1) {
      corrInd <- corrIndices(datTemp, index)
      jpeg(file.path(dir, paste(s, " - Correlations.jpg", sep="")))
      corrplot(abs(corrInd), method="color", cl.lim=c(0,1), 
               col=colorRampPalette(c("blue","white","orange"))(200), 
               tl.cex=1, mar=c(1,0,0,0))
      dev.off()
      saveResults(corrInd, filename=file.path(dir, paste(s, " - Correlations.csv", sep="")),
                  col.names=NA, row.names=TRUE)
    }
  }
}