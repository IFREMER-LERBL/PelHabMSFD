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

#' MSFD displays the graphical user interface to compute PH2 and PH3 indicators
#' @title Graphical User Interface for MSFD indicators
#' @description Display the graphical user interface to compute PH2 and PH3 indicators.
#' @import tcltk tcltk2 
#' @importFrom tkrplot tkrplot
#' 
D1_MSFD <- function() {
  graphics.off()
  fontTitle <- tkfont.create(family="Helvetica",size=18,weight="bold")
  fontText <- tkfont.create(family="Helvetica", size=8, weight="bold")
  tcl("image", "create", "photo", "settingsFile", file=system.file("images", "settings.gif", package="MSFD"))
  #tcl("image", "create", "photo", "csv", file=system.file("images", "csv.gif", package="MSFD"))
  #tcl("image", "create", "photo", "msfd", file=system.file("images", "msfd.gif", package="MSFD"))
  #tcl("image", "create", "photo", "ospar", file=system.file("images", "ospar.gif", package="MSFD"))
  msfd <- tcl("image", "create", "photo", "msfd", file="MSFD/inst/images/msfd.gif")
  csv <- tcl("image", "create", "photo", "csv", file="MSFD/inst/images/csv.gif")
  ospar <- tcl("image", "create", "photo", "ospar", file="MSFD/inst/images/ospar.gif")
  
  tk2theme("alt")
  msfdWindow <- tktoplevel()
  tktitle(msfdWindow) <- "D1 MSFD indicators"
  tkgrid(tklabel(msfdWindow, text="   "), row=1, column=0, columnspan=4)
  
  # PH2 default options
  PH2settings <- list(consistencyVal="0", nbMonthsVal="1", nbMonths=6, 
                      nbYearsVal="1", nbYears=5, samplingVal="0", anomaliesVal="1", 
                      cumsumVal="1", decompositionVal="1")
  
  # PH3 default options
  PH3settings <- list(nbMonthsVal="1", nbMonths=6, nbYearsVal="1", nbYears=5, 
                      nbMissing=10, genusVal="0", frequencyVal="1", freqMode="Mean", 
                      richnessVal="0", menhinickVal="1", hulburtVal="1", giniVal="1", 
                      pattenVal="1", eqrVal="0", lcbdiviVal="1", lcbdperm=999, nbivi=5, 
                      lcbdtest=0.05, samplingVal="0", boxplotVal="1", contourVal="1", 
                      indiceVal="0")
  
  # Select the file (.CSV)
  fileButton <- tk2button(msfdWindow, text="SELECT CSV FILE", image="csv", 
                          compound="left", width=20, command=function() {
    datafile <<- tclvalue(tkgetOpenFile(filetypes="{{csv Files} {.csv}}"))
    if (!nchar(datafile)) {
      tkmessageBox(message="No file selected!")
    } else {
      tkgrid(tklabel(msfdWindow, text=basename(datafile), font=fontText, 
                     foreground="darkgreen"), row=4, columnspan=4)
      tkconfigure(ph1Button, state="normal")
      tkconfigure(ph2Button, state="normal")
      tkconfigure(ph3Button, state="normal")
      tkconfigure(PH2settingsButton, state="normal")
      tkconfigure(PH3settingsButton, state="normal")
      tkconfigure(pcButton, state="normal")
    }
  })
  tkgrid(fileButton, row=2, padx=c(80,80), column=0, columnspan=4)
  tkgrid(tklabel(msfdWindow, text="      "), row=4)
  tkgrid(tklabel(msfdWindow, text="      "), row=5)
  
  ############################
  ###   Button for 'PH1'   ###
  ############################
  ph1Button <- tkbutton(msfdWindow, text="PH1\nLifeforms", width=20, 
                        state="disabled", command=function() {
    tkmessageBox(message="TO DO...!")
  })
  tkgrid(ph1Button, pady=c(0,10), row=6, column=1, columnspan=2)

  ############################
  ###   Button for 'PH2'   ###
  ############################
  ph2Button <- tkbutton(msfdWindow, text="PH2\nBiomasse-Abundance", 
                        width=20, state="disabled", command=function() {
    # PH2 data importation
    cat("PH2 - Data importation... ")
    resDir <<- dirname(datafile)
    dfr <<- NULL
    dfr <<- dataImport(datafile)
    if (length(which(is.na(dfr$values))) > 0)
      dfr <<- dfr[-which(is.na(dfr$values)), ]
    cat("Done!!!\n")
    
    func <- "mean"
    # Result folders
    dirPH2 <- file.path(resDir, "PH2 Indicator")
    dir.create(dirPH2)
    # Anomalies computation
    datMonthly <<- ph2MonthlyPool(dfr, func=get(func))
    
    # Window for selection of reference/evaluation periods
    datett <- tktoplevel()
    tktitle(datett) <- "Reference/Evaluation period selection"
    minVal <- as.numeric(format(min(datMonthly$date), "%Y"))
    maxVal <- as.numeric(format(max(datMonthly$date), "%Y"))
    # Frame for reference period
    tkgrid(tk2label(datett, text="   "), row=1, sticky="w")
    tkgrid(tk2label(datett, text="Select the period for reference", 
                    justify="center"), row=2, column=1, columnspan=3)
    tcl.lowerThresholdRef <<- tclVar(as.character(minVal))
    tk.lowerThresholdRef <<- tkentry(datett, textvariable=tcl.lowerThresholdRef,
                                     width=8, state="normal", background="white")
    tkgrid(tk.lowerThresholdRef, row=3, column=1)
    tkgrid(tk2label(datett, text=" < X < "), row=3, column=2)
    tcl.upperThresholdRef <<- tclVar(as.character(maxVal))
    tk.upperThresholdRef <<- tkentry(datett, textvariable=tcl.upperThresholdRef, 
                                     width=8, state="normal", background="white")
    tkgrid(tk.upperThresholdRef, row=3, column=3)
    # Frame for evaluation period
    tkgrid(tk2label(datett, text="   "), row=4, sticky="w")
    tkgrid(tk2label(datett, text="Select the period for evaluation", 
                    justify="center"), row=5, column=1, columnspan=3)
    tcl.lowerThreshold <<- tclVar(as.character(minVal))
    tk.lowerThreshold <<- tkentry(datett, textvariable=tcl.lowerThreshold, 
                                  width=8, state="normal", background="white")
    tkgrid(tk.lowerThreshold, row=6, column=1)
    tkgrid(tk2label(datett, text=" < X < "), row=6, column=2)
    tcl.upperThreshold <<- tclVar(as.character(maxVal))
    tk.upperThreshold <<- tkentry(datett, textvariable=tcl.upperThreshold, 
                                  width=8, state="normal", background="white")
    tkgrid(tk.upperThreshold, row=6, column=3)
    tkgrid(tk2label(datett, text="   "), row=7, column=4)
    # Button for validation
    onOK <- function() {
      nLowerRef <<- as.integer(tclvalue(tcl.lowerThresholdRef))
      nUpperRef <<- as.integer(tclvalue(tcl.upperThresholdRef))
      nLower <<- as.integer(tclvalue(tcl.lowerThreshold))
      nUpper <<- as.integer(tclvalue(tcl.upperThreshold))
      tkdestroy(datett)
    }
    butOK <- tk2button(datett, text="OK", width=-6, command=onOK)
    tkgrid(butOK, row=8, column=2)
    tkwait.window(datett)

    # Data cleaning and selection
    datMonthly <<- ph2DataCleaning(datMonthly, check.consistency=PH2settings$consistencyVal, 
                                   check.nbMonths=PH2settings$nbMonthsVal, minMonths=PH2settings$nbMonths, 
                                   check.nbYears=PH2settings$nbYearsVal, minYears=PH2settings$nbYears, 
                                   nLowerRef=nLowerRef, nUpperRef=nUpperRef)
    datMonthly <<- ph2DataSelection(datMonthly, nLowerRef=nLowerRef, 
                                    nUpperRef=nUpperRef, nLower=nLower, nUpper=nUpper)
    stationsRetained <- unique(datMonthly$station)
    if (PH2settings$samplingVal == "1") {
      cat("Sampling computation... \n")
      dirPH2Sampling <- file.path(dirPH2, "PH2 Sampling")
      dir.create(dirPH2Sampling)
      indexSampling(datMonthly, station="all", dir=dirPH2Sampling,
                    mainTitle=paste("PH2 Indicator - Evaluation ", 
                                    nLower, "-", nUpper, sep=""))
      saveResults(stationsRetained, col.names=FALSE, row.names=FALSE,
                  filename=file.path(dirPH2Sampling, "retained_stations.csv"))
    }
      
    # Anomalies computation
    cat("Anomalies computation... \n")
    cpt <- 1
    nbRef <- NULL
    trendEvalCumsum <<- NULL
    resOutput <- NULL
    for (i in unique(datMonthly$station)) {
      allDates <- unique(as.numeric(format(datMonthly[which(datMonthly$station == i),]$date, "%Y")))
      if (any(allDates >= nLower)) {
        # Check for reference years
        nbYearsRef <- nLower-as.numeric(format(min(datMonthly[which(datMonthly$station == i),]$date), "%Y"))
        nbRef <- rbind(nbRef, c(i,nbYearsRef))
        if (nbYearsRef <= 0) {
          resAno <- anomalies(datMonthly[which(datMonthly$station == i),])
        } else {
          resAno <- anomalies(datMonthly[which(datMonthly$station == i),], 
                              nLower=nLower, nUpper=nUpper)
        }
        resAllAno <- anomalies(datMonthly[which(datMonthly$station == i),])
        resAnoToSave <- NULL
        resAnoToSave <- merge(resAno$anomonthly, resAno$rDatamonthly, by="anodate")
        resAnoToSave <- merge(resAnoToSave, resAno$repCyclemonthly, by="anodate")
        
        t_test <- testStudent(resAnoToSave, nLower)
        sign_t <- sign(t_test$statistic)
        if (sign_t == 1) {
          sign_t <- "+"
        } else if (sign_t == -1) {
          sign_t <- "-"
        }
        
        if (t_test$p.value <= 0.01) {
          signif_t <- "YES"
        } else {
          signif_t <- "NO"
        }
        
        var_test <- varTest(resAnoToSave, nLower)
        evol_sd <- SDevolution(resAnoToSave, nLower)
        
        resOutput <- rbind(resOutput, c(station=i, t_test=t_test$statistic, 
                                        sign_t=sign_t, p_value_t=t_test$p.value, 
                                        signif_t_0.01=signif_t, 
                                        p_value_variance=var_test$p.value, 
                                        percent_evolution_SD=evol_sd))
        
        dirPH2Ano <- file.path(dirPH2, "PH2 Anomalies")
        dir.create(dirPH2Ano)
        saveResults(resAnoToSave, row.names = FALSE, 
                    col.names = c("date","monthlyAnomalies","change","serie","cycle"),
                    filename=file.path(dirPH2Ano, paste("ph2_monthlyAnomalies_", i, ".csv", sep="")))
        
        ResultFinPeriod <- NULL
        if (nbYearsRef <= 0) {
          resAnoPeriod <- resAno$anomonthly
        } else {
          resAnoPeriod <- resAno$anomonthly[format(resAno$anomonthly$anodate, "%Y") >= 
                                              nLower & format(resAno$anomonthly$anodate, "%Y") <= 
                                              nUpper, ]
        }
        if (dim(resAnoPeriod)[1] > 0) {
          # Total number of values present in the evaluation period
          TotAnomValuesEval <- length(which(!is.na(resAno$anomonthly$ano))) 
          TotAnomValuesEvalPeriod <- length(which(!is.na(resAnoPeriod$ano)))
          
          NbBothPosiPeriod <- length(which(resAnoPeriod$ano >= as.numeric(resAno$LSS)))
          NbBothNegaPeriod <- length(which(resAnoPeriod$ano <= as.numeric(resAno$LSI)))
          # Number of strong positive anomalies (in important and extreme changes) during the evaluation period
          NbToTBothAnomPeriod <- NbBothPosiPeriod+NbBothNegaPeriod 
          # Percentage of anomalies withing the "Strong ones" (important and extreme) during the evaluation period
          PerBothPeriod <- (NbToTBothAnomPeriod/TotAnomValuesEval)*100 
          
          NbExtremPosiPeriod <- length(which(resAnoPeriod$ano >= as.numeric(resAno$LCS)))
          NbExtremNegaPeriod <- length(which(resAnoPeriod$ano <= as.numeric(resAno$LCI)))
          NbToTExtremAnomPeriod <- NbExtremPosiPeriod+NbExtremNegaPeriod
          PerExtremPeriod <- (NbToTExtremAnomPeriod/TotAnomValuesEval)*100
          
          NbStrongPosiPeriod <- length(which((resAnoPeriod$ano >= as.numeric(resAno$LSS)) & 
                                               (resAnoPeriod$ano < as.numeric(resAno$LCS))))
          NbStrongNegaPeriod <- length(which((resAnoPeriod$ano <= as.numeric(resAno$LSI)) & 
                                               (resAnoPeriod$ano > as.numeric(resAno$LCI))))
          NbToTStrongAnomPeriod <- NbStrongPosiPeriod+NbStrongNegaPeriod
          PerStrongPeriod <- (NbToTStrongAnomPeriod/TotAnomValuesEval)*100
            
          ResultFinPeriod <- c(TotAnomValuesEvalPeriod, NbToTBothAnomPeriod, 
                               NbBothPosiPeriod, NbBothNegaPeriod, PerBothPeriod, 
                               NbToTExtremAnomPeriod, NbExtremPosiPeriod, 
                               NbExtremNegaPeriod, PerExtremPeriod, NbToTStrongAnomPeriod, 
                               NbStrongPosiPeriod, NbStrongNegaPeriod, PerStrongPeriod)
          names(ResultFinPeriod) <- paste(names(resAno$ResultFin), 
                                          paste(nLower, "_", nUpper, sep=""), sep="_")
        }
        ResultFinPeriod <- c(resAno$ResultFin, ResultFinPeriod)
        saveResults(ResultFinPeriod, col.names = FALSE, row.names = names(ResultFinPeriod),
                    filename=file.path(dirPH2Ano, paste("ph2_final_", i, ".csv", sep="")))
        
        if (PH2settings$anomaliesVal == "1") {
          jpeg(file.path(dirPH2Ano, paste(i, "_anomalies.jpg", sep="")), width=1000, height=600)
          plotAnomalies(resAno$anomonthly[, c("anodate","ano")], resAno$LCS, 
                        resAno$LCI, resAno$LSS, resAno$LSI, resAno$lim_sup, 
                        resAno$lim_inf, station=i,
                        mainTitle=paste("PH2 Indicator - Evaluation ", nLower, "-", nUpper, sep=""))
          dev.off()
          
          jpeg(file.path(dirPH2Ano, paste(i, "_barplot_all.jpg", sep="")), width=1000, height=600)
          par(mar=c(5,5,2,5))
          plotAllAnomaliesBarplot(resAllAno$anomonthly, resAllAno$rDatamonthly, 
                                  station=i, mainTitle="PH2 Indicator")
          par(new=TRUE)
          loAno <- local.trend(resAllAno$anomonthly$ano, k=0, axes=FALSE, plotit=FALSE)
          plot(as.vector(loAno), type="l", col="black", lwd=2, 
               ylim=c(-max(c(-min(loAno),max(loAno))), max(c(-min(loAno),max(loAno)))),
               axes=FALSE, xlab=NA, ylab=NA)
          axis(side=4)
          mtext(side=4, line=3, "Somme cumulative")
          dev.off()
          
          jpeg(file.path(dirPH2Ano, paste(i, "_barplot.jpg", sep="")), width=1000, height=600)
          par(mar=c(5,5,2,5))
          if (nbYearsRef <= 0) {
            plotAnomaliesBarplot(resAno$anomonthly, resAno$rDatamonthly, station=i,
                                 mainTitle=paste("Indicateur PH2 - Evaluation ", nLower, "-", nUpper, sep=""))
          } else {
            plotAnomaliesBarplot(resAno$anomonthly, resAno$rDatamonthly, nLower=nLower, 
                                 nUpper=nUpper, station=i,
                                 mainTitle=paste("Indicateur PH2 - Evaluation ", nLower, "-", nUpper, sep=""))
          }
          par(new=TRUE)
          loAno <- local.trend(resAno$anomonthly$ano, k=0, axes=FALSE, plotit=FALSE)
          plot(as.vector(loAno), type="l", col="black", lwd=2, 
               ylim=c(-max(c(-min(loAno),max(loAno))), max(c(-min(loAno),max(loAno)))),
               axes=FALSE, xlab=NA, ylab=NA)
          axis(side=4)
          mtext(side=4, line=3, "Somme cumulative")
          dev.off()
          saveResults(data.frame(date=resAno$anomonthly$anodate, cumsum=as.vector(loAno)), 
                      filename=file.path(dirPH2Ano, paste(i, "_cumsum.csv", sep="")), 
                      row.names=FALSE)
          
          # Trend detection on cumsum (evaluation period)
          if (nbYearsRef <= 0) {
            trend <- trendDetection(loAno, resAno$rDatamonthly$anodate, station=i)
          } else {
            trend <- trendDetection(loAno, resAno$rDatamonthly$anodate, 
                                    nLower=nLower, nUpper=nUpper, station=i)
          }
          trendEvalCumsum <<- rbind(trendEvalCumsum, c(i, trend$estimate, trend$p.value))
        }
        
        # # Cumsum on anomalies
        # if (cumsumVal == "1") {
        #   jpeg(file.path(dirPH2Ano, paste(i, "_cumsum.jpg", sep="")), width=1000, height=600)
        #   if (nbYearsRef <= 0) {
        #     loAno <- plotCumSum(resAno$anomonthly$ano, resAno$anomonthly$anodate, k=0, station=i,
        #                         mainTitle=paste("Indicateur PH2 - Evaluation ", nLower, "-", nUpper, sep=""))
        #   } else {
        #     loAno <- plotCumSum(resAno$anomonthly$ano, resAno$anomonthly$anodate, k=0, nLower=nLower, 
        #                         nUpper=nUpper, station=i,
        #                         mainTitle=paste("Indicateur PH2 - Evaluation ", nLower, "-", nUpper, sep=""))
        #   }
        #   dev.off()
        # }
        
        dirPH2Tse <- file.path(dirPH2, "PH2 Time Series")
        dir.create(dirPH2Tse)
        jpeg(file.path(dirPH2Tse, paste(i, ".jpg", sep="")), width=1000, height=600)
        tse <- plotTimeSeries(datMonthly[which(datMonthly$station==i),], station=i,
                              mainTitle=paste("Indicateur PH2 - Evaluation ", nLower, "-", nUpper, sep=""))
        dev.off()
        saveResults(tse, filename=file.path(dirPH2Tse, paste("ph2_timeSeries_", i, ".csv", sep="")), 
                    row.names=FALSE, col.names=c("date","values","change"))
        
        if (PH2settings$decompositionVal == "1") {
          dirPH2Dec <- file.path(dirPH2, "PH2 Decomposition")
          dir.create(dirPH2Dec)
          jpeg(file.path(dirPH2Dec, paste("Decomposition_", i, ".jpg", sep="")))
          plotDecomposition(dat=resAno, station=i, 
                            mainTitle=paste("Indicateur PH2 - Evaluation ", nLower, "-", nUpper, sep=""))
          dev.off()
        }
      }
      progress(cpt, length(unique(datMonthly$station)), progress.bar=TRUE)
      cpt <- cpt+1
    }
    saveResults(nbRef, filename=file.path(dirPH2, "nbYearsReference.csv"), 
                row.names=FALSE, col.names=c("station","nbYearsReference"))
    saveResults(resOutput, filename=file.path(dirPH2, "ph2_outputs.csv"), 
                row.names=FALSE)
    if (PH2settings$anomaliesVal == "1")
      saveResults(trendEvalCumsum, filename=file.path(dirPH2, "trendEval.csv"), 
                  row.names=FALSE, col.names=c("station","Trend_Cumsum","Signif_Cumsum"))
    message("Done!!!")
  })
  tkgrid(ph2Button, pady=c(0,10), row=7, column=1, columnspan=2)
  
  PH2settingsButton <- tk2button(msfdWindow, text="", image="settingsFile", 
                                 compound="center", width=2, state="disabled", 
                                 command=function() {
    PH2settings <<- PH2settingsWindow(consistencyVal=PH2settings$consistencyVal, 
                                      nbMonthsVal=PH2settings$nbMonthsVal, 
                                      nbMonths=PH2settings$nbMonths, 
                                      nbYearsVal=PH2settings$nbYearsVal, 
                                      nbYears=PH2settings$nbYears, 
                                      samplingVal=PH2settings$samplingVal, 
                                      anomaliesVal=PH2settings$anomaliesVal, 
                                      cumsumVal=PH2settings$cumsumVal, 
                                      decompositionVal=PH2settings$decompositionVal)
  })
  tkgrid(PH2settingsButton, pady=c(0,10), row=7, column=3, sticky="w")
  
  ############################
  ###   Button for 'PH3'   ###
  ############################
  ph3Button <- tkbutton(msfdWindow, text="PH3\nDiversity", width=20, 
                        state="disabled", command=function() {
    # PH3 data importation
    cat("PH3 - Data importation... ")
    resDir <<- dirname(datafile)
    dfr <<- NULL
    dfr <<- read.table(datafile, quote="", fill=TRUE, sep=";", dec=",", 
                       header=TRUE, stringsAsFactors=FALSE)
    dfr[sapply(dfr, function(x) all(is.na(x)))] <<- NULL
    
    taxaFormattt <- tktoplevel()
    tktitle(taxaFormattt) <- "File format"
    done <- tclVar(1)   
    taxaFormattt$env$butRow <- tk2button(taxaFormattt, text="Taxa in rows", 
                                         width=-6, command=function() tclvalue(done) <- 1)
    taxaFormattt$env$butCol <- tk2button(taxaFormattt, text="Taxa in columns", 
                                         width=-6, command=function() tclvalue(done) <- 2)
    tkgrid(taxaFormattt$env$butRow, taxaFormattt$env$butCol, padx=20, pady=15)
    tkbind(taxaFormattt, "<Destroy>", function() tclvalue(done) <- 1)
    tkfocus(taxaFormattt)
    tkwait.variable(done)
    doneVal <- tclvalue(done)
    tkdestroy(taxaFormattt)
    
    if (doneVal == "1") { # Taxa in rows
      if (length(which(dfr$abundance==0)) > 0)
        dfr <<- dfr[-which(dfr$abundance==0), ]
      # Data transformation (RESOMAR format, if needed)
      dfr$abundance <- round(as.numeric(sub(",", ".", dfr$abundance, fixed=TRUE)))
      dat <<- ph3DataTransform(dat=dfr, origin="rephy")
      taxa <<- sort(unique(dfr$taxon))
    } else {
      vecMetadata <- c("station","day","month","year","latitude","longitude")
      dfr[, which(!(names(dfr) %in% vecMetadata))] <- sapply(dfr[, which(!(names(dfr) %in% vecMetadata))], 
                                                             gsub, pattern=",", replacement=".")
      dfr[, which(!(names(dfr) %in% vecMetadata))] <- sapply(dfr[, which(!(names(dfr) %in% vecMetadata))], 
                                                             as.numeric)
      dfr[, which(!(names(dfr) %in% vecMetadata))] <- round(dfr[, which(!(names(dfr) %in% vecMetadata))])
      dat <<- dfr
      taxa <<- sort(unique(names(dfr)[!(names(dfr) %in% vecMetadata)]))
    }
    cat("Done!!!\n")
    
    # Result folders
    dirPH3 <- file.path(resDir, "PH3 Indicator")
    dir.create(dirPH3)
    
    # Monthly or random pool computation
    if (PH3settings$frequencyVal == "1") {
      cat("PH3 - Monthly indices computation... ")
      if (PH3settings$freqMode == "Mean")
        datMonthly <<- ph3MonthlyPool(dat, taxa, func=mean)
      if (PH3settings$freqMode == "Sum")
        datMonthly <<- ph3MonthlyPool(dat, taxa, func=sum)
      if (PH3settings$freqMode == "Random")
        datMonthly <<- ph3RandomPool(dat, taxa)
      cat("Done!!!\n")
    }
    
    datett <- tktoplevel()
    tktitle(datett) <- "Evaluation period selection"
    tkgrid(tk2label(datett, text="   "), row=1, sticky="w")
    tkgrid(tk2label(datett, text="Select the period of evaluation", justify="center"),
           row=2, column=1, columnspan=3)
    minVal <- as.numeric(min(datMonthly$year))
    maxVal <- as.numeric(max(datMonthly$year))
    
    tcl.lowerThreshold <<- tclVar(as.character(minVal))
    tk.lowerThreshold <<- tkentry(datett, textvariable=tcl.lowerThreshold, 
                                  width=8, state="normal", background="white")
    tkgrid(tk.lowerThreshold, row=4, column=1)
    tkgrid(tk2label(datett, text=" < X < "), row=4, column=2)
    tcl.upperThreshold <<- tclVar(as.character(maxVal))
    tk.upperThreshold <<- tkentry(datett, textvariable=tcl.upperThreshold, 
                                  width=8, state="normal", background="white")
    tkgrid(tk.upperThreshold, row=4, column=3)
    tkgrid(tk2label(datett, text="   "), row=5, column=4)
    
    onOK <- function() {
      nLower <<- as.integer(tclvalue(tcl.lowerThreshold))
      nUpper <<- as.integer(tclvalue(tcl.upperThreshold))
      tkdestroy(datett)
    }
    butOK <- tk2button(datett, text="OK", width=-6, command=onOK)
    tkgrid(butOK, row=5, column=2)
    tkwait.window(datett)
    
    cat("PH3 - Data cleaning... \n")
    stationsRetained <- ph3DataCleaning(datMonthly, check.nbMonths=PH3settings$nbMonthsVal, 
                                        minMonths=PH3settings$nbMonths,
                                        check.nbYears=PH3settings$nbYearsVal, 
                                        minYears=PH3settings$nbYears)
    if (PH3settings$samplingVal == "1") {
      dirPH3Sampling <- file.path(dirPH3, "PH3 Sampling")
      dir.create(dirPH3Sampling)
      indexSampling(datMonthly[, which(!(names(datMonthly) %in% taxa))], station="all",
                    mainTitle=paste("PH3 Indicator - Evaluation ", nLower, "-", nUpper, sep=""), 
                    dir=dirPH3Sampling)
      saveResults(stationsRetained, col.names=FALSE, row.names=FALSE,
                  filename=file.path(dirPH3Sampling, "retained_stations.csv"))
    }
    datMonthly <<- datMonthly[which(datMonthly$station %in% stationsRetained), ]
    datMonthly <<- datMonthly[which((datMonthly$year>=nLower) & (datMonthly$year<=nUpper)), ]

    # Indices computation
    cat("Indices computation... ")
    resIndex <- NULL
    resIndexMean <- NULL
    resIndex <- datMonthly[, which(!(names(datMonthly) %in% taxa))]
    idxToPlot <- NULL
    
    if (PH3settings$richnessVal == "1") {
      resIndex$richness <- richnessIndex(datMonthly, taxa)
      idxToPlot <- c(idxToPlot, "richness")
    }
    if (PH3settings$menhinickVal == "1") {
      resIndex$menhinick <- menhinickIndex(datMonthly, taxa)
      idxToPlot <- c(idxToPlot, "menhinick")
    }
    if (PH3settings$hulburtVal == "1") {
      resIndex$hulburt <- hulburtIndex(datMonthly, taxa)
      idxToPlot <- c(idxToPlot, "hulburt")
    }
    if (PH3settings$giniVal == "1") {
      resIndex$gini <- giniIndex(datMonthly, taxa)
      idxToPlot <- c(idxToPlot, "gini")
    }
    if (PH3settings$pattenVal == "1") {
      resIndex$patten <- pattenIndex(datMonthly, taxa)
      idxToPlot <- c(idxToPlot, "patten")
    }

    saveResults(resIndex, row.names=FALSE,
                filename=file.path(dirPH3, "ph3_indices.csv"))
    resIndexMean <- aggregate(. ~ station+year, resIndex[, -which(names(resIndex) == "month")], mean)
    resIndexMean <- resIndexMean[order(resIndexMean$station), ]
    if (PH3settings$eqrVal == "1") {
      resBloom <- bloom(datMonthly, taxa)
      resMissingMonths <- missingMonths(datMonthly, taxa)
      resIndexMean <- cbind(resIndexMean, centbloom=resBloom$bloom)
      resIndexMean <- cbind(resIndexMean, missingMonths=resMissingMonths$missingMonths)
      resIndexMean <- eqrIndices(resIndexMean)
    }
    saveResults(resIndexMean, row.names = FALSE,
                filename=file.path(dirPH3, "ph3_indices_mean.csv"))
      
    # Boxplots display
    if (PH3settings$boxplotVal == "1") {
      dirPH3BoxplotM <- file.path(dirPH3, "PH3 Monthly Boxplots")
      dir.create(dirPH3BoxplotM)
      indexBoxplot(resIndex, index=idxToPlot, station="all", mode="month", 
                   mainTitle=paste("PH3 Indicator - Evaluation ", nLower, "-", nUpper, sep=""), 
                   dir=dirPH3BoxplotM)
      dirPH3BoxplotY <- file.path(dirPH3, "PH3 Yearly Boxplots")
      dir.create(dirPH3BoxplotY)
      indexBoxplot(resIndex, index=idxToPlot, station="all", mode="year", 
                   mainTitle=paste("PH3 Indicator - Evaluation ", nLower, "-", nUpper, sep=""), 
                   dir = dirPH3BoxplotY)
    }
    # Contours display
    if (PH3settings$contourVal == "1") {
      dirPH3Contour <- file.path(dirPH3, "PH3 Contours")
      dir.create(dirPH3Contour)
      indexContour(resIndex, index=idxToPlot, station="all", 
                   mainTitle=paste("PH3 Indicator - Evaluation ", nLower, "-", nUpper, sep=""), 
                   dir=dirPH3Contour)
    }
    # Indices evolution
    if (PH3settings$indiceVal == "1") {
      dirPH3Indices <- file.path(dirPH3, "PH3 Indices")
      dir.create(dirPH3Indices)
      indicesGraph(resIndex, index=idxToPlot, 
                   mainTitle=paste("PH3 Indicator - Evaluation ", nLower, "-", nUpper, sep=""), 
                   dir=dirPH3Indices)
    }
    cat("Done!!!\n")
    
    # LCBD indices computation
    if (PH3settings$lcbdiviVal == "1") {
      # Yearly pool computation
      cat("PH3 - Yearly indices computation... ")
      datYearly <<- ph3YearlyPool(dat, taxa, func=sum)
      datYearly <<- datYearly[which(datYearly$station %in% stationsRetained), ]
      datYearly <<- datYearly[datYearly$year>=nLower & datYearly$year<=nUpper, ]
      cat("Done!!!\n")
      
      resLCBD <- lcdbIndex(datYearly, taxa)
      resLCBDPerm <- lcdbPerm(datYearly, resLCBD, taxa, nperm=PH3settings$lcbdperm)
      dirPH3LCBD <- file.path(dirPH3, "PH3 LCBD")
      dir.create(dirPH3LCBD)
      for (l in names(resLCBDPerm)) {
        resLCBDToSave <- cbind(resLCBD[[l]], resLCBDPerm[[l]])
        colnames(resLCBDToSave) <- c("lcbd","perm")
        saveResults(resLCBDToSave, col.names = NA, 
                    filename=file.path(dirPH3LCBD, paste("ph3_lcbd_", l, ".csv", sep="")))
      }
      # LCBD display
      lcbdGraph(resLCBD, station="all", years=min(datYearly$year):max(datYearly$year), 
                mainTitle=paste("PH3 Indicator - Evaluation ", nLower, "-", nUpper, sep=""), 
                dir=dirPH3LCBD)
      
      # IVI indices computation
      dirPH3IVI <- file.path(dirPH3, "PH3 IVI")
      dir.create(dirPH3IVI)
      resIVI <<- iviIndex(datMonthly, resLCBD, resLCBDPerm, nb=PH3settings$nbivi, 
                          signif.test=PH3settings$lcbdtest, taxa)
      iviGraph(resIVI, datMonthly, station="all", nb=PH3settings$nbivi,
               mainTitle=paste("Indicateur PH3 - Evaluation ", nLower, "-", nUpper, sep=""), 
               dir=dirPH3IVI)
      for (l in names(resIVI))
        saveResults(resIVI[[l]], col.names = FALSE, 
                    filename=file.path(dirPH3IVI, paste("ph3_ivi_", l, ".csv", sep="")))
      
      if (length(resIVI) == 0)
        tkmessageBox(message="NO IVI COMPUTED...\n\nPlease check the significance test value in the PH3 settings!", 
                     icon="warning", type="ok")
    }
  })
  tkgrid(ph3Button, pady=c(0,10), row=8, column=1, columnspan=2)

  PH3settingsButton <- tk2button(msfdWindow, text="", image="settingsFile", 
                                 compound="center", width=2, state="disabled", 
                                 command=function() {
    PH3settings <<- PH3settingsWindow(nbMonthsVal=PH3settings$nbMonthsVal, nbMonths=PH3settings$nbMonths, 
                                      nbYearsVal=PH3settings$nbYearsVal, nbYears=PH3settings$nbYears, 
                                      nbMissing=PH3settings$nbMissing, genusVal=PH3settings$genusVal, 
                                      frequencyVal=PH3settings$frequencyVal, freqMode=PH3settings$freqMode,
                                      richnessVal=PH3settings$richnessVal, menhinickVal=PH3settings$menhinickVal, 
                                      hulburtVal=PH3settings$hulburtVal, giniVal=PH3settings$giniVal, 
                                      pattenVal=PH3settings$pattenVal, eqrVal=PH3settings$eqrVal, 
                                      lcbdiviVal=PH3settings$lcbdiviVal, lcbdperm=PH3settings$lcbdperm, 
                                      nbivi=PH3settings$nbivi, lcbdtest=PH3settings$lcbdtest, 
                                      samplingVal=PH3settings$samplingVal, boxplotVal=PH3settings$boxplotVal, 
                                      contourVal=PH3settings$contourVal, indiceVal=PH3settings$indiceVal)
  })
  tkgrid(PH3settingsButton, pady=c(0,10), row=8, column=3, sticky="w")
  
  ####################################################
  ###   Button for 'Physico-Chemical parameters'   ###
  ####################################################
  pcButton <- tkbutton(msfdWindow, text="Physico-Chemical\nparameters", 
                       width=20, state="disabled", command=function() {
    # Physico-Chemical data importation
    cat("Physico-Chemical - Data importation... ")
    resDir <<- dirname(datafile)
    dfr <<- NULL
    dfr <<- dataImport(datafile)
    dfr$values <- as.numeric(sub(",", ".", dfr$values, fixed=TRUE))
    if (length(which(is.na(dfr$values))) > 0)
      dfr <<- dfr[-which(is.na(dfr$values)), ]
    cat("Done!!!\n")
    
    datett <- tktoplevel()
    tktitle(datett) <- "Evaluation period selection"
    tkgrid(tk2label(datett, text="   "), row=1, sticky="w")
    tkgrid(tk2label(datett, text="Select the period of evaluation", justify="center"),
           row=2, column=1, columnspan=3)
    tkgrid(tk2label(datett, text="(all previous years will be used as reference)", 
                    justify="center"), row=3, column=1, columnspan=3)
    minVal <- as.numeric(min(datMonthly$year))
    maxVal <- as.numeric(max(datMonthly$year))
    
    tcl.lowerThreshold <<- tclVar(as.character(minVal))
    tk.lowerThreshold <<- tkentry(datett, textvariable=tcl.lowerThreshold, 
                                  width=8, state="normal", background="white")
    tkgrid(tk.lowerThreshold, row=4, column=1)
    tkgrid(tk2label(datett, text=" < X < "), row=4, column=2)
    tcl.upperThreshold <<- tclVar(as.character(maxVal))
    tk.upperThreshold <<- tkentry(datett, textvariable=tcl.upperThreshold, 
                                  width=8, state="normal", background="white")
    tkgrid(tk.upperThreshold, row=4, column=3)
    tkgrid(tk2label(datett, text="   "), row=5, column=4)
    
    onOK <- function() {
      nLower <<- as.integer(tclvalue(tcl.lowerThreshold))
      nUpper <<- as.integer(tclvalue(tcl.upperThreshold))
      tkdestroy(datett)
    }
    butOK <- tk2button(datett, text="OK", width=-6, command=onOK)
    tkgrid(butOK, row=5, column=2)
    tkwait.window(datett)
    
    # Result folders
    dirPC <- file.path(resDir, "Physico-Chemical")
    dir.create(dirPC)
    # PC parameters computation
    cat("Physico-Chemical - Parameters computation... ")
    datMonthly <<- pcMonthlyPool(dfr, func=mean)
    datMonthly <<- datMonthly[format(datMonthly$date, "%Y") >= nLower & 
                                format(datMonthly$date, "%Y") <= nUpper, ]
    datMonthlyToSave <- datMonthly
    datMonthlyToSave$month <- format(datMonthlyToSave$date, "%m")
    datMonthlyToSave$year <- format(datMonthlyToSave$date, "%Y")
    datMonthlyToSave <- datMonthlyToSave[c("station","param","month","year","values")]
    saveResults(datMonthlyToSave, row.names = FALSE, 
                filename=file.path(dirPC, "pc_pool_monthly.csv"))
    
    plotPC(datMonthly, station="all", dir=dirPC)
    datMeanMonth <- meanPC(datMonthly, mode="month")
    datMeanYear <- meanPC(datMonthly, mode="year")
    saveResults(datMeanMonth, row.names = FALSE,
                filename=file.path(dirPC, "pc_mean_monthly.csv"))
    saveResults(datMeanYear, row.names = FALSE,
                filename=file.path(dirPC, "pc_mean_yearly.csv"))
    
    # Boxplots display
    dirPCMonth <- file.path(dirPC, "PC Boxplots month")
    dir.create(dirPCMonth)
    dirPCYear <- file.path(dirPC, "PC Boxplots year")
    dir.create(dirPCYear)
    pcBoxplot(datMonthly, station="all", mode="month", dir=dirPCMonth)
    pcBoxplot(datMonthly, station="all", mode="year", dir=dirPCYear)
    cat("Done!!!\n")
  })
  tkgrid(pcButton, pady=c(0,10), row=9, column=1, columnspan=2)
  
  tkgrid(tklabel(msfdWindow, text="PELAGIC HABITAT AND FOOD-WEBS", 
                 font=fontTitle, foreground="dodgerblue4"), padx=c(40,40), 
         row=0, column=0, rowspan=2, columnspan=4)
  l <- ttklabel(msfdWindow, image="msfd", compound="image")
  tkgrid(l, row=11, column=0, columnspan=2, rowspan=2, padx=c(40,0))
  l <- ttklabel(msfdWindow, image="ospar", compound="image")
  tkgrid(l, row=11, column=2, columnspan=2, rowspan=2, padx=c(0,40))
}