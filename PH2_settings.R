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

#' PH2settingsWindow displays a window to set the PH2 computation and graphical outputs
#' @title Window for PH2 parameters setting
#' @description Display a window to set the PH2 computation and graphical outputs.
#' @import tcltk tcltk2 
#' @importFrom tkrplot tkrplot
#' 
PH2settingsWindow <- function(consistencyVal="0", nbMonthsVal="1", nbMonths=6, 
                              nbYearsVal="1", nbYears=5, samplingVal="0", 
                              anomaliesVal="1", cumsumVal="1", decompositionVal="1") {
  # Create new window for plot configuration
  tt <- tktoplevel()
  tktitle(tt) <- "PH2 - Advanced settings"

  # Preprocessing options
  fontTitle <- tkfont.create(family="Arial", size=12, weight="bold")
  tkgrid(tk2label(tt, text="PREPROCESSING (CLEANING)", font=fontTitle), 
         row=1, sticky="w")
  
  tt$env$consistency <- tk2checkbutton(tt, text="Consistency of time series")
  consistencyValue <- tclVar(consistencyVal)
  tkconfigure(tt$env$consistency, variable=consistencyValue)
  tkgrid(tt$env$consistency, padx=20, pady=5, row=2, sticky="w")
  
  # nbMonths
  nbMonths.check <- nbMonthsVal=="1"
  # Check nbMonths button
  OnnbMonthsCheck <- function() {
    nbMonths.check <<- tclvalue(tcl.nbMonths.check)=="1"
    if (nbMonths.check) {
      tkconfigure(tk.nbMonths, state="normal")
    } else {
      tkconfigure(tk.nbMonths, state="disabled")
    }
  }
  tcl.nbMonths.check <- tclVar(as.character(as.integer(nbMonths.check)))
  tt$env$nbMonths <- tk2checkbutton(tt, text="Minimal number of months in one year",
                                    variable=tcl.nbMonths.check, command=OnnbMonthsCheck)
  tcl.nbMonths <- tclVar(as.character(nbMonths))
  tk.nbMonths <- tkentry(tt, textvariable=tcl.nbMonths, width=2, background="white")
  if (nbMonths.check) {
    tkconfigure(tk.nbMonths, state="normal")
  } else tkconfigure(tk.nbMonths, state="disabled")
  tkgrid(tt$env$nbMonths, padx=20, pady=5, row=3, sticky="w")
  tkgrid(tk.nbMonths, padx=5, pady=5, row=3, column=1, sticky="w")
  
  # nbYears
  nbYears.check <- nbMonthsVal=="1"
  # Check nbYears button
  OnnbYearsCheck <- function() {
    nbYears.check <<- tclvalue(tcl.nbYears.check)=="1"
    if (nbYears.check) {
      tkconfigure(tk.nbYears, state="normal")
    } else {
      tkconfigure(tk.nbYears, state="disabled")
    }
  }
  tcl.nbYears.check <- tclVar(as.character(as.integer(nbYears.check)))
  tt$env$nbYears <- tk2checkbutton(tt, text="Minimal number of reference years",
                                    variable=tcl.nbYears.check, command=OnnbYearsCheck)
  tcl.nbYears <- tclVar(as.character(nbYears))
  tk.nbYears <- tkentry(tt, textvariable=tcl.nbYears, width=2, background="white")
  if (nbYears.check) {
    tkconfigure(tk.nbYears, state="normal")
  } else tkconfigure(tk.nbYears, state="disabled")
  tkgrid(tt$env$nbYears, padx=20, pady=5, row=4, sticky="w")
  tkgrid(tk.nbYears, padx=5, pady=5, row=4, column=1, sticky="w")
  
  ## Graphical outputs options
  tkgrid(tk2label(tt, text="GRAPHICAL OUTPUTS", font=fontTitle), 
         row=5, sticky="w")
  
  tt$env$sampling <- tk2checkbutton(tt, text="Sampling matrix")
  samplingValue <- tclVar(samplingVal)
  tkconfigure(tt$env$sampling, variable=samplingValue)
  tkgrid(tt$env$sampling, padx=20, pady=5, row=6, sticky="w")
  
  tt$env$anomalies <- tk2checkbutton(tt, text="Anomalies barplot + Cumulative sum")
  anomaliesValue <- tclVar(anomaliesVal)
  tkconfigure(tt$env$anomalies, variable=anomaliesValue)
  tkgrid(tt$env$anomalies, padx=20, pady=5, row=7, column=0, sticky="w")
  
  tt$env$decomposition <- tk2checkbutton(tt, text="Time series decomposition")
  decompositionValue <- tclVar(decompositionVal)
  tkconfigure(tt$env$decomposition, variable=decompositionValue)
  tkgrid(tt$env$decomposition, padx=20, pady=5, row=8, column=0, sticky="w")
  
  onOK <- function() {
    ## Preprocessing
    consistencyVal <<- as.character(tclvalue(consistencyValue))
    nbMonthsVal <<- as.character(tclvalue(tcl.nbMonths.check))
    nbMonths <<- as.integer(tclvalue(tcl.nbMonths))
    nbYearsVal <<- as.character(tclvalue(tcl.nbYears.check))
    nbYears <<- as.integer(tclvalue(tcl.nbYears))

    ## Graphical outputs
    samplingVal <<- as.character(tclvalue(samplingValue))
    anomaliesVal <<- as.character(tclvalue(anomaliesValue))
    decompositionVal <<- as.character(tclvalue(decompositionValue))
    
    tkdestroy(tt)
  }
  tt$env$butOK <- tk2button(tt, text="OK", width=-6, command=onOK)
  tkgrid(tt$env$butOK, padx=10, columnspan=2, pady=c(0,15))
  tkwait.window(tt)
  
  return(list(consistencyVal=consistencyVal, nbMonthsVal=nbMonthsVal, 
              nbMonths=nbMonths, nbYearsVal=nbYearsVal, nbYears=nbYears, 
              samplingVal=samplingVal, anomaliesVal=anomaliesVal, 
              cumsumVal=cumsumVal, decompositionVal=decompositionVal))
}