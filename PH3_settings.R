# MSFD: indicators computation.
#
# Copyright 2018 Guillaume Wacquet, Duflos Marie, Mialet Benoit, Rombouts Isabelle
#                
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

#' PH3settingsWindow displays a window to set the PH3 computation and graphical outputs
#' @title Window for PH3 parameters setting
#' @description Display a window to set the PH3 computation and graphical outputs.
#' @import tcltk tcltk2 
#' @importFrom tkrplot tkrplot
#'
PH3settingsWindow <- function(nbMonthsVal="1", nbMonths=6, nbYearsVal="1", 
                              nbYears=5, nbMissing=10, genusVal="0",
                              frequencyVal="0", freqMode="Mean", richnessVal="0", 
                              menhinickVal="1", hulburtVal="1", giniVal="1", 
                              pattenVal="1", eqrVal="0", lcbdiviVal="1", 
                              lcbdperm=999, nbivi=5, lcbdtest=0.05, samplingVal="0", 
                              boxplotVal="1", contourVal="1", indiceVal="0") {
  # Create new window for plot configuration
  tt <- tktoplevel()
  tktitle(tt) <- "PH3 - Advanced settings"
  
  ##### Preprocessing options
  fontTitle <- tkfont.create(family="Arial", size=12, weight="bold")
  tkgrid(tk2label(tt, text="PREPROCESSING (CLEANING)", font=fontTitle), 
         row=1, sticky="w")
  
  # Minimal number of months in one year
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
  tkgrid(tt$env$nbMonths, padx=20, pady=5, row=2, sticky="w")
  tkgrid(tk.nbMonths, padx=5, pady=5, row=2, column=1, sticky="w")
  
  # Minimal number of years
  nbYears.check <- nbYearsVal=="1"
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
  tkgrid(tt$env$nbYears, padx=20, pady=5, row=3, sticky="w")
  tkgrid(tk.nbYears, padx=5, pady=5, row=3, column=1, sticky="w")
  
  # # Maximal number of missing years
  # tt$env$nbMissing <- tk2checkbutton(tt, text="Maximal number of missing years")
  # nbMissingValue <- tclVar("1")
  # tkconfigure(tt$env$nbMissing, variable=nbMissingValue)
  # tkgrid(tt$env$nbMissing, padx=20, pady=5, row=4, sticky="w")
  # nbMissing <<- nbMissing
  # tcl.nbMissing <- tclVar(as.character(nbMissing))
  # tk.nbMissing <- tkentry(tt, textvariable=tcl.nbMissing, width=2, background="white")
  # tkgrid(tk.nbMissing, padx=5, row=4, column=1, sticky="w")
  
  # # Grouping taxa to the genus
  # tt$env$genus <- tk2checkbutton(tt, text="Grouping taxa to the genus")
  # genusValue <- tclVar(genusVal)
  # tkconfigure(tt$env$genus, variable=genusValue)
  # tkgrid(tt$env$genus, padx=20, pady=5, row=5, sticky="w")
  
  # If data frequency upper than monthly
  frequency.check <- frequencyVal=="1"
  freq <- tclVar(freqMode)
  # Check frequency
  OnFrequencyCheck <- function() {
    frequency.check <<- tclvalue(tcl.frequency.check)=="1"
    if (frequency.check) {
      tkconfigure(tt$env$rb_mean, state="normal")
      tkconfigure(tt$env$rb_random, state="normal")
    } else {
      tkconfigure(tt$env$rb_mean, state="disabled")
      tkconfigure(tt$env$rb_random, state="disabled")
    }
  }
  # Mean of monthly pool
  tt$env$rb_mean <- tkradiobutton(tt, variable=freq, 
                                  value="Mean", text="Monthly mean")
  if (frequency.check) {
    tkconfigure(tt$env$rb_mean, state="normal")
  } else tkconfigure(tt$env$rb_mean, state="disabled")
  # Sum of monthly pool
  tt$env$rb_sum <- tkradiobutton(tt, variable=freq, 
                                 value="Sum", text="Monthly sum")
  if (frequency.check) {
    tkconfigure(tt$env$rb_sum, state="normal")
  } else tkconfigure(tt$env$rb_sum, state="disabled")
  # Random selection
  tt$env$rb_random <- tkradiobutton(tt, variable=freq, 
                                    value="Random", text="Random selection")
  if (frequency.check) {
    tkconfigure(tt$env$rb_random, state="normal")
  } else tkconfigure(tt$env$rb_random, state="disabled")
  tcl.frequency.check <- tclVar(as.character(as.integer(frequency.check)))
  tt$env$frequency <- tk2checkbutton(tt, text="If data frequency upper than monthly, compute:", 
                                     variable=tcl.frequency.check, command=OnFrequencyCheck)
  tkgrid(tt$env$frequency, padx=20, pady=5, row=6, sticky="w")
  tkgrid(tt$env$rb_mean, padx=5, row=6, column=1, sticky="w")
  tkgrid(tt$env$rb_sum, padx=5, row=7, column=1, sticky="w")
  tkgrid(tt$env$rb_random, padx=5, pady=5, row=8, column=1, sticky="w")

  ##### Indicator computation options
  tkgrid(tk2label(tt, text="INDICATOR COMPUTATION", font=fontTitle), 
         row=9, sticky="w")
  
  # richness
  tt$env$richness <- tk2checkbutton(tt, text="Richness index")
  richnessValue <- tclVar(richnessVal)
  tkconfigure(tt$env$richness, variable=richnessValue)
  tkgrid(tt$env$richness, padx=20, pady=5, row=10, sticky="w")
  
  # menhinick
  tt$env$menhinick <- tk2checkbutton(tt, text="Menhinick index")
  menhinickValue <- tclVar(menhinickVal)
  tkconfigure(tt$env$menhinick, variable=menhinickValue)
  tkgrid(tt$env$menhinick, padx=5, pady=5, row=10, column=1, sticky="w")
  
  # hulburt
  tt$env$hulburt <- tk2checkbutton(tt, text="Hulburt index")
  hulburtValue <- tclVar(hulburtVal)
  tkconfigure(tt$env$hulburt, variable=hulburtValue)
  tkgrid(tt$env$hulburt, padx=20, pady=5, row=11, sticky="w")
  
  # gini
  tt$env$gini <- tk2checkbutton(tt, text="Gini index")
  giniValue <- tclVar(giniVal)
  tkconfigure(tt$env$gini, variable=giniValue)
  tkgrid(tt$env$gini, padx=5, pady=5, row=11, column=1, sticky="w")
  
  # patten
  tt$env$patten <- tk2checkbutton(tt, text="Patten index")
  pattenValue <- tclVar(pattenVal)
  tkconfigure(tt$env$patten, variable=pattenValue)
  tkgrid(tt$env$patten, padx=20, pady=5, row=12, sticky="w")
  
  # eqr
  tt$env$eqr <- tk2checkbutton(tt, text="EQR index")
  eqrValue <- tclVar(eqrVal)
  tkconfigure(tt$env$eqr, variable=eqrValue)
  tkgrid(tt$env$eqr, padx=5, pady=5, row=12, column=1, sticky="w")
  
  # lcbd_ivi
  lcbdivi.check <- lcbdiviVal=="1"
  #lcbdperm <- lcbdperm
  #nbivi <- nbivi
  #lcbdtest <- lcbdtest
  # Check lcbd_ivi button
  OnlcbdiviCheck <- function() {
    lcbdivi.check <<- tclvalue(tcl.lcbdivi.check)=="1"
    if (lcbdivi.check) {
      tkconfigure(tk.lcbdperm, state="normal")
      tkconfigure(tk.nbivi, state="normal")
      tkconfigure(tk.lcbdtest, state="normal")
    } else {
      tkconfigure(tk.lcbdperm, state="disabled")
      tkconfigure(tk.nbivi, state="disabled")
      tkconfigure(tk.lcbdtest, state="disabled")
    }
  }
  tcl.lcbdivi.check <- tclVar(as.character(as.integer(lcbdivi.check)))
  tt$env$lcbdivi <- tk2checkbutton(tt, text="LCBD and IVI indices",
                                   variable=tcl.lcbdivi.check, command=OnlcbdiviCheck)
  tcl.lcbdperm <- tclVar(as.character(lcbdperm))
  tk.lcbdperm <- tkentry(tt, textvariable=tcl.lcbdperm, width=4, background="white")
  if (lcbdivi.check) {
    tkconfigure(tk.lcbdperm, state="normal")
  } else tkconfigure(tk.lcbdperm, state="disabled")
  tcl.nbivi <- tclVar(as.character(nbivi))
  tk.nbivi <- tkentry(tt, textvariable=tcl.nbivi, width=4, background="white")
  if (lcbdivi.check) {
    tkconfigure(tk.nbivi, state="normal")
  } else tkconfigure(tk.nbivi, state="disabled")
  tcl.lcbdtest <- tclVar(as.character(lcbdtest))
  tk.lcbdtest <- tkentry(tt, textvariable=tcl.lcbdtest, width=4, background="white")
  if (lcbdivi.check) {
    tkconfigure(tk.lcbdtest, state="normal")
  } else tkconfigure(tk.lcbdtest, state="disabled")
  tkgrid(tt$env$lcbdivi, padx=20, pady=5, row=13, sticky="w")
  tkgrid(tk2label(tt, text="Nb permutations:"), padx=5, row=13, column=1, sticky="w")
  tkgrid(tk.lcbdperm, padx=5, pady=5, row=13, column=2, sticky="w")
  tkgrid(tk2label(tt, text="Nb taxa for IVI:"), padx=5, row=14, column=1, sticky="w")
  tkgrid(tk.nbivi, padx=5, pady=5, row=14, column=2, sticky="w")
  tkgrid(tk2label(tt, text="Significance test:"), padx=5, row=15, column=1, sticky="w")
  tkgrid(tk.lcbdtest, padx=5, pady=5, row=15, column=2, sticky="w")
  
  ##### Graphical outputs options
  tkgrid(tk2label(tt, text="GRAPHICAL OUTPUTS", font=fontTitle), 
         row=16, sticky="w")
  
  tt$env$sampling <- tk2checkbutton(tt, text="Sampling matrix")
  samplingValue <- tclVar(samplingVal)
  tkconfigure(tt$env$sampling, variable=samplingValue)
  tkgrid(tt$env$sampling, padx=20, pady=5, row=17, sticky="w")
  
  tt$env$boxplot <- tk2checkbutton(tt, text="Indices boxplots")
  boxplotValue <- tclVar(boxplotVal)
  tkconfigure(tt$env$boxplot, variable=boxplotValue)
  tkgrid(tt$env$boxplot, padx=5, pady=5, row=17, column=1, sticky="w")
  
  tt$env$contour <- tk2checkbutton(tt, text="Indices contours")
  contourValue <- tclVar(contourVal)
  tkconfigure(tt$env$contour, variable=contourValue)
  tkgrid(tt$env$contour, padx=20, pady=5, row=18, sticky="w")
  
  tt$env$indice <- tk2checkbutton(tt, text="Indices evolution")
  indiceValue <- tclVar(indiceVal)
  tkconfigure(tt$env$indice, variable=indiceValue)
  tkgrid(tt$env$indice, padx=5, pady=5, row=18, column=1, sticky="w")
  
  onOK <- function() {
    ##### Preprocessing
    nbMonthsVal <<- as.character(tclvalue(tcl.nbMonths.check))
    nbMonths <<- as.integer(tclvalue(tcl.nbMonths))
    nbYearsVal <<- as.character(tclvalue(tcl.nbYears.check))
    nbYears <<- as.integer(tclvalue(tcl.nbYears))
    #nbMissing <<- as.integer(tclvalue(tcl.nbMissing))
    #genusVal <<- as.character(tclvalue(genusValue))
    frequencyVal <<- as.character(tclvalue(tcl.frequency.check))
    freqMode <<- as.character(tclvalue(freq))

    ##### Computation
    richnessVal <<- as.character(tclvalue(richnessValue))
    menhinickVal <<- as.character(tclvalue(menhinickValue))
    hulburtVal <<- as.character(tclvalue(hulburtValue))
    giniVal <<- as.character(tclvalue(giniValue))
    pattenVal <<- as.character(tclvalue(pattenValue))
    eqrVal <<- as.character(tclvalue(eqrValue))
    lcbdiviVal <<- as.character(tclvalue(tcl.lcbdivi.check))
    lcbdperm <<- as.integer(tclvalue(tcl.lcbdperm))
    nbivi <<- as.integer(tclvalue(tcl.nbivi))
    lcbdtest <<- as.numeric(tclvalue(tcl.lcbdtest))
    
    ##### Graphical outputs
    samplingVal <<- as.character(tclvalue(samplingValue))
    boxplotVal <<- as.character(tclvalue(boxplotValue))
    contourVal <<- as.character(tclvalue(contourValue))
    indiceVal <<- as.character(tclvalue(indiceValue))
    
    tkdestroy(tt)
  }
  tt$env$butOK <- tk2button(tt, text="OK", width=-6, command=onOK)
  tkgrid(tt$env$butOK, padx=10, columnspan=2, pady=c(0,15))
  tkwait.window(tt)
  
  return(list(nbMonthsVal=nbMonthsVal, nbMonths=nbMonths, nbYearsVal=nbYearsVal, 
              nbYears=nbYears, nbMissing=nbMissing, genusVal=genusVal, 
              frequencyVal=frequencyVal, freqMode=freqMode, richnessVal=richnessVal, 
              menhinickVal=menhinickVal, hulburtVal=hulburtVal, giniVal=giniVal, 
              pattenVal=pattenVal, eqrVal=eqrVal, lcbdiviVal=lcbdiviVal, 
              lcbdperm=lcbdperm, nbivi=nbivi, lcbdtest=lcbdtest, 
              samplingVal=samplingVal, boxplotVal=boxplotVal, 
              contourVal=contourVal, indiceVal=indiceVal))
}