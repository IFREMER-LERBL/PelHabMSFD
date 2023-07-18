# PelHabMSFD
GUI for computation of PH2 and PH3 indicators in the frame of MSFD, Descriptor 1: "Pelagic Habitat" 

## Installation

**R environment**

The version 1.0 of the PelHabMSFD package needs a R version >= 3.5.0. It can be directly downloaded on the CRAN website.

The R-packages needed by PelHabMSFD are: adespatial, corrplot, ggplot2, gplots, jpeg, pastecs, svMisc, tcltk, tcltk2, testit, vegan, viridis.

**PelHabMSFD installation**

PelHabMSFD is an R-package which can be downloaded (see: Releases/v1.0 in this repository) and installed directly from the R console with the following command line: 

*install.packages("/.../PelHabMSFD_1.0.tar.gz", repos = NULL, type = "source")*

## Graphical User Interface

To launch the Graphical User Interface of EcoTransLearn, run these command lines in the R console:

*library(PelHabMSFD)*

*D1_MSFD()*

![image](https://github.com/IFREMER-LERBL/D1_MSFD/assets/104447521/85458cf1-d34c-4ec0-ac76-c435d9f5f6b5)

## Data format

Data format for PH2 and PH3 indices computation (D1 MSFD: Pelagic Habitat).

PH1:	Not developed

PH2:	CSV file (separator ';' and decimal ',')
	Filename must contain 'PHYTO' or 'ZOO'
	First column --> "station" (character format) without `'`
	Second column --> "date" (yyyy-mm-dd format)
	Third column --> "values" (numeric format)
Data cleaning:	Remove years with less than 9 months
		Remove years before year missing
		"values"=NA if the begininng month is not "O1"

PH3:	CSV file (separator ';' and decimal ',')
	First column --> "station" (character format)
	Second column --> "day" (numeric format)
	Third column --> "month" (numeric format)
	Fourth column --> "year" (numeric format)
	Fifth column --> "longitude" (numeric format)
	Sixth column --> "latitude" (numeric format)
	Seventh column --> "taxon" (character format)
	Eighth column --> "abundance" (numeric format)
Data cleaning:	Remove years with less than 6 months
		Remove stations with less than 5 years

Physico-Chemical:	CSV file (separator ';' and decimal ',')
			First column --> "station" (character format) without `'`
			Second column --> "date" (yyyy-mm-dd format)
			Third column --> "param" (character format)
			Fourth column --> "values" (numeric format)

## Authors

Guillaume Wacquet, Anaïs Aubert, Alexandre Budria, Marie Duflos, Benoit Mialet, Isabelle Rombouts, Luis Felipe Artigas
