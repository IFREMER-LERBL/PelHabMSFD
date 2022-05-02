rm(list = ls(all=TRUE))
#_____________________________________________________________ Required packages
#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_161/")
packages.list <- c("adespatial","corrplot","ggplot2","gplots","jpeg","pastecs",
                   "svMisc","tcltk","tcltk2","testit","vegan","viridis")
for (p in packages.list)
  require(p, character.only=T)
rm(list=c("packages.list","p"))

#_________________________________________________________________Source r-files
source("D1_MSFD.R") # Graphical User Interface
source("dataImport.R") # Data importation/transformation
source("PH2_settings.R") # PH2 indicator settings
source("PH3_settings.R") # PH3 indicator settings
source("PH2_indicator.R") # PH2 indicator computation
source("PH3_indicator.R") # PH3 indicator computation
source("PC_parameters.R") # Physico-Chemical parameters

#____________________________________________________________Display information
cat("Execute 'D1_MSFD()' function to launch GUI...\n")
cat("Warning: PH1 indicator not developed...!\n\n")

cat("PH2: CSV file (separator `;` and decimal `,`)\n")
cat("First column --> `station` (character format)\n")
cat("Second column --> `date` (YYYY-MM-DD format)\n")
cat("Third column --> `analyst` (character format)\n")
cat("Fourth column --> `sampling` (character format)\n")
cat("Fifth column --> `measure` (character format)\n")
cat("Sixth column --> `values` (numeric format)\n\n")

cat("PH3: CSV file (separator `;` and decimal `,`)\n")
cat("First column --> `station` (character format)\n")
cat("Second column --> `day` (numeric format)\n")
cat("Third column --> `month` (numeric format)\n")
cat("Fourth column --> `year` (numeric format)\n")
cat("Fifth column --> `longitude` (numeric format)\n")
cat("Sixth column --> `latitude` (numeric format)\n")
cat("Seventh column --> `analyst` (character format)\n")
cat("Eighth column --> `sampling` (character format)\n")
cat("Ninth column --> `measure` (character format)\n")
cat("Tenth column --> `taxon` (character format)\n")
cat("Eleventh column --> `abundance` (numeric format)\n\n")

cat("Physico-Chemical: CSV file (separator `;` and decimal `,`)\n")
cat("First column --> `station` (character format)\n")
cat("Second column --> `date` (YYYY-MM-DD format)\n")
cat("Third column --> `analyst` (character format)\n")
cat("Fourth column --> `sampling` (character format)\n")
cat("Fifth column --> `measure` (character format)\n")
cat("Sixth column --> `param` (character format)\n")
cat("Seventh column --> `values` (numeric format)\n\n")