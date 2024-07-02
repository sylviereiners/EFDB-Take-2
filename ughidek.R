## Test read in of new CSVs ##
trop_subtrop <- read.csv("/Users/sylviereiners/Desktop/R19table4.9_tropsubtrop.csv")
temp <- read.csv("/Users/sylviereiners/Desktop/R19table4.9_temp.csv")
boreal <- read.csv("/Users/sylviereiners/Desktop/R19table4.9_boreal.csv")
uncertainty <- read.csv("/Users/sylviereiners/Desktop/R19table4.9_uncertainty.csv")
references <- read.csv("/Users/sylviereiners/Desktop/R19table4.9_references.csv")
library(dplyr)
rowranges <- c(1:108)
trop_subtrop <- trop_subtrop %>%
                slice(rowranges)
unique_sources.all <- unique(CO2EFDB$Source.of.data)
source_matrix.all <- matrix(unique_sources.all)
sample_bound <- distinct(sample_bound)