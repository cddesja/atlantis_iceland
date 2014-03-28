# This script is to generate the biol.prm file ----------------------
# 
# 26/03/2014, Christopher David Desjardins
# - Initial creation

# Load required packages  
library(ggplot2)

# Read in fishbase from Iceland
iceland_fdb <- read.csv(file = "Dropbox/HI/Research/IcelandicAtlantis/AtlantisFiles/fishbase/iceland_fishbase.csv",header=T)

# Clean up the fishbase data --------------------------------------------

# Remove the nextline and clean up variable names
iceland_fdb$Scientific.Name <- gsub("\n\n","",iceland.fdb$Scientific.Name)
colnames(iceland.fdb) <- gsub(".","",colnames(iceland.fdb),fixed=TRUE)
tmp <- gsub("+/- s.e.","",iceland.fdb$Trophiclevel)
tmp2 <- strsplit(tmp,split="\\+")
