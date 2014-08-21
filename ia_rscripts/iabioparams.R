#
# IAbioparams.R
# 

# Read in biology parameter prm
k_bio <- "~/Dropbox/hi/atlantis/iceland_atlantis/iceland_biol.prm"
bio_params <- readLines(k_bio)
write(bio_params, file = "~/Dropbox/hi/atlantis/iceland_atlantis/iceland_biol_backup.prm")

# Load packages
library("gdata")

# This script is a helper script to read in the IAbioparams.xls file
# and not rely on the macros.

# Name of spreadsheet
k_name <- "IAbioparams.xls"

# Working directory
k_wdir <- "~/Dropbox/hi/atlantis/iceland_atlantis/helper_files/"

# Read in codes from FunctionalGroup.csv
codes <- read.csv("~/Dropbox/hi/atlantis/iceland_atlantis/GroupsIceland.csv", header = T)[,1]

# Save only the verts
vert_codes <- codes[1:34]

# Need to omit FPO and FPI
vert_codes <- factor(vert_codes[-c(24, 25)])

# Read in all sheets
ia_sheets <- list()
for(i in 1:length(vert_codes)){
  ia_sheets[[i]] <- read.xls(xls = paste(k_wdir, k_name, sep = ""), sheet = vert_codes[i], blank.lines.skip = FALSE)
  cat(paste(vert_codes[i]), "is done\n")
}

# Give the list sensible names
names(ia_sheet) <- vert_codes

#
# mum
#

# If this is like the ECCAL.xls file then the first mum is located at [44,6]

for(i in 1:length(ia_sheets)){
  mum_calc <- ia_sheets[[i]][44:53,6]
  match <- grep(paste("mum_", vert_codes[i], " 10", sep = ""), bio_params)
  bio_params[match + 1] <- paste(mum_calc, collapse = " ")
}

#
# clearance rates
#

# If this is like the ECCAL.xls file then the first mum is located at [44,7]
for(i in 1:length(ia_sheets)){
  clear_calc <- ia_sheets[[i]][44:53,7]
  match <- grep(paste("C_", vert_codes[i], sep = ""),bio_params)
  bio_params[match + 1] <- paste(clear_calc, collapse = " ")
}

#
# assimilation efficiency 
#

for(i in 1:length(ia_sheets)){
  E_calc <- ia_sheets[[i]][15,7]
  match <- grep(paste("E_", vert_codes[i], sep = ""),bio_params)
  bio_params[match] <- paste("E_", vert_codes[i], "  ", E_calc, " Assimilation efficiency of ", vert_codes[i], sep = "")
}

#
# size of recruits, structural weight
#
for(i in 1:length(ia_sheets)){
  KWSR_calc <- ia_sheets[[i]][43,4]
  match <- grep(paste("KWSR_", vert_codes[i], sep = ""),bio_params)
  bio_params[match] <- paste("KWSR_", vert_codes[i], "  ", KWSR_calc, " Structural weight of ", vert_codes[i], " recruits mg N", sep = "")
}

#
# size of recruits, reserve weight
#

for(i in 1:length(ia_sheets)){
  KWRR_calc <- ia_sheets[[i]][43,5]
  match <- grep(paste("KWRR_", vert_codes[i], sep = ""),bio_params)
  bio_params[match] <- paste("KWRR_", vert_codes[i], "  ", KWRR_calc, " Reserve weight of ", vert_codes[i], " recruits mg N", sep = "")
}

#
# proportion spawning
#

# omit for FCD, FHA, and FSA
omitters <- c("FCD", "FHA", "FSA")
for(i in 1:length(ia_sheets)){
  amat <- ia_sheets[[i]][14,2]
  if(!(vert_codes[i] %in% omitters)){
    prop_spawn <- c(rep(0, as.numeric(paste(amat)) - 1), .1, .5, .9, rep(1, 10 - (as.numeric(paste(amat)) + 2)))
    match <- grep(paste("FSPB_", vert_codes[i], " 10", sep = ""), bio_params)
    bio_params[match + 1] <- paste(prop_spawn, collapse = " ")
  }
}

# Write changes to the updated bio params file
write(bio_params, file = k_bio)


