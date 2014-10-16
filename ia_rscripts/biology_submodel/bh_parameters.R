#
# Beverton Holt
# 

# Packages
library("dplyr")
library("gdata")

# Set data directoy
data_dir <- "~/Dropbox/hi/atlantis/hafro_data/"

# Set working directory
setwd(data_dir)

# Path to IAbioparams.xls
ia_path <- "~/Dropbox/hi/atlantis/iceland_atlantis/helper_files/IAbioparams.xls"

# Path to Atlantis functions
source("~/Github/iceland_atlantis/ia_rscripts/atlantis_helpers.R")

#
# Cod
#
cod <- read.csv("http://data.hafro.is/assmt/2014/cod/summary.csv", header = T)
cod <- na.omit(cod)
S <- cod$SSB
R <- cod$Recruitment
bh_atl(R = R,S = S,recrwt = 0.501973812,plot = TRUE)

#
# Haddock
#
had <- read.csv("http://data.hafro.is/assmt/2014/haddock/summary.csv", header = T)
had <- had[-(which.max(had$Recruitment)),]
had <- na.omit(had)
S <- had$SSB
R <- had$Recruitment
bh_atl(R = R,S = S,recrwt = 0.99933498,plot = TRUE)

#
# Saithe
#
saithe <- read.csv("http://data.hafro.is/assmt/2014/saithe/summary.csv", header = T)
saithe <- na.omit(saithe)
S <- saithe$SSB
R <- saithe$Recruitment
bh_atl(R = saithe$Recruitment, S = saithe$SSB, recrwt = 0.99933498)  # need recruitment weight first



#
# All other fish species 
#
# Assume recruitment weights of .5

yr_biom <- readRDS("yearly_biomass.Rds")

# Drop þorskur, ýsa, ufsi
fish_w_rec <- 1:3
yr_biom <- filter(yr_biom, !(tegund %in% fish_w_rec)) 

# Anglerfish
fan <- filter(yr_biom, tegund == 14)
fan <- fan[4:16,]

# From iabioparams.xls
fan_ia <- read.xls(xls = ia_path, sheet = "FAN", blank.lines.skip = FALSE)
prop_spawn <- 1-as.numeric(as.character(fan_ia[44,9]))  ## average wt
spawners <- prop_spawn*fan$tots
young <- as.numeric(as.character(fan_ia[44,9]))*fan$tots

land <- read.csv("http://data.hafro.is/assmt/2014/anglerfish/landings.csv")
land <- land[37:49,]

F = .5
M = .2
Z = F + M
young_1 <- exp(-M)*young
n1 <- exp(-Z)*spawners
length(young_1)
length(n1)
spawn <- spawners[-1]
adv <- (young_1 + n1)[-length(n1)]
rec <- spawn - adv
data <- data.frame(spawn, rec)
plot(rec~spawn, data)
data <- data[data$rec > 0,]
S <- data$spawn
R <- data$rec
recrwt = fan_ia[43,3]
bh_atl(R = R,S = S,recrwt = .5,plot = TRUE)

#
# FGR
#
fgr <- filter(yr_biom, tegund == 5)
fgr <- fgr[12:25,]

# From iabioparams.xls
fgr_ia <- read.xls(xls = ia_path, sheet = "FGR", blank.lines.skip = FALSE)
prop_spawn <- 1-as.numeric(as.character(fgr_ia[44,9]))  ## average wt
spawners <- prop_spawn*fgr$tots
young <- as.numeric(as.character(fgr_ia[44,9]))*fgr$tots

F = .5
M = .2
Z = F + M
young_1 <- exp(-M)*young
n1 <- exp(-Z)*spawners
length(young_1)
length(n1)
spawn <- spawners[-1]
adv <- (young_1 + n1)[-length(n1)]
rec <- spawn - adv
data <- data.frame(spawn, rec)
plot(rec~spawn, data)
data <- data[data$rec > 0,]
data <- data[-c(5,7),]
S <- data$spawn
R <- data$rec
recrwt = fgr_ia[43,3]
bh_atl(R = R,S = S,recrwt = .5,plot = TRUE)
