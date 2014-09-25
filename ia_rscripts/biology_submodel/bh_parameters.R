#
# Beverton Holt
# 

# Path to Atlantis functions
source("~/Github/iceland_atlantis/ia_rscripts/atlantis_helpers.R")

# Cod
cod <- read.csv("http://data.hafro.is/assmt/2014/cod/summary.csv", header = T)
cod <- na.omit(cod)
S <- cod$SSB
R <- cod$Recruitment
bh_atl(R = R,S = S,recrwt = 0.501973812,plot = TRUE)


# Haddock
had <- read.csv("http://data.hafro.is/assmt/2014/haddock/summary.csv", header = T)
#had <- had[-(which.max(had$Recruitment)),]
had <- na.omit(had)
S <- had$SSB
R <- had$Recruitment
bh_atl(R = R,S = S,recrwt = 0.99933498,plot = TRUE)

# Saithe
saithe <- read.csv("http://data.hafro.is/assmt/2014/saithe/summary.csv", header = T)
saithe <- na.omit(saithe)
S <- saithe$SSB
R <- saithe$Recruitment
bh_atl(R = saithe$Recruitment, S = saithe$SSB, recrwt = 0.99933498)  # need recruitment weight first
