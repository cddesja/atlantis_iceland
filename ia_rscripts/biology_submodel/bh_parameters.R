#
# Beverton Holt
# 

#
# Preamble - must run
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

# Assume natural mortality of .2
M = .2

# --------------------------------------------------------------------- #

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
recrwt = as.numeric(as.character(fgr_ia[43,3]))
bh_atl(R = R,S = S,recrwt = recrwt,plot = TRUE)
K0<-min(S)/2
alpha0<-mean(R)/K0

#
# Saithe
#
saithe <- read.csv("http://data.hafro.is/assmt/2014/saithe/summary.csv", header = T)
saithe <- na.omit(saithe)
fsa_ia <- read.xls(xls = ia_path, sheet = "FSA", blank.lines.skip = FALSE)
S <- saithe$SSB
R <- saithe$Recruitment
recrwt = fsa_ia[43,3]
bh_atl(R = saithe$Recruitment, S = saithe$SSB, recrwt = as.numeric(as.character(recrwt)))  # need recruitment weight first

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
fan <- fan[10:17,]
#fan <- subset(fan, fan$tots > quantile(fan$tots,prob = c(.05,.95))[1] & fan$tots < quantile(fan$tots,prob = c(.05,.95))[2])

# From iabioparams.xls
options(scipen = 0)
M = .2
fan_ia <- read.xls(xls = ia_path, sheet = "FAN", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fan_ia[19,2]))
age1 <- as.numeric(as.character(fan_ia[44,9]))*(fan$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fan_ia[43,3]))
ssb <- sum(as.numeric(as.character(fan_ia[46:53,9])))*fan$tots
ssb <- ssb[-8]
K0<-min(ssb)/2
alpha0<-mean(rec)/K0
alpha0*K0*1e6/recrwt  ## alpha
tons_mgN(K0,1)
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FGR
#
fgr <- filter(yr_biom, tegund == 5)
fgr <- fgr[17:25,]

# From iabioparams.xls
M = .2
fgr_ia <- read.xls(xls = ia_path, sheet = "FGR", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fgr_ia[19,2]))
age1 <- as.numeric(as.character(fgr_ia[44,9]))*(fgr$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fgr_ia[43,3]))
ssb <- sum(as.numeric(as.character(fgr_ia[46:53,9])))*fgr$tots
ssb <- ssb[-length(ssb)]
K0<-min(ssb)/2
alpha0<-mean(rec)/K0
options(scipen = -10)
alpha0*K0*1e6/recrwt  ## alpha
tons_mgN(K0,1)
options(scipen = 0)
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

# FDR
fdr <- filter(yr_biom, tegund == 61)
fdr <- fdr[11:22,]
# From iabioparams.xls
M = .2
fdr_ia <- read.xls(xls = ia_path, sheet = "FDR", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fdr_ia[19,2]))
age1 <- as.numeric(as.character(fdr_ia[44,9]))*(fdr$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fdr_ia[43,3]))
ssb <- sum(as.numeric(as.character(fdr_ia[46:53,9])))*fdr$tots
ssb <- ssb[-length(ssb)]
K0<-min(ssb)/2
alpha0<-mean(rec)/K0
options(scipen = -10)
alpha0*K0*1e6/recrwt  ## alpha
tons_mgN(K0,1)
options(scipen = 0)
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FGH
#
fgh <- filter(yr_biom, tegund == 22)
fgh <- fgh[10:20,]
# From iabioparams.xls
M = .2
fgh_ia <- read.xls(xls = ia_path, sheet = "FGH", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fgh_ia[19,2]))
age1 <- as.numeric(as.character(fgh_ia[44,9]))*(fgh$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fgh_ia[43,3]))
ssb <- sum(as.numeric(as.character(fgh_ia[47:53,9])))*fgh$tots
ssb <- ssb[-length(ssb)]
K0<-min(ssb)/2
alpha0<-mean(rec)/K0
options(scipen = -10)
alpha0*K0*1e6/recrwt  ## alpha
tons_mgN(K0,1)
options(scipen = 0)
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FPL
#
fpl <- filter(yr_biom, tegund == 23)
fpl <- fpl[12:22,]
# From iabioparams.xls
fpl_ia <- read.xls(xls = ia_path, sheet = "FPL", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fpl_ia[19,2]))
age1 <- as.numeric(as.character(fpl_ia[44,9]))*(fpl$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fpl_ia[43,3]))
ssb <- sum(as.numeric(as.character(fpl_ia[47:53,9])))*fpl$tots
ssb <- ssb[-length(ssb)]
K0<-min(ssb)/2
alpha0<-mean(rec)/K0
options(scipen = -10)
alpha0*K0*1e6/recrwt  ## alpha
tons_mgN(K0,1)
options(scipen = 0)
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FDA
#
fda <- filter(yr_biom, tegund == 27)
fda <- fda[9:19,]

# From iabioparams.xls
fda_ia <- read.xls(xls = ia_path, sheet = "FDA", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fda_ia[19,2]))
age1 <- as.numeric(as.character(fda_ia[44,9]))*(fda$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fda_ia[43,3]))
ssb <- sum(as.numeric(as.character(fda_ia[47:53,9])))*fda$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FLR
#
flr <- filter(yr_biom, tegund == 28)
flr <- flr[10:22,]
# From iabioparams.xls
flr_ia <- read.xls(xls = ia_path, sheet = "FLR", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(flr_ia[19,2]))
age1 <- as.numeric(as.character(flr_ia[44,9]))*(flr$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(flr_ia[43,3]))
ssb <- sum(as.numeric(as.character(flr_ia[47:53,9])))*flr$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FLS
#
fls <- filter(yr_biom, tegund == 24)
fls <- fls[10:20,]
# From iabioparams.xls
fls_ia <- read.xls(xls = ia_path, sheet = "FLS", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fls_ia[19,2]))
age1 <- as.numeric(as.character(fls_ia[44,9]))*(fls$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fls_ia[43,3]))
ssb <- sum(as.numeric(as.character(fls_ia[47:53,9])))*fls$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FAW
#
faw <- filter(yr_biom, tegund == 9)
faw <- faw[9:21,]
# From iabioparams.xls
faw_ia <- read.xls(xls = ia_path, sheet = "FAW", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(faw_ia[19,2]))
age1 <- as.numeric(as.character(faw_ia[44,9]))*(faw$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(faw_ia[43,3]))
ssb <- sum(as.numeric(as.character(faw_ia[47:53,9])))*faw$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FHE
#
fhe <- filter(yr_biom, tegund == 30)
fhe <- fhe[c(9:15),]
# From iabioparams.xls
fhe_ia <- read.xls(xls = ia_path, sheet = "FHE", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fhe_ia[19,2]))
age1 <- as.numeric(as.character(fhe_ia[44,9]))*(fhe$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fhe_ia[43,3]))
ssb <- sum(as.numeric(as.character(fhe_ia[47:53,9])))*fhe$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FCA
#
fca <- filter(yr_biom, tegund == 31)
fca <- fca[c(11:19),]
# From iabioparams.xls
fca_ia <- read.xls(xls = ia_path, sheet = "FCA", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fca_ia[19,2]))
age1 <- as.numeric(as.character(fca_ia[44,9]))*(fca$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
rec <- rec[-c(2,5)]
recrwt = as.numeric(as.character(fca_ia[43,3]))
ssb <- sum(as.numeric(as.character(fca_ia[47:53,9])))*fca$tots
ssb <- ssb[-length(ssb)]
ssb <- ssb[-c(2,5)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FBW
#
fbw <- filter(yr_biom, tegund == 34)
fbw <- fbw[c(7:18),]
# From iabioparams.xls
fbw_ia <- read.xls(xls = ia_path, sheet = "FBW", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fbw_ia[19,2]))
age1 <- as.numeric(as.character(fbw_ia[44,9]))*(fbw$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fbw_ia[43,3]))
ssb <- sum(as.numeric(as.character(fbw_ia[47:53,9])))*fbw$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FMA
#
fma <- filter(yr_biom, tegund == 36)
fma <- fma[c(3:9),]
# From iabioparams.xls
fma_ia <- read.xls(xls = ia_path, sheet = "FMA", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fma_ia[19,2]))
age1 <- as.numeric(as.character(fma_ia[44,9]))*(fma$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fma_ia[43,3]))
ssb <- sum(as.numeric(as.character(fma_ia[47:53,9])))*fma$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FBL
#
fbl <- filter(yr_biom, tegund == 7)
fbl <- fbl[c(5:17),]
# From iabioparams.xls
fbl_ia <- read.xls(xls = ia_path, sheet = "FBL", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fbl_ia[19,2]))
age1 <- as.numeric(as.character(fbl_ia[44,9]))*(fbl$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fbl_ia[43,3]))
ssb <- sum(as.numeric(as.character(fbl_ia[47:53,9])))*fbl$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FLI
#
fli <- filter(yr_biom, tegund == 6)
fli <- fli[c(7:17),]
# From iabioparams.xls
fli_ia <- read.xls(xls = ia_path, sheet = "FLI", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fli_ia[19,2]))
age1 <- as.numeric(as.character(fli_ia[44,9]))*(fli$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fli_ia[43,3]))
ssb <- sum(as.numeric(as.character(fli_ia[47:53,9])))*fli$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FTI
#
fti <- filter(yr_biom, tegund == 8)
fti <- fti[c(8:19),]
# From iabioparams.xls
fti_ia <- read.xls(xls = ia_path, sheet = "FTI", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fti_ia[19,2]))
age1 <- as.numeric(as.character(fti_ia[44,9]))*(fti$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fti_ia[43,3]))
ssb <- sum(as.numeric(as.character(fti_ia[47:53,9])))*fti$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FGA
#
fga <- filter(yr_biom, tegund == 19)
fga <- fga[c(4:18),]
# From iabioparams.xls
fga_ia <- read.xls(xls = ia_path, sheet = "FGA", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fga_ia[19,2]))
age1 <- as.numeric(as.character(fga_ia[44,9]))*(fga$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fga_ia[43,3]))
ssb <- sum(as.numeric(as.character(fga_ia[47:53,9])))*fga$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FFO
#
ffo_id <- c(25, 26, 527, 95, 21)
ffo <- filter(yr_biom, tegund %in% ffo_id)
ffo <- ffo %>%
  group_by(ar) %>%
  summarize(tots = mean(tots))
ffo <- ffo[c(10:20),]
# From iabioparams.xls
ffo_ia <- read.xls(xls = ia_path, sheet = "FFO", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(ffo_ia[19,2]))
age1 <- as.numeric(as.character(ffo_ia[44,9]))*(ffo$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(ffo_ia[43,3]))
ssb <- sum(as.numeric(as.character(ffo_ia[47:53,9])))*ffo$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FDF
#
fdf_id <- c(13, 53, 56, 57, 58, 60, 63, 65, 69, 71, 74, 76, 79, 88, 89, 94, 98, 166, 98, 170, 33)
fdf <- filter(yr_biom, tegund %in% fdf_id)
fdf <- fdf %>%
  group_by(ar) %>%
  summarize(tots = mean(tots))
fdf <- fdf[c(12:23),]
# From iabioparams.xls
fdf_ia <- read.xls(xls = ia_path, sheet = "FDF", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fdf_ia[19,2]))
age1 <- as.numeric(as.character(fdf_ia[44,9]))*(fdf$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fdf_ia[43,3]))
ssb <- sum(as.numeric(as.character(fdf_ia[47:53,9])))*fdf$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FDL
#
fdl_id <- c(39, 47, 62, 161)
fdl <- filter(yr_biom, tegund %in% fdl_id)
fdl <- fdl %>%
  group_by(ar) %>%
  summarize(tots = mean(tots))
fdl <- fdl[c(13:22),]
# From iabioparams.xls
fdl_ia <- read.xls(xls = ia_path, sheet = "FDL", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fdl_ia[19,2]))
age1 <- as.numeric(as.character(fdl_ia[44,9]))*(fdl$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fdl_ia[43,3]))
ssb <- sum(as.numeric(as.character(fdl_ia[47:53,9])))*fdl$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# PLD
#
pld_id <- c(35, 48, 83)
pld <- filter(yr_biom, tegund %in% pld_id)
pld <- pld %>%
  group_by(ar) %>%
  summarize(tots = mean(tots))
pld <- pld[c(8:20),]
# From iabioparams.xls
pld_ia <- read.xls(xls = ia_path, sheet = "PLD", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(pld_ia[19,2]))
age1 <- as.numeric(as.character(pld_ia[44,9]))*(pld$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(pld_ia[43,3]))
ssb <- sum(as.numeric(as.character(pld_ia[47:53,9])))*pld$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# PID
#
pid_id <- c(564, 225, 99, 97, 80, 4)
pid <- filter(yr_biom, tegund %in% pid_id)
pid <- pid %>%
  group_by(ar) %>%
  summarize(tots = mean(tots))
pid <- pid[c(7:17),]
# From iabioparams.xls
pid_ia <- read.xls(xls = ia_path, sheet = "PID", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(pid_ia[19,2]))
age1 <- as.numeric(as.character(pid_ia[44,9]))*(pid$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(pid_ia[43,3]))
ssb <- sum(as.numeric(as.character(pid_ia[47:53,9])))*pid$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FMP
#
fmp_id <- c(10, 37, 128, 155, 172, 173)
fmp <- filter(yr_biom, tegund %in% fmp_id)
fmp <- fmp %>%
  group_by(ar) %>%
  summarize(tots = mean(tots))
# From iabioparams.xls
fmp_ia <- read.xls(xls = ia_path, sheet = "FMP", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fmp_ia[19,2]))
age1 <- as.numeric(as.character(fmp_ia[44,9]))*(fmp$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fmp_ia[43,3]))
ssb <- sum(as.numeric(as.character(fmp_ia[47:53,9])))*fmp$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)

#
# FBP
#
fbp_id <- c(130, 148, 149, 209, 204, 232, 241, 243)
fbp <- filter(yr_biom, tegund %in% fbp_id)
fbp <- fbp %>%
  group_by(ar) %>%
  summarize(tots = mean(tots))
fbp <- fbp[5:16,]
# From iabioparams.xls
fbp_ia <- read.xls(xls = ia_path, sheet = "FBP", blank.lines.skip = FALSE)
ave_wt <- as.numeric(as.character(fbp_ia[19,2]))
age1 <- as.numeric(as.character(fbp_ia[44,9]))*(fbp$tots*1e6/ave_wt)
rec <- (age1/exp(-M)*ave_wt/1e6)[-1]
recrwt = as.numeric(as.character(fbp_ia[43,3]))
ssb <- sum(as.numeric(as.character(fbp_ia[47:53,9])))*fbp$tots
ssb <- ssb[-length(ssb)]
options(scipen = -10)
bh_atl(R = rec, S = ssb, recrwt = recrwt)
options(scipen = 0)
