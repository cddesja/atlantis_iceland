# Generate initial conditions
#
# This script will be used to generate the initial conditions for the 
# Iceland Atlantis model.

# Christopher David Desjardins

# Constants
kScriptDir <- "~/Dropbox/hi/atlantis/atlantis_r_scripts/"
kDataDir <- "~/Dropbox/hi/atlantis/hafro_data/"
kLandings <- "init_conditions/"

# Change working directory
setwd(kDataDir)

# Load appropriate library
library(fjolst)
library(dplyr)
library(reshape2)

# Load weight-length allometric and von-Bertalanffy parameters
load("vb_ab_params.Rdata")

# Source R scripts
source(paste(kScriptDir, "skalamedtoldum.R", sep = ""))  # Length Distributions

# Read in tegund CSV
fisk_tegund <- read.csv("fiskar_heiti.csv", header = TRUE)

# Keep only tegund and Atlantis codes
fiskur <- select(fisk_tegund, tegund, Code)  

# Convert stodvar to a tbl_df
stodvar_df <- tbl_df(stodvar)

# stodvar_df$stype <- ifelse(stodvar_df$synaflokkur == 30, "spring",
#                            ifelse(stodvar_df$synaflokkur == 35, "fall",
#                              "other"))

# Join stodvar with length and numbers 
le_yr <- inner_join(all.le,stodvar_df)
nu_yr <- inner_join(all.nu,stodvar_df)

# Unconditioned length distributions
unc_length <- Skala.med.toldum(le_yr,nu_yr)
length_red <- select(unc_length, synis.id, tegund, lat, lon, lengd,
                     fj.alls, ar, larett.opnun)
length_red <- filter(length_red, fj.alls > -1)
length_red <- filter(length_red, fj.alls != "Inf")
length_df <- tbl_df(length_red)

# Free up some RAM
rm(le_yr, nu_yr, unc_length, length_red, stodvar_df, fisk_tegund)

# Combine w-l & vB params with fiskur nafn
wvb_params <- merge(wvb.params, fiskur)

# Join the w-l and vb params with the length distribution data
length_abvb <- merge(length_df, wvb_params)
length_abvb <- merge(length_abvb, fiskur)


# Remove those where the width of the survey equipment is not known
length_abvb_nona <- filter(length_abvb, larett.opnun != "NA")

#
# Calculate abundance indices
#
abund_ind <-
  length_abvb %>%
  group_by(Code, ar) %>%
  do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))

# Select on long/lat and towing width
ll_tow <- select(length_abvb_nona, synis.id, ar, tegund, lat, lon, larett.opnun)

# Select only the unique cases
ll_tow_u <- unique(ll_tow)


# Combine abundance indices with 
abund_length <- inner_join(abund_ind, ll_tow_u)

# BGM units are m^2 
# trawl length is 1.8*4 = 7.2 km or 7200 meters

# Calculate the biomass index
abund_length$bio_indx <- abund_length$biomass/(abund_length$larett.opnun*7200)
abund_index <- abund_length

# Save abundance index
save(abund_index, file = "abund_index.Rdata")

#
# Read in landings data
#

# Create a data set of all the landings data
file_names <- dir("init_conditions")
name_tmp <- strsplit(file_names, "_")
code <- NULL
for(i in 1:length(name_tmp)){
  code[i] <- name_tmp[[i]][1]
}
tegund <- NULL
for(i in 1:length(name_tmp)){
  tegund[i] <- name_tmp[[i]][2]
}
code_tegund <- data.frame(Code = code, tegund = as.numeric(tegund))
code_tegund[tegund == "landings.csv","tegund"] <- -999
length_code <- filter(length_abvb, Code %in% code_tegund$Code)

landings <- list()
for(i in 1:length(name_tmp)){
  if(code_tegund$tegund[i] == -999){
    landings[[i]] <- read.csv(paste("init_conditions/", code_tegund$Code[i], "_landings.csv", sep = ""), header = T) } else
      landings[[i]] <- read.csv(paste("init_conditions/", code_tegund$Code[i], "_", code_tegund$tegund[i], "_", "landings.csv", sep = ""), header = T)
}
names(landings) <- code_tegund$Code
landings[["FHE"]]$Total <- landings[["FHE"]]$Spring + landings[["FHE"]]$Discards + landings[["FHE"]]$Summer
landings[[9]]$Total <- landings[[9]]$Females + landings[[9]]$Roe 
landings[["WHALE"]]$Total <- rowSums(landings[["WHALE"]][-1], na.rm = T)
landings[[10]]$Total <- landings[[10]]$Iceland

landings_dataframe <- NULL
for(i in 1:length(landings)){
  if("Total" %in% colnames(landings[[i]])){
    tmp <- data.frame(ar = landings[[i]]$Year, Total = landings[[i]]$Total)
  } else 	tmp <- data.frame(ar = landings[[i]]$Year, Total = landings[[i]]$Landings)
  landings_dataframe <- rbind(landings_dataframe, tmp)
}
yrs <- NULL
for(i in 1:length(landings)){
  yrs[i] <- nrow(landings[[i]])
}

landings_dataframe$Code <- rep(code_tegund$Code, yrs)	
landings_dataframe$tegund <- rep(code_tegund$tegund, yrs)	

# Remove tegund -999
landings_subset <- filter(landings_dataframe, tegund > 0)

# Join length_abvb and landings_datamframe
# ab_land <- inner_join(landings_subset, length_abvb)
# ab_land <- tbl_df(ab_land)

# Combine abundance indices with landings data
landings_subset <- merge(landings_subset, fiskur)

ab_land <- merge(abund_ind, landings_subset)


# Calculate abundance by year and species
ab_yr <- ab_land %>%
  group_by(Code, ar) %>%
  summarize(biomass = sum(biomass))

# Rejoin landings data
ab_yr <- merge(ab_yr, landings_subset)

# Get biomass estimates
bio_est <- ab_yr %>%
  group_by(Code) %>%
  do(model = tryCatch(nlm(ssefcn, beta <- log(c(K = sum(.$Total),
                                                r = .05,
                                                q = mean(.$biomass)/sum(.$Total))),
                          .$ar, .$Total, .$biomass), warning = function(w) NULL, error = function(e) NULL)) %>%
  filter(model != "NULL") %>%
  group_by(stype,tegund) %>% 
  do(data.frame(var = c("K","r", "q"),
                coef = .$model[[1]]$estimate)) %>%
  dcast(., stype + tegund~var, value.var = "coef")

cod_biomass <- na.omit(filter(ab_land, Code == "FCD"))

# Landings data
Y <- cod_biomass$Total
# Years
yrs <- cod_biomass$ar
# Abundance indices, this is what I calculated based on w-l relationship
I <- cod_biomass$biomass

source(paste(kScriptDir, "ssefcn.R", sep = ""))

# Initial conditions
r <- .05
K <- sum(Y[length(Y)-20:length(Y)])
BO <- K
q <- mean(I)/K
beta <- log(c(K, BO, r, q))
fit <- nlm(ssefcn, beta, yrs, Y, I) 
fit$est
fit
