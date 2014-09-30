#
# Generate initial conditions
#

# This script will be used to generate the initial conditions for the 
# Iceland Atlantis model.

# Christopher David Desjardins

# Constants
k_scriptdir <- "~/GitHub/atlantis_iceland/ia_rscripts/"
k_datadir <- "~/Dropbox/hi/atlantis/hafro_data/"
k_landings <- "init_conditions/"

# Change working directory
setwd(k_datadir)

# Load appropriate library
library("fjolst")
library("dplyr")
library("reshape2")
library("sp")
library("rgdal")

# ------- #
# Biomass #
# ------- #

# Read in tegund CSV
fiskur <- read.csv("hafro_atlantis_link.csv", header = TRUE)

# Load weight-length allometric parameters
iabio <- read.csv(file = "ab_params.csv")

# Source R scripts
source(paste(k_scriptdir, "atlantis_helpers.R", sep = ""))  # Length Distributions

# Convert stodvar to a tbl_df
stodvar_df <- tbl_df(stodvar)
stodvar_df$stype <- ifelse(stodvar_df$synaflokkur == 30, "spring",
                           ifelse(stodvar_df$synaflokkur == 35, "fall",
                                  "other"))

# Omit all years outside of my model run
stodvar_surveys <- filter(stodvar_df, ar > 1947 & ar < 2015)

#
# Assign the box ids
#

# # Read in spatial data
# atlantis <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/atlantis/", layer = "atlantis_WGS84")
# 
# work <- stodvar_surveys[,c("synis.id", "lat", "lon")]
# work <- na.omit(work)  # Omit the surveys with lat/lon identifiers
# coords <- data.frame(lon = work$lon, lat = work$lat)
# coords_sp <- SpatialPoints(coords, proj4string = CRS("+proj=longlat +datum=WGS84"))
# ob <- SpatialPolygons(atlantis@polygons,proj4string=CRS("+proj=longlat +datum=WGS84"))
# box_ids <- coords_sp %over% ob
# 
# # Read in the correct order of the boxes
# row_order <- atlantis@data$box_id
# 
# pts_in <- rep(NA,length(box_ids))
# for(i in 1:53){
#   subs <- which(box_ids == i)
#   pts_in[subs] <- row_order[i]
# }
# 
# work$box_id <- pts_in
# work19 <- work[work$box_id == "19",]
# 
# # Fix the surveys in box 19
# #write.csv(work19, file = "all_surveys_fix.csv", row.names = FALSE)
# 
# survey_fix <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/fixed_points_around_iceland/all_surveys/", layer = "all_surveys")
# 
# survey_fix <- survey_fix@data
# work <- work[work$box_id != 19,]
# work <- rbind(work, survey_fix)

# Read in the data
box_assign <- readRDS(file = "~/Dropbox/hi/atlantis/hafro_data/boxid_survey.Rds")

# Combine the box_id back with stodvar_surveys
stodvar <- inner_join(box_assign, stodvar_surveys, by = "synis.id")

# Join stodvar with length and numbers for spring
le_sp <- inner_join(stodvar, all.le, by = "synis.id")
nu_sp <- inner_join(stodvar, all.nu, by = "synis.id")

# Unconditioned length distributions
unc_length <- skala_med_toldum(le_sp,nu_sp)
length_red <- subset(unc_length, select = c(synis.id, tegund, lat.x, lon.x, lengd,
	fj.alls, ar, larett.opnun, synaflokkur, box_id))
length_red <- filter(length_red, fj.alls > -1 & fj.alls != "Inf")
length_df <- tbl_df(length_red)

# Free up some RAM
rm(le_sp, nu_sp, unc_length, length_red, stodvar_df, stodvar)

# Join the w-l and vb params with the length distribution data
wvb_params <- na.omit(iabio)
length_abvb <- inner_join(length_df, wvb_params, by = "tegund")

# Remove those where the width of the survey equipment is not known
length_abvb_nona <- filter(length_abvb, larett.opnun != "NA")

# Get a list of the tegund ids
ids <- unique(length_abvb_nona$tegund)

# Calculate year abundance indices
# abund_ind_yr <-
#   length_abvb_nona %>%
#     group_by(tegund, ar) %>%
#     do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))
# saveRDS(abund_ind_yr,file = "yearly_biomass.Rds")

#
# Calculate abundance indices
#
# s03 <- filter(length_abvb_nona, tegund %in% ids[1:4])
# s00 <- filter(length_abvb_nona, tegund %in% ids[5:10])
# s10 <- filter(length_abvb_nona, tegund %in% ids[11:20])
# s20 <- filter(length_abvb_nona, tegund %in% ids[21:30])
# s30 <- filter(length_abvb_nona, tegund %in% ids[31:40])
# s40 <- filter(length_abvb_nona, tegund %in% ids[41:50])
# s50 <- filter(length_abvb_nona, tegund %in% ids[51:60])
# s60 <- filter(length_abvb_nona, tegund %in% ids[61:70])
# s70 <- filter(length_abvb_nona, tegund %in% ids[71:80])
# s80 <- filter(length_abvb_nona, tegund %in% ids[81:89])
# 
# abund_ind_s03 <-
#   s03 %>%
#   group_by(tegund, synis.id, box_id) %>%
#   do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))
# abund_ind_s00 <-
#   s00 %>%
#   group_by(tegund, synis.id, box_id) %>%
#   do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))
# abund_ind_s10 <-
#   s10 %>%
#   group_by(tegund, synis.id, box_id) %>%
#   do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))
# abund_ind_s20 <-
#   s20 %>%
#   group_by(tegund, synis.id, box_id) %>%
#   do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))
# abund_ind_s30 <-
#   s30 %>%
#   group_by(tegund, synis.id, box_id) %>%
#   do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))
# abund_ind_s40 <-
#   s40 %>%
#   group_by(tegund, synis.id, box_id) %>%
#   do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))
# abund_ind_s50 <-
#   s50 %>%
#   group_by(tegund, synis.id, box_id) %>%
#   do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))
# abund_ind_s60 <-
#   s60 %>%
#   group_by(tegund, synis.id, box_id) %>%
#   do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))
# abund_ind_s70 <-
#   s70 %>%
#   group_by(tegund, synis.id, box_id) %>%
#   do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))
# abund_ind_s80 <-
#   s80 %>%
#   group_by(tegund, synis.id, box_id) %>%
#   do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))
# 
# abund_ind <- rbind(abund_ind_s03,
#   abund_ind_s00,
#   abund_ind_s10,
#   abund_ind_s20,
#   abund_ind_s30,
#   abund_ind_s40,
#   abund_ind_s50,
#   abund_ind_s60,
#   abund_ind_s70,
#   abund_ind_s80)
# 
# 
# # Save abundance estimate
# saveRDS(abund_ind, file = "abund_ind_17sept2014.rds")

# Read in abundance estimate
abund_ind <- readRDS(file = "abund_ind_17sept2014.rds")


# Select on long/lat and towing width
ll_tow <- select(length_abvb_nona, synis.id, tegund, larett.opnun, box_id)

# Select only the unique cases
ll_tow_u <- unique(ll_tow)

# Combine abundance indices with 
abund_length <- inner_join(abund_ind, ll_tow_u)

# Calculate the biomass index
abund_length$bio_indx <- abund_length$biomass/(as.numeric(paste(abund_length$larett.opn))*7200)
abund_index <- abund_length

# Calculate ave abund by box and Atlantis code
abund_index <- merge(abund_index, fiskur)

# Calculate ave abund by tegund first
ai_atl <- abund_index %>%
  group_by(box_id, tegund) %>%
  summarize(ai = mean(bio_indx))

# Read in size of the boxes from the BGM file
bgm <- readLines("../iceland_atlantis/atlantis_L93.bgm")
area <- grep("area",bgm)
bgm_area <- bgm[area]
bgm_area <- strsplit(bgm_area, "\t")
area <- NULL
for(i in 1:length(bgm_area))
  area[i] <- bgm_area[[i]][[2]]
area_atl <- data.frame(box_area = as.numeric(area), box_id = 0:52)

# Merge area with ai
ai_area <- merge(ai_atl, area_atl, by = "box_id")

# Calculate abundance
ai_area$abund <- ai_area$ai*ai_area$box_area

# Islands and boundary boxes
bboxes <- c(0,2:3,19,26:28,41:42,44:50,52)
ai_area_tmp <- ai_area[!(ai_area$box_id %in% bboxes), ]
ai_area <- ai_area_tmp

# Calculate total abundance, ie. over boxes
tot_abund <- 
  ai_area %>%
    group_by(tegund) %>%
    summarize(tots = sum(abund/(1e6)))
tot_abund$ttons <- tot_abund$tots/1000
options(scipen = 999)
initial_abund <- left_join(tot_abund, fiskur)
#write.csv(initial_abund, file = "initial_conds.csv")

#
# Fall for FGH and FDR
#
# 
# # Join stodvar with length and numbers for spring
# le_fl <- inner_join(stodvar_fall, all.le, by = "synis.id")
# nu_fl <- inner_join(stodvar_fall, all.nu, by = "synis.id")
# 
# # Unconditioned length distributions
# unc_length <- Skala.med.toldum(le_fl,nu_fl)
# length_red <- subset(unc_length, select = c("synis.id", "tegund", "lat", "lon", "lengd",
#                      "fj.alls", "larett.opn", "box_id"))
# #length_red <- filter(length_red, fj.alls > -1)
# #length_red <- filter(length_red, fj.alls != "Inf")
# length_df <- tbl_df(length_red)
# 
# # Join the w-l and vb params with the length distribution data
# wvb_params <- na.omit(select(wvb.params, tegund, a, b))
# length_abvb <- merge(length_df, wvb_params, by = "tegund")
# 
# # Remove those where the width of the survey equipment is not known
# length_abvb_nona <- filter(length_abvb, larett.opn !=  "NA")


#
# Calculate proportion per boxes
# 

ai_atl <- left_join(ai_atl, fiskur)
ai_atl <- ai_atl[!(ai_atl$box_id %in% bboxes), ]

prop <- ai_atl %>%
  group_by(Code) %>%
  summarize(ai_sum = sum(ai))

prop_box <- merge(ai_atl, prop, by = "Code")
prop_box$perc <- prop_box$ai/prop_box$ai_sum

prop_box <- prop_box[order(prop_box$Code,prop_box$box_id),]
box_id <- 0:52
ids <- rep(unique(prop_box$Code),each = length(box_id))
prop_data <- data.frame(Code = ids,box_id = box_id, perc = 0)

prop_box$unq <- paste(prop_box$Code, prop_box$box_id, sep = "")
prop_data$unq <- paste(prop_data$Code, prop_data$box_id, sep = "")

for(i in 1:nrow(prop_box)){
  prop_data$perc[which(prop_box$unq[i] == prop_data$unq)] = prop_box$perc[i]
}

write.csv(prop_data, file = "prop_per_box.csv", row.names = FALSE)

#
# Merge in FillValues in .cdf 
#

# Initial conditions nc file
initial <- readLines("~/Dropbox/hi/atlantis/iceland_atlantis/iceland_init.cdf.old")

# Created by IAbioparams.xls
fill_value <- readLines("~/Dropbox/hi/atlantis/iceland_atlantis/helper_files/VertFillValues_for_CDF.txt")
fill_split <- strsplit(fill_value, split = "=")

for(i in 1:length(fill_value)){
  initial[grep(paste("^",fill_split[[i]][1],"$", sep = ""), initial, fixed = TRUE)] <- paste("\t\t",fill_value[i],sep = "") 
}
sink(file = "~/Dropbox/hi/atlantis/iceland_atlantis/iceland_init.cdf")
cat(initial, sep = "\n")
sink()

#
# Temperature, Salinity
# Copied from temp and salinity cdfs

#
# Pelagic Bact_N
#

cat(rep(c(rep(paste("0.01, "), 6, sep = ""), paste("_,\n")), 18),
    rep("_, ", 7),
    rep(c(rep(paste("0.01, "), 6, sep = ""), paste("_,\n")), 32),
    rep("_, ", 7), sep = "")

#
# Sed Bact
# 

cat(rep(c(rep(paste("1, "), 6, sep = ""), paste("_,\n")), 18),
    rep("_, ", 7),
    rep(c(rep(paste("1, "), 6, sep = ""), paste("_,\n")), 32),
    rep("_, ", 7), sep = "")

#
# Nitrate
#

options(digits = 16)
nit_data_raw <- nc_open("~/Dropbox/hi/atlantis/other_data/chemistry/no3/dataset-global-nahindcast-bio-001-018-no3_1408517812635.nc")

varNames <- names(nit_data_raw$var)
varData <- list() 
for(i in 1:length(varNames)){
  varData[[i]] <- ncvar_get(nc=nit_data_raw,varid=varNames[i])
}
names(varData) <- varNames

lon <- NULL
lat <- NULL
for(i in 1:ncol(varData$nav_lon)){
  lo <- varData$nav_lon[,i]
  la <- varData$nav_lat[,i]
  lon <- c(lon, lo)
  lat <- c(lat, la)
  rm(lo, la)
}

lon[lon == 9969209968386869046778552952102584320] <- NA
lat[lat == 9969209968386869046778552952102584320] <- NA

no3 <- NULL
for(j in 1:75){
  dat <- varData$NO3[,,j]
  no3_tmp <- NULL
  for(i in 1:120){
    no3_tmp2 <- dat[,i]
    no3_tmp <- c(no3_tmp, no3_tmp2)
  }
  no3 <- c(no3, no3_tmp)
}

depth_tmp <- ncvar_get(nc=nit_data_raw,varid="deptht")
depths <- rep(depth_tmp, each = 164*120)

no3_data <- data.frame(lon = lon, lat = lat, no3 = no3, depths = depths)

# Omit the values missing lat/lon and no3
no3_data <- na.omit(no3_data)

# Read in spatial data
atlantis <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/atlantis/", layer = "atlantis_WGS84")

coords <- data.frame(lon = no3_data$lon, lat = no3_data$lat)
coords_sp <- SpatialPoints(coords, proj4string = CRS("+proj=longlat +datum=WGS84"))
ob <- SpatialPolygons(atlantis@polygons,proj4string=CRS("+proj=longlat +datum=WGS84"))
no3_data$box_ids <- coords_sp %over% ob

# Omit those points outside the boxes
no3_data <- na.omit(no3_data)

no3_data$depth_layer <- ifelse(no3_data$depths <= 50, 1,
                               ifelse(no3_data$depths > 50 & no3_data$depths <= 150, 2,
                                      ifelse(no3_data$depths > 150 & no3_data$depths <= 300, 3,
                                             ifelse(no3_data$depths > 300 & no3_data$depths <= 600, 4,
                                                    ifelse(no3_data$depths > 600 & no3_data$depths <= 1000, 5,6)))))

no3_agg <- no3_data %>%
  group_by(box_ids, depth_layer) %>%
  summarize(ave_no3 = mean(no3))

no3_agg$milligrams_no3 <- no3_agg$ave_no3*62.005010/1000 

# --------------------- #

options(digits = 16)
nit_data_raw <- nc_open("~/Dropbox/hi/atlantis/other_data/chemistry/no3/data_with_zeros.nc")

varNames <- names(nit_data_raw$var)
varData <- list() 
for(i in 1:length(varNames)){
  varData[[i]] <- ncvar_get(nc=nit_data_raw,varid=varNames[i])
}
names(varData) <- varNames

lon <- NULL
lat <- NULL
for(i in 1:ncol(varData$lon)){
  lo <- varData$lon[,i]
  la <- varData$lat[,i]
  lon <- c(lon, lo)
  lat <- c(lat, la)
  rm(lo, la)
}

lon[lon == 9969209968386869046778552952102584320] <- NA
lat[lat == 9969209968386869046778552952102584320] <- NA

no3 <- NULL
for(j in 1:48){
  dat <- varData$no3[,,j,1]
  no3_tmp <- NULL
  for(i in 1:120){
    no3_tmp2 <- dat[,i]
    no3_tmp <- c(no3_tmp, no3_tmp2)
  }
  no3 <- c(no3, no3_tmp)
}

depth_tmp <- ncvar_get(nc=nit_data_raw,varid="depth")
depths <- rep(depth_tmp, each = 164*120)

no3_data <- data.frame(lon = lon, lat = lat, no3 = no3, depths = depths)

# Omit the values missing lat/lon and no3
no3_data <- na.omit(no3_data)

# Read in spatial data
atlantis <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/atlantis/", layer = "atlantis_WGS84")

coords <- data.frame(lon = no3_data$lon, lat = no3_data$lat)
coords_sp <- SpatialPoints(coords, proj4string = CRS("+proj=longlat +datum=WGS84"))
ob <- SpatialPolygons(atlantis@polygons,proj4string=CRS("+proj=longlat +datum=WGS84"))
no3_data$box_ids <- coords_sp %over% ob

# Omit those points outside the boxes
no3_data <- na.omit(no3_data)

no3_data$depth_layer <- ifelse(no3_data$depths <= 50, 1,
                               ifelse(no3_data$depths > 50 & no3_data$depths <= 150, 2,
                                      ifelse(no3_data$depths > 150 & no3_data$depths <= 300, 3,
                                             ifelse(no3_data$depths > 300 & no3_data$depths <= 600, 4,
                                                    ifelse(no3_data$depths > 600 & no3_data$depths <= 1000, 5,6)))))

no3_agg_2 <- no3_data %>%
  group_by(box_ids, depth_layer) %>%
  summarize(ave_no3 = mean(no3))

no3_agg_2$millimoles <- no3_agg_2$ave_no3*1000  
no3_agg_2$micrograms_no3 <- no3_agg_2$millimoles*62.005010

no3_agg$milligrams_no3 <- no3_agg$ave_no3*62.005010/1000 


