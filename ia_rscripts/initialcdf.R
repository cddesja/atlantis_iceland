#
# Generate initial conditions
#

# This script will be used to generate the initial conditions for the 
# Iceland Atlantis model.

# Christopher David Desjardins

# Constants
k_scriptdir <- "~/Github/atlantis_iceland/ia_rscripts/"
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

# Load weight-length allometric and von-Bertalanffy parameters
load("vb_ab_params.Rdata")

# Source R scripts
source(paste(k_scriptdir, "skalamedtoldum.R", sep = ""))  # Length Distributions

# Convert stodvar to a tbl_df
stodvar_df <- tbl_df(stodvar)
stodvar_df$stype <- ifelse(stodvar_df$synaflokkur == 30, "spring",
                           ifelse(stodvar_df$synaflokkur == 35, "fall",
                                  "other"))

# Select just the spring surveys
stodvar_spring <- filter(stodvar_df, stype == "spring", ar == "2013")
stodvar_fall <- filter(stodvar_df, stype == "fall", ar == "2013")
stodvar_2013 <- filter(stodvar_df, ar == "2013")

#
# Assign the box ids
#

# Read in spatial data
atlantis <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/atlantis/", layer = "atlantis_WGS84")


#
# First, Spring Survey
#

work <- select(stodvar_spring, synis.id, lat, lon)
coords <- data.frame(lon = work$lon, lat = work$lat)
coords_sp <- SpatialPoints(coords, proj4string = CRS("+proj=longlat +datum=WGS84"))
ob <- SpatialPolygons(atlantis@polygons,proj4string=CRS("+proj=longlat +datum=WGS84"))
box_ids <- coords_sp %over% ob

# Read in the correct order of the boxes
row_order <- atlantis@data$box_id

pts_in <- rep(NA,length(box_ids))
for(i in 1:53){
  subs <- which(box_ids == i)
  pts_in[subs] <- row_order[i]
}

stodvar_spring$box_id <- pts_in

# Fix the surveys in box 19
#write.csv(stodvar_spring, file = "spring_fix.csv")
spring_fix <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/fixed_points_around_iceland/", layer = "spring_fix")
stodvar_spring <- spring_fix@data

# 
#
# Second, Fall Survey
#

work <- select(stodvar_fall, synis.id, lat, lon)
coords <- data.frame(lon = work$lon, lat = work$lat)
coords_sp <- SpatialPoints(coords, proj4string = CRS("+proj=longlat +datum=WGS84"))
ob <- SpatialPolygons(atlantis@polygons,proj4string=CRS("+proj=longlat +datum=WGS84"))
box_ids <- coords_sp %over% ob

pts_in <- rep(NA,length(box_ids))
for(i in 1:53){
  subs <- which(box_ids == i)
  pts_in[subs] <- row_order[i]
}

stodvar_fall$box_id <- pts_in

# Fix the surveys in box 19
#write.csv(stodvar_fall, file = "fall_fix.csv")
fall_fix <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/fixed_points_around_iceland/", layer = "fall_fix")
stodvar_fall <- fall_fix@data

#
# Finally, all 2013 surveys
#

work <- select(stodvar_2013, synis.id, lat, lon)
work <- na.omit(work)
coords <- data.frame(lon = work$lon, lat = work$lat)
coords_sp <- SpatialPoints(coords, proj4string = CRS("+proj=longlat +datum=WGS84"))
ob <- SpatialPolygons(atlantis@polygons,proj4string=CRS("+proj=longlat +datum=WGS84"))
box_ids <- coords_sp %over% ob

# Read in the correct order of the boxes
row_order <- atlantis@data$box_id

pts_in <- rep(NA,length(box_ids))
for(i in 1:53){
  subs <- which(box_ids == i)
  pts_in[subs] <- row_order[i]
}
work$box_id <- pts_in
stodvar_2013 <- merge(work, stodvar_2013, by = "synis.id")

# Fix the surveys in box 19
write.csv(stodvar_2013, file = "s2013_fix.csv")
spring_fix <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/fixed_points_around_iceland/", layer = "spring_fix")
stodvar_spring <- spring_fix@data

# Join stodvar with length and numbers for spring
le_sp <- inner_join(stodvar_spring, all.le, by = "synis.id")
nu_sp <- inner_join(stodvar_spring, all.nu, by = "synis.id")

# Unconditioned length distributions
unc_length <- Skala.med.toldum(le_sp,nu_sp)
length_red <- select(unc_length, synis.id, tegund, lat, lon, lengd,
	fj.alls, ar, larett.opn, stype, box_id)
#length_red <- filter(length_red, fj.alls > -1)
#length_red <- filter(length_red, fj.alls != "Inf")
length_df <- tbl_df(length_red)

# Free up some RAM
rm(le_sp, nu_sp, unc_length, length_red, stodvar_df)

# Join the w-l and vb params with the length distribution data
wvb_params <- na.omit(select(wvb.params, tegund, a, b))
length_abvb <- merge(length_df, wvb_params, by = "tegund")

# Remove those where the width of the survey equipment is not known
length_abvb_nona <- filter(length_abvb, larett.opn != "NA")

#
# Calculate abundance indices
#

abund_ind <-
  length_abvb_nona %>%
  group_by(tegund, synis.id, box_id) %>%
  do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))

# Select on long/lat and towing width
ll_tow <- select(length_abvb_nona, synis.id, tegund, larett.opn, box_id)

# Select only the unique cases
ll_tow_u <- unique(ll_tow)

# Combine abundance indices with 
abund_length <- inner_join(abund_ind, ll_tow_u)

# Calculate the biomass index
abund_length$bio_indx <- abund_length$biomass/(as.numeric(paste(abund_length$larett.opn))*7200)
abund_index <- abund_length

# Calculate ave abund by box and Atlantis code
abund_index <- merge(abund_index, hafro_link)

ai_atl <- abund_index %>%
  group_by(box_id, Code) %>%
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

# Calculate total abundance, ie. over boxes
tot_abund <- 
  ai_area %>%
    group_by(Code) %>%
    summarize(tots = sum(abund/(1e6)))
tot_abund$ttons <- tot_abund$tots/1000
options(scipen = 999)
tot_abund

#
# Fall for FGH and FDR
#

# Join stodvar with length and numbers for spring
le_fl <- inner_join(stodvar_fall, all.le, by = "synis.id")
nu_fl <- inner_join(stodvar_fall, all.nu, by = "synis.id")

# Unconditioned length distributions
unc_length <- Skala.med.toldum(le_fl,nu_fl)
length_red <- subset(unc_length, select = c("synis.id", "tegund", "lat", "lon", "lengd",
                     "fj.alls", "larett.opn", "box_id"))
#length_red <- filter(length_red, fj.alls > -1)
#length_red <- filter(length_red, fj.alls != "Inf")
length_df <- tbl_df(length_red)

# Join the w-l and vb params with the length distribution data
wvb_params <- na.omit(select(wvb.params, tegund, a, b))
length_abvb <- merge(length_df, wvb_params, by = "tegund")

# Remove those where the width of the survey equipment is not known
length_abvb_nona <- filter(length_abvb, larett.opn !=  "NA")

#
# Calculate abundance indices
#

abund_ind <-
  length_abvb_nona %>%
  group_by(tegund, synis.id, box_id) %>%
  do(data.frame(biomass = sum(.$fj.alls * (.$a * .$lengd^.$b))))

# Select on long/lat and towing width
ll_tow <- select(length_abvb_nona, synis.id, tegund, larett.opn, box_id)

# Select only the unique cases
ll_tow_u <- unique(ll_tow)

# Combine abundance indices with 
abund_length <- inner_join(abund_ind, ll_tow_u)

# Calculate the biomass index
abund_length$bio_indx <- abund_length$biomass/(as.numeric(paste(abund_length$larett.opn))*7200)
abund_index <- abund_length

# Calculate ave abund by box and Atlantis code
abund_index <- merge(abund_index, hafro_link)

ai_atl <- abund_index %>%
  group_by(box_id, Code) %>%
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

# Calculate total abundance, ie. over boxes
tot_abund_fall <- 
  ai_area %>%
  group_by(Code) %>%
  summarize(tots = sum(abund/(1e6)))
tot_abund$ttons <- tot_abund$tots/1000
options(scipen = 999)
tot_abund_fall



#
# Calculate proportion per boxes
# 

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


