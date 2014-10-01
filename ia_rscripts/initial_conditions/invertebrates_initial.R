library("geo")
library("sp")
library("dplyr")
library("rgdal")

# Functions
source("~/Github/iceland_atlantis/ia_rscripts/atlantis_helpers.R")

# Box Areas
# Find the areas from the BGM file
bgm <- readLines("~/Dropbox/hi/atlantis/iceland_atlantis/atlantis_L93.bgm")
match <- grep("area",bgm)
bgm_area <- bgm[match]
bgm_area <- strsplit(bgm_area, "\t")

# Select the 2nd element from each list
tot_area <- as.numeric(sapply(bgm_area, "[[", 2))

# Digits to display
options(digits = 22)

# Inshore-ish boxes
inshore <- c(5,6,7,18,16,15,36,35,34,31,32,30,20,21,22,51)  ## 51 b/c borders F.I.

#
# Nephrops
# 
# Read in nephrops data from Jónas
data <- read.csv(file = "~/Dropbox/hi/atlantis/hafro_data/nephrops/afli00.csv")

# Create a unique identifier
data$id <- 1:nrow(data)

# Select only the lat and lon
box_ids <- select(data, id, lat, lon)
atlantis <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/atlantis/", layer = "atlantis_WGS84")

coords <- data.frame(lon = box_ids$lon, lat = box_ids$lat)
coords_sp <- SpatialPoints(coords, proj4string = CRS("+proj=longlat +datum=WGS84"))
ob <- SpatialPolygons(atlantis@polygons,proj4string=CRS("+proj=longlat +datum=WGS84"))
b_ids <- coords_sp %over% ob

row_order <- atlantis@data$box_id

pts_in <- rep(NA,length(b_ids))
for(i in 1:53){
  subs <- which(b_ids == i)
  pts_in[subs] <- row_order[i]
}

#sur_fix <- data.frame(id = box_ids$id, box_id = pts_in, lat = box_ids$lat, lon = box_ids$lon)

junk <- data
junk$box_id <- pts_in

junk1 <- junk[! is.na(junk$dypi),]
junk1$mdypi <- junk1$dypi *1.8288 # change fm to meters
junk2 <-  junk1[junk1$mdypi < 400,] # always some errors...
junk2 <-  junk2[junk2$mdypi > 50,] #  always some errors...
svaedi <- c(5, 20, 21, 22, 23, 24, 25)  # these value are set up to match jónas's areas
junk3 <- junk2[junk2$box_id %in% svaedi, ] # from reg.bc

catch_dat <- junk3 %>%
  group_by(box_id) %>%
  summarize(tot = sum(afli))

catch_dat$percent <- catch_dat$tot/sum(catch_dat$tot)
catch_dat$biomass_tonnes <- catch_dat$percent * 30.940 * 1e3
bt <- rep(0, 53)

# Atlantis starts at 0 not 1
catch_dat$box_id <- catch_dat$box_id
bt[catch_dat$box_id] <- catch_dat$biomass_tonnes
mgN <- tons_mgN(bt, unit = area)
# Omit the zeros
mgN <- mgN[mgN>0]

ids <- 0:52
bio <- c(rep("_", 53))
catch_dat$box_id <- catch_dat$box_id + 1  # Because Atlantis starts at 0
bio[catch_dat$box_id] <- mgN

# Copy/Paste this into initial conditions
cat(cat(bio, sep = ", "), ";", sep ="")

#
# Iceland Scallop
#

scallop_landings <- read.csv("http://data.hafro.is/assmt/2013/scallop/landings.csv", header = T)
totals <- colSums(scallop_landings)[-c(1,11)]
prop <- totals/sum(totals)
bm_box <- prop*1e5
boxs <- c(31, 34, 34, 35, 20, 34, 34, 36, 16)
box_bio <- data.frame(bio = bm_box, ids = boxs)

bio_box <- box_bio %>%
  group_by(ids) %>%
  summarize(tot_bio = sum(bio))

areas <- bio_box$ids + 1

wt <- tons_mgN(bio_box$tot_bio, tot_area[areas])
dat <- data.frame(box = areas, wt = wt)
init_cond_data(data = dat, nbox = 53, numlayer = 1, varname = "Scallop_N")

# Cover
dat <- data.frame(box = areas, wt = 0.01)
init_cond_data(data = dat, nbox = 53, numlayer = 1, varname = "Scallop_Cover")

#
# Quahog
#

# Virgin biomass estimate
wt <- 1.3e6  ## tons

# Create proportions around Iceland
props <- c(0.124, 0.076, 0.104, 0.088, 0.245, 0.116)
props <- props/sum(props)
loc <- c("NW", "N", "A", "W", "SW", "SA")
prop_link <- c(rep(props[1], 2), props[2], rep(props[3], 3), props[4], rep(props[5],4), props[6]) 

# Extract area of boxes
boxs <- c(35, 34, 36, 15, 16, 18, 31, 20, 21, 22, 30, 5)
loc <- c("NW", "NW", "N", "A", "A", "A", "W", "SW", "SW", "SW", "SW", "SA") 
b_areas <- tot_area[boxs + 1]
tmp <- data.frame(boxs, loc, areas = b_areas)
tmp2 <- tmp %>%
  group_by(loc) %>%
  mutate(prop_area = areas/sum(areas))
tmp2$prop <- prop_link
tmp2$adj_prop <- tmp2$prop*tmp2$prop_area

# Distribute the wt across the boxes
tmp2$wt_box <- tmp2$adj_prop*wt

nbox = 53
numlayer = 1
data <- dat
# Calculate the N mg wt
Nwt <- tons_mgN(tmp2$wt_box, b_areas)
dat <- data.frame(box = boxs+1, wt = Nwt)
init_cond_data(data = dat, nbox = 53, numlayer = 1, varname = "Quahog_N")

# Cover
dat <- data.frame(box = boxs+1, wt = 0.01)
init_cond_data(data = dat, nbox = 53, numlayer = 1, varname = "Quahog_Cover")

#
# Other invertebrates and tracers
#

#
# Macroalgae, use values from SETas of 0.5 and 100
#

# Cover
dat <- data.frame(box = inshore+1, wt = 0.5)
init_cond_data(data = dat, nbox = 53, numlayer = 1, varname = "Macroalgae_Cover")

# N
dat <- data.frame(box = inshore+1, wt = 1)
init_cond_data(data = dat, nbox = 53, numlayer = 1, varname = "Macroalgae_N")

#
# Benthic carnivores - use values from SETas 
#
dat <- c("0.001,", " 0.001,", " 0.001,", " 0.001,", " 0.001,", " 0.001,", " 60\n")
cat(rep(dat,53), sep = "")

#
# Benthic Grazer - SETas
#
dat <- data.frame(box = inshore+1, wt = 1)
init_cond_data(data = dat, nbox = 53, numlayer = 1, varname = "Benthic_grazer_N")

#
# Filter_Other_Cover - SETas
#
dat <- data.frame(box = 1:53, wt = 0.03)
init_cond_data(data = dat, nbox = 53, numlayer = 1, varname = "Filter_Other_Cover")

dat <- data.frame(box = 1:53, wt = 5)
init_cond_data(data = dat, nbox = 53, numlayer = 1, varname = "Filter_Other_N")

#
# Deposit_Feeder_N - SETas
#
dat <- c("0.001,", " 0.001,", " 0.001,", " 0.001,", " 0.001,", " 0.001,", " _\n")
cat(rep(dat,53), sep = "")

#
# Megazoobenthos - SETas
#
dat <- data.frame(box = inshore+1, wt = 1)
init_cond_data(data = dat, nbox = 53, numlayer = 1, varname = "Megazoobenthos_N")

#
# Gelatinous zoo - SETas
#
dat <- c("_,", " _,", " _,", " _,", " _,", " _,", " 0.0001\n")
cat(rep(dat,53), sep = "")
