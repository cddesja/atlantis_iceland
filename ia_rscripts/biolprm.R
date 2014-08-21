# Calculate values for the biol.prm file 
# This script calculates the following based on the fjolst data sets from MRI:
# - a and b in the weight-length allometric formula
# - Linf, k, and t.o for the von-Bertalanffy
# -
# minimum length at maturity

# Load necessary libraries
library("fjolst")
library("dplyr")
library("reshape2")
library("sp")
library("ggplot2")
library("rgdal")
library("maptools")

# Define constants
k_dir <- "~/Dropbox/HI/atlantis/hafro_data/"

# Read in biology parameter prm
k_bio <- "~/Dropbox/hi/atlantis/iceland_atlantis/iceland_biol.prm"
bio_params <- readLines(k_bio)
write(bio_params, file = "~/Dropbox/hi/atlantis/iceland_atlantis/iceland_biol_backup.prm")


# Read in tegund CSV
hafro_link <- read.csv(paste(k_dir, "hafro_atlantis_link.csv", sep = ""), header = T)


# This is to load all the data from fjolst
# data(package = "fjolst")

# all.kv, otoliths data
# stodvar, stations data

# These are the following prefixes used:
# sp - spring
# au - autumn

# Convert stodvar to a tbl_df
stodvar_df <- tbl_df(stodvar)

# Create a variable that indexes type of survey
stodvar_df$stype <- ifelse(stodvar_df$synaflokkur == 30, "spring",
                           ifelse(stodvar_df$synaflokkur == 35, "fall",
                           "other"))
stodvar_df$stype <- factor(stodvar_df$stype, levels = c("spring", "fall",
  "other"))

#stodvar_df <- filter(stodvar_df, ar == "2013")

# Convert kv to a tbl_df
kv_df <- tbl_df(all.kv)

# Join these tables by synis.id
kv_st <- inner_join(stodvar_df, kv_df)

# 
# Weight-Length Allometric Formula
#

# Aggregate length and weight by age and species
length.quantiles <- kv_st %>% 
  group_by(stype,tegund,add=FALSE) %>%
  filter(length(tegund)>10) %>%
  mutate(length.cat = 
           findInterval(lengd, quantile(lengd, probs = seq(0,.9,.1))))

teg.age <- group_by(length.quantiles, stype, tegund, length.cat)
wl.agg <- summarise(teg.age, 
                    oslaegt.mean = mean(oslaegt, na.rm = T), 
                    lengd.mean = mean(lengd, na.rm = T),
                    length = length(oslaegt))

# Select only length greater than 4
wl.agg <- filter(wl.agg, length > 4)

# Calculate the weight-length parameters
wl.params <- wl.agg %>% 
  group_by(stype,tegund) %>% 
  do(model = tryCatch(lm(log(oslaegt.mean) ~ log(lengd.mean), data = .),
                      error = function(e) NULL)) %>%
  filter(model != "NULL") %>%
  group_by(stype,tegund) %>% 
  do(data.frame(var = c("a","b"),
                coef = coef(.$model[[1]]))) %>%
  dcast(., stype + tegund~var, value.var = "coef")

# Put a on the correct scale and omit missing data
wl.params$a <- exp(wl.params$a)
wl.params <- na.omit(wl.params)

# Combine with species names 

wl.params$stype <- factor(wl.params$stype, levels = c("spring", "fall", "other"))

# Summarize over code for the aggregated groups
# Then select and keep data based on the following rank of surveys:
# 1. spring
# 2. fall
# 3. other
wl.params <- wl.params %>% 
  group_by(stype, tegund) %>%
  summarize(a = mean(a),
            b = mean(b)) %>%
  group_by(tegund) %>%
  filter(rank(stype) == 1)
wl.params <- merge(wl.params, hafro_link, by = "tegund", all.x = FALSE,
                   all.y = FALSE)
# Copy into IAbioparams.xls
write.csv(wl.params, file = "ab_params.csv")

# Summarize over species
wl.fg <- wl.params %>%
  group_by(Code) %>%
  summarize(a = mean(a),
            b = mean(b))

cat(paste("li_a_", wl.fg$Code, " ", wl.fg$a,
          " Coefficient of allometic length-weight relation for ",
          wl.fg$Code, "\n", sep = ""), sep ="")

cat(paste("li_b_", wl.fg$Code, " ", wl.fg$b,
          " Coefficient of allometic length-weight relation for ",
          wl.fg$Code, "\n", sep = ""), sep ="")

#
# von Bertalanffy parameters 
#

vb.agg <-
kv_st %>%
  group_by(stype, tegund, aldur) %>%
  summarise(oslaegt.mean = mean(oslaegt, na.rm = T), 
            lengd.mean = mean(lengd, na.rm = T),
            length = length(oslaegt))

# Select only length greater than 4
vb.agg <- filter(vb.agg, length > 4)

vb.params <- vb.agg %>% 
  group_by(stype,tegund) %>% 
  do(model = tryCatch(nls(lengd.mean~Linf*(1-exp(-K*(aldur-t0))),
                          data= ., start=list(Linf=80, K=0.2, t0=-1)),
                      error = function(e) NULL)) %>%
  filter(model != "NULL") %>%
  group_by(stype,tegund) %>% 
  do(data.frame(var = c("Linf","k", "t0"),
                coef = coef(.$model[[1]]))) %>%
  dcast(., stype + tegund~var, value.var = "coef")

# Combine with species names ------------------------------
vb.params$stype <- factor(vb.params$stype, levels = c("spring", "fall", "other"))

# Summarize over code for the aggregated groups
vb.params <- vb.params %>% 
  group_by(stype, tegund) %>%
  summarize(k = mean(k),
            Linf = mean(Linf),
            t0 = mean(t0)) %>%
  group_by(tegund) %>%
  filter(rank(stype) == 1)

vb.params <- merge(vb.params, hafro_link, by = "tegund", all.x = TRUE,
                   all.y = FALSE)
# Copy into IAbioparams.xls
write.csv(vb.params, file = "vonbert_params.csv")

# Combine w and vb params and save them
wvb.params <- merge(wl.params, vb.params, by = "tegund", all.x = TRUE, all.y = TRUE)
wvb.params <- select(wvb.params, -starts_with("stype"))
save(wvb.params, file = paste(k_dir, "vb_ab_params.Rdata", sep = ""))

#
# Minimum length at maturity
#

# Subset maturity flag based on kynthroski > 1
mature <- filter(kv.st, kynthroski > 1)
min.len <-
  mature %>%
  group_by(stype, tegund) %>%
  summarize(min.length = min(lengd))
  
# Combine with species names
minlen.params <- inner_join(min.len, fisk.tegund)
minlen.params$stype <- factor(minlen.params$stype, levels = c("spring", "fall", "other"))

# Summarize over code for the aggregated groups
minlen.params <-
  minlen.params %>% 
  group_by(stype, Code) %>%
  summarize(min.length = mean(min.length)) %>%
  group_by(Code) %>%
  filter(rank(stype) == 1)

cat(paste("min_li_mat_", minlen.params$Code, " ", minlen.params$min.length,
          "  minimum length at maturity for ", minlen.params$Code,"\n", 
          sep = ""), sep = "")

#
# AgeClassSize
#

age.class.size <- kv.st %>%
  group_by(stype, tegund) %>%
  summarize(ageclass= quantile(aldur, prob = .99, na.rm = T))

acs.params <- inner_join(age.class.size, fisk.tegund)
acs.params$stype <- factor(acs.params$stype, levels = c("spring", "fall", "other"))
acs.params <- filter(acs.params, ageclass !="NaN")
acs.params <-
  acs.params %>% 
  group_by(stype, Code) %>%
  summarize(ageclass = ceiling(mean(ageclass, na.rm=T))) %>%
  group_by(Code) %>%
  filter(rank(stype) == 1)

cat(paste(acs.params$Code, "_AgeClassSize ",round(acs.params$ageclass/10),
          "   Number of years represented in each age class of ",
          acs.params$Code,"\n",sep=""), sep = "")

#
# First mature age class
#

mat.ageclass <- mature %>%
  group_by(stype, tegund) %>%
  summarize(age.matur = min(aldur, na.rm = T))
mat.params <- inner_join(mat.ageclass, fisk.tegund)
mat.params$stype <- factor(mat.params$stype, levels = c("spring", "fall", "other"))
mat.params <- filter(mat.params, age.matur !="NaN")
mat.params <-
  mat.params %>% 
  group_by(stype, Code) %>%
  summarize(age.matur = mean(age.matur, na.rm=T)) %>%
  group_by(Code) %>%
  filter(rank(stype) == 1)

mat.params <- inner_join(mat.params,acs.params)
mat.params$acm <- floor(mat.params$age.matur/round((mat.params$ageclass/10)))

cat(paste(mat.params$Code, "_age_mat ",mat.params$acm,
          "    First mature age class of ", mat.params$Code,"\n",
          sep = ""), sep = "")

#
# mum parameters
#

# Load the data set
load(paste(kDir,"/vb_ab_params.Rdata", sep = ""))

#
# Proportion mature 
#
prop_mature <- select(kv.st, tegund, stype, aldur, kynthroski)

mature_by_age <- prop_mature %>%
	group_by(stype, tegund, aldur) %>%
	summarize(mature = sum(kynthroski > 1, na.rm = T)/length(na.omit(kynthroski)))

#
# Distribution of vertebrate groups
#

stodvar_df$quarter <- ifelse(stodvar_df$man <= 3, "Q1",
                             ifelse(stodvar_df$man > 3 & stodvar_df$man <= 6, "Q2",
                                    ifelse(stodvar_df$man > 6 & stodvar_df$man <= 9, "Q3", "Q4")))


# Read in spatial data
atlantis <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/atlantis/", layer = "atlantis_WGS84")

# Coordinate points
work <- select(stodvar_df, synis.id, lat, lon)
work <- na.omit(work)
coords <- data.frame(long = work$lon, lat = work$lat)
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

# This is to use the survey points as best as possible
# and not throw away those in box 19.
# So I remove points that overlap with Iceland, proper
# Select only those points in Iceland

sub <- work[work$box_id == 19,]
coords_zero <- data.frame(long = sub$lon, lat = sub$lat, synis.id = sub$synis.id)
coords_zero <- na.omit(coords_zero)
coords_zero <- SpatialPoints(coords_zero, proj4string = CRS("+proj=longlat +datum=WGS84"))

# Read in Iceland Polygon
iceland <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/island/wgs84/", layer = "island_poly_WGS84")
ice_ob <- SpatialPolygons(iceland@polygons,proj4string=CRS("+proj=longlat +datum=WGS84"))
box_ice <- coords_zero %over% ice_ob
box_ice[is.na(box_ice)] <- 999
ice_zeros <- data.frame(lon = coords_zero$long, lat = coords_zero$lat, synis.id = coords_zero$synis.id, box_id = box_ice)

# I am writing this to CSV to work on it in QGIS
write.csv(ice_zeros, file = "Desktop/ice_zeros.csv")

# Read in the now fixed polygon
fixed_ice <- readOGR(dsn = "/Users/chris/Dropbox/hi/atlantis/spatial_data/fixed_points_around_iceland/", layer = "fix")
fixed_box <- fixed_ice@data

# Combine the box_id with coords
box_assign <- data.frame(lon = work$lon, lat = work$lat, 
                         synis.id = work$synis.id, box_id = pts_in)
# Drop box 19
box_assign <- box_assign[box_assign$box_id != 19,]

# Combine the fixed data
box_assign <- rbind(box_assign, fixed_box)
saveRDS(box_assign, file = "~/Dropbox/hi/atlantis/hafro_data/boxid_survey.Rds")

# Read in the data
box_assign <- readRDS(file = "~/Dropbox/hi/atlantis/hafro_data/boxid_survey.Rds")

# Join stodvar and box_assign
st_box <- inner_join(box_assign, stodvar_df, by = "synis.id")
st_box$half <- ifelse(st_box$man < 7, "H1", "H2")

# Remove box IDs that are boundary
boundaries <- c("44","45", "46", "47", "48", "49", "50", "0", "2", "3", "26", "27", "28", "41", "42", "19", "52")
st_box <- filter(st_box, !(box_id %in% boundaries))

# Calculate the weights 
wts_box_qt <- st_box %>%
  group_by(box_id, half) %>%
  summarize(num = length(synis.id))

wts_qt <- st_box %>%
  group_by(half) %>%
  summarize(wts = length(synis.id))

wts <- left_join(wts_box_qt, wts_qt, by = "half")
wts$weights <- wts$num/wts$wts

wts_tmp <- merge(st_box, wts)

## Merge this with the all.kv
wts_kv <- merge(wts_tmp, all.kv)
wts_kv <- merge(wts_kv, hafro_link)
wts_kv$mat <- ifelse(wts_kv$kynthroski > 1, 1, 0)

## Calculate how many species by quarter
sp_qt <- wts_kv %>%
  group_by(Code, box_id, half, mat) %>%
  summarize(nums = length(lengd))

# Select only the variables needed from wts_kv
kv_sub <- select(wts_kv, box_id, Code, weights, half)

# Merge these together
wt_sp <- inner_join(sp_qt, kv_sub, by = c("box_id", "Code", "half"))

# Take only the unique rows
wts_unique <- unique(wt_sp)

# Multiply numbers by weights
wts_unique$wt_num <- wts_unique$nums*wts_unique$weights
wts_unique <- na.omit(wts_unique)

wts_unique <- wts_unique %>%
  group_by(Code, half, mat) %>%
  mutate(prop_box = wt_num/sum(wt_num))

wts_unique <- wts_unique[order(wts_unique$Code,wts_unique$half,wts_unique$mat, wts_unique$box_id),]
wts_unique$unq <- paste(wts_unique$Code, wts_unique$box_id, sep = "")

box_id <- 0:52

#
# 1st half, adults
#

wts_first_adults <- filter(wts_unique, half == "H1" & mat == "1")
h1a_ids <- rep(unique(wts_first_adults$Code),each = length(box_id))
h1a_prop_data <- data.frame(Code = h1a_ids,box_id = box_id, perc = 0)
h1a_prop_data$unq <-paste(h1a_prop_data$Code, h1a_prop_data$box_id, sep = "")

for(i in 1:nrow(wts_first_adults)){
  h1a_prop_data$perc[which(wts_first_adults$unq[i] == h1a_prop_data$unq)] = wts_first_adults$prop_box[i]
}

ads <- split(h1a_prop_data, h1a_prop_data$Code)
for(i in 1:length(ads)){
  match_s1 <- grep(paste("F", names(ads)[i], "_S1 53", sep = ""), bio_params)
  match_s2 <- grep(paste("F", names(ads)[i], "_S2 53", sep = ""), bio_params)
  if(length(ads[[i]]$perc) == 0){
    dist <- c(0, 1,rep(0,51))
  } else dist <- ads[[i]]$perc
  bio_params[match_s1 + 1] <- paste(dist, collapse = " ") 
  bio_params[match_s2 + 1] <- paste(dist, collapse = " ") 
}

#
# 2nd half, adults
#

wts_second_adults <- filter(wts_unique, half == "H2" & mat == "1")
h2a_ids <- rep(unique(wts_second_adults$Code),each = length(box_id))
h2a_prop_data <- data.frame(Code = h2a_ids,box_id = box_id, perc = 0)
h2a_prop_data$unq <-paste(h2a_prop_data$Code, h2a_prop_data$box_id, sep = "")

for(i in 1:nrow(wts_second_adults)){
  h2a_prop_data$perc[which(wts_second_adults$unq[i] == h2a_prop_data$unq)] = wts_second_adults$prop_box[i]
}

ads <- split(h2a_prop_data, h2a_prop_data$Code)
for(i in 1:length(ads)){
  match_s3 <- grep(paste("F", names(ads)[i], "_S3 53", sep = ""), bio_params)
  match_s4 <- grep(paste("F", names(ads)[i], "_S4 53", sep = ""), bio_params)
  if(length(ads[[i]]$perc) == 0){
    dist <- c(0, 1,rep(0,51))
  } else dist <- ads[[i]]$perc
  bio_params[match_s3 + 1] <- paste(dist, collapse = " ") 
  bio_params[match_s4 + 1] <- paste(dist, collapse = " ") 
}

#
# 1st half, juvs
#

wts_first_juv <- filter(wts_unique, half == "H1" & mat == "0")
h1j_ids <- rep(unique(wts_first_juv$Code),each = length(box_id))
h1j_prop_data <- data.frame(Code = h1j_ids,box_id = box_id, perc = 0)
h1j_prop_data$unq <-paste(h1j_prop_data$Code, h1j_prop_data$box_id, sep = "")

for(i in 1:nrow(wts_first_juv)){
  h1j_prop_data$perc[which(wts_first_juv$unq[i] == h1j_prop_data$unq)] = wts_first_juv$prop_box[i]
}

juv <- split(h1a_prop_data, h1a_prop_data$Code)
for(i in 1:length(ads)){
  match_s1 <- grep(paste("F", names(ads)[i], "_S1juv 53", sep = ""), bio_params)
  match_s2 <- grep(paste("F", names(ads)[i], "_S2juv 53", sep = ""), bio_params)
  if(length(ads[[i]]$perc) == 0){
    dist <- c(0, 1,rep(0,51))
  } else dist <- ads[[i]]$perc
  bio_params[match_s1 + 1] <- paste(dist, collapse = " ") 
  bio_params[match_s2 + 1] <- paste(dist, collapse = " ") 
}

#
# 2nd half, juv
#

wts_second_juv <- filter(wts_unique, half == "H2" & mat == "0")
h2j_ids <- rep(unique(wts_second_juv$Code),each = length(box_id))
h2j_prop_data <- data.frame(Code = h2j_ids,box_id = box_id, perc = 0)
h2j_prop_data$unq <-paste(h2j_prop_data$Code, h2j_prop_data$box_id, sep = "")

for(i in 1:nrow(wts_second_juv)){
  h2j_prop_data$perc[which(wts_second_juv$unq[i] == h2j_prop_data$unq)] = wts_second_juv$prop_box[i]
}

juv <- split(h2j_prop_data, h2j_prop_data$Code)
for(i in 1:length(juv)){
  match_s3 <- grep(paste("F", names(juv)[i], "_S3juv 53", sep = ""), bio_params)
  match_s4 <- grep(paste("F", names(juv)[i], "_S4juv 53", sep = ""), bio_params)
  if(length(juv[[i]]$perc) == 0){
    dist <- c(0, 1,rep(0,51))
  } else dist <- juv[[i]]$perc
  bio_params[match_s3 + 1] <- paste(dist, collapse = " ") 
  bio_params[match_s4 + 1] <- paste(dist, collapse = " ") 
}

write(bio_params, file = k_bio)

#
# Cephlapods
#

st_box_c <- filter(st_box, !(box_id %in% boundaries))

# Calculate the weights 
wts_box_qt <- st_box %>%
  group_by(box_id) %>%
  summarize(num = length(synis.id))
wts_box_qt$weights <-wts_box_qt$num/sum(wts_box_qt$num)
wts_tmp <- merge(st_box, wts_box_qt)

## Merge this with the all.kv
wts_kv <- merge(wts_tmp, all.kv)
wts_kv <- merge(wts_kv, hafro_link)
wts_ceph <- filter(wts_kv, Code == "CEP")

sp_cep <- wts_ceph %>%
  group_by(box_id) %>%
  summarize(nums = length(lengd))

# Select only the variables needed from wts_kv
cep_sub <- select(wts_ceph, box_id, weights)

# Merge these together
wt_cep <- inner_join(sp_cep, cep_sub, by = c("box_id"))

# Take only the unique rows
cep_unique <- unique(wt_cep)

# Multiply numbers by weights
cep_unique$wt_num <- cep_unique$nums*cep_unique$weights

cep_unique <- cep_unique %>%
  mutate(prop_box = wt_num/sum(wt_num))

cep_unique <- cep_unique[order(cep_unique$box_id),]
h1a_prop_data <- data.frame(box_id = box_id, perc = 0)

for(i in 1:nrow(cep_unique)){
  h1a_prop_data$perc[which(cep_unique$box_id[i] == h1a_prop_data$box_id)] = cep_unique$prop_box[i]
}

match_s1 <- grep(paste("FCEP", "_S1 53", sep = ""), bio_params)
match_s2 <- grep(paste("FCEP", "_S2 53", sep = ""), bio_params)
match_s3 <- grep(paste("FCEP", "_S3 53", sep = ""), bio_params)
match_s4 <- grep(paste("FCEP", "_S4 53", sep = ""), bio_params)
match_s1j <- grep(paste("FCEP", "_S1juv 53", sep = ""), bio_params)
match_s2j <- grep(paste("FCEP", "_S2juv 53", sep = ""), bio_params)
match_s3j <- grep(paste("FCEP", "_S3juv 53", sep = ""), bio_params)
match_s4j <- grep(paste("FCEP", "_S4juv 53", sep = ""), bio_params)
dist <- h1a_prop_data$perc
bio_params[match_s1 + 1] <- paste(dist, collapse = " ") 
bio_params[match_s2 + 1] <- paste(dist, collapse = " ") 
bio_params[match_s3 + 1] <- paste(dist, collapse = " ") 
bio_params[match_s4 + 1] <- paste(dist, collapse = " ") 
bio_params[match_s1j + 1] <- paste(dist, collapse = " ") 
bio_params[match_s2j + 1] <- paste(dist, collapse = " ") 
bio_params[match_s3j + 1] <- paste(dist, collapse = " ") 
bio_params[match_s4j + 1] <- paste(dist, collapse = " ") 

write(bio_params, file = k_bio)
