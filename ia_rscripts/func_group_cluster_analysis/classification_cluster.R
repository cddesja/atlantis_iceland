library(dplyr)
library(rfishbase)

fiskur_nafn <- read.csv("~/Dropbox/hi/atlantis/hafro_data/fiskar_heiti_link.csv")

# Remove non-fish
fish <- filter(fiskur_nafn, fish == "FISH")

# Discard fish observations that were fewer than 50 for the model
all_kv <- tbl_df(all.kv)
all_kv <- filter(all_kv, tegund %in% fish$tegund)

# Convert stodvar to a tbl_df
stodvar_df <- tbl_df(stodvar)

# Join these tables by synis.id
kv_st <- inner_join(all_kv, stodvar_df)

# Select only those that have been observed on several occasions
numyrs <- kv_st %>%
  group_by(tegund) %>%
  summarize(num_yrs = length(unique(ar)),
            uni_id = length(unique(synis.id)))

# Drop all species observed less than two years
species_two <- numyrs$num_yrs >= 2
numyrs <- numyrs[species_two,]

# Subset kv_st
kv_st <- filter(kv_st, tegund %in% numyrs$tegund)

#
# Lmax, max age, maximum weight
#

morphometrics <- kv_st %>% 
  group_by(tegund) %>%
  summarize(max_size = quantile(lengd, prob = .99, na.rm = T),
            max_age = quantile(aldur, prob = .99, na.rm = T),
            max_weight = quantile(oslaegt, prob = .99, na.rm = T),
            max_depth = quantile(dypi, prob = .99, na.rm = T),
            mean_depth = mean(dypi,na.rm = T))

# Subset out the FDF and FMP fish
morphometrics <- inner_join(morphometrics, fiskur_nafn)
class_fish <- filter(morphometrics, Code == "FDF" | Code == "FMP")
class_fish <- select(class_fish, -icelandic, -fish)
write.csv(class_fish, file = "other_fish.csv")
class <- read.csv(file = "class_fish.csv")

# Get food diet, try and combine with other species
ffiskar_sub <- filter(ffiskar, ranfiskur %in% class_fish$tegund)

# Combine this with the prey species
diet_sub <- inner_join(ffiskar_sub, fhopar)
table(diet_sub$ranfiskur)

diets <- diet_sub %>%
  group_by(ranfiskur, faeduhopur) %>%
  summarize(amt_grp = length(faeduhopur))

classify_fish <- read.csv(file = "other_fish.csv")

# Other fish
fdf <- filter(classify_fish, Code == "FDF")
fdf <- fdf[, c(1,3:5, 7:11)]
fdf <- na.omit(fdf)
fdf[,-1] <- scale(fdf[,-1])

# Ward's Hierarchical Clustering
d <- dist(fdf[,-1], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=4) # cut tree into 5 clusters
names(groups) <- fdf$tegund
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=4, border="red")
table(groups)
write.csv(cbind(fdf,groups), file = "fdf_groups.csv")

# Other fish
fmp <- filter(classify_fish, Code == "FMP" & tegund != 11)  # only ok before groups are added
fmp <- fmp[, c(1,3:5, 7:11)]
fmp <- na.omit(fmp)
fmp[,-1] <- scale(fmp[,-1])

# Ward's Hierarchical Clustering
d <- dist(fmp[,-1], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=3, border="red")
write.csv(cbind(fmp,groups), file = "fmp_groups.csv")