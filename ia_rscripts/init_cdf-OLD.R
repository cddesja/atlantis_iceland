bgm <- readLines("Dropbox/hi/atlantis/iceland_atlantis/atlantis_L93.bgm")

## Area --------
match <- grep("area",bgm)
area_string <- bgm[match]
area_split <- strsplit(area_string,"\t")
areas <- NULL
for(i in 1:length(area_split)){
  areas[i] <- area_split[[i]][2]  
}

## Depth ----
match_d <- grep("botz",bgm)
depth_string <- bgm[match_d]
depth_string <- depth_string[-1]
depth_split <- strsplit(depth_string,"\t")
depths <- NULL
for(i in 1:length(depth_split)){
  depths[i] <- depth_split[[i]][2]  
}

area_csv <- data.frame(ids = 0:52,areas = areas, depths = depths)
write.csv(area_csv,file="Downloads/areas.csv",row.names=FALSE)
area_csv <- read.csv(file = "Downloads/areas.csv")

# Assign Iceland and Faroe Islands to 0
area_csv$areas[20] <- 0
area_csv$areas[53] <- 0
area_csv$prop=area_csv$areas/sum(area_csv$areas)
write.csv(area_csv,file="Downloads/areas.csv",row.names=FALSE)

## Fill in FillValues :)

fillValues <- readLines("Dropbox/hi/atlantis/iceland_atlantis/helper_files/VertFillValues_for_CDF.txt")
cdf <- readLines("Dropbox/hi/atlantis/iceland_atlantis/iceland_init.cdf")
fv <- strsplit(fillValues, " = ")

fv_ds <- NULL
for(i in 1:length(fv)){
  fv_ds[i] <- strsplit(fv[[i]][2],". ")[[1]][1]
}

for(i in 1:length(fv)){
  if(grep(fv[[i]][1],cdf)){
    x <- grep(fv[[i]][1],cdf)
    if(is.numeric(grep("1000.",cdf[x]))){
      cdf[x] <- gsub("1000.",fv_ds[i],cdf[x])
    }
  }
}

sink(file = "test.cdf")
cat(cdf, sep = "\n")
sink()

