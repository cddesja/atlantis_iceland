#
# Atlantis maps
#

library(ggplot2) # graphing
library(dplyr)
library(geo)
library(mapproj)
library(rgdal)

atlantis.map <- readOGR(dsn = "Dropbox/hi/atlantis/spatial_data/atlantis/", layer = "atlantis_WGS84")
atlantis.map.tmp <- fortify(atlantis.map)
atlantis.map.tmp$island <- as.character(ifelse(atlantis.map.tmp$id == 51 | atlantis.map.tmp$id == 35, 1, 0))
#bound_boxes <- c(50,49,46:48, 43:45, 36:42)
#matches <- which(atlantis.map.tmp$id %in% bound_boxes)
#atlantis.map.tmp[matches,"island"] = 2
#atlantis.map.tmp$island <- as.factor(atlantis.map.tmp$island)

# Add Greenland
greenland.map <- readOGR(dsn = "Dropbox/hi/atlantis/spatial_data/island/wgs84/", layer = "greenland_poly_WGS84")
greenland.map.tmp <- fortify(greenland.map)

# Add Iceland
island.map <- readOGR(dsn = "Dropbox/hi/atlantis/spatial_data/island/wgs84/", layer = "island_poly_WGS84")
island.map.tmp <- fortify(island.map)

# Add Faroe
faroe.map <- readOGR(dsn = "Dropbox/hi/atlantis/spatial_data/island/wgs84/", layer = "faroe_poly_WGS84")
faroe.map.tmp <- fortify(faroe.map)

ggplot(data=atlantis.map.tmp, aes(long, lat, group=group)) + geom_polygon(fill = "#0072B2") + geom_path(col = "red") + theme_bw() + xlab("Longitude") + ylab("Latitude") + coord_map() + theme(legend.position="none") + geom_polygon(data = greenland.map.tmp, aes(long, lat), col = "#999999") + geom_polygon(data = island.map.tmp, aes(long, lat),col = "#999999") + geom_polygon(data = faroe.map.tmp, aes(long, lat),col = "#999999")   


