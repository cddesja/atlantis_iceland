## This script is for the Icelandic Atlantis model
## This is original created by Christopher David Desjardins
##
## 04/03/2014 - Initial creation
##            - Merge with pre-existing Atlantis R scripts

# install.packages("geo", repos="http://r-forge.r-project.org", type="source")

################
## Preface    ##
################
# Load necessary libraries
################
library(ggplot2) # graphing
library(reshape2) # data manipulation
library(ncdf4)   # manipulating netcdf4 files
library(grid)
library(gridExtra)
library(plyr)
library(geo)
library(mapproj)
library(SDMTools)
library(rgdal)

###############
## Section 1 ##
###############
# Setting up the spatial data
###############

# Converting the statistical rectangles to latitude and longitude
reitmapping <-
  ddply(read.table('/home/chris/Documents/HI/Research/MareFrame/IcelandicAtlantis/spatial/reitmapping.tsv',header=TRUE,as.is=TRUE),'GRIDCELL',mutate,
        lat = geo::sr2d(GRIDCELL)$lat,
        lon = geo::sr2d(GRIDCELL)$lon)

test <- 
  ddply(reitmapping, 'SUBDIVISION',
        function(x){
          ##   2
          ## 1 x 3
          ##   4
          sides <- rep(10*x$GRIDCELL,each=4) + 1:4
          up <- d2sr(x$lat + 0.125, x$lon)
          down <- d2sr(x$lat - 0.126, x$lon)
          left <- d2sr(x$lat, x$lon - 0.25)
          right <- d2sr(x$lat, x$lon + 0.26)
          
          for(i in 1:nrow(x)){
            ## up
            if(up[i] %in% x$GRIDCELL){
              sides[4*(i-1)+2] <- NA
              sides[sides==(10*up[i]+4)] <- NA
            }
            ## down
            if(down[i] %in% x$GRIDCELL){
              sides[4*(i-1)+4] <- NA
              sides[sides==(10*down[i]+2)] <- NA
            }
            ## left
            if(left[i] %in% x$GRIDCELL){
              sides[4*(i-1)+1] <- NA
              sides[sides==(10*left[i]+3)] <- NA
            }
            ## right
            if(right[i] %in% x$GRIDCELL){
              sides[4*(i-1)+3] <- NA
              sides[sides==(10*right[i]+1)] <- NA
            }
          }
          sides <- sides[!is.na(sides)]
          corners <- ddply(data.frame(side=rep(sides,each=2)),
                       'side',
                       function(x){
                         loc <- sr2d(floor(x$side[1]/10))
                         loc <- c(loc$lat,loc$lon)
                         if((x$side[1] %% 10) == 1){
                           up.loc <- loc+c(0.125,-0.25)
                           down.loc <- loc+c(-0.125,-0.25)
                         }
                         if((x$side[1] %% 10) == 3){
                           up.loc <- loc+c(0.125,0.25)
                           down.loc <- loc+c(-0.125,0.25)
                         }
                         if((x$side[1] %% 10) == 2){
                           up.loc <- loc+c(0.125,-0.25)
                           down.loc <- loc+c(0.125,0.25)
                         }
                         if((x$side[1] %% 10) == 4){
                           up.loc <- loc+c(-0.125,-0.25)
                           down.loc <- loc+c(-0.125,0.25)
                         }
                         tmp <- as.data.frame(rbind(up.loc,down.loc))
                         names(tmp) <- c('lat','lon')
                         return(tmp)                        
                       })
          corners$order <- NA
          corners$order[1:2] <- 1:2
          counter <- 1:nrow(corners)
          i <- 2
          for(order in 3:(length(sides))){
            tmp <- counter[is.na(corners$order) &
                           corners$lat == corners$lat[i] &
                           corners$lon == corners$lon[i]][1]
            corners$order[corners$side == corners$side[tmp]] <- order
            corners <- corners[-tmp,]
            i <- which(corners$order == order)
            
          }
          corners$side <- NULL
          return(arrange(corners,order))
          
        })

write.csv(test,file = "stat_rec.csv",row.names=F)
write.csv(island,file = "island.csv",row.names=F)


(bcareas.plot <- 
  ggplot(data.frame(lat=0,lon=0), aes(lon,lat)) +
#  geom_polygon(alpha=0.5,data=gbdypi.100,fill='gray90') +
  geom_path(data=gbdypi.100,colour = "red",size = 0.3) + #alpha=0.5,fill='gray90',
 geom_path(data=gbdypi.200,colour = "blue",size = 0.3) + #alpha=0.5,fill='gray90',
 geom_path(data=gbdypi.200,colour = "orange",size = 0.3) +
  geom_path(data=gbdypi.1000,lty=2,size = 0.3) +
# geom_path(aes(lon,lat,group=SUBDIVISION),
#            data=subset(test,SUBDIVISION %in% bcareas),size = 0.3) +
 geom_path(aes(lon,lat,group=SUBDIVISION),data=test,size = 0.3) +
  geom_polygon(data=island, col = 'black' ,fill = 'gray90',size = 0.3) +
  geom_polygon(data=greenland, col = 'black', fill = 'white') +
  geom_polygon(data=eyjar, col = 'black', fill = 'grey90',size = 0.3) +
  geom_polygon(data=faeroes, col = 'black', fill = 'white') +
  geom_point() +
  coord_map('mercator', xlim=c(-36,0),ylim=c(56,74)) +
#  xlab('LONGITUDE') +
#  ylab('LATITUDE') + 
  theme_bw() +
  theme(panel.border = element_rect(colour='black',size = 0.5),
        #legend.position=c(0.9, 0.85),
        panel.background = element_rect(fill='white'),#element_rect(fill='deepskyblue'),
        axis.ticks.length = unit(-5 , "points"),
        axis.ticks.margin = unit(10 * 1.25, "points"),
        axis.text.x = element_text(family='Arial',size=10),
        panel.grid=element_blank(),
        axis.title = element_blank()) +
  scale_x_continuous(breaks=seq(-26,-10,4))  +
  scale_y_continuous(breaks=seq(63, 67, 2)))# +
#  geom_path(data = twohmiles, lty = 2) +
#  geom_path(data=icelandrivers,size=0.1,col='blue') +
#  geom_path(data=glaciers,size=0.5))

length(table(test$SUBDIVISION))


# Atlantis Plots
atlantis.map <- readOGR(dsn = "Dropbox/hi/atlantis/spatial_data/atlantis/", layer = "atlantis_WGS84")
atlantis.map.tmp <- fortify(atlantis.map)
atlantis.map.tmp$island <- ifelse(atlantis.map.tmp$id == 51 | atlantis.map.tmp$id == 35, 1, 0)
atlantis.map.tmp$island <- as.factor(atlantis.map.tmp$island)

# Add Greenland
greenland.map <- readOGR(dsn = "Dropbox/hi/atlantis/spatial_data/island/wgs84/", layer = "greenland_poly_WGS84")
greenland.map.tmp <- fortify(greenland.map)

# Add Iceland
island.map <- readOGR(dsn = "Dropbox/hi/atlantis/spatial_data/island/wgs84/", layer = "island_poly_WGS84")
island.map.tmp <- fortify(island.map)

# Add Faroe
faroe.map <- readOGR(dsn = "Dropbox/hi/atlantis/spatial_data/island/wgs84/", layer = "faroe_poly_WGS84")
faroe.map.tmp <- fortify(faroe.map)



# Four different plots - different colors and level of labelling of the axes
pdf(file = "iceland_contours_100m.pdf")
ggplot(data=atlantis.map.tmp, aes(long, lat, group=group))+ geom_path() + theme_bw() + xlab("Longitude") + ylab("Latitude") + coord_map() + theme(legend.position="none")
dev.off()

ggplot(data=atlantis.map.tmp, aes(long, lat, group=group)) + geom_polygon(aes(fill = island)) + geom_path() + theme_bw() + xlab("Longitude") + ylab("Latitude") + coord_map() + scale_y_continuous(breaks=seq(58, 76, 1)) + scale_x_continuous(breaks =seq(-50,0,2)) + scale_fill_manual(values = c("#6699FF","white")) + theme(legend.position="none") + geom_path(data = greenland.map.tmp, aes(long, lat))

ggplot(data=atlantis.map.tmp, aes(long, lat, group=group)) + geom_polygon(aes(fill = island)) + geom_path() + theme_bw() + xlab("Longitude") + ylab("Latitude") + coord_map() + theme(legend.position="none")

ggplot(data=atlantis.map.tmp, aes(long, lat, group=group)) + geom_polygon(aes(fill = island)) + geom_path() + theme_bw() + xlab("Longitude") + ylab("Latitude") + coord_map() + scale_fill_manual(values = c("#6699FF","white")) + theme(legend.position="none")

# Map with Greenland, Iceland, and Faroe Islands
pdf(file = "study_map.pdf")
ggplot(data=atlantis.map.tmp, aes(long, lat, group=group)) + geom_polygon(aes(fill = island)) + geom_path() + theme_bw() + xlab("Longitude") + ylab("Latitude") + coord_map() + scale_fill_manual(values = c("#6699FF","white")) + theme(legend.position="none") + geom_path(data = greenland.map.tmp, aes(long, lat)) + geom_path(data = island.map.tmp, aes(long, lat)) + geom_path(data = faroe.map.tmp, aes(long, lat))  + scale_y_continuous(breaks=seq(58, 76, 1)) + scale_x_continuous(breaks =seq(-50,0,2)) + coord_cartesian(xlim = c(-48, 2), ylim = c(58, 74))
dev.off()


################
## Section X  ##
################
# Exploring initial conditions file
###############

# Set working directory to example
setwd("~/Documents/HI/Research/MareFrame/Atlantis/Example/SETas_model_New/")

init.cond <- nc_open("init_vmpa_setas_25032013.nc")

# List the contents of the object
names(init.cond)

# List all the variables in the initial conditions
names(init.cond$var)

# Get the data on the first variable, Benthic Carnivore N
ben.car <- ncvar_get(nc=init.cond,varid="Benthic_Carniv_N")

 

################
## Appendix A ##
################
# Running Atlantis example file
###############

# Set the correct working directory
setwd("~/Dropbox/HI/Research/Atlantis/Example/SETas_model_New/")

# ------------ #
# Run Atlantis # 
# ------------ #
system("atlantisNew -i iceland_init.nc 0 -o output_iceland.nc -r iceland_run.prm -f iceland_force.prm -p iceland_physics.prm -b iceland_biol.prm -s GroupsIceland.csv -d OutputFake")

# Set new directory to output folder
setwd("OutputFolderTest")

# ----------- #
# Output file #
# ----------- # 
outputSETAS <- nc_open("outputSETAS.nc")

# Gather all the variable names
varNames <- names(outputSETAS$var)

# This gets the data from the outputSETAS file
varData <- list() 
for(i in 1:length(varNames)){
varData[[i]] <- ncvar_get(nc=outputSETAS,varid=varNames[i])
}
names(varData) <- varNames

# Times
time <- outputSETAS$dim$t$vals

# Examine Diatom_N
DiatomData <- NULL
for(i in 1:length(time)){
	DiatomData <- rbind(DiatomData,varData$Diatom_N[,,i])
}
rep(time,5)

# Read in biological indice data
BioIndx <- read.table(file = "outputSETASBiomIndx.txt", header = T)

# ---
# Try and plot most of the variables in the data set
# ---
BioIndxRel <- BioIndx[,c(1,34:64)]

# This stacks all the variables on each other nicely and creates a new variable called Species
# that stores the names of the species
BioIndxRel.w <- melt(BioIndxRel ,  id = 'Time', variable_name = 'Species')

# Drop pelgaic bacteria as there densities are crazy high (> 50!)
BioIndxRel.w <- BioIndxRel.w[BioIndxRel.w$Species != "RelPB",]

# Plot the species
pdf(file = "viewOutput/relative_densities_biology.pdf", width = 8.267, height = 11.692)
ggplot(aes(x = Time, y = value), data = BioIndxRel.w) + geom_line(aes(color = Species)) + ylab("Relative Density") + theme_bw() + ggtitle("Relative Density of Various Oceanic Biota") + guides(col=guide_legend(ncol=2))
dev.off()




# Read in the catch per fishery
CatchFish <- read.table(file = "outputSETASCatchPerFishery.txt", header = T)
CatchFish$Fishery.s <- as.character(CatchFish$Fishery)

strsplit(CatchFish$Fishery.s[1],nchar(CatchFish$Fishery.s[1])-3,fixed=TRUE)
strsplit(CatchFish$Fishery.s, split="[[:upper:]]")

gsub('([[:upper:]])', ' \\1', CatchFish$Fishery.s)

# NetCDF4 files



