# load data 
library("ncdf4")
output <- nc_open("data/output_iceland.nc")
biomass <- read.table("data/output_icelandBiomIndx.txt", header = T)
ssb <- read.table("data/output_icelandSSB.txt", header = TRUE)
yoy <- read.table("data/output_icelandYOY.txt", header = TRUE)
bgm <- readLines("data/atlantis_L93.bgm")
match <- grep("Rel",colnames(biomass))  # identifies relative biomass
relative <- biomass[,c(1,match)]

# extract tracer numbers, levels for the sliders,
# and relative biomass
ssb_names <- colnames(ssb)[-1]
yoy_names <- colnames(yoy)[-1]
rel_names <- colnames(relative)[-1]
max_tracer <- output$nvars
max_layers <- length(output$dim$z$vals)
max_time <- length(output$dim$t$vals)
var_names <- names(output$var)

# extract tracers for the ncd4 object
vars <- list()
for (i in 1:output$nvars){
  vars[[i]] <- ncvar_get(nc = output, output$var[[i]])
}
names(vars) <- var_names
