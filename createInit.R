
# Set Path of Function Group
FGfilePath = "/home/chris/github/AtlantisTools/python/SETasGroups.csv"

# Read in function group csv
FunGroup <- read.csv(FGfilePath,header=T,sep="\t")

# Number of layers
z = 7

# Number of boxes
b = 11

# Name for model
modelName = "IcelandAtlantis"

# Head of the init cdf file
header <- c("netcdf ",modelName," { ","\n","dimensions:", "\n",
	"\t","t = UNLIMITED ; // ", "\n",
	"\t","b = ",b,";","\n",
	"\t","z = ",z,";", "\n","variables:","\n")

# For testing
# cat(head)

# Save functional group names
fgnames <- FunGroup$Name

# Set parameters
params <- c("bmtype = ","units = ","long_name = ","sumtype = ","dtype = ","inwc = ","insed = ","dissol = ", "partic = ","passive = ","svel = ","xvel = ","psize = ","b_dens = ","i_conc = ","f_conc = ","_FillValue = ")

for(i in 1:length(fgnames)){
	var <- paste("double ",fgnames[i],"(t, b, z)", ";",sep="")
}


varDefn <- list()
for(j in 1:length(fgnames)){
	vartmp <- NULL
	for(i in 1:length(params)){
		x <- paste(fgnames[j],params[i],sep=":")
		vartmp <-rbind(vartmp,x)
		}
	varDefn[[j]] <- vartmp
	rm(vartmp)
	}

# Create a dummy matrix 
x <- matrix(nrow = b, ncol = z)
x[1,2] = 2
x[2,1] = 4


# Transpose the matrix to keep the 
# entries in the right place
x <- t(x)

# Get the data in the correct format
dummy.data <- paste(x,collapse=", ")

###############################################
# Creating the actual initial conditions file #
###############################################

# Specify the name of the cdf file
init.file <- paste(modelName,".cdf",sep="")

sink(file=init.file)
cat(header,sep="")
for(i in 1:length(varDefn)){
	cat("\t",varDefn[[i]],sep="\n","\n")
}
# This loop creates all the dummy data
for(i in 1:length(fgnames)){
cat(paste(fgnames[i], "=","\n",
	dummy.data,";","\n\n"))
}
sink()