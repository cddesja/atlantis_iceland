# Number of layers
z = 7

# Number of boxes
b = 53

# Define t, this will become a vector ...
t = 86400

# Name for model
modelName = "Iceland_Hydro"

# Variable names
varNames = c("exchange","dest_b","dest_k")

# Insert fill values for exchange, dest_b and dest_k
fillExch <- 0
fillDestB <- -1
fillDestK <- -1
dest <- 1

# Name of BGM file
BGMfile <- "atlantis_L93.bgm"

# Specify the name of the cdf file
init.file <- paste(modelName,".cdf",sep="")


# Head of the init cdf file
header <- c("netcdf ",modelName," { ","\n","dimensions:", "\n",
	"\t","t = UNLIMITED ; // ", "\n",
	"\t","b = ",b,";","\n",
	"\t","z = ",z,";", "\n",
	"\tdest = ",dest," ;\n",
	"variables:\n")



varDefn <- c("\tdouble t(t) ; \n",
		"\t\tt:units = \"seconds since 2008-06-01 00:00:00 +10\" ;\n",
		"\t\tt:dt = 86400. ;\n",
	"\tdouble exchange(t, b, z, dest) ;\n",
		"\t\texchange:_FillValue = ", fillExch,". ;\n",
	"\tint dest_b(t, b, z, dest) ;\n",
		"\t\tdest_b:_FillValue = ", fillDestB," ;\n",
	"\tint dest_k(t, b, z, dest) ;\n",
		"\t\tdest_k:_FillValue = ", fillDestK," ;\n\n",
	"// global attributes:\n",
		"\t\t:title = \"trivial\" ;\n",
		"\t\t:geometry = \"",BGMfile,"\" ;\n",
		"\t\t:parameters = \"\" ; \n",
		"data:\n\n")

cat(varDefn,sep="")


dummy.data <- matrix(nrow = b*dest, ncol= z)
dummy.data[is.na(dummy.data)] <- "_"

# Get the data in the correct format
dummy.data <- paste(dummy.data,collapse=", ")

###############################################
# Creating the actual initial conditions file #
###############################################

sink(file=init.file)
cat(header,sep="")
cat(varDefn,sep="")

# This loop creates all the dummy data
cat("t = ",t," ;\n\n",sep="")
for(i in 1:length(varNames)){
cat(paste(varNames[i], "=","\n",
	dummy.data,";","\n\n"))
}
sink()
