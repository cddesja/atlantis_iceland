# Initial conditions -------------
#
# 28/03/2013, Christopher David Desjardins
# - Initial creation of script

# Required libraries
library(ncdf4)

# Path to init.nc file
init_file <- "/home/chris/Dropbox/HI/Research/Atlantis/Example/SETas_model_New/init_vmpa_setas_25032013.nc"

# Load the data
init_ex <- nc_open(file=init_file)

# Save all the variables names
init_names <- names(init.ex$var)

# Number of layers
z = 7

# Number of boxes
b = 53

# Name for model
model_name = "iceland_init"

# Name the output CDF file
init_file <- paste(model_name,".cdf",sep="")

# Create the header ---------------------------------------------------------------
header <- c("netcdf ",model_name," { ","\n","dimensions:", "\n",
            "\t","t = UNLIMITED ; // ", "\n",
            "\t","b = ",b," ;","\n",
            "\t","z = ",z," ;", "\n",
            "variables:\n")


# Pull out the attribute data for all of the variables

# Variables that need periods
periods <- c("decay", "svel", "xvel", "psize", "b_dens", "i_conc", "f_conc", "_FillValue")

# Quoted variables
quotes <- c("bmtype","units", "long_name")

sink(file = init_file)
cat(header,sep="")
for(i in 1:length(init.names)){
  tmp <- ncatt_get(nc = init_ex,varid=init_names[i])
  tmp_unlist <- unlist(tmp)
  att <- attributes(tmp_unlist)$names
  for(j in 1:length(att)){
    if(any(att[j] == PERIODS)){
      tmp_unlist[j] <- paste(tmp[j],".",sep="")
    }
  }

  tmp_test <- NULL
  for(k in 1:length(tmp_unlist)){
    if(any(att[k] == quotes)){
      tmp_test[k] <- paste("\t\t",init_names[i],":",att[k]," = \"",tmp_unlist[k],"\" ;\n", sep="")
    } else 
      tmp_test[k] <- paste("\t\t",init_names[i],":",att[k]," = ",tmp_unlist[k]," ;\n", sep="")
  }
 
  var_type <- init_ex[[13]][i][[1]][[7]]
  if(sum(init_ex[[13]][i][[1]][[17]]) == 19)
  {
    cat("\t",var_type,  " ", init_names[i],"(t, b, z) ;\n", tmp_test, sep="")
  } else if(sum(init.ex[[13]][i][[1]][[17]]) == 12){
    cat("\t",var_type, " ", init_names[i],"(t, b) ;\n", tmp_test, sep="")
  } else 
  {
    cat("\t",var_type, " ", init_names[i],"(b, z) ; \n",tmp_test, sep="")
  }
}
cat("\tdouble t(t) ;\n",
    "\t\tt:units = \"seconds since 1983-01-01 00:00:00 +10\" ;\n")
sink()

tmp$decay <- paste(tmp$decay,".",sep="")

# Top of the file information
x <-ncatt_get(nc=init.ex,varid="Benthic_Carniv_N")
cat(x$bmtype)

# Variable data
ben.car <- ncvar_get(nc=init.ex,varid="nominal_dz")


for(i in 1:length(init.names)){
  var.type <- init.ex[[13]][i][[1]][[7]]
  if(sum(init.ex[[13]][i][[1]][[17]]) == 19)
  {
    cat("\t",var.type,  " ", init.names[i],"(t, b, z) ;\n",sep="")
  } else if(sum(init.ex[[13]][i][[1]][[17]]) == 12){
    cat("\t",var.type, " ", init.names[i],"(t, b) ;\n",sep="")
  } else 
    {
      cat("\t",var.type, " ", init.names[i],"(b, z) ; \n",sep="")
    }
}

