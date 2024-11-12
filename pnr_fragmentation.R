install.packages("raster")
install.packages("ncdf4")

library(raster)
library(ncdf4)


nc_file <- "C:/Users/uqjli47/Downloads/9bc77f13bf50cbdddf34c6b44c299e27/2022.nc"

# Open the NC file
nc_data <- nc_open(nc_file)

# Print the variable names
print(nc_data$var)

# Load the NetCDF file as a raster brick
nc_raster <- brick(nc_file, varname = "lccs_class")  # Replace 'your_variable_name' with the variable you're interested in

print(nc_raster)


# Save as a GeoTIFF file
writeRaster(nc_raster, filename = "2022LC.tif", format = "GTiff", overwrite = TRUE)

getwd()
