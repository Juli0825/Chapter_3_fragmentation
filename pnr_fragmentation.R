library(landscapemetrics)
library(raster)
library(terra)
library(sf)

######### 5km grid is too fine, use 50km gird for a trial and then reduce grid size ###############
# Define 50 km grid size in meters (Mollweide CRS uses meters)
grid_50km <- 50000  # 50 km

# Create a fishnet raster with 50 km grid resolution
fishnet_50km_rast <- rast(grid_extent, resolution = grid_50km, crs = crs(forest_cover_rast))

# # Assign unique IDs to each cell in the grid
# fishnet_rast[] <- 1:ncell(fishnet_rast)

# Convert the fishnet raster to polygons
fishnet_50km_poly <- as.polygons(fishnet_50km_rast)

# Calculate fragmentation metrics for each 50 km grid cell
metrics <- sample_lsm(
  landscape = forest_cover_rast,
  y = fishnet_50km_poly,
  what = c("lsm_c_ed", "lsm_c_pd", "lsm_c_area_mn")
)

# Convert metrics to a data frame and save as CSV
metrics_df <- as.data.frame(metrics)
write.csv(metrics_df, "R:/Chapter_3_fragmentation/fragmentation_metrics_50km.csv")

# Check the results
print(metrics_df)













































# # Load the forest cover raster (10 m resolution)
# forest_cover <- raster("global_forest-watch_Tropical_Tree_Cover/10m_forest_cover/10N_110E.tif")
# 
# # Check that the raster data is loaded correctly
# print(forest_cover)
# 
# # Define the Mollweide CRS
# mollweide_crs <- "+proj=moll +datum=WGS84 +no_defs"
# 
# # Project raster to Mollweide
# forest_cover_moll <- projectRaster(forest_cover, crs = mollweide_crs, method = 'ngb')
# print(forest_cover_moll)

# Using Terra might be quicker
forest_cover_rast <- rast(forest_cover_moll)

# # Define the grid size as 5km, 
# # Assuming 1 degree = 111.32 km (near the equator), 5 km - 0.045 degrees
# grid_size <- 0.045  
# 
# # Create a fishnet over the extent of the raster
# fishnet <- raster(extent(forest_cover_moll), res = grid_size, crs = crs(mollweide_crs))
# fishnet[] <- 1:length(fishnet)  # Assign unique IDs to each cell if desired


grid_extent <- ext(forest_cover_rast)
# Generate a fishnet (raster with unique IDs for each cell)
fishnet_rast <- rast(grid_extent, resolution = grid_size, crs = crs(forest_cover_rast))
# Convert the raster fishnet into polygons
fishnet_poly <- as.polygons(fishnet_rast)
# Generate a fishnet polygon directly
fishnet_poly <- vect(grid_extent, type = "grid", res = grid_size, crs = mollweide_crs)


# Transfer fishnet into a rast
fishnet_5km_rast <- rast(fishnet)

# Convert the fishnet raster to polygons
fishnet_poly <- rasterToPolygons(fishnet, dissolve = TRUE)

# Convert to polygons
fishnet_5km <- as.polygons(fishnet_5km_rast)

# Save as a shapefile
writeVector(fishnet_poly, "R:/Chapter_3_fragmentation/fishnet_5km2.shp", filetype = "ESRI Shapefile")

# Calculate fragmentation metrics within each fishnet cell
metrics <- sample_lsm(
  landscape = forest_cover,
  y = fishnet_poly,
  what = c("lsm_c_ed", "lsm_c_pd", "lsm_c_area_mn")
)

# Convert results to a data frame and save as CSV
metrics_df <- as.data.frame(metrics)
write.csv(metrics_df, "R:/Chapter_3_fragmentation/global_forest-watch_Tropical_Tree_Cover/fragmentation_metrics_5km2.csv")
















# Downsample the raster to a lower resolution (e.g., 30 m) for testing
forest_cover_resampled <- aggregate(forest_cover, fact=3)  # 'fact=3' makes each new cell 30 m

# Now calculate the metrics on the resampled raster
metrics <- calculate_lsm(
  landscape = forest_cover_resampled,
  what = c("lsm_c_ed", "lsm_c_pd", "lsm_c_area_mn")
)

# Calculate fragmentation metrics on the entire 5 km² raster (10 m resolution)
# You are calculating Edge Density (ED), Patch Density (PD), and Mean Patch Area (MPA)
metrics <- calculate_lsm(
  landscape = forest_cover,
  what = c("lsm_c_ed", "lsm_c_pd", "lsm_c_area_mn")
)

# Convert results to a data frame for easy viewing and further analysis
metrics_df <- as.data.frame(metrics)

# Save the results to a CSV file
write.csv(metrics_df, "R:/Chapter_3_fragmentation/global_forest-watch_Tropical_Tree_Cover/10m_forest_cover/fragmentation_metrics_5km2.csv")

# View results
print(metrics_df)
















