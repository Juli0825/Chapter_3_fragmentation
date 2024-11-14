library(landscapemetrics)
library(raster)

# Load the forest cover raster (binary 10 m resolution)
forest_cover <- raster("global_forest-watch_Tropical_Tree_Cover/10m_forest_cover/10N_110E.tif")

# Check that the raster data is loaded correctly
print(forest_cover)

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
















