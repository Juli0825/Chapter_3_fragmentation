### This script is to get the change in current and future borneo FFI
# Just for ICCB so everything is wrong

library(dplyr)
library(terra)

s <- Sys.time()
s

# Read both datasets
global_ffi <- read.csv("Data/not_in_use/global_ffi_results.csv")  # Current forest
borneo_ffi <- read.csv("Data/ICCB/Borneo_FFI/borneo_metrics_ffi.csv")  # Regional analysis

# Clean up tile names for matching
# Remove "_fishnet" suffix from borneo tiles and "moll_binary_" prefix for comparison
borneo_ffi <- borneo_ffi %>%
  mutate(
    tile_clean = gsub("_fishnet$", "", fishnet_tile),
    tile_clean = gsub("^moll_binary_", "", tile_clean)
  )

global_ffi <- global_ffi %>%
  mutate(
    tile_clean = gsub("^moll_binary_", "", tile_id)
  )

# Get unique tiles from borneo data
borneo_tiles <- unique(borneo_ffi$tile_clean)
print(paste("Borneo tiles found:", paste(borneo_tiles, collapse = ", ")))

# Filter global data for tiles that exist in borneo data
global_borneo <- global_ffi %>%
  filter(tile_clean %in% borneo_tiles)

print(paste("Global data filtered to", nrow(global_borneo), "rows"))
print(paste("Borneo data has", nrow(borneo_ffi), "rows"))

# Check for matching plot_ids within tiles
global_borneo %>%
  group_by(tile_clean) %>%
  summarise(n_plots = n_distinct(plot_id)) %>%
  print()

borneo_ffi %>%
  group_by(tile_clean) %>%
  summarise(n_plots = n_distinct(plot_id)) %>%
  print()

# Join datasets by plot_id and tile
# Calculate ΔFFI (global - borneo, so positive means global > borneo)
delta_ffi <- global_borneo %>%
  inner_join(borneo_ffi, 
             by = c("plot_id", "tile_clean"),
             suffix = c("_global", "_borneo")) %>%
  mutate(
    # Calculate change: borneo - global (future - current)
    delta_ffi = FFI - ffi,
    change_direction = case_when(
      delta_ffi < -0.05 ~ "Strong Decrease",
      delta_ffi < 0 ~ "Decrease", 
      delta_ffi > 0.05 ~ "Strong Increase",
      delta_ffi > 0 ~ "Increase",
      TRUE ~ "No Change"
    ),
    change_magnitude = abs(delta_ffi)
  ) %>%
  select(
    plot_id, x, y, 
    tile_id = tile_id, 
    ffi_current = ffi, 
    ffi_future = FFI, 
    delta_ffi, 
    change_direction, 
    change_magnitude,
    fragmentation_class_current = fragmentation_class
  )

# Print summary statistics
cat("\nSummary of FFI changes:\n")
print(summary(delta_ffi$delta_ffi))

cat("\nChange direction distribution:\n")
print(table(delta_ffi$change_direction))

# Save results
write.csv(delta_ffi, "borneo_delta_ffi2.csv", row.names = FALSE)
cat(paste("\nSaved", nrow(delta_ffi), "records to borneo_delta_ffi.csv\n"))

# Optional: Create a simple raster from the results
if(nrow(delta_ffi) > 0) {
  # Create a SpatVector from the points
  # Try different CRS specifications for Mollweide
  mollweide_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  delta_points <- vect(delta_ffi, geom = c("x", "y"), crs = mollweide_crs)
  
  # Create a raster template (adjust resolution as needed)
  ext_borneo <- ext(delta_points)
  rast_template <- rast(ext_borneo, resolution = 5000, crs = mollweide_crs)
  
  # Rasterize the delta_ffi values
  delta_raster <- rasterize(delta_points, rast_template, field = "delta_ffi", fun = "mean")
  
  # Save the raster
  writeRaster(delta_raster, "borneo_delta_ffi.tif", overwrite = TRUE)
  cat("Saved raster to borneo_delta_ffi.tif\n")
  
  # Also create a raster of change magnitude
  mag_raster <- rasterize(delta_points, rast_template, field = "change_magnitude", fun = "mean")
  writeRaster(mag_raster, "borneo_change_magnitude.tif", overwrite = TRUE)
  cat("Saved change magnitude raster to borneo_change_magnitude.tif\n")
}

# Print some example records to verify
cat("\nFirst 10 records:\n")
print(head(delta_ffi, 10))

e <- Sys.time()
total <- e-s
total