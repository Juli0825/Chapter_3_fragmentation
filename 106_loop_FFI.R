library(terra)
library(sf)
library(sp)
library(landscapemetrics)
library(raster)

# Define the Mollweide projection
mollweide_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

start_time <- Sys.time()
start_time

# Function to process a single tile
process_tile <- function(tile_path, output_folder) {
  # Extract tile name from path
  tile_name <- tools::file_path_sans_ext(basename(tile_path))
  cat("Processing tile:", tile_name, "\n")
  
  # Check if this tile has already been processed
  if(file.exists(file.path(output_folder, paste0(tile_name, "_metrics.csv")))) {
    cat("Tile", tile_name, "already processed. Skipping.\n")
    return(NULL)
  }
  
  # Load the tile
  tile <- tryCatch({
    rast(tile_path)
  }, error = function(e) {
    cat("Error loading tile", tile_name, ":", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(tile)) return(NULL)
  
  # Project to Mollweide
  tile_moll <- project(tile, mollweide_crs)
  # Clean up original tile to free memory
  rm(tile)
  gc()
  
  # Create binary forest map
  tile_binary <- ifel(tile_moll > 30, 1, 0)
  
  # Get the extent of the projected raster
  ext_moll <- ext(tile_moll)
  
  # Create the grid
  grid_res <- 5000
  grid <- rast(ext=ext_moll, resolution=grid_res, crs=mollweide_crs)
  
  # Fill with cell numbers
  values(grid) <- 1:ncell(grid)
  
  # Convert to polygons
  grid_poly <- as.polygons(grid)
  # Clean up grid to free memory
  rm(grid)
  gc()
  
  # Convert to spatial polygons don't think we need this
  #fishnet_sp <- as(grid_poly, "Spatial") 
  
  # Clean up grid_poly
  rm(grid_poly)
  gc()
  
  # Add ID column
  fishnet_sp@data <- data.frame(FID = 1:length(fishnet_sp))
  
  # Calculate metrics
  metrics <- tryCatch({
    result <- sample_lsm(tile_binary, fishnet_sp, what = c("lsm_c_ed", "lsm_c_pd", "lsm_c_area_mn"))
    # Immediately clean up large objects
    rm(tile_moll, tile_binary, fishnet_sp)
    gc()
    result
  }, error = function(e) {
    cat("Error calculating metrics for", tile_name, ":", e$message, "\n")
    # Clean up in case of error too
    rm(tile_moll, tile_binary, fishnet_sp)
    gc()
    return(NULL)
  })
  
  if(is.null(metrics)) return(NULL)
  
  # Add tile ID
  metrics$tile_id <- tile_name
  
  # Proper NA filtering
  metrics_clean <- metrics[!is.na(metrics$value), ]
  
  # Save metrics
  write.csv(metrics_clean, file.path(output_folder, paste0(tile_name, "_metrics.csv")), 
            row.names = FALSE)
  
  cat("Processed tile:", tile_name, "- Saved", nrow(metrics_clean), "rows (filtered from", 
      nrow(metrics), "original rows)\n")
  
  # Final cleanup
  rm(metrics, metrics_clean)
  gc()
  
  return(tile_name)
}

# Create output folder
output_folder <- "metrics_before_pnr"
dir.create(output_folder, showWarnings = FALSE)

# Get list of all tile files
tiles_folder <- "R:/Chapter_3_fragmentation/global_forest-watch_Tropical_Tree_Cover/10m_forest_cover"
tile_files <- list.files(tiles_folder, pattern = "\\.tif$", full.names = TRUE)

# Find which tiles have already been processed
processed_files <- list.files(output_folder, pattern = "_metrics.csv", full.names = FALSE)
processed_tiles <- gsub("_metrics.csv", "", processed_files)

cat("Found", length(tile_files), "total tile files\n")
cat("Already processed", length(processed_tiles), "tiles\n")

# Process only tiles that haven't been processed yet
for(tile_path in tile_files) {
  tile_name <- tools::file_path_sans_ext(basename(tile_path))
  
  # Skip if already processed
  if(tile_name %in% processed_tiles) {
    cat("Skipping already processed tile:", tile_name, "\n")
    next
  }
  
  tryCatch({
    process_tile(tile_path, output_folder)
    # Force garbage collection after each tile
    gc()
  }, error = function(e) {
    cat("Error processing tile", tile_name, ":", e$message, "\n")
    # Force cleanup even after errors
    gc()
  })
  
  # Display memory usage after each tile
  cat("Memory in use after tile:", format(utils::object.size(globalenv()), units = "auto"), "\n")
}

cat("Processing complete!\n")

end_time <- Sys.time()
end_time