#using 0/1 binary map, but only join forest ffi to clean fishnet
#i think this workded! 
#and it's more effiecent to start from na/1 binary maps
# but it's masking out non-forest using moll_binary layers
# for fluent workflow, it would be better to join forest ffi with clean fishnet

# Load required libraries
library(tidyverse)
library(sf)
library(terra)
library(ggplot2)
library(viridis)

# Function to process a single tile with diagnostic outputs
test_single_tile_masking <- function(tile_id, 
                                     binary_rasters_dir = "Data/moll_binary/",
                                     fishnet_dir = "Data/fishnets/",
                                     raster_dir = "Data/ffi_rasters_masked/",
                                     chunks = 4,  # Adjustable chunk parameter
                                     create_diagnostic_plots = TRUE) {
  
  print(paste("Starting test process for tile:", tile_id))
  
  # Find the binary forest raster file
  binary_raster_file <- list.files(binary_rasters_dir, 
                                   pattern = paste0(tile_id, "\\.tif$"), 
                                   full.names = TRUE)
  
  # Find the existing fishnet metrics file
  fishnet_metrics_file <- file.path(fishnet_dir, paste0("moll_binary_", tile_id, "_ffi_metrics.gpkg"))
  
  # Check if required files exist
  if(length(binary_raster_file) == 0) {
    stop(paste("No binary forest raster found for tile:", tile_id))
  }
  
  if(!file.exists(fishnet_metrics_file)) {
    stop(paste("No fishnet metrics file found for tile:", tile_id))
  }
  
  print(paste("Found binary raster:", binary_raster_file))
  print(paste("Found fishnet metrics:", fishnet_metrics_file))
  
  # Load the binary forest raster
  print("Loading binary forest raster...")
  binary_r <- terra::rast(binary_raster_file)
  print(paste("Binary raster dimensions:", dim(binary_r)[1], "x", dim(binary_r)[2]))
  
  # Check memory usage
  print(paste("Current memory usage:", format(utils::object.size(binary_r), units = "MB")))
  
  # Load the fishnet with metrics
  print("Loading fishnet with FFI metrics...")
  fishnet_with_metrics <- sf::st_read(fishnet_metrics_file, quiet = TRUE)
  print(paste("Fishnet contains", nrow(fishnet_with_metrics), "cells"))
  
  # Report memory use after loading fishnet
  print(paste("Current memory usage after loading fishnet:", 
              format(utils::object.size(fishnet_with_metrics), units = "MB")))
  
  # Filter for plots with valid FFI values
  valid_plots <- fishnet_with_metrics %>% dplyr::filter(!is.na(ffi))
  print(paste("Valid plots with FFI values:", nrow(valid_plots), 
              "(", round(100 * nrow(valid_plots) / nrow(fishnet_with_metrics), 1), "%)"))
  
  # Create diagnostic plots if requested
  if(create_diagnostic_plots && nrow(valid_plots) > 0) {
    print("Creating diagnostic plots...")
    
    # Plot histogram of FFI values
    ffi_hist <- ggplot(valid_plots, aes(x = ffi)) +
      geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
      labs(title = paste("Distribution of FFI Values for Tile", tile_id),
           x = "Forest Fragmentation Index (FFI)",
           y = "Count") +
      theme_minimal()
    
    # Save histogram
    ggsave(paste0("tile_", tile_id, "_ffi_histogram.png"), plot = ffi_hist, 
           width = 8, height = 6, dpi = 150)
    print("Created FFI histogram plot")
    
    # Create a small sample plot of fishnet FFI values (if not too large)
    if(nrow(valid_plots) < 10000) {
      ffi_map <- ggplot() +
        geom_sf(data = valid_plots, aes(fill = ffi), color = NA) +
        scale_fill_viridis_c(option = "plasma", direction = -1) +
        labs(title = paste("FFI Values in Fishnet for Tile", tile_id),
             fill = "FFI") +
        theme_minimal()
      
      ggsave(paste0("tile_", tile_id, "_ffi_fishnet_map.png"), plot = ffi_map, 
             width = 8, height = 6, dpi = 150)
      print("Created FFI fishnet map")
    } else {
      print("Fishnet too large for diagnostic plot, skipping")
    }
  }
  
  if(nrow(valid_plots) > 0) {
    # Get original resolution
    original_res <- res(binary_r)
    print(paste("Original raster resolution:", original_res[1], "x", original_res[2]))
    
    # Convert valid plots to terra vect
    valid_vect <- terra::vect(valid_plots)
    
    # Create an empty raster at the original resolution
    output_raster <- terra::rast(ext=ext(binary_r), resolution=original_res, crs=crs(binary_r))
    
    # Process in chunks to handle memory constraints
    print(paste("Processing in", chunks, "x", chunks, "chunks"))
    
    # Create chunk boundaries
    e <- ext(binary_r)
    x_range <- e[2] - e[1]
    y_range <- e[4] - e[3]
    
    # Track memory at each step
    chunk_count <- 0
    total_chunks <- chunks * chunks
    
    for(i in 1:chunks) {
      for(j in 1:chunks) {
        chunk_count <- chunk_count + 1
        # Calculate chunk extent
        x_min <- e[1] + (i-1) * x_range/chunks
        x_max <- e[1] + i * x_range/chunks
        y_min <- e[3] + (j-1) * y_range/chunks
        y_max <- e[3] + j * y_range/chunks
        
        chunk_ext <- ext(x_min, x_max, y_min, y_max)
        print(paste("Processing chunk", chunk_count, "of", total_chunks, 
                    "(", round(100 * chunk_count/total_chunks), "%)"))
        
        # Create a template raster for this chunk
        chunk_rast <- terra::crop(binary_r, chunk_ext)
        
        # Rasterize FFI values for this chunk
        chunk_ffi <- terra::rasterize(valid_vect, chunk_rast, field = "ffi")
        
        # Mask with binary forest
        chunk_ffi_masked <- terra::mask(chunk_ffi, chunk_rast, maskvalues=0, updatevalue=NA)
        
        # Add to output raster
        output_raster <- terra::merge(output_raster, chunk_ffi_masked)
        
        # Report memory usage
        print(paste("  Memory usage after chunk", chunk_count, ":", 
                    format(utils::memory.size(), units = "MB"), "MB"))
        
        # Clear memory
        rm(chunk_rast, chunk_ffi, chunk_ffi_masked)
        gc()
      }
    }
    
    # Save the complete masked raster
    raster_file <- file.path(raster_dir, paste0(tile_id, "_ffi.tif"))
    print(paste("Writing masked FFI raster for tile:", tile_id))
    terra::writeRaster(output_raster, raster_file, overwrite = TRUE)
    
    # Create final diagnostic plot
    if(create_diagnostic_plots) {
      print("Creating masked raster visualization...")
      
      # Convert output raster to data frame for plotting
      output_df <- as.data.frame(output_raster, xy = TRUE)
      colnames(output_df) <- c("x", "y", "ffi")
      output_df <- output_df %>% dplyr::filter(!is.na(ffi))
      
      # Create visualization
      masked_map <- ggplot() +
        geom_raster(data = output_df, aes(x = x, y = y, fill = ffi)) +
        scale_fill_gradientn(
          colors = rev(viridis(10, option = "plasma")),
          limits = c(0, 1),
          breaks = seq(0, 1, by = 0.1),
          name = "FFI"
        ) +
        labs(
          title = paste("Masked Forest Fragmentation Index for Tile", tile_id),
          subtitle = "FFI values shown only in forest areas",
          caption = "Higher values indicate more fragmentation"
        ) +
        theme_minimal() +
        coord_equal()
      
      # Save the visualization
      #ggsave(paste0("tile_", tile_id, "_masked_ffi_map.png"), plot = masked_map, 
             #width = 10, height = 8, dpi = 200)
      print("Created masked FFI visualization")
    }
    
    print(paste("Successfully created masked FFI raster for tile:", tile_id))
    print(paste("Output saved to:", raster_file))
    return(TRUE)
  } else {
    print(paste("No valid FFI values for tile:", tile_id))
    return(FALSE)
  }
}

# Run for a single tile
start_time <- Sys.time()
cat("Starting single tile FFI masking test at:", as.character(start_time), "\n")

# Choose a tile ID to test - REPLACE WITH YOUR TILE ID
test_tile_id <- "00N_010E"  # e.g., "tile_001"

# Run the test function with the selected tile
result <- tryCatch({
  test_single_tile_masking(
    tile_id = test_tile_id,
    binary_rasters_dir = "Data/moll_binary/",
    fishnet_dir = "Data/fishnets/",
    raster_dir = "Data/ffi_rasters_masked/",
    chunks = 4,  # Adjust if needed (higher number = smaller chunks, less memory per chunk)
    create_diagnostic_plots = TRUE
  )
}, error = function(e) {
  print(paste("Error processing tile", test_tile_id, ":", e$message))
  return(NULL)
})

end_time <- Sys.time()
time_taken <- end_time - start_time
cat("Test process completed in:", as.character(time_taken), "\n")

if(!is.null(result) && result) {
  cat("SUCCESS: Masked FFI raster was created successfully!\n")
} else {
  cat("FAILED: Could not create masked FFI raster.\n")
}