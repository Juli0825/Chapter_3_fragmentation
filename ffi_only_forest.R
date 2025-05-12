# Complete FFI calculation pipeline - Saves clean fishnets separately for reuse
library(tidyverse)
library(sf)
library(terra)
library(ggplot2)
library(viridis)

# Function to process a single tile properly
process_tile_forest_only <- function(tile_id, 
                                     metrics_folder = "Data/metrics_before_pnr",
                                     binary_rasters_dir = "Data/moll_binary/",
                                     fishnet_dir = "Data/fishnets_clean/",
                                     results_dir = "Data/ffi_results_forest_only/",
                                     raster_dir = "Data/ffi_rasters_forest_only/",
                                     grid_resolution = 5000) {
  
  print(paste("Processing tile:", tile_id))
  
  # Create output directories if they don't exist
  dir.create(fishnet_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(raster_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 1. Load binary forest raster
  binary_raster_file <- list.files(binary_rasters_dir, 
                                   pattern = paste0(tile_id, "\\.tif$"), 
                                   full.names = TRUE)
  
  if(length(binary_raster_file) == 0) {
    print(paste("No binary raster found for tile:", tile_id))
    return(NULL)
  }
  
  binary_r <- terra::rast(binary_raster_file)
  print(paste("Loaded binary raster with dimensions:", 
              paste(dim(binary_r), collapse = " x ")))
  
  # 2. Create the COMPLETE fishnet (same as fragmentation metrics script)
  print("Creating complete fishnet...")
  ext_moll <- ext(binary_r)
  
  # Create grid with 5000m resolution (same as your fragmentation script)
  grid <- terra::rast(ext = ext_moll, resolution = grid_resolution, crs = crs(binary_r))
  
  # Fill with cell numbers (same as fragmentation script)
  values(grid) <- 1:ncell(grid)
  
  # Convert to polygons
  print("Converting grid to polygons...")
  grid_poly <- terra::as.polygons(grid)
  grid_sf <- st_as_sf(grid_poly)
  
  # Add proper IDs and structure (matching your fragmentation script)
  names(grid_sf)[1] <- "plot_id"  # This should correspond to the FID in fragmentation script
  grid_sf$tile_id <- tile_id
  
  # Calculate rows and columns for grid reference
  n_cols <- ncol(grid)
  n_rows <- nrow(grid)
  
  grid_sf <- grid_sf %>%
    mutate(
      row = ceiling(plot_id / n_cols),
      col = ((plot_id - 1) %% n_cols) + 1,
      grid_resolution = grid_resolution
    )
  
  print(paste("Created", nrow(grid_sf), "grid cells"))
  
  # Clean up grid raster to free memory
  rm(grid)
  gc()
  
  # SAVE CLEAN FISHNET (reusable for all scenarios)
  clean_fishnet_file <- file.path(fishnet_dir, paste0(tile_id, "_fishnet_clean.gpkg"))
  st_write(grid_sf, clean_fishnet_file, delete_dsn = TRUE)
  print(paste("Saved CLEAN fishnet:", clean_fishnet_file))
  
  # 3. Identify which cells contain forest
  print("Identifying forest cells...")
  # Extract forest coverage for each cell (same approach as would be used in sample_lsm)
  forest_coverage <- terra::extract(binary_r, grid_poly, fun = "sum", na.rm = TRUE)
  
  # Add forest coverage to grid
  grid_sf$forest_pixels <- forest_coverage[,2]  # Second column contains the sum
  grid_sf$has_forest <- grid_sf$forest_pixels > 0
  
  print(paste("Forest cells:", sum(grid_sf$has_forest), "out of", nrow(grid_sf), "total cells"))
  
  # Clean up
  rm(grid_poly)
  gc()
  
  # 4. Load metrics from CSV
  print("Loading metrics CSV...")
  
  # Find the metrics file for this tile
  metrics_file <- list.files(metrics_folder, 
                             pattern = paste0(tile_id, "_metrics.csv$"), 
                             full.names = TRUE)
  
  if(length(metrics_file) == 0) {
    print(paste("No metrics file found for tile:", tile_id))
    return(NULL)
  }
  
  # Read metrics
  tile_metrics <- read.csv(metrics_file)
  
  # Check the structure of the metrics
  print("Metrics structure:")
  print(head(tile_metrics))
  print(paste("Unique plot_ids in metrics:", length(unique(tile_metrics$plot_id))))
  
  # Filter for forest class and cells that actually contain forest
  forest_cells <- grid_sf %>% 
    filter(has_forest == TRUE) %>% 
    pull(plot_id)
  
  # Filter metrics for forest class (1) and metric types needed for FFI
  forest_metrics <- tile_metrics %>%
    filter(class == 1) %>%  # Forest class
    filter(plot_id %in% forest_cells) %>%  # Only cells that contain forest
    filter(metric %in% c("area_mn", "ed", "pd"))
  
  print(paste("Forest metrics records:", nrow(forest_metrics)))
  
  if(nrow(forest_metrics) == 0) {
    print("No forest metrics found for this tile")
    return(NULL)
  }
  
  # 5. Calculate FFI for forest cells only (same as your original FFI calculation)
  print("Calculating FFI for forest cells...")
  
  # Calculate boundaries for normalization
  calculate_boundaries <- function(data, metric_name) {
    values <- data %>% filter(metric == metric_name) %>% pull(value)
    q1 <- quantile(values, 0.25, na.rm = TRUE)
    q3 <- quantile(values, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    upper <- q3 + 1.5 * iqr
    lower <- q1 - 1.5 * iqr
    return(list(lower = lower, upper = upper))
  }
  
  area_boundaries <- calculate_boundaries(forest_metrics, "area_mn")
  ed_boundaries <- calculate_boundaries(forest_metrics, "ed")
  pd_boundaries <- calculate_boundaries(forest_metrics, "pd")
  
  # Cap and normalize values
  forest_metrics_capped <- forest_metrics %>%
    group_by(metric) %>%
    mutate(
      value_capped = case_when(
        metric == "area_mn" & value > area_boundaries$upper ~ area_boundaries$upper,
        metric == "area_mn" & value < area_boundaries$lower ~ area_boundaries$lower,
        metric == "ed" & value > ed_boundaries$upper ~ ed_boundaries$upper,
        metric == "ed" & value < ed_boundaries$lower ~ ed_boundaries$lower,
        metric == "pd" & value > pd_boundaries$upper ~ pd_boundaries$upper,
        metric == "pd" & value < pd_boundaries$lower ~ pd_boundaries$lower,
        TRUE ~ value
      )
    ) %>%
    ungroup()
  
  # Normalize to 0-1 range
  forest_metrics_normalized <- forest_metrics_capped %>%
    group_by(metric) %>%
    mutate(
      value_normalized = (value_capped - min(value_capped, na.rm = TRUE)) / 
        (max(value_capped, na.rm = TRUE) - min(value_capped, na.rm = TRUE))
    ) %>%
    ungroup()
  
  # Reshape to wide format
  forest_metrics_wide <- forest_metrics_normalized %>%
    select(plot_id, metric, value_normalized) %>%
    pivot_wider(names_from = metric, values_from = value_normalized)
  
  # Calculate FFI only for plots with all three metrics
  forest_metrics_wide <- forest_metrics_wide %>%
    filter(!is.na(area_mn) & !is.na(ed) & !is.na(pd)) %>%
    mutate(
      area_mn_inv = 1 - area_mn,
      ffi = (ed + pd + area_mn_inv) / 3,
      fragmentation_class = case_when(
        ffi <= 0.2 ~ "low",
        ffi <= 0.8 ~ "medium",
        TRUE ~ "high"
      )
    )
  
  print(paste("Calculated FFI for", nrow(forest_metrics_wide), "forest cells"))
  
  # 6. Create FFI results by joining to a COPY of the grid (not the original clean one)
  print("Creating FFI results...")
  
  # Make a copy of grid_sf for FFI results
  grid_with_ffi <- grid_sf %>%
    left_join(forest_metrics_wide %>% select(plot_id, ffi, fragmentation_class), 
              by = "plot_id")
  
  # Add percentage_inside column for compatibility
  grid_with_ffi$percentage_inside <- 100  # All cells are fully inside the tile
  
  # SAVE FFI RESULTS (separate from clean fishnet)
  ffi_results_file <- file.path(results_dir, paste0(tile_id, "_ffi_results.gpkg"))
  st_write(grid_with_ffi, ffi_results_file, delete_dsn = TRUE)
  print(paste("Saved FFI results:", ffi_results_file))
  
  # Also save with metrics naming for compatibility with previous scripts
  metrics_file <- file.path(results_dir, paste0(tile_id, "_ffi_metrics.gpkg"))
  st_write(grid_with_ffi, metrics_file, delete_dsn = TRUE)
  print(paste("Saved FFI metrics:", metrics_file))
  
  # 7. Create raster with FFI values only in forest areas
  forest_cells_with_ffi <- grid_with_ffi %>% 
    filter(!is.na(ffi))
  
  if(nrow(forest_cells_with_ffi) > 0) {
    # Convert to vector
    ffi_vect <- terra::vect(forest_cells_with_ffi)
    
    # Create template raster matching binary raster resolution
    template <- terra::rast(ext = ext_moll, resolution = res(binary_r), crs = crs(binary_r))
    
    # Rasterize FFI values
    ffi_rast <- terra::rasterize(ffi_vect, template, field = "ffi")
    
    # Save raster
    raster_file <- file.path(raster_dir, paste0(tile_id, "_ffi.tif"))
    terra::writeRaster(ffi_rast, raster_file, overwrite = TRUE)
    print(paste("Saved FFI raster:", raster_file))
    
    # Create diagnostic visualization
    create_diagnostic_plot(binary_r, ffi_rast, grid_with_ffi, tile_id)
    
  } else {
    print("No FFI values to rasterize")
  }
  
  # Return summary statistics
  summary_stats <- list(
    tile_id = tile_id,
    total_cells = nrow(grid_with_ffi),
    forest_cells = sum(grid_with_ffi$has_forest),
    cells_with_ffi = sum(!is.na(grid_with_ffi$ffi)),
    mean_ffi = mean(grid_with_ffi$ffi, na.rm = TRUE),
    fragmentation_summary = table(grid_with_ffi$fragmentation_class),
    clean_fishnet_path = clean_fishnet_file,
    ffi_results_path = ffi_results_file
  )
  
  return(summary_stats)
}

# Function to create diagnostic visualization
create_diagnostic_plot <- function(binary_r, ffi_rast, grid_with_ffi, tile_id) {
  print("Creating diagnostic plots...")
  
  # Convert rasters to data frames for plotting
  binary_df <- as.data.frame(binary_r, xy = TRUE)
  names(binary_df) <- c("x", "y", "forest")
  
  ffi_df <- as.data.frame(ffi_rast, xy = TRUE)
  names(ffi_df) <- c("x", "y", "ffi")
  
  # Plot binary forest
  p1 <- ggplot() +
    geom_raster(data = binary_df, aes(x = x, y = y, fill = factor(forest))) +
    scale_fill_manual(values = c("0" = "brown", "1" = "forestgreen"), 
                      labels = c("Non-forest", "Forest"),
                      name = "Land Cover") +
    labs(title = paste("Binary Forest -", tile_id)) +
    theme_minimal() +
    coord_equal()
  
  # Plot FFI
  p2 <- ggplot() +
    geom_raster(data = ffi_df %>% filter(!is.na(ffi)), 
                aes(x = x, y = y, fill = ffi)) +
    scale_fill_gradientn(
      colors = c("darkblue", "blue", "green", "yellow", "orange", "red"),
      limits = c(0, 1),
      name = "FFI"
    ) +
    labs(title = paste("Forest Fragmentation Index -", tile_id)) +
    theme_minimal() +
    coord_equal()
  
  # Plot fishnet showing which cells have FFI values
  p3 <- ggplot() +
    geom_sf(data = grid_with_ffi, aes(fill = !is.na(ffi)), color = "gray30", size = 0.1) +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "lightcoral"),
                      labels = c("No FFI", "Has FFI"),
                      name = "FFI Status") +
    labs(title = paste("Fishnet FFI Coverage -", tile_id)) +
    theme_minimal()
  
  # Save plots
  ggsave(paste0("diagnostic_", tile_id, "_binary.png"), p1, width = 8, height = 6)
  ggsave(paste0("diagnostic_", tile_id, "_ffi.png"), p2, width = 8, height = 6)
  ggsave(paste0("diagnostic_", tile_id, "_fishnet.png"), p3, width = 8, height = 6)
  
  print("Diagnostic plots saved")
}

# Run for test tile
start_time <- Sys.time()
test_tile_id <- "moll_binary_00N_000E"

result <- process_tile_forest_only(test_tile_id)

end_time <- Sys.time()
time_taken <- end_time - start_time

if(!is.null(result)) {
  print("Processing completed successfully!")
  print(paste("Time taken:", format(time_taken)))
  print("Summary statistics:")
  print(result)
  print("\nCheck the diagnostic plots to verify:")
  print("1. Complete fishnet structure is maintained")
  print("2. FFI values only appear in forest cells")
  print("3. Proper alignment between binary forest and FFI")
  print("\nFor natural regeneration scenarios, you can reuse:")
  print(paste("Clean fishnet:", result$clean_fishnet_path))
} else {
  print("Processing failed. Check the error messages above.")
}