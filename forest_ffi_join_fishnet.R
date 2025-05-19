# Script to reuse existing clean fishnets and calculate correct FFI
# join forest ffi with clean fishnet
library(tidyverse)
library(sf)
library(terra)

start_time <- Sys.time()
start_time
# Function to calculate FFI for a specific tile ID
calculate_ffi_raster <- function(tile_id,
                                 metrics_folder = "Data/metrics_before_pnr_01",
                                 fishnets_dir = "Data/fishnets_clean",
                                 results_dir = "Data/ffi_results_forest",
                                 raster_dir = "Data/ffi_rasters_forest") {
  
  print(paste("Processing tile:", tile_id))
  
  # Create output directories
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(raster_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 1. Load clean fishnet
  fishnet_file <- list.files(fishnets_dir, 
                             pattern = paste0(tile_id, "_fishnet\\.gpkg$"), 
                             full.names = TRUE)
  
  if(length(fishnet_file) == 0) {
    print(paste("No fishnet found for tile:", tile_id))
    return(NULL)
  }
  
  grid_sf <- st_read(fishnet_file, quiet = TRUE)
  
  # 2. Load metrics from CSV
  metrics_file <- list.files(metrics_folder, 
                             pattern = paste0(tile_id, "_metrics.csv$"), 
                             full.names = TRUE)
  
  if(length(metrics_file) == 0) {
    print(paste("No metrics file found for tile:", tile_id))
    return(NULL)
  }
  
  # Read metrics with your actual column names
  metrics <- read.csv(metrics_file)
  
  # 3. Filter for forest class (1) only and metrics needed for FFI
  forest_metrics <- metrics %>%
    filter(class == 1) %>%  # Forest class only
    filter(metric %in% c("area_mn", "ed", "pd"))
  
  print(paste("Forest metrics records:", nrow(forest_metrics)))
  
  if(nrow(forest_metrics) == 0) {
    print("No forest metrics found for this tile")
    return(NULL)
  }
  
  # 4. Calculate boundaries for capping outliers (more robust approach)
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
  
  # 5. Reshape to wide format for FFI calculation
  metrics_wide <- forest_metrics_normalized %>%
    select(plot_id, metric, value_normalized) %>%
    pivot_wider(names_from = metric, values_from = value_normalized)
  
  # 6. Calculate FFI
  ffi_data <- metrics_wide %>%
    filter(!is.na(area_mn) & !is.na(ed) & !is.na(pd)) %>%
    mutate(
      area_mn_inv = 1 - area_mn,  # Invert area_mn since larger patches = less fragmentation
      ffi = (ed + pd + area_mn_inv) / 3,
      fragmentation_class = case_when(
        ffi <= 0.2 ~ "low",
        ffi <= 0.8 ~ "medium",
        TRUE ~ "high"
      )
    )
  
  print(paste("Calculated FFI for", nrow(ffi_data), "forest cells"))
  
  # 7. Join FFI to fishnet
  grid_with_ffi <- grid_sf %>%
    left_join(ffi_data %>% select(plot_id, ffi, fragmentation_class), 
              by = "plot_id")
  
  # 8. Save results as geopackage
  # ffi_results_file <- file.path(results_dir, paste0(tile_id, "_ffi_results.gpkg"))
  # st_write(grid_with_ffi, ffi_results_file, delete_dsn = TRUE)
  # print(paste("Saved FFI results:", ffi_results_file))
  
  # 9. Create raster with FFI values (only for cells with FFI)
  # Convert to terra SpatVector
  ffi_vect <- terra::vect(grid_with_ffi %>% filter(!is.na(ffi)))
  
  # Get resolution from the fishnet cell size
  # Assuming square cells of equal size
  if(nrow(grid_sf) > 0) {
    cell_size <- sqrt(as.numeric(st_area(grid_sf[1,])))
    
    # Create template raster at appropriate resolution
    bbox <- st_bbox(grid_sf)
    template <- terra::rast(
      xmin = bbox["xmin"], xmax = bbox["xmax"],
      ymin = bbox["ymin"], ymax = bbox["ymax"],
      resolution = c(cell_size, cell_size),
      crs = st_crs(grid_sf)$wkt
    )
    
    # Rasterize the FFI values
    print("Creating FFI raster...")
    ffi_rast <- terra::rasterize(ffi_vect, template, field = "ffi")
    
    # Save raster
    raster_file <- file.path(raster_dir, paste0(tile_id, "_ffi.tif"))
    terra::writeRaster(ffi_rast, raster_file, overwrite = TRUE)
    print(paste("Saved FFI raster:", raster_file))
    
    # Generate a simple visualization
  #   png(file.path(results_dir, paste0(tile_id, "_ffi_map.png")), 
  #       width = 800, height = 600)
  #   plot(ffi_rast, main = paste("Forest Fragmentation Index -", tile_id),
  #        col = hcl.colors(100, "RdYlGn", rev = TRUE))
  #   dev.off()
   }
  
  # Return summary statistics
  return(list(
    tile_id = tile_id,
    total_cells = nrow(grid_with_ffi),
    forest_cells_with_ffi = sum(!is.na(grid_with_ffi$ffi)),
    mean_ffi = mean(grid_with_ffi$ffi, na.rm = TRUE),
    fragmentation_summary = table(grid_with_ffi$fragmentation_class, useNA = "ifany")
  ))
}

# Example usage to process a single tile
tile_id <- "moll_binary_00N_000E"  # Replace with your actual tile ID
result <- calculate_ffi_raster(tile_id)

end_time <- Sys.time()
time_taken <- end_time - start_time
cat("Test process completed in:", as.character(time_taken), "\n")

# print(result)

# Function to process all tiles
# process_all_tiles <- function(metrics_folder = "Data/metrics_before_pnr",
#                               fishnets_dir = "Data/fishnets_clean") {
#   # Get all tile IDs from fishnet files
#   fishnet_files <- list.files(fishnets_dir, pattern = "_fishnet\\.gpkg$")
#   tile_ids <- gsub("_fishnet\\.gpkg$", "", fishnet_files)
#   
#   # Process each tile and collect results
#   results <- list()
#   for(i in 1:length(tile_ids)) {
#     print(paste("\nProcessing tile", i, "of", length(tile_ids)))
#     results[[i]] <- tryCatch({
#       calculate_ffi_raster(tile_ids[i])
#     }, error = function(e) {
#       print(paste("Error processing tile", tile_ids[i], ":", e$message))
#       return(NULL)
#     })
#   }
#   
#   # Combine results into a summary dataframe
#   valid_results <- results[!sapply(results, is.null)]
#   summary_df <- data.frame(
#     tile_id = sapply(valid_results, function(x) x$tile_id),
#     total_cells = sapply(valid_results, function(x) x$total_cells),
#     forest_cells_with_ffi = sapply(valid_results, function(x) x$forest_cells_with_ffi),
#     mean_ffi = sapply(valid_results, function(x) x$mean_ffi)
#   )
#   
#   # Save summary
#   write.csv(summary_df, "ffi_processing_summary.csv", row.names = FALSE)
#   print("Processing complete! Summary saved to ffi_processing_summary.csv")
#   
#   return(summary_df)
# }

# Uncomment to process all tiles
# all_results <- process_all_tiles()