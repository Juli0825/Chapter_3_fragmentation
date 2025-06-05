library(terra)
library(sf)
library(landscapemetrics)
library(dplyr)

# ===== CONFIGURATION =====
# Update these paths for your server
base_path <- "R:/Chapter_3_fragmentation/Data"
global_boundaries_file <- file.path(base_path, "global_boundaries_from_5km.csv")
fishnets_folder <- file.path(base_path, "fishnets_clean")
future_forest_folder <- file.path(base_path, "ICCB/regional_10m_processed")  # Adjust as needed


s <- Sys.time()
s

# Define regions and their corresponding fishnet patterns
regions <- list(
  borneo = list(
    future_forest = "borneo_future_forest_10m.tif",
    fishnet_patterns = c("10N_100E", "10N_110E", "00N_100E", "00N_110E")
  # ),
  # brazil = list(
  #   future_forest = "brazil_future_forest_10m.tif", 
  #   fishnet_patterns = c("00N_050W", "00N_040W", "10S_050W", "10S_040W")
  )
)

# Create output directory
dir.create("ffi_results", showWarnings = FALSE)

# ===== STEP 1: Load Global Boundaries =====
load_global_boundaries <- function() {
  cat("Loading global FFI normalization boundaries...\n")
  
  if(!file.exists(global_boundaries_file)) {
    stop("Global boundaries file not found: ", global_boundaries_file)
  }
  
  boundaries <- read.csv(global_boundaries_file)
  cat("Global boundaries loaded:\n")
  print(boundaries)
  return(boundaries)
}

# ===== STEP 2: Calculate FFI with Global Normalization =====
calculate_ffi_with_global_boundaries <- function(metrics_df, global_boundaries) {
  cat("Applying global normalization to calculate FFI...\n")
  
  # Extract boundaries (format: metric, boundary_type, value)
  ed_min <- global_boundaries[global_boundaries$metric == "ed" & global_boundaries$boundary_type == "lower", "value"]
  ed_max <- global_boundaries[global_boundaries$metric == "ed" & global_boundaries$boundary_type == "upper", "value"]
  pd_min <- global_boundaries[global_boundaries$metric == "pd" & global_boundaries$boundary_type == "lower", "value"] 
  pd_max <- global_boundaries[global_boundaries$metric == "pd" & global_boundaries$boundary_type == "upper", "value"]
  mpa_min <- global_boundaries[global_boundaries$metric == "area_mn" & global_boundaries$boundary_type == "lower", "value"]
  mpa_max <- global_boundaries[global_boundaries$metric == "area_mn" & global_boundaries$boundary_type == "upper", "value"]
  
  cat("Boundary values - ED:", ed_min, "to", ed_max, "\n")
  cat("Boundary values - PD:", pd_min, "to", pd_max, "\n") 
  cat("Boundary values - MPA:", mpa_min, "to", mpa_max, "\n")
  
  # Apply normalization (0-1 scale)
  metrics_df$ED_norm <- pmax(0, pmin(1, (metrics_df$ED - ed_min) / (ed_max - ed_min)))
  metrics_df$PD_norm <- pmax(0, pmin(1, (metrics_df$PD - pd_min) / (pd_max - pd_min)))
  metrics_df$MPA_norm <- pmax(0, pmin(1, (metrics_df$MPA - mpa_min) / (mpa_max - mpa_min)))
  
  # Calculate FFI using Ma et al formula: FFI = (ED_norm + PD_norm + (1 - MPA_norm)) / 3
  metrics_df$FFI <- (metrics_df$ED_norm + metrics_df$PD_norm + (1 - metrics_df$MPA_norm)) / 3
  
  cat("FFI calculated with global normalization\n")
  cat("FFI range:", round(range(metrics_df$FFI, na.rm = TRUE), 3), "\n")
  
  return(metrics_df)
}

# ===== STEP 3: Process Single Fishnet =====
process_single_fishnet <- function(fishnet_file, future_forest_raster, global_boundaries) {
  fishnet_name <- tools::file_path_sans_ext(basename(fishnet_file))
  cat("\n--- Processing:", fishnet_name, "---\n")
  
  # Load fishnet
  tryCatch({
    fishnet <- st_read(fishnet_file, quiet = TRUE)
    cat("Loaded fishnet with", nrow(fishnet), "polygons\n")
  }, error = function(e) {
    cat("ERROR loading fishnet:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(fishnet) || nrow(fishnet) == 0) {
    cat("Skipping empty fishnet\n")
    return(NULL)
  }
  
  # Get polygon centroids for coordinates
  centroids <- st_centroid(fishnet)
  coords <- st_coordinates(centroids)
  
  # Calculate landscape metrics
  cat("Calculating landscape metrics...\n")
  tryCatch({
    metrics <- sample_lsm(
      landscape = future_forest_raster,
      y = fishnet,
      what = c("lsm_c_ed", "lsm_c_pd", "lsm_c_area_mn"),
      verbose = FALSE
    )
    
    cat("Metrics calculated for", nrow(metrics), "records\n")
  }, error = function(e) {
    cat("ERROR calculating metrics:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(metrics) || nrow(metrics) == 0) {
    cat("No metrics calculated\n")
    return(NULL)
  }
  
  # Reshape metrics to wide format
  metrics_wide <- metrics %>%
    select(plot_id, class, metric, value) %>%
    filter(class == 1) %>%  # Forest class
    select(-class) %>%
    tidyr::pivot_wider(names_from = metric, values_from = value, names_prefix = "")
  
  # Rename columns to standard names
  names(metrics_wide)[names(metrics_wide) == "ed"] <- "ED"
  names(metrics_wide)[names(metrics_wide) == "pd"] <- "PD" 
  names(metrics_wide)[names(metrics_wide) == "area_mn"] <- "MPA"
  
  # Calculate forest coverage percentage
  cat("Calculating forest coverage...\n")
  tryCatch({
    coverage <- exactextractr::exact_extract(
      future_forest_raster, 
      fishnet, 
      fun = c("mean", "count"),
      summarize_df = TRUE
    )
    
    coverage$plot_id <- 1:nrow(coverage)
    coverage$forest_coverage_pct <- coverage$mean * 100  # Convert to percentage
    
    cat("Forest coverage calculated\n")
  }, error = function(e) {
    cat("ERROR calculating coverage:", e$message, "\n")
    # Fallback method if exactextractr not available
    coverage <- data.frame(
      plot_id = 1:nrow(fishnet),
      forest_coverage_pct = NA
    )
  })
  
  # Combine all data
  result <- metrics_wide %>%
    left_join(coverage[c("plot_id", "forest_coverage_pct")], by = "plot_id") %>%
    mutate(
      x = coords[, 1],
      y = coords[, 2],
      fishnet_tile = fishnet_name
    )
  
  # Apply global FFI normalization
  result <- calculate_ffi_with_global_boundaries(result, global_boundaries)
  
  # Select and order final columns
  result <- result %>%
    select(plot_id, x, y, ED, PD, MPA, ED_norm, PD_norm, MPA_norm, FFI, 
           forest_coverage_pct, fishnet_tile) %>%
    arrange(plot_id)
  
  cat("Final result:", nrow(result), "records\n")
  cat("Sample FFI values:", paste(round(head(result$FFI, 5), 3), collapse = ", "), "\n")
  
  return(result)
}

# ===== STEP 4: Process Region =====
process_region <- function(region_name, region_config, global_boundaries) {
  cat("\n", strrep("=", 50), "\n")
  cat("PROCESSING REGION:", toupper(region_name), "\n")
  cat(strrep("=", 50), "\n")
  
  # Load future forest raster
  future_forest_file <- file.path(future_forest_folder, region_config$future_forest)
  
  if(!file.exists(future_forest_file)) {
    cat("ERROR: Future forest file not found:", future_forest_file, "\n")
    return(NULL)
  }
  
  cat("Loading future forest:", future_forest_file, "\n")
  future_forest <- rast(future_forest_file)
  cat("Future forest loaded - dimensions:", dim(future_forest)[1:2], "\n")
  
  # Find fishnet files
  fishnet_files <- c()
  for(pattern in region_config$fishnet_patterns) {
    fishnet_file <- file.path(fishnets_folder, paste0("moll_binary_", pattern, "_fishnet.gpkg"))
    if(file.exists(fishnet_file)) {
      fishnet_files <- c(fishnet_files, fishnet_file)
      cat("Found fishnet:", basename(fishnet_file), "\n")
    } else {
      cat("WARNING: Fishnet not found:", fishnet_file, "\n")
    }
  }
  
  if(length(fishnet_files) == 0) {
    cat("ERROR: No fishnet files found for", region_name, "\n")
    return(NULL)
  }
  
  # Process each fishnet
  all_results <- list()
  for(i in seq_along(fishnet_files)) {
    result <- process_single_fishnet(fishnet_files[i], future_forest, global_boundaries)
    if(!is.null(result)) {
      all_results[[i]] <- result
    }
  }
  
  # Combine all results
  if(length(all_results) > 0) {
    combined_result <- do.call(rbind, all_results)
    cat("\nCombined results for", region_name, ":", nrow(combined_result), "total records\n")
    
    # Save regional results
    output_file <- file.path("ffi_results", paste0(region_name, "_metrics_ffi.csv"))
    write.csv(combined_result, output_file, row.names = FALSE)
    cat("✓ Saved:", output_file, "\n")
    
    # Print summary statistics
    cat("\nSUMMARY for", toupper(region_name), ":\n")
    cat("Total polygons:", nrow(combined_result), "\n")
    cat("FFI range:", paste(round(range(combined_result$FFI, na.rm = TRUE), 3), collapse = " - "), "\n")
    cat("Mean FFI:", round(mean(combined_result$FFI, na.rm = TRUE), 3), "\n")
    cat("Forest coverage range:", paste(round(range(combined_result$forest_coverage_pct, na.rm = TRUE), 1), collapse = " - "), "%\n")
    
    return(combined_result)
  } else {
    cat("ERROR: No results generated for", region_name, "\n")
    return(NULL)
  }
}

# ===== MAIN EXECUTION =====
main <- function() {
  cat("Starting FFI calculation workflow...\n")
  cat("Timestamp:", Sys.time(), "\n\n")
  
  # Load global boundaries
  global_boundaries <- load_global_boundaries()
  
  # Process each region
  results <- list()
  for(region_name in names(regions)) {
    results[[region_name]] <- process_region(region_name, regions[[region_name]], global_boundaries)
  }
  
  # Final summary
  cat("\n", strrep("=", 60), "\n")
  cat("WORKFLOW COMPLETE\n")
  cat(strrep("=", 60), "\n")
  
  for(region_name in names(results)) {
    if(!is.null(results[[region_name]])) {
      cat("✓", toupper(region_name), ":", nrow(results[[region_name]]), "records\n")
    } else {
      cat("✗", toupper(region_name), ": FAILED\n")
    }
  }
  
  cat("\nOutput files saved in: ffi_results/\n")
  cat("Files created:\n")
  for(region_name in names(regions)) {
    file_path <- file.path("ffi_results", paste0(region_name, "_metrics_ffi.csv"))
    if(file.exists(file_path)) {
      cat("  ✓", file_path, "\n")
    }
  }
  
  cat("\nColumns in output CSV:\n")
  cat("  - plot_id: Fishnet polygon ID\n")
  cat("  - x, y: Polygon centroid coordinates (Mollweide)\n") 
  cat("  - ED, PD, MPA: Raw landscape metrics\n")
  cat("  - ED_norm, PD_norm, MPA_norm: Globally normalized metrics\n")
  cat("  - FFI: Forest Fragmentation Index (Ma et al method)\n")
  cat("  - forest_coverage_pct: % forest coverage in polygon\n")
  cat("  - fishnet_tile: Source fishnet tile name\n")
}

# Run the workflow
main()

e <- Sys.tim()
total <- e-s
total 