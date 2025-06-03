# This script is to add forest coverage into global_ffi_resul.csv
# To test out different threshold
# And add coordination into that csv as well?
#  81 out of 106 tiles and added forest coverage

library(dplyr)

s<- Sys.time()
s
# Function to add forest coverage data to global FFI results
add_forest_coverage_to_global_ffi <- function(global_ffi_path, coverage_data_dir, output_path) {
  
  # Read the global FFI results
  cat("Reading global FFI results...\n")
  global_ffi <- read.csv(global_ffi_path, stringsAsFactors = FALSE)
  cat("Global FFI shape:", nrow(global_ffi), "rows x", ncol(global_ffi), "columns\n")
  cat("Columns:", paste(names(global_ffi), collapse = ", "), "\n")
  
  # Get unique tile_ids from global FFI
  unique_tiles <- unique(global_ffi$tile_id)
  cat("Found", length(unique_tiles), "unique tiles in global FFI\n")
  
  # Initialize forest coverage column with NA
  global_ffi$forest_coverage_pct <- NA
  
  # Initialize counters and tracking lists
  matched_plots <- 0
  total_coverage_plots <- 0
  processed_tiles <- c()
  missing_tiles <- c()
  error_tiles <- c()
  
  # Process each tile
  for (tile_id in unique_tiles) {
    # Construct the expected filename for this tile
    coverage_filename <- paste0(tile_id, "_pct.csv")
    coverage_filepath <- file.path(coverage_data_dir, coverage_filename)
    
    if (file.exists(coverage_filepath)) {
      cat("Processing", tile_id, "...\n")
      
      # Try to read the forest coverage data for this tile
      tryCatch({
        coverage_data <- read.csv(coverage_filepath, stringsAsFactors = FALSE)
        total_coverage_plots <- total_coverage_plots + nrow(coverage_data)
        
        # Check if required columns exist
        if (!("plot_id" %in% names(coverage_data)) || !("MEAN" %in% names(coverage_data))) {
          cat("Warning: Missing required columns in", coverage_filename, "\n")
          error_tiles <- c(error_tiles, tile_id)
          next
        }
        
        # Get indices for this tile in global FFI
        tile_indices <- which(global_ffi$tile_id == tile_id)
        
        # Create a lookup for forest coverage
        coverage_lookup <- setNames(coverage_data$MEAN, coverage_data$plot_id)
        
        # Add forest coverage for matching plots
        tile_matched <- 0
        for (idx in tile_indices) {
          plot_id <- global_ffi$plot_id[idx]
          if (as.character(plot_id) %in% names(coverage_lookup)) {
            global_ffi$forest_coverage_pct[idx] <- coverage_lookup[[as.character(plot_id)]]
            matched_plots <- matched_plots + 1
            tile_matched <- tile_matched + 1
          }
        }
        
        cat("  - Matched", tile_matched, "plots for", tile_id, "\n")
        processed_tiles <- c(processed_tiles, tile_id)
        
      }, error = function(e) {
        cat("Error reading", coverage_filename, ":", e$message, "\n")
        error_tiles <- c(error_tiles, tile_id)
      })
      
    } else {
      cat("Skipping", tile_id, "- coverage file not found\n")
      missing_tiles <- c(missing_tiles, tile_id)
    }
  }
  
  # Summary statistics
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("SUMMARY:\n")
  cat("Total tiles in global FFI:", length(unique_tiles), "\n")
  cat("Successfully processed tiles:", length(processed_tiles), "\n")
  cat("Missing coverage files:", length(missing_tiles), "\n")
  cat("Tiles with errors:", length(error_tiles), "\n")
  cat("\nTotal plots in global FFI:", nrow(global_ffi), "\n")
  cat("Total plots in coverage files:", total_coverage_plots, "\n")
  cat("Successfully matched plots:", matched_plots, "\n")
  cat("Plots with forest coverage data:", sum(!is.na(global_ffi$forest_coverage_pct)), "\n")
  cat("Plots without forest coverage data:", sum(is.na(global_ffi$forest_coverage_pct)), "\n")
  
  # Show which tiles still need to be processed
  if (length(missing_tiles) > 0) {
    cat("\nMissing coverage files for tiles:\n")
    cat(paste(missing_tiles, collapse = ", "), "\n")
  }
  
  if (length(error_tiles) > 0) {
    cat("\nTiles with errors:\n")
    cat(paste(error_tiles, collapse = ", "), "\n")
  }
  
  if (length(processed_tiles) > 0) {
    cat("\nSuccessfully processed tiles:\n")
    cat(paste(processed_tiles, collapse = ", "), "\n")
  }
  
  # Show statistics about forest coverage
  coverage_data_clean <- global_ffi$forest_coverage_pct[!is.na(global_ffi$forest_coverage_pct)]
  if (length(coverage_data_clean) > 0) {
    cat("\nForest coverage statistics:\n")
    cat("Mean:", round(mean(coverage_data_clean), 3), "\n")
    cat("Median:", round(median(coverage_data_clean), 3), "\n")
    cat("Min:", round(min(coverage_data_clean), 3), "\n")
    cat("Max:", round(max(coverage_data_clean), 3), "\n")
  }
  
  # Save the updated dataframe
  write.csv(global_ffi, output_path, row.names = FALSE)
  cat("\nUpdated data saved to:", output_path, "\n")
  
  # Save list of missing tiles for future reference
  if (length(missing_tiles) > 0) {
    missing_tiles_file <- gsub("\\.csv$", "_missing_tiles.txt", output_path)
    writeLines(missing_tiles, missing_tiles_file)
    cat("List of missing tiles saved to:", missing_tiles_file, "\n")
  }
  
  return(global_ffi)
}

# Alternative function using dplyr for more efficient joining
add_forest_coverage_to_global_ffi_dplyr <- function(global_ffi_path, coverage_data_dir, output_path) {
  
  # Load required libraries
  library(dplyr)
  
  # Read the global FFI results
  cat("Reading global FFI results...\n")
  global_ffi <- read.csv(global_ffi_path, stringsAsFactors = FALSE)
  cat("Global FFI shape:", nrow(global_ffi), "rows x", ncol(global_ffi), "columns\n")
  
  # Get unique tile_ids
  unique_tiles <- unique(global_ffi$tile_id)
  cat("Found", length(unique_tiles), "unique tiles in global FFI\n")
  
  # Initialize list to store all coverage data and tracking
  all_coverage_data <- list()
  processed_tiles <- c()
  missing_tiles <- c()
  error_tiles <- c()
  
  # Read all coverage files
  for (tile_id in unique_tiles) {
    coverage_filename <- paste0(tile_id, "_pct.csv")
    coverage_filepath <- file.path(coverage_data_dir, coverage_filename)
    
    if (file.exists(coverage_filepath)) {
      cat("Reading", tile_id, "...\n")
      
      tryCatch({
        coverage_data <- read.csv(coverage_filepath, stringsAsFactors = FALSE)
        
        if ("plot_id" %in% names(coverage_data) && "MEAN" %in% names(coverage_data)) {
          # Add tile_id and select only needed columns
          coverage_subset <- coverage_data %>%
            select(plot_id, forest_coverage_pct = MEAN) %>%
            mutate(tile_id = tile_id)
          
          all_coverage_data[[tile_id]] <- coverage_subset
          processed_tiles <- c(processed_tiles, tile_id)
        } else {
          cat("Warning: Missing required columns in", coverage_filename, "\n")
          error_tiles <- c(error_tiles, tile_id)
        }
        
      }, error = function(e) {
        cat("Error reading", coverage_filename, ":", e$message, "\n")
        error_tiles <- c(error_tiles, tile_id)
      })
    } else {
      cat("Skipping", tile_id, "- coverage file not found\n")
      missing_tiles <- c(missing_tiles, tile_id)
    }
  }
  
  # Combine all coverage data
  if (length(all_coverage_data) > 0) {
    combined_coverage <- bind_rows(all_coverage_data)
    
    # Join with global FFI data
    updated_global_ffi <- global_ffi %>%
      left_join(combined_coverage, by = c("plot_id", "tile_id"))
    
    # Summary statistics
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
    cat("SUMMARY:\n")
    cat("Total tiles in global FFI:", length(unique_tiles), "\n")
    cat("Successfully processed tiles:", length(processed_tiles), "\n")
    cat("Missing coverage files:", length(missing_tiles), "\n")
    cat("Tiles with errors:", length(error_tiles), "\n")
    cat("\nTotal plots in global FFI:", nrow(global_ffi), "\n")
    cat("Total plots in coverage files:", nrow(combined_coverage), "\n")
    cat("Plots with forest coverage data:", sum(!is.na(updated_global_ffi$forest_coverage_pct)), "\n")
    cat("Plots without forest coverage data:", sum(is.na(updated_global_ffi$forest_coverage_pct)), "\n")
    
    # Show which tiles still need to be processed
    if (length(missing_tiles) > 0) {
      cat("\nMissing coverage files for tiles:\n")
      cat(paste(missing_tiles, collapse = ", "), "\n")
    }
    
    if (length(error_tiles) > 0) {
      cat("\nTiles with errors:\n")
      cat(paste(error_tiles, collapse = ", "), "\n")
    }
    
    if (length(processed_tiles) > 0) {
      cat("\nSuccessfully processed tiles:\n")
      cat(paste(processed_tiles, collapse = ", "), "\n")
    }
    
    # Forest coverage statistics
    coverage_stats <- updated_global_ffi$forest_coverage_pct[!is.na(updated_global_ffi$forest_coverage_pct)]
    if (length(coverage_stats) > 0) {
      cat("\nForest coverage statistics:\n")
      cat("Mean:", round(mean(coverage_stats), 3), "\n")
      cat("Median:", round(median(coverage_stats), 3), "\n")
      cat("Min:", round(min(coverage_stats), 3), "\n")
      cat("Max:", round(max(coverage_stats), 3), "\n")
    }
    
    # Save results
    write.csv(updated_global_ffi, output_path, row.names = FALSE)
    cat("\nUpdated data saved to:", output_path, "\n")
    
    # Save list of missing tiles for future reference
    if (length(missing_tiles) > 0) {
      missing_tiles_file <- gsub("\\.csv$", "_missing_tiles.txt", output_path)
      writeLines(missing_tiles, missing_tiles_file)
      cat("List of missing tiles saved to:", missing_tiles_file, "\n")
    }
    
    return(updated_global_ffi)
    
  } else {
    cat("No coverage data found!\n")
    return(global_ffi)
  }
}

# Example usage:
# Set your file paths here
global_ffi_path <- "Data/not_in_use/global_ffi_results.csv"  # Adjust path as needed
coverage_data_dir <- "Data/PNRs/coverage_csv2"   # Directory containing the coverage CSV files
output_path <- "global_ffi_with_coverage.csv"  # Output file

# Run the function
updated_data <- add_forest_coverage_to_global_ffi_dplyr(
    global_ffi_path,
    coverage_data_dir,
    output_path
  )

e<-Sys.time()
t<- e-s
t