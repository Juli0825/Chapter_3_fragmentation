library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(viridis)

start_time <- Sys.time()
cat("Starting analysis at:", as.character(start_time), "\n")

# Detach potentially conflicting packages
if("plyr" %in% (.packages())) detach("package:plyr", unload = TRUE)

# Path to metrics files
metrics_folder <- "Data/metrics_before_pnr"

# Function to extract coordinates from tile ID
extract_coords_from_tile <- function(tile_id) {
  # Example for "moll_binary_00N_000E" format
  lat_str <- stringr::str_extract(tile_id, "\\d+[NS]")
  lon_str <- stringr::str_extract(tile_id, "\\d+[EW]")
  
  if(is.na(lat_str) || is.na(lon_str)) {
    return(c(NA, NA))  # Return NA for invalid patterns
  }
  
  lat_deg <- as.numeric(stringr::str_extract(lat_str, "\\d+"))
  lat_dir <- stringr::str_extract(lat_str, "[NS]")
  if(lat_dir == "S") lat_deg <- -lat_deg
  
  lon_deg <- as.numeric(stringr::str_extract(lon_str, "\\d+"))
  lon_dir <- stringr::str_extract(lon_str, "[EW]")
  if(lon_dir == "W") lon_deg <- -lon_deg
  
  return(c(lon_deg, lat_deg))
}

# 1. Read all metrics files
metrics_files <- list.files(path = metrics_folder, 
                            pattern = "_metrics.csv$", 
                            full.names = TRUE)

print(paste("Found", length(metrics_files), "metrics files"))

# First let's get a sample file to understand the structure
sample_file <- read.csv(metrics_files[1])
print("Sample file structure:")
print(str(sample_file))

# Read and combine all metrics files
all_metrics <- data.frame()

for (file in metrics_files) {
  # Extract tile name from filename
  tile_name <- tools::file_path_sans_ext(basename(file))
  tile_name <- gsub("_metrics", "", tile_name)  # Remove metrics suffix if present
  
  # Read file
  df <- read.csv(file)
  
  # Check if tile_id column exists, and add it if not
  if(!"tile_id" %in% colnames(df)) {
    df$tile_id <- tile_name
  }
  
  # Add to combined dataframe
  all_metrics <- dplyr::bind_rows(all_metrics, df)
}

print(paste("Total number of records:", nrow(all_metrics)))
print(paste("Number of unique plot IDs:", length(unique(all_metrics$plot_id))))
print(paste("Number of unique tile IDs:", length(unique(all_metrics$tile_id))))

# 2. Filter for forest (class 1) and relevant metrics for FFI calculation
print("Filtering for forest class and relevant metrics...")
forest_metrics <- all_metrics %>%
  dplyr::filter(class == 1) %>%
  dplyr::filter(metric %in% c("area_mn", "ed", "pd"))

print(paste("Number of forest metrics records:", nrow(forest_metrics)))

# 3. Calculate outlier boundaries for each metric as per the paper
calculate_boundaries <- function(data, metric_name) {
  values <- data %>% dplyr::filter(metric == metric_name) %>% dplyr::pull(value)
  q1 <- quantile(values, 0.25, na.rm = TRUE)
  q3 <- quantile(values, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  upper <- q3 + 1.5 * iqr
  lower <- q1 - 1.5 * iqr
  return(list(lower = lower, upper = upper, min = min(values, na.rm = TRUE), max = max(values, na.rm = TRUE)))
}

# Get boundaries for each metric
print("Calculating boundaries for metrics...")
area_boundaries <- calculate_boundaries(forest_metrics, "area_mn")
ed_boundaries <- calculate_boundaries(forest_metrics, "ed")
pd_boundaries <- calculate_boundaries(forest_metrics, "pd")

# Print boundary values for reference
print("Boundaries for area_mn:")
print(area_boundaries)
print("Boundaries for ed:")
print(ed_boundaries)
print("Boundaries for pd:")
print(pd_boundaries)

# 4. Cap values at boundaries and normalize
print("Capping and normalizing metrics...")
forest_metrics_capped <- forest_metrics %>%
  dplyr::group_by(metric) %>%
  dplyr::mutate(
    # Cap values at boundaries
    value_capped = dplyr::case_when(
      metric == "area_mn" & value > area_boundaries$upper ~ area_boundaries$upper,
      metric == "area_mn" & value < area_boundaries$lower ~ area_boundaries$lower,
      metric == "ed" & value > ed_boundaries$upper ~ ed_boundaries$upper,
      metric == "ed" & value < ed_boundaries$lower ~ ed_boundaries$lower,
      metric == "pd" & value > pd_boundaries$upper ~ pd_boundaries$upper,
      metric == "pd" & value < pd_boundaries$lower ~ pd_boundaries$lower,
      TRUE ~ value
    )
  ) %>%
  dplyr::ungroup()

# Normalize each metric to 0-1 range
forest_metrics_normalized <- forest_metrics_capped %>%
  dplyr::group_by(metric) %>%
  dplyr::mutate(
    value_normalized = dplyr::case_when(
      metric == "area_mn" ~ (value_capped - min(value_capped, na.rm = TRUE)) / 
        (max(value_capped, na.rm = TRUE) - min(value_capped, na.rm = TRUE)),
      metric == "ed" ~ (value_capped - min(value_capped, na.rm = TRUE)) / 
        (max(value_capped, na.rm = TRUE) - min(value_capped, na.rm = TRUE)),
      metric == "pd" ~ (value_capped - min(value_capped, na.rm = TRUE)) / 
        (max(value_capped, na.rm = TRUE) - min(value_capped, na.rm = TRUE))
    )
  ) %>%
  dplyr::ungroup()

# 5. Reshape to wide format for FFI calculation
print("Preparing data for FFI calculation...")
forest_metrics_wide <- forest_metrics_normalized %>%
  dplyr::select(plot_id, tile_id, metric, value_normalized, percentage_inside) %>%
  tidyr::pivot_wider(names_from = metric, values_from = value_normalized)

# Check for completeness - each plot should have all three metrics
complete_plots <- forest_metrics_wide %>%
  dplyr::filter(!is.na(area_mn) & !is.na(ed) & !is.na(pd))

incomplete_plots <- forest_metrics_wide %>%
  dplyr::filter(is.na(area_mn) | is.na(ed) | is.na(pd))

print(paste("Plots with complete metrics:", nrow(complete_plots)))
print(paste("Plots with incomplete metrics:", nrow(incomplete_plots)))

# 6. Calculate FFI as per the paper formula
print("Calculating FFI...")
forest_metrics_wide <- forest_metrics_wide %>%
  # Only calculate FFI for plots with all three metrics
  dplyr::filter(!is.na(area_mn) & !is.na(ed) & !is.na(pd)) %>%
  dplyr::mutate(
    # Invert area_mn (higher MPA = less fragmentation)
    area_mn_inv = 1 - area_mn,
    # Calculate FFI - average of the three metrics
    ffi = (ed + pd + area_mn_inv)/3,
    # Classify fragmentation
    fragmentation_class = dplyr::case_when(
      ffi <= 0.2 ~ "low",
      ffi <= 0.8 ~ "medium",
      TRUE ~ "high"
    )
  )

# 7. Calculate summary statistics
ffi_summary <- forest_metrics_wide %>%
  dplyr::group_by(fragmentation_class) %>%
  dplyr::summarise(
    count = n(),
    percentage = n() / nrow(forest_metrics_wide) * 100
  )

print("FFI Summary Statistics:")
print(ffi_summary)

# 8. Add coordinates information
print("Adding coordinate information...")
ffi_with_coords <- forest_metrics_wide %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    coords = list(extract_coords_from_tile(tile_id)),
    lon = coords[1],
    lat = coords[2]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-coords)

# Check coordinates extraction
invalid_coords <- sum(is.na(ffi_with_coords$lon) | is.na(ffi_with_coords$lat))
print(paste("Rows with invalid coordinates:", invalid_coords))

# 9. Save the FFI results to CSV
output_file <- "Data/ffi_results2.csv"
write.csv(forest_metrics_wide, output_file, row.names = FALSE)
print(paste("Saved FFI results to", output_file))

coords_output_file <- "Data/ffi_with_coords.csv"
write.csv(ffi_with_coords, coords_output_file, row.names = FALSE)
print(paste("Saved FFI results with coordinates to", coords_output_file))

print("FFI calculation complete!")

end_time <- Sys.time()
cat("Finished at:", as.character(end_time), "\n")
cat("Total time:", format(end_time - start_time), "\n")

###### validation ########

# Check the basic FFI results file
ffi_results_file <- "Data/ffi_results.csv"
ffi_with_coords_file <- "Data/ffi_with_coords.csv"

# Function to check and summarize a CSV file
check_csv <- function(file_path) {
  print(paste("Examining file:", file_path))
  
  # Check if file exists
  if (!file.exists(file_path)) {
    print(paste("ERROR: File not found:", file_path))
    return(NULL)
  }
  
  # Get file size
  file_size <- file.info(file_path)$size / (1024^2)  # Size in MB
  print(paste("File size:", round(file_size, 2), "MB"))
  
  # Read just the first few rows to check structure
  data_sample <- read.csv(file_path, nrows = 10)
  
  # Print structure
  print("Data structure:")
  print(str(data_sample))
  
  # Print head
  print("First few rows:")
  print(head(data_sample))
  
  # If it's the coords file, check coordinate ranges
  if (grepl("coords", file_path) && "lon" %in% colnames(data_sample) && "lat" %in% colnames(data_sample)) {
    # Read the full file for coordinate check
    full_data <- read.csv(file_path)
    
    # Check coordinate ranges
    lon_range <- range(full_data$lon, na.rm = TRUE)
    lat_range <- range(full_data$lat, na.rm = TRUE)
    
    print("Longitude range:")
    print(lon_range)
    
    print("Latitude range:")
    print(lat_range)
    
    # Check for missing coordinates
    missing_coords <- sum(is.na(full_data$lon) | is.na(full_data$lat))
    print(paste("Records with missing coordinates:", missing_coords, 
                "(", round(missing_coords/nrow(full_data)*100, 2), "%)"))
  }
  
  # If FFI results file, check for basic statistics
  if ("ffi" %in% colnames(data_sample)) {
    # Read the full file for FFI analysis
    full_data <- read.csv(file_path)
    
    # Calculate FFI summary
    ffi_range <- range(full_data$ffi, na.rm = TRUE)
    ffi_mean <- mean(full_data$ffi, na.rm = TRUE)
    ffi_median <- median(full_data$ffi, na.rm = TRUE)
    
    print("FFI statistics:")
    print(paste("Range:", round(ffi_range[1], 3), "-", round(ffi_range[2], 3)))
    print(paste("Mean:", round(ffi_mean, 3)))
    print(paste("Median:", round(ffi_median, 3)))
    
    # Check fragmentation class distribution
    frag_classes <- table(full_data$fragmentation_class)
    frag_percent <- round(prop.table(frag_classes) * 100, 1)
    
    print("Fragmentation class distribution:")
    for (class in names(frag_classes)) {
      print(paste(class, ":", frag_classes[class], "plots (", frag_percent[class], "%)"))
    }
    
    # Create a quick histogram of FFI values
    png("ffi_histogram.png", width = 800, height = 600)
    hist(full_data$ffi, 
         breaks = 30, 
         main = "Distribution of Forest Fragmentation Index",
         xlab = "FFI",
         col = "steelblue")
    abline(v = 0.2, col = "darkgreen", lwd = 2, lty = 2)
    abline(v = 0.8, col = "darkred", lwd = 2, lty = 2)
    dev.off()
    print("Created FFI histogram: ffi_histogram.png")
    
    # Return the data for further use
    return(full_data)
  }
  
  return(data_sample)
}

# Check both files
print("===== CHECKING BASIC FFI RESULTS =====")
ffi_data <- check_csv(ffi_results_file)

print("\n===== CHECKING FFI RESULTS WITH COORDINATES =====")
ffi_coords_data <- check_csv(ffi_with_coords_file)

# Create a quick map to verify coordinates
if (!is.null(ffi_coords_data) && all(c("lon", "lat", "ffi") %in% colnames(ffi_coords_data))) {
  # Create a sample map with a subset of points (for speed)
  print("Creating a sample map to verify coordinates...")
  
  # Sample points (100 per fragmentation class for balanced visualization)
  map_sample <- ffi_coords_data %>%
    group_by(fragmentation_class) %>%
    slice_sample(n = 100) %>%
    ungroup()
  
  # Get world map
  world_map <- map_data("world")
  
  # Create a simple map
  sample_map <- ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = "lightgray", color = "white", size = 0.1) +
    geom_point(data = map_sample, aes(x = lon, y = lat, color = ffi), size = 3, alpha = 0.7) +
    scale_color_viridis(option = "plasma") +
    theme_minimal() +
    labs(title = "Sample of FFI Points (100 per class)",
         subtitle = "Verifying coordinate extraction",
         color = "FFI")
  
  # Save the map
  ggsave("sample_coordinate_check.png", sample_map, width = 10, height = 6, dpi = 150)
  print("Created sample map: sample_coordinate_check.png")
}

print("Validation complete!")


















