# Take out 30m pnr cost-benefit cells from 1km cost-benefit pnr layer
# Resample to 10m
# Overlay with current forest
# Generate a current forest + 9M km2 FUTURE FOREST LAYER
# Load the saved RData file
load("Data/df_natRege.RData")

# Check dataframe
print(head(pnr_df_updated))

# Convert all 30m pnr to dataframe first
library(terra)
library(dplyr)

a <- rast("Data/pnr_30m/pnv_pct_30m_tile_0_10_-5_5.tif")
print(a)
plot(a)

b<- rast("Data/pnv_bin_30m/pnv_bin_30m_tile_1.tif")
print(b)
plot(b)

c <- rast("Data/mo_bi_pnr/mo_pnv_bin_30m_tile_1.tif")
print(c)
plot(c)

s_time <-Sys.time()
s_time
# Function to process and save each tile as RData
process_and_save_tile <- function(tile_path, output_dir = "processed_pnr_tiles") {
  if (!dir.exists(output_dir)) dir.create(output_dir)
  
  cat("Processing:", basename(tile_path), "\n")
  
  tile <- rast(tile_path)
  pnr_points <- tile %>%  # Give it a meaningful name
    as.data.frame(xy = TRUE) %>%
    filter(.[[3]] == 1) %>%
    select(x, y)
  
  # Save as RData
  tile_name <- tools::file_path_sans_ext(basename(tile_path))
  output_file <- file.path(output_dir, paste0(tile_name, "_pnr_points.RData"))
  save(pnr_points, file = output_file)
  
  cat("  PNR cells:", nrow(pnr_points), "- saved to:", output_file, "\n")
  return(pnr_points)
}

# Later combine all RData files
rdata_files <- list.files("processed_pnr_tiles", pattern = "*.RData$", full.names = TRUE)
pnr_combined_df <- rdata_files %>%
  map_dfr(~{
    load(.x)  # Loads object named 'pnr_points'
    return(pnr_points)
  })

# Save combined version
save(pnr_combined_df, file = "pnr_all_combined.RData")


e_time <- Sys.time()
total_time <- e_time - s_time
total_time

# If it works well, process all tiles
# all_pnr_dfs <- lapply(projected_tiles, process_pnr_tile)
# pnr_combined_df <- bind_rows(all_pnr_dfs)