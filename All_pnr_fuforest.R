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
# Function to process each projected PNR tile
process_pnr_tile <- function(tile_path) {
  cat("Processing:", basename(tile_path), "\n")
  
  # Load projected tile
  tile <- rast(tile_path)
  
  # Convert to dataframe, keep only PNR=1 cells
  tile_df <- tile %>%
    as.data.frame(xy = TRUE) %>%
    filter(.[[3]] == 1) %>%  # Keep only regenerable cells
    select(x, y)  # Just coordinates for now
  
  cat("  PNR cells found:", nrow(tile_df), "\n")
  return(tile_df)
}

# Get list of all projected tiles
projected_tiles <- list.files("Data/mo_bi_pnr", pattern = "*.tif$", full.names = TRUE)

# Process first tile as test
test_tile_df <- process_pnr_tile(projected_tiles[1])
head(test_tile_df)


e_time <- Sys.time()
total_time <- e_time - s_time
totoal_time

# If it works well, process all tiles
# all_pnr_dfs <- lapply(projected_tiles, process_pnr_tile)
# pnr_combined_df <- bind_rows(all_pnr_dfs)