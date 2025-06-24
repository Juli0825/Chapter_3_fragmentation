library(terra)
library(landscapemetrics)
library(ggplot2)
library(gridExtra)

# CREATE SYNTHETIC RASTERS TO TEST FFI BEHAVIOR
# Goal: Test how FFI responds to different forest landscape changes

# ===== SETUP =====
# Create 5km x 5km rasters at 10m resolution (500 x 500 pixels)
create_base_raster <- function() {
  r <- rast(nrows = 500, ncols = 500, 
            xmin = 0, xmax = 5000, ymin = 0, ymax = 5000,
            crs = "EPSG:3857")
  values(r) <- 0  # Start with all non-forest (0)
  return(r)
}

# ===== SCENARIO 1: INTACT → FRAGMENTED (Deforestation) =====
create_scenario_1 <- function() {
  cat("Creating Scenario 1: Intact → Fragmented (Deforestation)\n")
  
  # Before: Large intact forest (central square)
  r1_before <- create_base_raster()
  r1_before[150:350, 150:350] <- 1  # 200x200 pixels = large patch
  
  # After: Same area but fragmented into many small pieces
  r1_after <- create_base_raster()
  
  # Create multiple small patches with similar total area
  patches <- list(
    c(50, 50, 100, 100),     # 2500 pixels
    c(200, 200, 240, 240),   # 1600 pixels  
    c(350, 100, 390, 130),   # 1200 pixels
    c(100, 350, 130, 390),   # 1200 pixels
    c(400, 400, 430, 430),   # 900 pixels
    c(50, 200, 80, 220),     # 600 pixels
    c(300, 300, 320, 320),   # 400 pixels
    c(150, 150, 170, 170),   # 400 pixels
    c(250, 50, 270, 70),     # 400 pixels
    c(450, 200, 470, 220),   # 400 pixels
    c(200, 450, 220, 470),   # 400 pixels
    c(350, 350, 370, 370),   # 400 pixels
    c(480, 480, 500, 500),   # 400 pixels
    c(10, 10, 30, 30),       # 400 pixels
    c(100, 100, 120, 120),   # 400 pixels
    c(460, 50, 480, 70)      # 400 pixels
  )
  
  for(patch in patches) {
    r1_after[patch[1]:patch[3], patch[2]:patch[4]] <- 1
  }
  
  return(list(before = r1_before, after = r1_after))
}

# ===== SCENARIO 2: EXPANSION + ADDITION (Restoration) =====
create_scenario_2 <- function() {
  cat("Creating Scenario 2: Expansion + Addition (Restoration)\n")
  
  # Before: Medium-sized forest patch
  r2_before <- create_base_raster()
  r2_before[200:300, 200:300] <- 1  # 100x100 = smaller patch
  
  # After: Expand original + add new patches
  r2_after <- create_base_raster()
  
  # Expand original patch
  r2_after[150:350, 150:350] <- 1  # 200x200 = expanded
  
  # Add new patches (restoration)
  new_patches <- list(
    c(50, 50, 100, 100),     # 2500 pixels
    c(400, 400, 450, 450),   # 2500 pixels  
    c(50, 400, 90, 440),     # 1600 pixels
    c(400, 50, 440, 90)      # 1600 pixels
  )
  
  for(patch in new_patches) {
    r2_after[patch[1]:patch[3], patch[2]:patch[4]] <- 1
  }
  
  return(list(before = r2_before, after = r2_after))
}

# ===== SCENARIO 3: CONNECTION (Fragment Connection) =====
create_scenario_3 <- function() {
  cat("Creating Scenario 3: Fragment Connection\n")
  
  # Before: Many small fragments
  r3_before <- create_base_raster()
  
  small_patches <- list(
    c(100, 100, 150, 150),   # 2500 pixels
    c(200, 200, 240, 240),   # 1600 pixels
    c(300, 300, 340, 340),   # 1600 pixels
    c(150, 300, 180, 330),   # 900 pixels
    c(300, 150, 330, 180),   # 900 pixels
    c(250, 100, 280, 120),   # 600 pixels
    c(100, 250, 120, 280),   # 600 pixels
    c(350, 200, 370, 230),   # 600 pixels
    c(200, 350, 230, 370)    # 600 pixels
  )
  
  for(patch in small_patches) {
    r3_before[patch[1]:patch[3], patch[2]:patch[4]] <- 1
  }
  
  # After: Connect fragments into larger connected areas
  r3_after <- create_base_raster()
  
  # Create connected patches
  r3_after[100:400, 100:400] <- 1  # Large connected area
  
  # Remove some areas to create realistic gaps
  r3_after[200:250, 200:250] <- 0
  r3_after[320:350, 320:350] <- 0
  
  return(list(before = r3_before, after = r3_after))
}

# ===== NEW SCENARIO 4: WHOLE FOREST CUT IN HALF =====
create_scenario_4 <- function() {
  cat("Creating Scenario 4: Whole Forest Cut in Half\n")
  
  # Before: Entire grid is forest
  r4_before <- create_base_raster()
  r4_before[50:450, 50:450] <- 1  # Almost entire grid is forest
  
  # After: Cut in half with a gap in the middle
  r4_after <- create_base_raster()
  
  # Left half
  r4_after[50:450, 50:240] <- 1   # Left side
  # Right half  
  r4_after[50:450, 260:450] <- 1  # Right side
  # Gap in middle (240:260) creates the "cut"
  
  return(list(before = r4_before, after = r4_after))
}

# ===== CALCULATE FFI FOR SCENARIOS =====
calculate_ffi_for_scenario <- function(raster_before, raster_after, scenario_name) {
  cat("\n=== CALCULATING FFI FOR", scenario_name, "===\n")
  
  # Calculate metrics for both timepoints
  metrics_before <- calculate_lsm(raster_before, what = c("lsm_c_ed", "lsm_c_pd", "lsm_c_area_mn"))
  metrics_after <- calculate_lsm(raster_after, what = c("lsm_c_ed", "lsm_c_pd", "lsm_c_area_mn"))
  
  # Filter for forest class (class = 1)
  metrics_before <- metrics_before[metrics_before$class == 1, ]
  metrics_after <- metrics_after[metrics_after$class == 1, ]
  
  # Extract values
  extract_metric <- function(metrics_df, metric_name) {
    if(nrow(metrics_df) == 0) return(0)
    val <- metrics_df$value[metrics_df$metric == metric_name]
    if(length(val) == 0) return(0)
    return(val)
  }
  
  ed_before <- extract_metric(metrics_before, "ed")
  pd_before <- extract_metric(metrics_before, "pd")  
  mpa_before <- extract_metric(metrics_before, "area_mn")
  
  ed_after <- extract_metric(metrics_after, "ed")
  pd_after <- extract_metric(metrics_after, "pd")
  mpa_after <- extract_metric(metrics_after, "area_mn")
  
  # Print raw metrics
  cat("Raw metrics BEFORE:\n")
  cat("  ED:", round(ed_before, 3), "  PD:", round(pd_before, 3), "  MPA:", round(mpa_before, 3), "\n")
  
  cat("Raw metrics AFTER:\n") 
  cat("  ED:", round(ed_after, 3), "  PD:", round(pd_after, 3), "  MPA:", round(mpa_after, 3), "\n")
  
  # Normalize within global ranges (simplified for this example)
  # Use max values across all scenarios for normalization
  all_ed <- c(ed_before, ed_after)
  all_pd <- c(pd_before, pd_after)
  all_mpa <- c(mpa_before, mpa_after)
  
  if(max(all_ed) > 0) {
    ed_norm_before <- ed_before / max(all_ed)
    ed_norm_after <- ed_after / max(all_ed)
  } else {
    ed_norm_before <- ed_norm_after <- 0
  }
  
  if(max(all_pd) > 0) {
    pd_norm_before <- pd_before / max(all_pd)
    pd_norm_after <- pd_after / max(all_pd)
  } else {
    pd_norm_before <- pd_norm_after <- 0
  }
  
  if(max(all_mpa) > 0) {
    mpa_norm_before <- mpa_before / max(all_mpa)
    mpa_norm_after <- mpa_after / max(all_mpa)
  } else {
    mpa_norm_before <- mpa_norm_after <- 0
  }
  
  # Calculate FFI using Ma et al formula: FFI = (ED_norm + PD_norm + (1 - MPA_norm)) / 3
  ffi_before <- (ed_norm_before + pd_norm_before + (1 - mpa_norm_before)) / 3
  ffi_after <- (ed_norm_after + pd_norm_after + (1 - mpa_norm_after)) / 3
  
  cat("FFI RESULTS:\n")
  cat("  FFI before:", round(ffi_before, 4), "\n")
  cat("  FFI after:", round(ffi_after, 4), "\n")
  cat("  FFI change (Delta_FFI):", round(ffi_after - ffi_before, 4), "\n")
  
  # Interpretation
  if(ffi_after > ffi_before) {
    cat("  Interpretation: MORE fragmented (FFI increased)\n")
  } else if(ffi_after < ffi_before) {
    cat("  Interpretation: LESS fragmented (FFI decreased)\n")
  } else {
    cat("  Interpretation: NO CHANGE in fragmentation\n")
  }
  
  return(list(
    scenario = scenario_name,
    ffi_before = ffi_before,
    ffi_after = ffi_after,
    ffi_change = ffi_after - ffi_before,
    forest_before = sum(values(raster_before), na.rm=TRUE),
    forest_after = sum(values(raster_after), na.rm=TRUE)
  ))
}

# ===== CREATE VISUALIZATION =====
plot_scenario <- function(scenario_data, scenario_name) {
  # Convert rasters to data frames for ggplot
  before_df <- as.data.frame(scenario_data$before, xy = TRUE)
  after_df <- as.data.frame(scenario_data$after, xy = TRUE)
  
  # Rename column for consistency
  names(before_df)[3] <- "forest"
  names(after_df)[3] <- "forest"
  
  # Create plots
  p1 <- ggplot(before_df, aes(x = x, y = y, fill = factor(forest))) +
    geom_raster() +
    scale_fill_manual(values = c("0" = "white", "1" = "darkgreen"), 
                      name = "Land Cover", labels = c("Non-forest", "Forest")) +
    labs(title = paste(scenario_name, "- BEFORE")) +
    theme_minimal() +
    theme(legend.position = "bottom", 
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    coord_equal()
  
  p2 <- ggplot(after_df, aes(x = x, y = y, fill = factor(forest))) +
    geom_raster() +
    scale_fill_manual(values = c("0" = "white", "1" = "darkgreen"), 
                      name = "Land Cover", labels = c("Non-forest", "Forest")) +
    labs(title = paste(scenario_name, "- AFTER")) +
    theme_minimal() +
    theme(legend.position = "bottom", 
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    coord_equal()
  
  return(grid.arrange(p1, p2, ncol = 2))
}

# ===== MAIN EXECUTION =====
run_ffi_test <- function() {
  cat("SYNTHETIC FFI TESTING\n")
  cat("=====================\n")
  cat("Testing how FFI responds to four different scenarios\n\n")
  
  # Create all scenarios
  scenario_1 <- create_scenario_1()
  scenario_2 <- create_scenario_2()  
  scenario_3 <- create_scenario_3()
  scenario_4 <- create_scenario_4()
  
  # Calculate FFI for each scenario
  results_1 <- calculate_ffi_for_scenario(scenario_1$before, scenario_1$after, "SCENARIO 1 (Deforestation)")
  results_2 <- calculate_ffi_for_scenario(scenario_2$before, scenario_2$after, "SCENARIO 2 (Expansion + Addition)")
  results_3 <- calculate_ffi_for_scenario(scenario_3$before, scenario_3$after, "SCENARIO 3 (Fragment Connection)")
  results_4 <- calculate_ffi_for_scenario(scenario_4$before, scenario_4$after, "SCENARIO 4 (Whole Forest Cut in Half)")
  
  # Create visualizations
  cat("\nCreating visualizations...\n")
  
  plot_scenario(scenario_1, "Scenario 1: Deforestation")
  plot_scenario(scenario_2, "Scenario 2: Restoration") 
  plot_scenario(scenario_3, "Scenario 3: Connection")
  plot_scenario(scenario_4, "Scenario 4: Cut in Half")
  
  # Summary table
  cat("\n", strrep("=", 80), "\n")
  cat("SUMMARY OF FFI RESPONSES\n")
  cat(strrep("=", 80), "\n")
  
  scenarios <- list(results_1, results_2, results_3, results_4)
  
  for(result in scenarios) {
    cat("\n", result$scenario, ":\n")
    cat("  Forest pixels before:", result$forest_before, "\n")
    cat("  Forest pixels after:", result$forest_after, "\n")
    cat("  Forest change:", result$forest_after - result$forest_before, "\n")
    cat("  FFI before:", round(result$ffi_before, 4), "\n")
    cat("  FFI after:", round(result$ffi_after, 4), "\n")
    cat("  FFI change (Delta_FFI):", round(result$ffi_change, 4))
    if(result$ffi_change > 0.001) {
      cat(" (MORE fragmented)\n")
    } else if(result$ffi_change < -0.001) {
      cat(" (LESS fragmented)\n")  
    } else {
      cat(" (NO CHANGE)\n")
    }
  }
  
  cat("\n", strrep("=", 80), "\n")
  cat("ECOLOGICAL INTERPRETATION\n")
  cat(strrep("=", 80), "\n")
  cat("Scenario 1 (Deforestation): Large intact patch → Many small patches\n")
  cat("  Expected: MORE fragmented ✓\n")
  cat("  FFI says:", ifelse(results_1$ffi_change > 0, "MORE fragmented ✓", "LESS fragmented ✗"), "\n\n")
  
  cat("Scenario 2 (Restoration): Small patch → Larger patch + new patches\n") 
  cat("  Expected: LESS fragmented (more forest, larger patches) ✓\n")
  cat("  FFI says:", ifelse(results_2$ffi_change > 0, "MORE fragmented ✗", "LESS fragmented ✓"), "\n\n")
  
  cat("Scenario 3 (Connection): Many fragments → Fewer, connected patches\n")
  cat("  Expected: LESS fragmented (connectivity improved) ✓\n") 
  cat("  FFI says:", ifelse(results_3$ffi_change > 0, "MORE fragmented ✗", "LESS fragmented ✓"), "\n\n")
  
  cat("Scenario 4 (Cut in Half): Whole forest → Two separate halves\n")
  cat("  Expected: MORE fragmented (reduced connectivity) ✓\n")
  cat("  FFI says:", ifelse(results_4$ffi_change > 0, "MORE fragmented ✓", "LESS fragmented ✗"), "\n\n")
  
  return(scenarios)
}

# Execute the test
cat("Starting FFI testing...\n")
results <- run_ffi_test()
cat("\nTesting complete!")