library(terra)
library(landscapemetrics)
library(ggplot2)
library(gridExtra)

# CREATE SYNTHETIC RASTERS TO TEST FFI BEHAVIOR
# Goal: Test how FFI responds to your three conceptual figures

# ===== SETUP =====
# Create 5km x 5km rasters at 10m resolution (500 x 500 pixels)
create_base_raster <- function() {
  # 5000m / 10m = 500 pixels per side
  r <- rast(nrows = 500, ncols = 500, 
            xmin = 0, xmax = 5000, ymin = 0, ymax = 5000,
            crs = "EPSG:3857")  # Simple projected CRS
  values(r) <- 0  # Start with all non-forest (0)
  return(r)
}

# ===== SCENARIO 1: INTACT → FRAGMENTED (Classic Deforestation) =====
create_scenario_1 <- function() {
  cat("Creating Scenario 1: Intact → Fragmented (Deforestation)\n")
  
  # Before: Large intact forest (60% of landscape)
  r1_before <- create_base_raster()
  # Create a large square forest patch in center
  forest_size <- 300  # 300x300 pixels = 3km x 3km
  start_row <- (500 - forest_size) / 2
  end_row <- start_row + forest_size
  start_col <- (500 - forest_size) / 2  
  end_col <- start_col + forest_size
  
  r1_before[start_row:end_row, start_col:end_col] <- 1
  
  # After: Same total forest area but fragmented into small pieces
  r1_after <- create_base_raster()
  
  # Create multiple small patches with same total area
  # 300x300 = 90,000 pixels total
  # Make ~20 patches of different sizes
  set.seed(123)
  patches <- list(
    # Some medium patches
    c(50, 50, 100, 100),   # 2500 pixels
    c(200, 200, 280, 250), # 4000 pixels  
    c(350, 100, 420, 150), # 3500 pixels
    c(100, 350, 170, 420), # 4900 pixels
    # Many small patches
    c(400, 400, 440, 430), # 1200 pixels
    c(50, 200, 80, 220),   # 600 pixels
    c(300, 300, 320, 320), # 400 pixels
    c(150, 150, 170, 170), # 400 pixels
    c(250, 50, 270, 70),   # 400 pixels
    c(450, 200, 470, 220), # 400 pixels
    c(200, 450, 220, 470), # 400 pixels
    c(350, 350, 370, 370), # 400 pixels
    c(400, 50, 420, 70),   # 400 pixels
    c(50, 450, 70, 470),   # 400 pixels
    c(480, 480, 500, 500), # 400 pixels
    c(10, 10, 30, 30),     # 400 pixels
    c(300, 450, 320, 470), # 400 pixels
    c(450, 300, 470, 320), # 400 pixels
    c(100, 100, 120, 120), # 400 pixels
    c(200, 300, 220, 320)  # 400 pixels
  )
  
  # Add patches to ensure we get close to original forest area
  total_pixels_needed <- 90000
  current_pixels <- 0
  
  for(i in seq_along(patches)) {
    if(current_pixels >= total_pixels_needed) break
    
    patch <- patches[[i]]
    r1_after[patch[1]:patch[3], patch[2]:patch[4]] <- 1
    current_pixels <- current_pixels + (patch[3]-patch[1]+1) * (patch[4]-patch[2]+1)
  }
  
  cat("Scenario 1 - Forest area before:", sum(values(r1_before), na.rm=TRUE), "pixels\n")
  cat("Scenario 1 - Forest area after:", sum(values(r1_after), na.rm=TRUE), "pixels\n")
  
  return(list(before = r1_before, after = r1_after))
}

# ===== SCENARIO 2: EXPANSION + ADDITION (Your Restoration) =====
create_scenario_2 <- function() {
  cat("\nCreating Scenario 2: Expansion + Addition (Restoration)\n")
  
  # Before: Medium-sized forest patch
  r2_before <- create_base_raster()
  # Single patch (40% of landscape)
  r2_before[150:350, 150:350] <- 1  # 200x200 = 40,000 pixels
  
  # After: Expand original patch + add several new patches
  r2_after <- create_base_raster()
  
  # Expand original patch
  r2_after[120:380, 120:380] <- 1  # 260x260 = 67,600 pixels
  
  # Add several new patches (restoration in abandoned areas)
  new_patches <- list(
    c(50, 50, 100, 100),     # 2500 pixels
    c(400, 400, 450, 450),   # 2500 pixels  
    c(50, 400, 90, 440),     # 1600 pixels
    c(400, 50, 440, 90),     # 1600 pixels
    c(200, 20, 230, 50),     # 900 pixels
    c(20, 200, 50, 230),     # 900 pixels
    c(450, 200, 480, 230),   # 900 pixels
    c(200, 450, 230, 480)    # 900 pixels
  )
  
  for(patch in new_patches) {
    r2_after[patch[1]:patch[3], patch[2]:patch[4]] <- 1
  }
  
  cat("Scenario 2 - Forest area before:", sum(values(r2_before), na.rm=TRUE), "pixels\n")
  cat("Scenario 2 - Forest area after:", sum(values(r2_after), na.rm=TRUE), "pixels\n")
  
  return(list(before = r2_before, after = r2_after))
}

# ===== SCENARIO 3: CONNECTION (Ideal Restoration) =====
create_scenario_3 <- function() {
  cat("\nCreating Scenario 3: Fragment Connection (Ideal Restoration)\n")
  
  # Before: Many small fragments
  r3_before <- create_base_raster()
  
  # Create scattered small patches
  small_patches <- list(
    c(100, 100, 150, 150),   # 2500 pixels
    c(200, 200, 240, 240),   # 1600 pixels
    c(300, 300, 340, 340),   # 1600 pixels
    c(150, 300, 180, 330),   # 900 pixels
    c(300, 150, 330, 180),   # 900 pixels
    c(250, 100, 280, 120),   # 600 pixels
    c(100, 250, 120, 280),   # 600 pixels
    c(350, 200, 370, 230),   # 600 pixels
    c(200, 350, 230, 370),   # 600 pixels
    c(120, 200, 140, 220),   # 400 pixels
    c(250, 250, 270, 270),   # 400 pixels
    c(180, 150, 200, 170),   # 400 pixels
    c(320, 120, 340, 140),   # 400 pixels
    c(380, 300, 400, 320),   # 400 pixels
    c(300, 380, 320, 400)    # 400 pixels
  )
  
  for(patch in small_patches) {
    r3_before[patch[1]:patch[3], patch[2]:patch[4]] <- 1
  }
  
  # After: Connect fragments into fewer, larger patches
  r3_after <- create_base_raster()
  
  # Create large connected patches by filling gaps
  # Main connected patch covering central area
  r3_after[100:400, 100:400] <- 1  # Large connected area
  
  # Remove some area to match roughly same total forest cover
  # Create some gaps to make it realistic
  r3_after[150:200, 150:200] <- 0
  r3_after[300:350, 300:350] <- 0
  r3_after[120:140, 300:350] <- 0
  
  cat("Scenario 3 - Forest area before:", sum(values(r3_before), na.rm=TRUE), "pixels\n")
  cat("Scenario 3 - Forest area after:", sum(values(r3_after), na.rm=TRUE), "pixels\n")
  
  return(list(before = r3_before, after = r3_after))
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
  
  # For simplicity, normalize within this scenario (0-1 scale)
  # In reality, you'd use global boundaries
  all_values <- c(ed_before, ed_after, pd_before, pd_after, mpa_before, mpa_after)
  
  if(max(all_values) > 0) {
    ed_norm_before <- ed_before / max(ed_before, ed_after)
    ed_norm_after <- ed_after / max(ed_before, ed_after)
    
    pd_norm_before <- pd_before / max(pd_before, pd_after)  
    pd_norm_after <- pd_after / max(pd_before, pd_after)
    
    mpa_norm_before <- mpa_before / max(mpa_before, mpa_after)
    mpa_norm_after <- mpa_after / max(mpa_before, mpa_after)
  } else {
    ed_norm_before <- ed_norm_after <- 0
    pd_norm_before <- pd_norm_after <- 0
    mpa_norm_before <- mpa_norm_after <- 0
  }
  
  # Calculate FFI using Ma et al formula
  ffi_before <- (ed_norm_before + pd_norm_before + (1 - mpa_norm_before)) / 3
  ffi_after <- (ed_norm_after + pd_norm_after + (1 - mpa_norm_after)) / 3
  
  cat("Normalized metrics BEFORE:\n")
  cat("  ED_norm:", round(ed_norm_before, 3), "  PD_norm:", round(pd_norm_before, 3), "  MPA_norm:", round(mpa_norm_before, 3), "\n")
  
  cat("Normalized metrics AFTER:\n")
  cat("  ED_norm:", round(ed_norm_after, 3), "  PD_norm:", round(pd_norm_after, 3), "  MPA_norm:", round(mpa_norm_after, 3), "\n")
  
  cat("FFI RESULTS:\n")
  cat("  FFI before:", round(ffi_before, 3), "\n")
  cat("  FFI after:", round(ffi_after, 3), "\n")
  cat("  FFI change:", round(ffi_after - ffi_before, 3), "\n")
  
  # Interpretation
  if(ffi_after > ffi_before) {
    cat("  Interpretation: MORE fragmented (FFI increased)\n")
  } else {
    cat("  Interpretation: LESS fragmented (FFI decreased)\n")
  }
  
  return(list(
    scenario = scenario_name,
    ffi_before = ffi_before,
    ffi_after = ffi_after,
    ffi_change = ffi_after - ffi_before,
    ed_before = ed_before, ed_after = ed_after,
    pd_before = pd_before, pd_after = pd_after,
    mpa_before = mpa_before, mpa_after = mpa_after
  ))
}

# ===== CREATE VISUALIZATION =====
plot_scenario <- function(scenario_data, scenario_name) {
  # Convert rasters to data frames for ggplot
  before_df <- as.data.frame(scenario_data$before, xy = TRUE)
  after_df <- as.data.frame(scenario_data$after, xy = TRUE)
  
  # Create plots
  p1 <- ggplot(before_df, aes(x = x, y = y, fill = factor(lyr.1))) +
    geom_raster() +
    scale_fill_manual(values = c("0" = "white", "1" = "darkgreen")) +
    labs(title = paste(scenario_name, "- BEFORE"), fill = "Forest") +
    theme_minimal() +
    theme(legend.position = "none", axis.text = element_blank())
  
  p2 <- ggplot(after_df, aes(x = x, y = y, fill = factor(lyr.1))) +
    geom_raster() +
    scale_fill_manual(values = c("0" = "white", "1" = "darkgreen")) +
    labs(title = paste(scenario_name, "- AFTER"), fill = "Forest") +
    theme_minimal() +
    theme(legend.position = "none", axis.text = element_blank())
  
  return(grid.arrange(p1, p2, ncol = 2))
}

# ===== MAIN EXECUTION =====
run_ffi_test <- function() {
  cat("SYNTHETIC FFI TESTING\n")
  cat("=====================\n")
  cat("Testing how FFI responds to three restoration scenarios\n\n")
  
  # Create all scenarios
  scenario_1 <- create_scenario_1()
  scenario_2 <- create_scenario_2()  
  scenario_3 <- create_scenario_3()
  
  # Calculate FFI for each scenario
  results_1 <- calculate_ffi_for_scenario(scenario_1$before, scenario_1$after, "SCENARIO 1 (Deforestation)")
  results_2 <- calculate_ffi_for_scenario(scenario_2$before, scenario_2$after, "SCENARIO 2 (Expansion + Addition)")
  results_3 <- calculate_ffi_for_scenario(scenario_3$before, scenario_3$after, "SCENARIO 3 (Fragment Connection)")
  
  # Summary table
  cat("\n", strrep("=", 60), "\n")
  cat("SUMMARY OF FFI RESPONSES\n")
  cat(strrep("=", 60), "\n")
  
  scenarios <- list(results_1, results_2, results_3)
  
  for(result in scenarios) {
    cat("\n", result$scenario, ":\n")
    cat("  FFI change:", round(result$ffi_change, 3))
    if(result$ffi_change > 0) {
      cat(" (MORE fragmented according to FFI)\n")
    } else {
      cat(" (LESS fragmented according to FFI)\n")  
    }
  }
  
  cat("\n", strrep("=", 60), "\n")
  cat("ECOLOGICAL vs FFI INTERPRETATION\n")
  cat(strrep("=", 60), "\n")
  cat("Scenario 1 (Deforestation):\n")
  cat("  Ecological reality: WORSE (large → small patches)\n")
  cat("  FFI says:", ifelse(results_1$ffi_change > 0, "WORSE ✓", "BETTER ✗"), "\n\n")
  
  cat("Scenario 2 (Expansion + Addition):\n") 
  cat("  Ecological reality: BETTER (more forest + expansion)\n")
  cat("  FFI says:", ifelse(results_2$ffi_change > 0, "WORSE ✗", "BETTER ✓"), "\n\n")
  
  cat("Scenario 3 (Connection):\n")
  cat("  Ecological reality: BETTER (fragments connected)\n") 
  cat("  FFI says:", ifelse(results_3$ffi_change > 0, "WORSE ✗", "BETTER ✓"), "\n\n")
  
  # Create visualizations
  cat("Creating visualizations...\n")
  
  png("scenario_1_deforestation.png", width = 800, height = 400, res = 150)
  plot_scenario(scenario_1, "Scenario 1: Deforestation")
  dev.off()
  
  png("scenario_2_restoration.png", width = 800, height = 400, res = 150)
  plot_scenario(scenario_2, "Scenario 2: Restoration")
  dev.off()
  
  png("scenario_3_connection.png", width = 800, height = 400, res = 150)
  plot_scenario(scenario_3, "Scenario 3: Connection")
  dev.off()
  
  cat("Visualizations saved as PNG files\n")
  
  return(scenarios)
}

# Execute the test
results <- run_ffi_test()