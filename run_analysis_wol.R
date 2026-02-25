#!/usr/bin/env Rscript
# run_analysis_wol.R — Perform cost-distance analysis for WOL dataset
# broken down into step-by-step chunks for easy troubleshooting.

library(terra)

# --- Define Directories ---
WKDir <- "C:/Users/z5161362/OneDrive - DPIE/Documents/PhD/spatRast/"

# Source the improved functions
source(paste0(WKDir, "R/cost_distance.R"))

# --- 1. Setup Terra ---
# Correctly set tempdir to your large drive and set memory fraction
setup_terra_for_large_rasters(
  tempdir = paste0(WKDir, "output/temp/"),
  memfrac = 0.6,
  verbose = TRUE
)

# --- 2. Load Inputs ---
message("\n--- Step 1: Loading Inputs ---")
cost <- load_cost_raster(paste0(WKDir, "data/processing/wol/cost.tif"))
track <- load_tracks(paste0(WKDir, "data/processing/wol/tracks.rds"))

# --- 3. Burn Tracks ---
# This creates a large intermediate file. 
# We save it to the output folder (on your large drive) instead of R's default temp folder.
message("\n--- Step 2: Burning Tracks ---")
burn_tracks(cost, track, filename = paste0(WKDir, "output/burn.tif"), overwrite = TRUE)

# Load the burned raster
burn <- terra::rast(paste0(WKDir, "output/burn.tif"))
message("Burned raster created at: ", paste0(WKDir, "output/burn.tif"))

# --- 4. Run Cost-Distance ---
message("\n--- Step 3: Running Cost-Distance ---")
# This is the step that creates the 645GB+ temp files (directed to output/temp/ by setup_terra)
run_cost_dist(burn, filename = paste0(WKDir, "output/cost_surface.tif"), overwrite = TRUE)

# Load final results
cost_surface <- terra::rast(paste0(WKDir, "output/cost_surface.tif"))
message("\nAnalysis Complete.")
message("Final cost surface saved to: ", paste0(WKDir, "output/cost_surface.tif"))

# --- 5. Optional Cleanup ---
# Once cost_surface is created, you can safely delete the large 'burn.tif' to free up space.
# Uncomment the line below if you want to delete it automatically:
# unlink(paste0(WKDir, "output/burn.tif"))
