#!/usr/bin/env Rscript
# run_benchmarks.R — Test calc_cost_to_tracks with davisia and banksia points
#
# Usage: Rscript run_benchmarks.R
# Run from the spatRast project root directory.

library(terra)

# Source the function
source("cost_distance.R")

# --- Define inputs ---
cost_file <- "cost_speed/cost.tif"
tracks_file <- "cost_speed/tracks_crop.rds"

# Test points (EPSG:28356 coordinates)
test_points <- data.frame(
  x = c(242329.4, 243198.9),
  y = c(6326149, 6326506),
  row.names = c("davisia", "banksia")
)

cat("=======================================================\n")
cat("  Cost-Distance Benchmark\n")
cat("=======================================================\n\n")
cat("Test points:\n")
print(test_points)
cat("\n")

# --- Run the analysis ---
result <- calc_cost_to_tracks(
  cost_raster = cost_file,
  tracks = tracks_file,
  query_points = test_points,
  verbose = TRUE
)

# --- Print results ---
cat("\n=======================================================\n")
cat("  Results Summary\n")
cat("=======================================================\n\n")

cat("Cost-distance values at test points:\n")
for (nm in names(result$point_costs)) {
  cat(sprintf("  %-10s: %.4f\n", nm, result$point_costs[nm]))
}

cat("\nBenchmark:\n")
cat(sprintf("  Elapsed time:    %.2f seconds\n", result$benchmark$elapsed_secs))
cat(sprintf("  Peak memory:     %.1f MB\n", result$benchmark$peak_mem_mb))
cat(sprintf("  Raster dims:     %d x %d\n",
            result$benchmark$raster_dims[1], result$benchmark$raster_dims[2]))
cat(sprintf("  Total cells:     %s\n",
            format(result$benchmark$n_cells, big.mark = ",")))
cat(sprintf("  Track cells:     %s (%.2f%%)\n",
            format(result$benchmark$n_track_cells, big.mark = ","),
            100 * result$benchmark$n_track_cells / result$benchmark$n_cells))

# --- Generate plot ---
cat("\nGenerating cost-distance plot...\n")
png("cost_distance_plot.png", width = 1200, height = 1000, res = 150)

plot(result$surface,
     main = "Cost-Distance to Nearest Track",
     col = grDevices::hcl.colors(50, "YlOrRd", rev = TRUE))

# Overlay tracks
tracks <- readRDS(tracks_file)
tracks_sv <- vect(tracks)
lines(tracks_sv, col = "black", lwd = 1.5)

# Overlay test points
points(test_points$x, test_points$y, pch = 21, bg = "cyan", cex = 2, lwd = 2)
text(test_points$x, test_points$y,
     labels = paste0(rownames(test_points), "\n(",
                     round(result$point_costs, 2), ")"),
     pos = 3, cex = 0.8, font = 2, offset = 1)

dev.off()
cat("Plot saved to: cost_distance_plot.png\n")

cat("\n=======================================================\n")
cat("  Benchmark complete\n")
cat("=======================================================\n")
