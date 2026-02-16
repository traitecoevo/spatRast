#!/usr/bin/env Rscript
# extract_test_points.R — Parse test_points.csv, extract cost-distance, plot
library(terra)
source("cost_distance.R")

# --- Parse test_points.csv ---
raw <- readLines("test_points.csv")
# Strip carriage returns, non-breaking spaces (U+00A0), and whitespace
lines <- gsub("\r", "", raw)
lines <- gsub("\u00A0", " ", lines)  # Replace NBSP with regular space
lines <- trimws(lines)
lines <- lines[nchar(lines) > 0]

# Parse "Name,POINT (x y)"
coord_pattern <- "POINT \\(([0-9.]+) ([0-9.]+)\\)"
parsed <- do.call(rbind, lapply(lines, function(ln) {
  # Extract name (everything before ,POINT)
  comma_pos <- regexpr(",POINT", ln)
  name <- trimws(substr(ln, 1, comma_pos - 1))
  # Extract coordinates via regex
  m <- regmatches(ln, regexec(coord_pattern, ln))[[1]]
  data.frame(
    name = name,
    x = as.numeric(m[2]),
    y = as.numeric(m[3]),
    stringsAsFactors = FALSE
  )
}))

cat("Parsed", nrow(parsed), "points\n")
cat("Unique locations:", nrow(unique(parsed[, c("x", "y")])), "\n\n")

# --- Run cost distance ---
result <- calc_cost_to_tracks(
  cost_raster = "cost_speed/cost.tif",
  tracks = "cost_speed/tracks_crop.rds",
  query_points = data.frame(x = parsed$x, y = parsed$y),
  verbose = TRUE
)

# --- Build results table ---
parsed$cost_distance <- result$point_costs
cat("\n=== Cost-Distance Results (sorted) ===\n")
print(parsed[order(parsed$cost_distance), ], row.names = FALSE)

# --- Save results to CSV ---
write.csv(parsed[order(parsed$cost_distance), ], "test_points_results.csv", row.names = FALSE)
cat("\nResults saved to: test_points_results.csv\n")

# --- Generate updated plot ---
cat("\nGenerating updated plot...\n")
png("cost_distance_plot.png", width = 2000, height = 1600, res = 150)

plot(result$surface,
     main = "Cost-Distance to Nearest Track (all values labelled)",
     col = grDevices::hcl.colors(50, "YlOrRd", rev = TRUE))

# Overlay tracks
tracks <- readRDS("cost_speed/tracks_crop.rds")
tracks_sv <- vect(tracks)
lines(tracks_sv, col = "black", lwd = 1.5)

# Overlay ALL test points
points(parsed$x, parsed$y, pch = 21, bg = "cyan", cex = 0.9, lwd = 1.2)

# Label every point with its cost-distance value
text(parsed$x, parsed$y,
     labels = round(parsed$cost_distance, 1),
     pos = 4, cex = 0.4, col = "black", font = 2, offset = 0.3)

dev.off()
cat("Plot saved to: cost_distance_plot.png\n")
