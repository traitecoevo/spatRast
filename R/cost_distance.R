# cost_distance.R — Functions for cost-distance analysis using terra
#
# Decomposed into modular subfunctions:
#   load_cost_raster()     — Load and validate a cost raster
#   load_tracks()          — Load and validate a track network
#   burn_tracks()          — Rasterize tracks onto cost surface (returns cost_with_tracks)
#   run_cost_dist()        — Run terra::costDist with benchmarking
#   extract_point_costs()  — Extract cost-distance at query points
#   calc_cost_to_tracks()  — Orchestrator that calls the above
#   plot_cost_with_tracks()— Visualise the cost raster with burned-in tracks


# ---- Load helpers --------------------------------------------------------

#' Load a cost/friction raster
#'
#' Accepts a file path (character) or an existing SpatRaster.
#'
#' @param cost_raster SpatRaster or character file path.
#' @param verbose Logical. Print info messages.
#' @return A SpatRaster.
load_cost_raster <- function(cost_raster, verbose = TRUE) {
  if (is.character(cost_raster)) {
    if (verbose) message("Loading cost raster: ", cost_raster)
    cost_raster <- terra::rast(cost_raster)
  }
  stopifnot(inherits(cost_raster, "SpatRaster"))
  cost_raster
}

#' Load a track/path network
#'
#' Accepts a file path (.rds or shapefile) , an sf object, or a SpatVector.
#' Always returns a SpatVector.
#'
#' @param tracks SpatVector, sf object, or character file path.
#' @param verbose Logical. Print info messages.
#' @return A SpatVector.
load_tracks <- function(tracks, verbose = TRUE) {
  if (is.character(tracks)) {
    if (verbose) message("Loading tracks: ", tracks)
    file_ext <- tolower(tools::file_ext(tracks))
    if (file_ext == "rds") {
      tracks <- readRDS(tracks)
    } else {
      tracks <- terra::vect(tracks)
    }
  }
  # Convert sf to SpatVector if needed
  if (inherits(tracks, "sf") || inherits(tracks, "sfc")) {
    tracks <- terra::vect(tracks)
  }
  stopifnot(inherits(tracks, "SpatVector"))
  tracks
}


# ---- Core analysis functions ---------------------------------------------

#' Burn tracks into a cost raster
#'
#' Rasterizes a track network onto the cost surface, setting track cells to
#' \code{target_value}. This prepares the raster for \code{terra::costDist()}.
#'
#' @param cost_raster SpatRaster. The friction/cost surface.
#' @param tracks SpatVector. The track/path network.
#' @param target_value Numeric. Value to assign to track cells (default 0).
#' @param verbose Logical. Print info messages.
#' @return A SpatRaster with track cells set to \code{target_value}.
burn_tracks <- function(cost_raster, tracks, target_value = 0, verbose = TRUE) {

  # CRS check
  raster_crs <- terra::crs(cost_raster, describe = TRUE)
  track_crs <- terra::crs(tracks, describe = TRUE)
  if (verbose) {
    message("Raster CRS: ", raster_crs$name, " (EPSG:", raster_crs$code, ")")
    message("Tracks CRS: ", track_crs$name, " (EPSG:", track_crs$code, ")")
  }
  if (raster_crs$code != track_crs$code) {
    warning(
      "CRS mismatch! Raster EPSG:", raster_crs$code,
      " vs Tracks EPSG:", track_crs$code,
      ". Results may be incorrect."
    )
  }

  # Raster info
  dims <- c(terra::nrow(cost_raster), terra::ncol(cost_raster))
  n_cells <- terra::ncell(cost_raster)
  if (verbose) {
    message(
      "Raster dimensions: ", dims[1], " rows x ", dims[2], " cols",
      " (", format(n_cells, big.mark = ","), " cells)"
    )
    message("Resolution: ", paste(terra::res(cost_raster), collapse = " x "), " m")
  }

  # Rasterize tracks
  if (verbose) message("Rasterizing tracks onto cost surface...")
  cost_with_tracks <- terra::rasterize(
    tracks,
    cost_raster,
    field = target_value,
    update = TRUE,
    touches = TRUE
  )

  # Count track cells
  n_track_cells <- sum(
    terra::values(cost_with_tracks) == target_value, na.rm = TRUE
  )
  if (verbose) {
    message(
      "Track cells rasterized: ", format(n_track_cells, big.mark = ","),
      " (", round(100 * n_track_cells / n_cells, 2), "% of raster)"
    )
  }
  if (n_track_cells == 0) {
    warning("No track cells were rasterized! Check track geometry and raster extent.")
  }

  cost_with_tracks
}


#' Run cost-distance calculation
#'
#' Thin wrapper around \code{terra::costDist()} that adds timing and memory
#' benchmarking.
#'
#' @param cost_with_tracks SpatRaster. Cost surface with track cells burned in.
#' @param target_value Numeric. Target cell value (default 0).
#' @param maxiter Numeric. Max iterations for convergence (default 50).
#' @param filename Character. Output file for out-of-memory processing.
#' @param verbose Logical. Print timing info.
#' @return A named list with \code{surface} (SpatRaster) and \code{benchmark}.
run_cost_dist <- function(cost_with_tracks, target_value = 0, maxiter = 50,
                          filename = "", verbose = TRUE, overwrite=TRUE) {
  if (verbose) message("Running costDist (maxiter=", maxiter, ")...")
  mem_before <- gc(reset = TRUE)
  timing <- system.time({
    cost_surface <- terra::costDist(
      cost_with_tracks,
      target = target_value,
      maxiter = maxiter,
      filename = filename,
      overwrite=TRUE
    )
  })
  mem_after <- gc()

  elapsed <- as.numeric(timing["elapsed"])
  peak_mem_mb <- max(mem_after[, "max used"] * c(8, 8) / 1024^2)

  if (verbose) {
    message("costDist completed in ", round(elapsed, 2), " seconds")
    message("Estimated peak memory: ", round(peak_mem_mb, 1), " MB")
  }

  dims <- c(terra::nrow(cost_with_tracks), terra::ncol(cost_with_tracks))
  benchmark <- list(
    elapsed_secs = elapsed,
    peak_mem_mb = peak_mem_mb,
    raster_dims = dims,
    n_cells = terra::ncell(cost_with_tracks)
  )

  list(surface = cost_surface, benchmark = benchmark)
}


#' Extract cost-distance values at query points
#'
#' @param cost_surface SpatRaster. The cost-distance surface.
#' @param query_points data.frame or matrix with columns x, y.
#' @param verbose Logical. Print extracted values.
#' @return Named numeric vector of cost-distance values.
extract_point_costs <- function(cost_surface, query_points, verbose = TRUE) {
  if (is.data.frame(query_points)) {
    xy_mat <- as.matrix(query_points[, c("x", "y")])
    pt_names <- rownames(query_points)
  } else if (is.matrix(query_points)) {
    xy_mat <- query_points
    pt_names <- rownames(query_points)
  } else {
    stop("query_points must be a data.frame or matrix with x, y columns")
  }

  # Check points fall within raster extent
  rast_ext <- as.vector(terra::ext(cost_surface))  # xmin, xmax, ymin, ymax
  in_extent <- xy_mat[, 1] >= rast_ext[1] & xy_mat[, 1] <= rast_ext[2] &
    xy_mat[, 2] >= rast_ext[3] & xy_mat[, 2] <= rast_ext[4]
  if (any(!in_extent)) {
    warning(
      sum(!in_extent), " query point(s) fall outside the raster extent: ",
      paste(pt_names[!in_extent], collapse = ", ")
    )
  }

  point_costs <- terra::extract(cost_surface, xy_mat)[, 1]
  if (!is.null(pt_names)) names(point_costs) <- pt_names

  if (verbose) {
    message("\nCost-distance at query points:")
    for (i in seq_along(point_costs)) {
      label <- if (!is.null(pt_names)) pt_names[i] else paste("Point", i)
      message("  ", label, ": ", round(point_costs[i], 4))
    }
  }

  point_costs
}


# ---- Orchestrator --------------------------------------------------------

#' Calculate Cost-Distance from All Cells to the Nearest Track
#'
#' Orchestrator that loads data, burns tracks, runs costDist, and optionally
#' extracts values at query points. See individual subfunctions for details.
#'
#' @param cost_raster SpatRaster or character file path to cost/friction surface.
#' @param tracks sf object, SpatVector, or character file path to tracks.
#' @param query_points Optional data.frame/matrix with x, y columns.
#' @param target_value Numeric. Value for track cells (default 0).
#' @param maxiter Numeric. Max iterations for costDist (default 50).
#' @param filename Character. Output filename for large rasters.
#' @param verbose Logical. Print progress info (default TRUE).
#'
#' @return A named list with \code{surface}, \code{cost_with_tracks},
#'   \code{point_costs}, and \code{benchmark}.
#'
#' @examples
#' \dontrun{
#' result <- calc_cost_to_tracks(
#'   cost_raster = "cost_speed/cost.tif",
#'   tracks = "cost_speed/tracks_crop.rds",
#'   query_points = data.frame(
#'     x = c(242329.4, 243198.9),
#'     y = c(6326149, 6326506),
#'     row.names = c("davisia", "banksia")
#'   )
#' )
#' print(result$point_costs)
#' print(result$benchmark)
#' }
calc_cost_to_tracks <- function(
    cost_raster,
    tracks,
    query_points = NULL,
    target_value = 0,
    maxiter = 50,
    filename = "",
    verbose = TRUE
) {

  # 1. Load inputs
  cost_raster <- load_cost_raster(cost_raster, verbose = verbose)
  tracks <- load_tracks(tracks, verbose = verbose)

  # 2. Burn tracks into cost surface
  cost_with_tracks <- burn_tracks(
    cost_raster, tracks,
    target_value = target_value, verbose = verbose
  )

  # 3. Run cost-distance
  cd_result <- run_cost_dist(
    cost_with_tracks,
    target_value = target_value, maxiter = maxiter,
    filename = filename, verbose = verbose
  )

  # 4. Extract query point values
  point_costs <- NULL
  if (!is.null(query_points)) {
    point_costs <- extract_point_costs(
      cd_result$surface, query_points, verbose = verbose
    )
  }

  list(
    surface = cd_result$surface,
    cost_with_tracks = cost_with_tracks,
    point_costs = point_costs,
    benchmark = cd_result$benchmark
  )
}


# ---- Plotting ------------------------------------------------------------

#' Plot cost raster with burned-in tracks
#'
#' Visualises the cost surface after tracks have been burned in, highlighting
#' the track cells (target_value) in a distinct colour.
#'
#' @param cost_with_tracks SpatRaster. Output from \code{burn_tracks()}. # rplace with cost_rast
#' @param tracks Optional SpatVector or sf to overlay as lines.
#' @param query_points Optional data.frame with x, y (and optionally name) cols.
#' @param title Character. Plot title.
#' @param filename Character. If provided, save plot to this file (PNG).
#' @param width Numeric. Plot width in pixels (default 1400).
#' @param height Numeric. Plot height in pixels (default 1100).
#' @param res Numeric. Plot resolution in DPI (default 150).
plot_cost_with_tracks <- function(
    cost_with_tracks,
    tracks = NULL,
    query_points = NULL,
    title = "Cost Surface with Burned-in Tracks",
    filename = "",
    width = 1400,
    height = 1100,
    res = 150
) {

  if (nchar(filename) > 0) {
    png(filename, width = width, height = height, res = res)
    on.exit(dev.off(), add = TRUE)
  }

  # Mask track cells for separate visualisation
  target_val <- min(terra::values(cost_with_tracks), na.rm = TRUE)
  is_track <- cost_with_tracks == target_val

  # Plot the cost surface (non-track cells)
  cost_masked <- cost_with_tracks
  cost_masked[is_track] <- NA
  plot(cost_masked,
       main = title,
       col = grDevices::hcl.colors(50, "YlOrRd", rev = TRUE),
       legend = TRUE)

  # Overlay track cells in blue
  track_raster <- cost_with_tracks
  track_raster[!is_track] <- NA
  plot(track_raster, col = "dodgerblue", add = TRUE, legend = FALSE)

  # Overlay track lines if provided
  if (!is.null(tracks)) {
    if (inherits(tracks, "sf") || inherits(tracks, "sfc")) {
      tracks <- terra::vect(tracks)
    }
    terra::lines(tracks, col = "black", lwd = 1.5)
  }

  # Overlay query points if provided
  if (!is.null(query_points)) {
    points(query_points$x, query_points$y,
           pch = 21, bg = "cyan", cex = 1.2, lwd = 1.5)
    if ("name" %in% names(query_points)) {
      text(query_points$x, query_points$y,
           labels = query_points$name,
           pos = 4, cex = 0.5, font = 2, offset = 0.3)
    }
  }

  invisible(NULL)
}
