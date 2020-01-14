#' Write to geojson easily
#'
#' Provides a user-friendly wrapper for `sf::st_write()`. Note,
#' `geojson_write` from the geojsonio package
#' provides the same functionality <https://github.com/ropensci/geojsonio>.
#'
#' @param filename File name of the output geojson
writeGeoJSON <- function(shp, filename) {
  name <- nm <- deparse(substitute(shp))
  newname <- paste0(filename, ".geojson")
  sf::st_write(sf::st_as_sf(shp), newname)
}

#' Simplify geometry of spatial objects with the mapshaper library
#'
#' @section Details:
#'
#' Note: more advance R/mapshaper tools are provided by the rmapshaper
#' package: <https://github.com/ateucher/rmapshaper>.
#'
#' Calls the JavaScript command-line GIS application mapshaper
#' (<https://github.com/mbloch/mapshaper>) from the system
#' to simplify geographic features, and then tidies up.
#' mapshaper must be installed and available to [system()].
#' `mapshape` writes new a file to disk.
#' Thanks to Richard and Adrian Ellison for demonstrating this in R.
#'
#' @param shp A spatial object to be simplified.
#' @param percent A number between 1 and 100 stating how aggressively to simplify
#'  the object (1 is a very aggressive simplification)
#' @param ms_options Text string of options passed to mapshaper such as
#' @param dsn The name of the temporary file to write to (deleted after use)
#' @param silent Logical determining whether the function call is printed to screen
#' `no-topology` (a flag) and `snap-interval=1` (a key value pair).
#' See the mapshaper documentation for details:
#' <https://github.com/mbloch/mapshaper/wiki/Command-Reference>.
#'
#' The percent argument refers to the percentage of removable points to retain.
#' So `percent = 1` is a very aggressive simplication, saving a huge amount of
#' hard-disk space.
#' @family geo
#' @export
#' @examples
#' \dontrun{
#' shp <- routes_fast[2, ]
#' plot(shp)
#' rfs10 <- mapshape(shp)
#' rfs5 <- mapshape(shp, percent = 5)
#' rfs1 <- mapshape(shp, percent = 1)
#' plot(rfs10, add = TRUE, col = "red")
#' plot(rfs5, add = TRUE, col = "blue")
#' plot(rfs1, add = TRUE, col = "grey")
#' # snap the lines to the nearest interval
#' rfs_int <- mapshape(shp, ms_options = "snap-interval=0.001")
#' plot(shp)
#' plot(rfs_int, add = TRUE)
#' mapshape(routes_fast_sf[2, ])
#' }
mapshape <- function(shp, percent = 10, ms_options = "", dsn = "mapshape", silent = FALSE) {
  shp_filename <- paste0(dsn, ".shp")
  new_filename <- paste0(dsn, "-ms.shp")
  if (!mapshape_available()) stop("mapshaper not available on this system")
  is_sp <- is(shp, "Spatial")
  if (is_sp) {
    shp <- sf::st_as_sf(shp)
  }
  sf::write_sf(shp, shp_filename, delete_layer = TRUE)
  cmd <- paste0("mapshaper ", ms_options, " ", shp_filename, " -simplify ", percent, "% -o ", new_filename)
  if (!silent) print(paste0("Running the following command from the system: ", cmd))
  system(cmd, ignore.stderr = TRUE)
  new_shp <- sf::st_read(paste0(dsn, "-ms.shp"))
  sf::st_crs(new_shp) <- sf::st_crs(shp)
  to_remove <- list.files(pattern = dsn)
  file.remove(to_remove)
  if (is_sp) {
    new_shp <- as(new_shp, "Spatial")
  }
  new_shp
}

#' Does the computer have mapshaper available?
#'
#' This helper function for [mapshape()]
#' determines whether or not the JavaScript library
#' mapshaper is available.
#'
#' @family geo
#' @export
#' @examples
#' mapshape_available()
mapshape_available <- function() {
  suppressWarnings(system("mapshaper --version")) != 127
}


#' Scale a bounding box
#'
#' Takes a bounding box as an input and outputs a bounding box of a different size, centred at the same point.
#'
#' @param scale_factor Numeric vector determining how much the bounding box will grow or shrink.
#' Two numbers refer to extending the bounding box in x and y dimensions, respectively.
#' If the value is 1, the output size will be the same as the input.
#' @family geo
#' @export
#' @examples
#' bb <- matrix(c(-1.55, 53.80, -1.50, 53.83), nrow = 2)
#' bb1 <- bbox_scale(bb, scale_factor = 1.05)
#' bb2 <- bbox_scale(bb, scale_factor = c(2, 1.05))
#' bb3 <- bbox_scale(bb, 0.1)
#' plot(x = bb2[1, ], y = bb2[2, ])
#' points(bb1[1, ], bb1[2, ])
#' points(bb3[1, ], bb3[2, ])
#' points(bb[1, ], bb[2, ], col = "red")
bbox_scale <- function(bb, scale_factor) {
  if (length(scale_factor == 1)) scale_factor <- rep(scale_factor, 2)
  b <- (bb - rowMeans(bb)) * scale_factor + rowMeans(bb)
  b
}



#' Create matrix representing the spatial bounds of an object
#'
#' Converts a range of spatial data formats into a matrix representing the bounding box
#'
#' @family geo
#' @export
geo_bb_matrix <- function(shp) {
  UseMethod("geo_bb_matrix")
}

#' @export
geo_bb_matrix.sf <- function(shp) {
  bb <- sf::st_bbox(shp)
  bb <- matrix(bb, ncol = 2)
  bb
}
#' @export
geo_bb_matrix.numeric <- function(shp) {
  matrix(rep(shp, 2), ncol = 2)
}
#' @export
geo_bb_matrix.matrix <- function(shp) {
  range_x <- range(shp[, 1])
  range_y <- range(shp[, 2])
  matrix(c(range_x, range_y), ncol = 2, byrow = TRUE)
}
