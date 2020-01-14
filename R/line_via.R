#' Convert 2 matrices to lines
#'
#' @param mat1 Matrix representing origins
#' @param mat2 Matrix representing destinations
#' @family lines
#'
#' @export
#' @examples {
#' m1 <- matrix(c(1, 2, 1, 2), ncol = 2)
#' m2 <- matrix(c(9, 9, 9, 1), ncol = 2)
#' l <- mats2line(m1, m2)
#' class(l)
#' lsf <- sf::st_sf(l, crs = 4326)
#' class(lsf)
#' plot(lsf)
#' # mapview::mapview(lsf)
#' }
mats2line <- function(mat1, mat2) {
  l <- lapply(1:nrow(mat1), function(i) {
    mat_combined <- rbind(mat1[i, ], mat2[i, ])
    sf::st_linestring(mat_combined)
  })
  sf::st_sfc(l)
}

