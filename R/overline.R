#' Do the intersections between two geometries create lines?
#'
#' This is a function required in [overline()]. It identifies
#' whether sets of lines overlap (beyond shared points) or
#' not.
#'
#' @param g1 A spatial object
#' @param g2 A spatial object
#' @family rnet
#' @export
#' @examples
#' \dontrun{
#' rnet <- overline(routes_fast[c(2, 3, 22), ], attrib = "length")
#' plot(rnet)
#' lines(routes_fast[22, ], col = "red") # line without overlaps
#' islines(routes_fast[2, ], routes_fast[3, ])
#' islines(routes_fast[2, ], routes_fast[22, ])
#' # sf implementation
#' islines(routes_fast_sf[2, ], routes_fast_sf[3, ])
#' islines(routes_fast_sf[2, ], routes_fast_sf[22, ])
#' }
islines <- function(g1, g2) {
  UseMethod("islines")
}

islines.sf <- function(g1, g2) {
  sf::st_geometry_type(sf::st_intersection(sf::st_geometry(g1), sf::st_geometry(g2))) == "MULTILINESTRING"
}

#' Function to split overlapping SpatialLines into segments
#'
#' Divides SpatialLinesDataFrame objects into separate Lines.
#' Each new Lines object is the aggregate of a single number
#' of aggregated lines.
#'
#' @param sl SpatialLinesDataFrame with overlapping Lines to split by
#' number of overlapping features.
#' @param buff_dist A number specifying the distance in meters of the buffer to be used to crop lines before running the operation.
#' If the distance is zero (the default) touching but non-overlapping lines may be aggregated.
#' @family rnet
#' @export
#' @examples
#' sl <- routes_fast[2:4, ]
#' rsec <- gsection(sl)
#' rsec_buff <- gsection(sl, buff_dist = 1)
#' plot(sl[1], lwd = 9, col = 1:nrow(sl))
#' plot(rsec, col = 5 + (1:length(rsec)), add = TRUE, lwd = 3)
#' plot(rsec_buff, col = 5 + (1:length(rsec_buff)), add = TRUE, lwd = 3)
#' \dontrun{
#' sl <- routes_fast_sf[2:4, ]
#' rsec <- gsection(sl)
#' rsec <- gsection(sl, buff_dist = 100) # 4 features: issue
#' }
gsection <- function(sl, buff_dist = 0) {
  UseMethod("gsection")
}

#' @export
gsection.sf <- function(sl, buff_dist = 0) {
  if (buff_dist > 0) {
    sl <- geo_toptail(sl, toptail_dist = buff_dist)
  }

  u <- sf::st_union(sl)
  u_merged <- sf::st_line_merge(u)
  u_disag <- sf::st_cast(u_merged, "LINESTRING")

  u_disag
}


#' Convert series of overlapping lines into a route network
#'
#' This function takes a series of Lines stored in a
#'  `SpatialLinesDataFrame`
#' and converts these into a single route network.
#'
#' @param sl A SpatialLinesDataFrame with overlapping elements
#' @param attrib A character vector corresponding to the variables in
#' `sl$` on which the function(s) will operate.
#' @param fun The function(s) used to aggregate the grouped values (default: sum).
#' If length of `fun` is smaller than `attrib` then the functions are
#' repeated for subsequent attributes.
#' @param na.zero Sets whether aggregated values with a value of zero are removed.
#' @inheritParams gsection
#' @author Barry Rowlingson
#' @references
#' Rowlingson, B (2015). Overlaying lines and aggregating their values for
#'  overlapping segments. Reproducible question from
#'  <http://gis.stackexchange.com>. See <http://gis.stackexchange.com/questions/139681/overlaying-lines-and-aggregating-their-values-for-overlapping-segments>.
#' @family rnet
#' @export
#' @examples
#' sl <- routes_fast[2:4, ]
#' rnet1 <- overline(sl = sl, attrib = "length")
#' rnet2 <- overline(sl = sl, attrib = "length", buff_dist = 1)
#' plot(rnet1, lwd = rnet1$length / mean(rnet1$length))
#' plot(rnet2, lwd = rnet2$length / mean(rnet2$length))
#' \dontrun{
#' # sf methods
#' sl <- routes_fast_sf[2:4, ]
#' rnet_sf <- overline(sl = sl, attrib = "length", buff_dist = 10)
#' plot(rnet_sf, lwd = rnet_sf$length / mean(rnet_sf$length))
#' }
overline <- function(sl, attrib, fun = sum, na.zero = FALSE, buff_dist = 0) {
  UseMethod("overline")
}
#' @export
overline.sf <- function(sl, attrib, fun = sum, na.zero = FALSE, buff_dist = 0) {

  overline2(x = sl, attrib = attrib)

}


#' Aggregate flows so they become non-directional (by geometry - the slow way)
#'
#' Flow data often contains movement in two directions: from point A to point B
#' and then from B to A. This can be problematic for transport planning, because
#' the magnitude of flow along a route can be masked by flows the other direction.
#' If only the largest flow in either direction is captured in an analysis, for
#' example, the true extent of travel will by heavily under-estimated for
#' OD pairs which have similar amounts of travel in both directions.
#' Flows in both direction are often represented by overlapping lines with
#' identical geometries (see [flowlines()]) which can be confusing
#' for users and are difficult to plot.
#'
#' This function aggregates directional flows into non-directional flows,
#' potentially halving the number of lines objects and reducing the number
#' of overlapping lines to zero.
#'
#' @param x A SpatialLinesDataFrame
#' @param attrib A text string containing the name of the line's attribute to
#' aggregate or a numeric vector of the columns to be aggregated
#'
#' @return `onewaygeo` outputs a SpatialLinesDataFrame with single lines
#' and user-selected attribute values that have been aggregated. Only lines
#' with a distance (i.e. not intra-zone flows) are included
#' @family lines
#' @export
#' @examples
#' plot(flowlines[1:30, ], lwd = flowlines$On.foot[1:30])
#' singlines <- onewaygeo(flowlines[1:30, ], attrib = which(names(flowlines) == "On.foot"))
#' plot(singlines, lwd = singlines$On.foot / 2, col = "red", add = TRUE)
#' \dontrun{
#' plot(flowlines, lwd = flowlines$All / 10)
#' singlelines <- onewaygeo(flowlines, attrib = 3:14)
#' plot(singlelines, lwd = singlelines$All / 20, col = "red", add = TRUE)
#' sum(singlelines$All) == sum(flowlines$All)
#' nrow(singlelines)
#' singlelines_sf <- onewaygeo(flowlines_sf, attrib = 3:14)
#' sum(singlelines_sf$All) == sum(flowlines_sf$All)
#' summary(singlelines$All == singlelines_sf$All)
#' }
onewaygeo <- function(x, attrib) {
  UseMethod("onewaygeo")
}
#' @export
onewaygeo.sf <- function(x, attrib) {
  geq <- sf::st_equals(x, x, sparse = FALSE) | sf::st_equals_exact(x, x, sparse = FALSE, par = 0.0)
  sel1 <- !duplicated(geq) # repeated rows
  x$matching_rows <- apply(geq, 1, function(x) paste0(formatC(which(x), width = 4, format = "d", flag = 0), collapse = "-"))

  singlelines <- stats::aggregate(x[attrib], list(x$matching_rows), FUN = sum)

  return(singlelines)
}


#' Convert series of overlapping lines into a route network (new method)
#'
#' @description This function is intended as a replacement for overline() and is significantly faster
#' especially on large datasets. However, it also uses more memory.
#'
#' @param x An SF data.frame of LINESTRINGS
#' @param attrib character, column names in x to be summed
#' @param ncores integer, how many cores to use in parallel processing, default = 1
#' @param simplify logical, if TRUE group final segments back into lines, default = TRUE
#' @param regionalise integer, during simplification regonalisation is used if the number of segments exceeds this value
#' @family rnet
#' @author Malcolm Morgan
#' @export
#' @return
#' An SF data.frame of LINESTRINGS
#' @details
#' The overline2 function breaks each line into many straight segments and then looks for duplicated segments.
#' Attributes are summed for all duplicated segments, and if simplify is TRUE the segments with identical attributes are
#' recombined into linestrings.
#'
#' Regionalisation breaks the dataset into a 10 x 10 grid and then performed the simplification across each grid.
#' This significantly reduces computation time for large datasets, but slightly increases the final file size.
#' For smaller datasets it increases computation time slightly but reduces memory usage and so may also be useful.
#'
#' A known limitation of this method is that overlapping segments of different lengths are not aggregated.
#' This can occur when lines stop halfway down a road. Typically these errors are small,
#' but some artefacts may remain within the resulting data.
#'
#' For very large datasets nrow(x) > 1000000, memory usage can be significant. In these cases is is possible
#' to overline subsets of the dataset, rbind the results together, and then overline again, to produce
#' a final result.
#'
#' Multicore support is only enabled for the regionalised simplification stage as it does not help with other stages.
#'
#' @examples
#' sl = routes_fast_sf[routes_fast_sf$length > 0, ]
#' sl$bicycle = 1
#' system.time({rnet1 = overline2(sl, "bicycle")})
#' system.time({rnet2 = overline2(sl, "bicycle", ncores = 4)})
#' identical(rnet1, rnet2)
#' lwd = rnet1$bicycle / mean(rnet1$bicycle)
#' plot(rnet1, lwd = lwd)
#' \donttest{
#' region = "isle-of-wight"
#'
#' u = paste0(
#'   "https://github.com/npct/pct-outputs-regional-notR/raw/master/commute/msoa/",
#'    region,
#'   "/rf.geojson"
#' )
#'
#' sl = sf::read_sf(u)
#' system.time({rnet1 = overline2(sl, "bicycle")})
#' system.time({rnet2 = overline2(sl, "bicycle", ncores = 4)})
#' identical(rnet1, rnet2)
#' lwd = rnet1$bicycle / mean(rnet1$bicycle)
#' plot(rnet1, lwd = lwd)
#' }
overline2 = function(x, attrib, ncores = 1, simplify = TRUE, regionalise = 1e5){
  if(!"sfc_LINESTRING" %in%  class(sf::st_geometry(x))){
    stop("Only LINESTRING is supported")
  }
  if(any(c("1","2","3","4","grid") %in% attrib)){
    stop("1, 2, 3, 4, grid are not a permitted column names, please rename that column")
  }

  x = sf::st_zm(x)
  x = x[, attrib]
  x_crs = sf::st_crs(x)

  message(paste0(Sys.time(), " constructing segments"))
  c1 = sf::st_coordinates(x)
  sf::st_geometry(x) = NULL
  l1 = c1[, 3] # Get which line each point is part of
  c1 = c1[, 1:2]
  l1_start = duplicated(l1) # find the break points between lines
  l1_start = c(l1_start[2:length(l1)], FALSE)
  c2 = c1[2:nrow(c1), 1:2] # Create new coords offset by one row
  c2 = rbind(c2, c(NA, NA))
  c2[nrow(c1), ] = c(NA, NA)
  c2[!l1_start, 1] = NA
  c2[!l1_start, 2] = NA
  c3 = cbind(c1, c2) # make new matrix of start and end coords
  rm(c1, c2)
  c3 = c3[!is.na(c3[, 3]), ]
  x = x[l1[l1_start], , drop = FALSE] # repeate attributes
  rm(l1, l1_start)

  #message(paste0(Sys.time(), " transposing 'B to A' to 'A to B'"))
  attributes(c3)$dimnames <- NULL
  c3 <- t(apply(c3, MARGIN = 1, FUN = function(y) {
    if(y[1] != y[3]){
      if (y[1] > y[3]) {
        c(y[3], y[4], y[1], y[2])
      } else {
        y
      }
    } else{
      if (y[2] > y[4]) {
        c(y[3], y[4], y[1], y[2])
      } else {
        y
      }
    }

  }))

  #message(paste0(Sys.time(), " removing duplicates"))
  x <- cbind(c3, x)
  rm(c3)
  x <- dplyr::group_by_at(x, c("1","2","3","4"))
  x <- dplyr::summarise_all(x, .funs = sum)
  coords <- as.matrix(x[,1:4])
  x <- x[,attrib]

  # Make Geometry
  message(paste0(Sys.time(), " building geometry"))
  sf::st_geometry(x) <- sf::st_as_sfc(pbapply::pblapply(1:nrow(coords), function(y) {
    sf::st_linestring(matrix(coords[y,], ncol = 2, byrow = T))
  }), crs = x_crs)
  rm(coords)

  # Recombine into fewer lines
  if(simplify){

    message(paste0(Sys.time(), " simplifying geometry"))
    if (nrow(x) > regionalise) {
      message(paste0("large data detected, using regionalisation, nrow = ",nrow(x)))
      suppressWarnings( cents <- sf::st_centroid(x))
      grid <- sf::st_make_grid(cents, what = "polygons")
      inter <- unlist(lapply(sf::st_intersects(cents, grid), `[[`, 1))
      x$grid <- inter
      rm(cents, grid, inter)
      # split into a list of df by grid
      x = split(x , f = x$grid)
      message(paste0(Sys.time(), " regionalisation complete, aggregating flows"))
      if (ncores > 1) {
        cl = parallel::makeCluster(ncores)
        parallel::clusterExport(cl = cl,
                                varlist = c("attrib"),
                                envir = environment())
        parallel::clusterEvalQ(cl, {
          library(sf)
          #library(dplyr)
        })
        overlined_simple = pbapply::pblapply(x, function(y) {
          y <- dplyr::group_by_at(y, attrib)
          y <- dplyr::summarise(y, do_union = FALSE)
        }, cl = cl)
        parallel::stopCluster(cl)
        rm(cl)
      } else{
        overlined_simple = pbapply::pblapply(x, function(y) {
          y <- dplyr::group_by_at(y, attrib)
          y <- dplyr::summarise(y, do_union = FALSE)
        })
      }
      rm(x)
      suppressWarnings(overlined_simple <-
                         dplyr::bind_rows(overlined_simple))
      overlined_simple = as.data.frame(overlined_simple)
      overlined_simple = sf::st_sf(overlined_simple)
      sf::st_crs(overlined_simple) = x_crs
      overlined_simple$grid = NULL
    } else{
      message(paste0(Sys.time(), " aggregating flows"))
      overlined_simple <- dplyr::group_by_at(x, attrib)
      overlined_simple <- dplyr::summarise(overlined_simple, do_union = FALSE)
      rm(x)
    }

    overlined_simple <- dplyr::ungroup(overlined_simple)

    #Separate our the linestrings and the mulilinestrings
    message(paste0(Sys.time(), " rejoining segments into linestrings"))
    overlined_simple <- sf::st_line_merge(overlined_simple)
    geom_types = sf::st_geometry_type(overlined_simple)
    overlined_simple_l = overlined_simple[geom_types == "LINESTRING", ]
    overlined_simple_ml = overlined_simple[geom_types == "MULTILINESTRING", ]
    suppressWarnings(overlined_simple_ml <-
                       sf::st_cast(
                         sf::st_cast(overlined_simple_ml, "MULTILINESTRING"),
                         "LINESTRING"
                       ))

    return(rbind(overlined_simple_l, overlined_simple_ml))
  }else{
    return(x)
  }

}


