#' Extract coordinates from OD data
#'
#' @details
#' Origin-destination (OD) data is often provided
#' in the form of 1 line per OD pair, with zone codes of the trip origin in the first
#' column and the zone codes of the destination in the second column
#' (see the [`vignette("stplanr-od")`](https://docs.ropensci.org/stplanr/articles/stplanr-od.html)) for details.
#' `od2odf()` creates an 'origin-destination data frame', based on a data frame containing
#' origin and destination cones (`flow`) that match the first column in a
#' a spatial (polygon or point) object (`zones`).
#'
#' The function returns a data frame with coordinates for the origin and destination.
#' @inheritParams od2line
#' @family od
#' @export
#' @examples
#' data(flow)
#' data(zones)
#' od2odf(flow[1:2, ], zones)
od2odf <- function(flow, zones) {
  coords <- dplyr::data_frame(
    code = as.character(zones[[1]]),
    fx = coordinates(zones)[, 1], fy = coordinates(zones)[, 2]
  )
  flowcode <- dplyr::data_frame(code_o = as.character(flow[[1]]), code_d = as.character(flow[[2]]))
  odf <- dplyr::left_join(flowcode, coords, by = c("code_o" = "code"))
  coords <- dplyr::rename_(coords, tx = quote(fx), ty = quote(fy))
  odf <- dplyr::left_join(odf, coords, by = c("code_d" = "code"))

  data.frame(odf) # return data.frame as more compatible with spatial data
}

#' Create matrices representing origin-destination coordinates
#'
#' This function takes a wide range of input data types (spatial lines, points or text strings)
#' and returns a matrix of coordinates representing origin (fx, fy) and destination (tx, ty) points.
#'
#' @param from An object representing origins
#' (if lines are provided as the first argument, from is assigned to `l`)
#' @param to An object representing destinations
#' @param l Only needed if from and to are empty, in which case this
#' should be a spatial object representing desire lines
#' @family od
#' @export
#' @examples
#' od_coords(from = c(0, 52), to = c(1, 53)) # lon/lat coordinates
#' od_coords(from = cents[1, ], to = cents[2, ]) # Spatial points
#' od_coords(cents_sf[1:3, ], cents_sf[2:4, ]) # sf points
#' # od_coords("Hereford", "Leeds") # geocode locations
#' od_coords(flowlines[1:3, ])
#' od_coords(flowlines_sf[1:3, ])
od_coords <- function(from = NULL, to = NULL, l = NULL) {

  if (is(object = from, class2 = "sf")) {
    is_sf_line <- all(sf::st_geometry_type(from) == "LINESTRING")
  } else {
    is_sf_line <- FALSE
  }

  if (is_sf_line | any(grepl(pattern = "Line", x = class(from)))) {
    l <- from
  }

  if (!is.null(l)) {
    coord_matrix <- line2df(l) %>%
      dplyr::select("fx", "fy", "tx", "ty")
  }

  else {
    # sf objects
    if (is(object = from, "sf") | is(object = from, "sfc")) from <- sf::st_coordinates(from)
    if (is(object = to, "sf") | is(object = to, "sfc")) to <- sf::st_coordinates(to)

    # Convert character strings to lon/lat if needs be
    if (is.character(from)) from <- matrix(geo_code(from), ncol = 2)
    if (is.character(to)) to <- matrix(geo_code(to), ncol = 2)
    if (is.vector(from) & is.vector(to)) {
      coord_matrix <- matrix(c(from, to), ncol = 4)
    } else {
      coord_matrix <- cbind(from, to)
    }
    colnames(coord_matrix) <- c("fx", "fy", "tx", "ty")
  }

  as.matrix(coord_matrix)

}

#' Convert origin-destination coordinates into desire lines
#'
#' @param odc A data frame or matrix representing the coordinates
#' of origin-destination data. The first two columns represent the
#' coordinates of the origin (typically longitude and latitude) points;
#' the third and fourth columns represent the coordinates of the destination
#' (in the same CRS). Each row represents travel from origin to destination.
#' @param crs A number representing the coordinate reference system
#' of the result, 4326 by default.
#' @param remove_duplicates Should rows with duplicated rows be removed? `TRUE` by default.
#' @family od
#' @export
#' @examples
#' odf <- od_coords(l = flowlines_sf)
#' odlines <- od_coords2line(odf)
#' odlines <- od_coords2line(odf, crs = 4326)
#' plot(odlines)
#' x_coords = 1:3
#' n = 50
#' d = data.frame(lapply(1:4, function(x) sample(x_coords, n, replace = TRUE)))
#' names(d) = c("fx", "fy", "tx", "ty")
#' l = od_coords2line(d)
#' plot(l)
#' nrow(l)
#' l_with_duplicates = od_coords2line(d, remove_duplicates = FALSE)
#' plot(l_with_duplicates)
#' nrow(l_with_duplicates)
od_coords2line <- function(odc, crs = 4326, remove_duplicates = TRUE) {
  # check for illegal NAs in coordinates
  odm_check(odc)
  odc_unique <- odc[!duplicated(odc[, 1:4, drop = FALSE]), , drop = FALSE]
  if(nrow(odc_unique) < nrow(odc) && remove_duplicates) {
    message("Duplicate OD pairs identified, removing ", nrow(odc) - nrow(odc_unique), " rows")
    odc <- odc_unique
    odc_unique$n = dplyr::group_size(dplyr::group_by_all(as.data.frame(odc[, 1:4])))
  }
  odm <- as.matrix(odc)
  linestring_list <- lapply(seq(nrow(odm)), function(i) {
    sf::st_linestring(rbind(odm[i, 1:2], odm[i, 3:4]))
  })
  sf::st_sf(odc, geometry = sf::st_sfc(linestring_list, crs = crs))
}
#' Convert origin-destination data to spatial lines
#'
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This can be tricky to plot and link-up with geographical data.
#' This function makes the task easier.
#'
#' @details
#' Origin-destination (OD) data is often provided
#' in the form of 1 line per OD pair, with zone codes of the trip origin in the first
#' column and the zone codes of the destination in the second column
#' (see the [`vignette("stplanr-od")`](https://docs.ropensci.org/stplanr/articles/stplanr-od.html)) for details.
#' `od2line()` creates a spatial (linestring) object representing movement from the origin
#' to the destination for each OD pair.
#' It takes data frame containing
#' origin and destination cones (`flow`) that match the first column in a
#' a spatial (polygon or point) object (`zones`).
#'
#' @param flow A data frame representing origin-destination data.
#'  The first two columns of this data frame should correspond
#' to the first column of the data in the zones. Thus in [cents()],
#' the first column is geo_code. This corresponds to the first two columns
#' of [flow()].
#' @param zones A spatial object representing origins (and destinations
#' if no separate destinations object is provided) of travel.
#' @param destinations A spatial object
#' representing destinations of travel flows.
#' @param zone_code Name of the variable in `zones` containing the ids of the zone.
#' By default this is the first column names in the zones.
#' @param origin_code Name of the variable in `flow` containing the ids of the zone of origin.
#' By default this is the first column name in the flow input dataset.
#' @param dest_code Name of the variable in `flow` containing the ids of the zone of destination.
#' By default this is the second column name in the flow input dataset or the first column name in the
#' destinations if that is set.
#' @param zone_code_d Name of the variable in `destinations` containing the ids of the zone.
#' By default this is the first column names in the destinations.
#' @param silent TRUE by default, setting it to TRUE will show you the matching columns
#' @family od
#' @export
#' @examples
#' od_data <- stplanr::flow[1:20, ]
#' l <- od2line(flow = od_data, zones = cents_sf)
#' plot(sf::st_geometry(cents_sf))
#' plot(l, lwd = l$All / mean(l$All), add = TRUE)
#' l <- od2line(flow = od_data, zones = cents)
#' # When destinations are different
#' head(destinations[1:5])
#' od_data2 <- flow_dests[1:12, 1:3]
#' od_data2
#' flowlines_dests <- od2line(od_data2, cents_sf, destinations = destinations_sf)
#' flowlines_dests
#' plot(flowlines_dests)
#' @name od2line
NULL

#' @rdname od2line
#' @export
od2line <- function(flow, zones, destinations = NULL,
                    zone_code = names(zones)[1],
                    origin_code = names(flow)[1],
                    dest_code = names(flow)[2],
                    zone_code_d = NA, silent = FALSE) {
  UseMethod("od2line", object = zones)
}
#' @export
od2line.sf <- function(flow, zones, destinations = NULL,
                       zone_code = names(zones)[1],
                       origin_code = names(flow)[1],
                       dest_code = names(flow)[2],
                       zone_code_d = NA, silent = TRUE) {
  if (grepl(pattern = "POLYGON", x = unique(sf::st_geometry_type(zones)))) {
    message("Creating centroids representing desire line start and end points.")
    suppressWarnings(zones <- sf::st_centroid(zones))
  }
  coords_o <- sf::st_coordinates(zones)[, 1:2]
  origin_matches <- match(flow[[origin_code]], zones[[zone_code]])

  # Check matches, provide message
  od_matches_check(origin_matches, flow[[origin_code]])
  origin_points <- coords_o[origin_matches, ]

  if (is.null(destinations)) {
    if (!silent) {
      message(paste(
        "Matching", zone_code, "in the zones to", origin_code, "and", dest_code,
        "for origins and destinations respectively"
      ))
    }
    dest_matches <- match(flow[[dest_code]], zones[[zone_code]])
    od_matches_check(dest_matches, flow[[dest_code]], type = "destination")
    dest_points <- coords_o[dest_matches, ]

  } else {
    if(is.na(zone_code_d)) {
      zone_code_d <- names(destinations)[1]
    }
    coords_d <- sf::st_coordinates(destinations)[, 1:2]
    dest_points <- coords_d[match(flow[[dest_code]], destinations[[zone_code_d]]), ]
  }

  odm = cbind(origin_points, dest_points)

  odsfc <- od_coords2line(odm, crs = sf::st_crs(zones), remove_duplicates = FALSE)
  sf::st_sf(flow, geometry = odsfc$geometry)

}


#' Convert geographic line objects to a data.frame with from and to coords
#'
#' This function returns a data frame with fx and fy and tx and ty variables
#' representing the beginning and end points of spatial line features respectively.
#'
#' @param l A spatial lines object
#' @family lines
#' @export
line2df <- function(l) {
  UseMethod("line2df")
}
#' @export
line2df.sf <- function(l) {
  X <- rlang::quo(X)
  Y <- rlang::quo(Y)
  L1 <- rlang::quo(L1)

  ldf_geom <- sf::st_coordinates(l)
  dplyr::group_by(dplyr::as_data_frame(ldf_geom), !!L1) %>%
    dplyr::summarise(
      fx = dplyr::first(!!X), fy = dplyr::first(!!Y),
      tx = dplyr::last(!!X), ty = dplyr::last(!!Y)
    )
}

#' Convert a spatial (linestring) object to points
#'
#' The number of points will be double the number of lines with `line2points`. A
#' closely related function, `line2pointsn` returns all the points that were
#' line vertices. #' The points corresponding with a given line, `i`, will be
#' `(2*i):((2*i)+1)`. The last function, `line2vertices`, returns all the points
#' that are vertices but not nodes.
#'
#' @param l An `sf` object or a `SpatialLinesDataFrame` from the older `sp` package
#' @param ids Vector of ids (by default `1:nrow(l)`)
#' @family lines
#' @export
#' @examples
#' l <- routes_fast_sf[2:4, ]
#' lpoints <- line2points(l)
#' lpoints2 <- line2pointsn(l)
#' plot(sf::st_geometry(lpoints), pch = lpoints$id, cex = lpoints$id, col = "black")
#' plot(lpoints2$geometry, add = TRUE)
#' # in sp data forms (may be depreciated)
#' l <- routes_fast[2:4, ]
#' lpoints <- line2points(l)
#' lpoints2 <- line2pointsn(l)
#' plot(lpoints, pch = lpoints$id, cex = lpoints$id)
#' points(lpoints2)
#' @export
line2points <- function(l, ids = rep(1:nrow(l))) {
  UseMethod("line2points")
}

#' @export
line2points.sf <- function(l, ids = rep(1:nrow(l), each = 2)) {
  y_coords <- x_coords <- double(length = length(ids)) # initiate coords
  d_indices <- 1:nrow(l) * 2
  o_indices <- d_indices - 1
  x_coords[o_indices] <- sapply(l$geometry, `[[`, 1) # first (x) element of each line
  x_coords[d_indices] <- sapply(l$geometry, function(x) x[length(x) / 2]) # last (x) element of each line
  y_coords[o_indices] <- sapply(l$geometry, function(x) x[length(x) / 2 + 1]) # first (y) element of each line
  y_coords[d_indices] <- sapply(l$geometry, tail, n = 1) # last (y) element of each line
  p_multi <- sf::st_multipoint(cbind(x_coords, y_coords))
  p <- sf::st_cast(sf::st_sfc(p_multi), "POINT")
  sf::st_sf(data.frame(id = ids), geometry = p, crs = sf::st_crs(l))
}

#' @rdname line2points
#' @export
line2pointsn <- function(l) {
  UseMethod("line2pointsn")
}

#' @export
line2pointsn.sf <- function(l) {
  suppressWarnings(sf::st_cast(l, "POINT"))
}

#' @rdname line2points
#' @export
line2vertices <- function(l) {
  UseMethod("line2vertices")
}
#' @export
line2vertices.sf <- function(l) {
  all_vertexes <- sf::st_coordinates(l)
  indexes_of_internal_vertexes <- lapply(
    split(1:nrow(all_vertexes), all_vertexes[, "L1"]),
    function(x) x[-c(1, length(x))] # exclude starting and ending point
  )
  # extract those indexes
  internal_vertexes <- all_vertexes[do.call("c", indexes_of_internal_vertexes), ]

  # transform back to sf
  internal_vertexes_sf <- sf::st_as_sf(data.frame(internal_vertexes),
    coords = c("X", "Y"), crs = sf::st_crs(l)
  )
  internal_vertexes_sf
}



#' Convert a series of points into a dataframe of origins and destinations
#'
#' Takes a series of geographical points and converts them into a data.frame
#' representing the potential flows, or 'spatial interaction', between every combination
#' of points.
#'
#' @param p A spatial points object
#' @family od
#' @export
points2odf <- function(p) {
  UseMethod("points2odf")
}
#' @export
points2odf.sf <- function(p) {
  odf <- data.frame(
    expand.grid(p[[1]], p[[1]])[2:1]
  )
  names(odf) <- c("O", "D")
  odf
}

#' Convert a series of points into geographical flows
#'
#' Takes a series of geographical points and converts them into a spatial (linestring) object
#' representing the potential flows, or 'spatial interaction', between every combination
#' of points.
#'
#' @param p A spatial (point) object
#' @family od
#'
#' @export
#' @examples
#' data(cents)
#' plot(cents)
#' flow <- points2flow(cents)
#' plot(flow, add = TRUE)
#' flow_sf <- points2flow(cents_sf)
#' plot(flow_sf)
points2flow <- function(p) {
  odf <- points2odf(p)
  od2line(flow = odf, zones = p)
}

#' Update line geometry
#'
#' Take two SpatialLines objects and update the geometry of the former with that of the latter,
#' retaining the data of the former.
#'
#' @param l A SpatialLines object, whose geometry is to be modified
#' @param nl A SpatialLines object of the same length as `l` to provide the new geometry
#' @family lines
#'
#' @export
#' @examples
#' data(flowlines)
#' l <- flowlines[2:5, ]
#' nl <- routes_fast
#' nrow(l)
#' nrow(nl)
#' l <- l[!is_linepoint(l), ]
#' names(l)
#' names(routes_fast)
#' l_newgeom <- update_line_geometry(l, nl)
#' plot(l, lwd = l$All / mean(l$All))
#' plot(l_newgeom, lwd = l$All / mean(l$All))
#' names(l_newgeom)
update_line_geometry <- function(l, nl) {
  for (i in 1:nrow(l)) {
    l@lines[[i]] <- Lines(nl@lines[[i]]@Lines, row.names(l[i, ]))
  }
  l
}

#' Quickly calculate Euclidean distances of od pairs
#'
#' It is common to want to know the Euclidean distance between origins and destinations
#' in OD data. You can calculate this by first converting OD data to SpatialLines data,
#' e.g. with [od2line()]. However this can be slow and overkill if you just
#' want to know the distance. This function is a few orders of magnitude faster.
#'
#' Note: this function assumes that the zones or centroids in `cents` have a geographic
#' (lat/lon) CRS.
#'
#' @inheritParams od2line
#' @family od
#' @export
#' @examples
#' data(flow)
#' data(cents)
#' od_dist(flow, cents)
od_dist <- function(flow, zones) {
  omatch <- match(flow[[1]], zones@data[[1]])
  dmatch <- match(flow[[2]], zones@data[[1]])
  cents_o <- zones@coords[omatch, ]
  cents_d <- zones@coords[dmatch, ]
  geosphere::distHaversine(p1 = cents_o, p2 = cents_d)
}

#' Convert a series of points, or a matrix of coordinates, into a line
#'
#' This is a simple wrapper around [spLines()] that makes the creation of
#' `SpatialLines` objects easy and intuitive
#'
#' @param p A spatial (points) obect or matrix representing the coordinates of points.
#' @family lines
#' @export
#' @examples
#' p <- matrix(1:4, ncol = 2)
#' library(sp)
#' l <- points2line(p)
#' plot(l)
#' l <- points2line(cents)
#' plot(l)
#' p <- line2points(routes_fast)
#' l <- points2line(p)
#' plot(l)
#' l_sf <- points2line(cents_sf)
#' plot(l_sf)
points2line <- function(p) {
  UseMethod("points2line")
}
#' @export
points2line.sf <- function(p) {
  points2flow(p = p)
}


#' Summary statistics of trips originating from zones in OD data
#'
#' This function takes a data frame of OD data and
#' returns a data frame reporting summary statistics for each unique zone of origin.
#'
#' It has some default settings: the default summary statistic is `sum()` and the
#' first column in the OD data is assumed to represent the zone of origin.
#' By default, if `attrib` is not set, it summarises all numeric columns.
#'
#' @inheritParams od2odf
#' @inheritParams overline
#' @param FUN A function to summarise OD data by
#' @param col The column that the OD dataset is grouped by
#' (1 by default, the first column usually represents the origin)
#' @param ... Additional arguments passed to `FUN`
#' @family od
#' @export
#' @examples
#' od_aggregate_from(flow)
od_aggregate_from <- function(flow, attrib = NULL, FUN = sum, ..., col = 1) {
  if(is.character(attrib)) {
    attrib_lgl <- grepl(pattern = attrib, x = names(flow))
    if(sum(attrib_lgl) == 0){
      stop("No columns match the attribute ", attrib)
    }
    attrib = which(attrib_lgl)
  }
  if(!is.null(attrib)) {
    flow <- flow[attrib]
  }
  flow_grouped <- dplyr::group_by_at(flow, col)
  dplyr::summarise_if(flow_grouped, is.numeric, .funs = FUN, ...)
}
#' Summary statistics of trips arriving at destination zones in OD data
#'
#' This function takes a data frame of OD data and
#' returns a data frame reporting summary statistics for each unique zone of destination.
#'
#' It has some default settings: it assumes the destination ID column is the 2nd
#' and the default summary statistic is `sum()`.
#' By default, if `attrib` is not set, it summarises all numeric columns.
#'
#' @inheritParams od_aggregate_from
#' @family od
#' @export
#' @examples
#' od_aggregate_to(flow)
od_aggregate_to <- function(flow, attrib = NULL, FUN = sum, ..., col = 2) {
  if(is.character(attrib)) {
    attrib_lgl <- grepl(pattern = attrib, x = names(flow))
    if(sum(attrib_lgl) == 0){
      stop("No columns match the attribute ", attrib)
    }
    attrib = which(attrib_lgl)
  }
  if(!is.null(attrib)) {
    flow <- flow[attrib]
  }
  flow_grouped <- dplyr::group_by_at(flow, col)
  dplyr::summarise_if(flow_grouped, is.numeric, .funs = FUN, ...)
}

#' Convert origin-destination data from long to wide format
#'
#' This function takes a data frame representing travel between origins
#' (with origin codes in `name_orig`, typically the 1st column)
#' and destinations
#' (with destination codes in `name_dest`, typically the second column) and returns a matrix
#' with cell values (from `attrib`, the third column by default) representing travel between
#' origins and destinations.
#'
#' @param flow A data frame representing flows between origin and destinations
#' @param attrib A number or character string representing the column containing the attribute data
#' of interest from the `flow` data frame
#' @param name_orig A number or character string representing the zone of origin
#' @param name_dest A number or character string representing the zone of destination
#' @family od
#' @export
#' @examples
#' od_to_odmatrix(flow)
#' od_to_odmatrix(flow[1:9, ])
#' od_to_odmatrix(flow[1:9, ], attrib = "Bicycle")
od_to_odmatrix <- function(flow, attrib = 3, name_orig = 1, name_dest = 2) {
  out <- matrix(
    nrow = length(unique(flow[[name_orig]])),
    ncol = length(unique(flow[[name_dest]])),
    dimnames = list(unique(flow[[name_orig]]), unique(flow[[name_dest]]))
  )
  out[cbind(flow[[name_orig]], flow[[name_dest]])] <- flow[[attrib]]
  out
}

#' Convert origin-destination data from wide to long format
#'
#' This function takes a matrix representing travel between origins
#' (with origin codes in the `rownames` of the matrix)
#' and destinations
#' (with destination codes in the `colnames` of the matrix)
#' and returns a data frame representing origin-destination pairs.
#'
#' The function returns a data frame with rows ordered by origin and then destination
#' zone code values and with names `orig`, `dest` and `flow`.
#'
#' @param odmatrix A matrix with row and columns representing origin and destination zone codes
#' and cells representing the flow between these zones.
#' @family od
#' @export
#' @examples
#' odmatrix <- od_to_odmatrix(flow)
#' odmatrix_to_od(odmatrix)
#' flow[1:9, 1:3]
#' odmatrix_to_od(od_to_odmatrix(flow[1:9, 1:3]))
odmatrix_to_od <- function(odmatrix) {
  od <- as.data.frame(as.table(odmatrix))
  names(od) <- c("orig", "dest", "flow")
  od <- stats::na.omit(od)
  od[order(paste0(od$orig, od$dest)), ]
}

# Check for NAs in matrix
odm_check <- function(odc) {
  if(any(is.na(odc[, 1:2]))) {
    na_row <- which(is.na(odc[, 1]) | is.na(odc[, 1]))
    stop("NAs detected in the origin coordinates on row number ", na_row, call. = FALSE)
  }
  if(any(is.na(odc[, 3:4]))) {
    na_row <- which(is.na(odc[, 3]) | is.na(odc[, 4]))
    stop("NAs detected in the origin coordinates on row number ", na_row, call. = FALSE)
  }
}

# Check for NAs in od matching
od_matches_check <- function(origin_matches, origin_codes, type = "origin") {
  if(anyNA(origin_matches)) {
    n_failing <- sum(is.na(origin_matches))
    first_offending_row <- which(is.na(origin_matches))[1]
    stop(call. = FALSE,
      n_failing, " non matching IDs in the ", type, ". ",
      "ID on row ",
      first_offending_row,
      " does not match any zone.\n",
      "The first offending id was ", origin_codes[first_offending_row])
  }
}
