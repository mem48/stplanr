#' Aggregate OD data between polygon geometries
#'
#' @section Details:
#' Origin-destination ('OD') flow data is often provided
#' in the form of 1 line per flow with zone codes of origin and destination
#' centroids. This function aggregates OD flows between polygon geometries
#' allocating the original flows to larger zones based on area.
#' @inheritParams od2line
#' @param aggzones A SpatialPolygonsDataFrame containing the new
#' boundaries to aggregate to.
#' @param aggzone_points Points representing origins of OD flows
#' (typically population-weighted centroids)
#' @param cols A character vector containing the names of columns on which to
#' apply FUN. By default, all numeric columns are aggregated.
#' @param aggcols A character vector containing the names of columns in
#' aggzones to retain in the aggregated data.frame. By default,
#' only the first column is retained. These columns are renamed with a prefix
#' of "o_" and "d_".
#' @param FUN Function to use on aggregation. Default is sum.
#' @return data.frame containing the aggregated od flows.
#' @family od
#' @export
od_aggregate <- function(flow, zones, aggzones,
                         aggzone_points = NULL,
                         cols = FALSE,
                         aggcols = FALSE,
                         FUN = sum,
                         prop_by_area = ifelse(identical(FUN, mean) == FALSE, TRUE, FALSE),
                         digits = getOption("digits")) {
  UseMethod("od_aggregate", zones)
}
#' @export
od_aggregate.sf <- function(flow, zones, aggzones,
                            aggzone_points = NULL,
                            cols = FALSE,
                            aggcols = FALSE,
                            FUN = sum,
                            prop_by_area = ifelse(identical(FUN, mean) == FALSE, TRUE, FALSE),
                            digits = getOption("digits")) {
  flow_first_col <- colnames(flow)[1]
  flow_second_col <- colnames(flow)[2]
  zonesfirstcol <- colnames(zones)[1]
  aggzonesfirstcol <- colnames(aggzones)[1]

  if (identical(cols, FALSE)) {
    col_ids <- sapply(flow, is.numeric)
    cols <- names(col_ids)[col_ids]
  }
  if (aggcols == FALSE) {
    aggcols <- colnames(aggzones)[1]
  }

  zone_points <- sf::st_centroid(zones)
  if (is.null(aggzone_points)) {
    aggzone_points <- sf::st_centroid(aggzones)
  }

  zones_agg <- zone_points %>%
    sf::st_join(y = aggzones[aggcols]) %>%
    sf::st_set_geometry(NULL)

  names(zones_agg)[1] <- flow_first_col
  zones_agg$new_orig <- zones_agg[, aggcols[1]]
  zones_agg$new_dest <- zones_agg[, aggcols[1]]

  flow_new_orig <- flow %>%
    dplyr::inner_join(y = zones_agg[c(flow_first_col, "new_orig")])

  names(zones_agg)[1] <- flow_second_col

  flow_new_dest <- flow_new_orig %>%
    dplyr::inner_join(y = zones_agg[c(flow_second_col, "new_dest")])

  flow_ag <- flow_new_dest %>%
    dplyr::group_by(!!rlang::sym("new_orig"), !!rlang::sym("new_dest")) %>%
    dplyr::summarise_at(.vars = cols, .funs = sum) %>%
    dplyr::ungroup()

  flow_ag

  # od2line(flow = flow_ag, zones = aggzones) # to export as sf
}

