#' Convert a bbox to an sf geospatial polygon (rectangle)
#'
#' @param x bbox - a named integer vector
#' @param crs Coordinate Reference System eg EPSG code
#'
#' @return an sf polygon object (sfc)
#' @export
bb_to_poly <- function(x, crs) {
  xmin <- x[["xmin"]]
  xmax <- x[["xmax"]]
  ymin <- x[["ymin"]]
  ymax <- x[["ymax"]]

  rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)) %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = crs)
}
