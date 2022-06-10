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

#' @export
fit_square <- function(x) {

  bb <- sf::st_bbox(x)
  ht <- (bb$ymax - bb$ymin)
  wd <- (bb$xmax - bb$xmin)

  if (ht > wd) {
    # "portrait"
    dif <- ht - wd
    bb2 <- c(bb$xmin - dif/2, bb$ymin, bb$xmax + dif/2, bb$ymax)
  } else {
    # "landscape"
    dif <- wd - ht
    bb2 <- c(bb$xmin, bb$ymin - dif/2, bb$xmax, bb$ymax + dif/2)
  }

  myrmidon::bb_to_poly(bb2, crs = sf::st_crs(x))
}
