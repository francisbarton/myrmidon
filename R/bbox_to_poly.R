#' Convert a bbox to an sf geospatial polygon (rectangle)
#'
#' @param x bbox - a named integer vector
#' @param crs Coordinate Reference System eg EPSG code
#'
#' @return an sf polygon object (sfc)
#' @export
bbox_to_poly <- function(x, crs) {
  xmin <- x[["xmin"]]
  ymin <- x[["ymin"]]
  xmax <- x[["xmax"]]
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

#' Create a square box around a polygon
#'
#' @param x A polygon
#' @param return Whether to return just a bbox, or an sf polygon
#' @export
fit_square <- function(x, return = c("bbox", "polygon")) {

  bb <- sf::st_bbox(x)
  ht <- (bb[["ymax"]] - bb[["ymin"]])
  wd <- (bb[["xmax"]] - bb[["xmin"]])

  if (ht > wd) {
    # "portrait"
    dif <- ht - wd
    bb2 <- c(
      xmin = bb[["xmin"]] - dif/2,
      ymin = bb[["ymin"]],
      xmax = bb[["xmax"]] + dif/2,
      ymax = bb[["ymax"]])
  } else {
    # "landscape"
    dif <- wd - ht
    bb2 <- c(
      xmin = bb[["xmin"]],
      ymin = bb[["ymin"]] - dif/2,
      xmax = bb[["xmax"]],
      ymax = bb[["ymax"]]  + dif/2)
  }

  # return
  if (return == "bbox") bb2
  if (return == "polygon") bbox_to_poly(bb2, crs = sf::st_crs(x))
}
