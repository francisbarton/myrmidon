#' Convert a bbox to an `sf` polygon
#'
#' @param x bbox. A named integer vector
#' @param crs numeric. A Coordinate Reference System, eg EPSG, code
#'
#' @returns an `sf` (sfc) polygon
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
    c(xmin, ymin)) |>
    list() |>
    sf::st_polygon() |>
    sf::st_sfc(crs = crs)
}


#' Create an expanded or contracted `sf` rectangular polygon based on an object
#'
#' Note that the factor is applied to the coordinates in the bbox, and there is
#' no correction for real world distance. In projected geometries, for example
#' with geographical coordinates, a horizontal (longitudinal) factor and a
#' vertical (latitudinal) factor may be numerically equal but will result in
#' quite different expansions in terms of geographical distance, depending on
#' latitude.
#'
#' @rdname bbox_to_poly
#'
#' @param x An `sf` object
#' @param factor The factor(s) to expand the bbox of `x` by. Can be length 1, 2
#' or 4. If length 1, the factor will be applied to all 4 sides of `x`. If
#' length 2,the first value will be applied to the vertical (top and bottom)
#' sides, the second value to the horizontal (left and right) sides. If length
#' 4, the factors will be applied to the top, right, bottom and left sides in
#' that order (think t,r,b,l ... 'trouble'). Negative values are valid and will
#' result in a contraction of sides as applicable.
#' @param crs the CRS of the returned object. Inherits the CRS of `x` by
#' default. Use `NA` if no CRS is desired.
#'
#' @returns a rectangular `sf` polygon
#' @export
expand_sf_bbox <- function(x, factor, crs = NULL) {

  sf_types <- c("sf", "sfc", "sfg", "stars")

  if (is.null(crs)) {
    if (length(intersect(class(x), sf_types)) > 0) {
      crs <- sf::st_crs(x)
    } else {
      crs <- NA
    }
  }

  orig_bbox <- sf::st_bbox(x)

  assert_that(purrr::is_double(orig_bbox))
  assert_that(length(orig_bbox) == 4)

  usethis::ui_info("The bounding box of the object you supplied was:")
  usethis::ui_code(orig_bbox)


  if (length(factor) == 1) {
    factor <- rep(factor, 4)
  } else if (length(factor) == 2) {
    factor <- rep(factor, 2)
  }

  assert_that(
    length(factor) == 4,
    msg = "The expansion factor you supplied is not of length 1, 2 or 4"
  )

  orig__width <- orig_bbox[["xmax"]] - orig_bbox[["xmin"]]
  orig_height <- orig_bbox[["ymax"]] - orig_bbox[["ymin"]]


  b <- c(
    xmin = orig_bbox[["xmin"]] - (orig__width * factor[[4]]), # left
    ymin = orig_bbox[["ymin"]] - (orig_height * factor[[3]]), # bottom
    xmax = orig_bbox[["xmax"]] + (orig__width * factor[[2]]), # right
    ymax = orig_bbox[["ymax"]] + (orig_height * factor[[1]])  # top
  )

  assert_that(purrr::is_double(b))
  bbox_to_poly(b)
}


#' Create a square box from a polygon
#'
#' Makes a bbox (rectangle) that just surrounds the polygon and then
#' expands the shorter sides of the bbox to make it a square
#'
#' @rdname bbox_to_poly
#'
#' @param x A polygon
#' @param output Whether to return just a bbox, or an `sf` polygon
#'
#' @returns a bbox (named vector) or an `sf` (sfc) polygon
#' @examples
#'
#' # oxford_bbox <- osmdata::create_bbox("Oxford", 27700)
#' oxford_bbox <- c(
#'   xmin = 448100.3,
#'   ymin = 201646.6,
#'   xmax = 457014.2,
#'   ymax = 211088.8)
#' fit_square(oxford_bbox)
#'
#' @export
fit_square <- function(x, output = c("bbox", "polygon")) {

  output <- match.arg(output)

  bb <- sf::st_bbox(x)
  ht <- (bb[["ymax"]] - bb[["ymin"]])
  wd <- (bb[["xmax"]] - bb[["xmin"]])

  half_dif <- abs(ht - wd) / 2

  if (ht > wd) {
    # "portrait"
    bb2 <- c(
      xmin = bb[["xmin"]] - half_dif,
      ymin = bb[["ymin"]],
      xmax = bb[["xmax"]] + half_dif,
      ymax = bb[["ymax"]])
  } else {
    # "landscape" (or already square!)
    bb2 <- c(
      xmin = bb[["xmin"]],
      ymin = bb[["ymin"]] - half_dif,
      xmax = bb[["xmax"]],
      ymax = bb[["ymax"]] + half_dif)
  }

  if (output == "bbox") bb2
  else if (output == "polygon") bbox_to_poly(bb2, crs = sf::st_crs(bb))
}
