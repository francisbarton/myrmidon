#' Create an expanded sf object
#'
#' @param x A rectangular sf object, or any object with a bbox
#' @param factor The factor(s) to expand the bbox by. Can be length 1, 2 or 4.
#' @param crs the CRS of the returned object. The same as x by default. Use NA if no CRS is desired.
#'
#' @return a rectangular sf object
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

  testthat::expect_true(purrr::is_double(orig_bbox))
  testthat::expect_length(orig_bbox, 4)

  usethis::ui_info("The bounding box of the object you supplied was:")
  usethis::ui_code(orig_bbox)


  if (length(factor) == 1) {
    factor <- rep(factor, 4)
  } else if (length(factor) == 2) {
    factor <- rep(factor, 2)
  }

  testthat::expect_length(factor, 4)
  if (!length(factor) == 4) {
    usethis::ui_stop("The factor you supplied is not of length 1, 2 or 4")
  }

  orig__width <- orig_bbox[["xmax"]] - orig_bbox[["xmin"]]
  orig_height <- orig_bbox[["ymax"]] - orig_bbox[["ymin"]]


  b <- c(
    xmin = orig_bbox[["xmin"]] - (orig__width * factor[[1]]),
    ymin = orig_bbox[["ymin"]] - (orig_height * factor[[2]]),
    xmax = orig_bbox[["xmax"]] + (orig__width * factor[[3]]),
    ymax = orig_bbox[["ymax"]] + (orig_height * factor[[4]])
  )

  testthat::expect_true(purrr::is_double(b))


  sf::st_linestring(
    matrix(
      c(
        b[["xmin"]], b[["ymin"]], # bottom left corner
        b[["xmax"]], b[["ymin"]], # bottom right corner
        b[["xmax"]], b[["ymax"]], # top right corner
        b[["xmin"]], b[["ymax"]], # top left corner
        b[["xmin"]], b[["ymin"]]  # bottom left corner
        ),
        ncol = 2, byrow = TRUE)
  ) %>%
    sf::st_polygonize() %>%
    sf::st_sfc(crs = crs)
}
