#' Convert Google Maps lat-lon data to geospatial `sf` points
#'
#' @description
#' If you right-click on a location in Google Maps, it gives you the option to
#' copy the latitude and longitude. This is really useful. But it provides this
#' latitude-first (latlon), which will be inaccurate if you try to feed it
#' directly to `sf::st_point()`, which expects longitude first (lonlat).
#'
#' This function allows you to create a list of these coordinate pairs as pasted
#' from GMaps, and convert it into a single, plottable, geospatial `sf` (sfc)
#' multipoint object.
#'
#' @param lst A list of coordinates as pasted from Google Maps right-click.
#'  Looks something like `list(c(43.2, 44.9), c(43.4, 45.2))`.
#'
#' @returns an `sf` object (sfc) with CRS 4326
#' @export
gmaps_to_sf <- function(lst) {

  lst |>
    # reverse lat & lon from what Google Maps gives you
    purrr::map(rev) |>
    # turn each one into a point
    purrr::map(sf::st_point) |>
    # turn list into an sf object
    sf::st_sfc(crs = 4326)
}
