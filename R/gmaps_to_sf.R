#' Convert Google Maps lat-lon Data to a Geospatial sf object
#'
#' If you right-click on a location in Google Maps, it gives you the option to copy the latitude and longitude. This is really useful. But it does provide the latitude figure first, which will be inaccurate if you try to feed it straight to `sf::st_point()`, which expects longitude first.
#' This function allows you to create a list of these coordinate pairs as pasted from GMaps, and convert it into a single, plottable, geospatial `sf` (sfc) object.
#'
#' @param lst a list of coordinates as pasted from Google Maps right-click ($lat, $lon)
#' @param crs the desired CRS of the output
#'
#' @return an `sf` (sfc) object (collection)
#' @export
gmaps_to_sf <- function(lst, crs) {

  lst %>%
    # reverse lat & lon from what Google Maps gives you
    purrr::map(rev) %>%
    # turn each one into a point
    purrr::map(sf::st_point) %>%
    # turn list into an sf object
    sf::st_sfc(crs = 4326) %>%
    sf::st_transform(crs = crs)

}
