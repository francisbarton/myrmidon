#' Sort a data frame using a secondary key, within primary groupings
#'
#' Also works with `sf` tibbles – use `order_along.sf` instead.
#' @param df data frame
#' @param order_along variable you want to order
#' @param sort_by variable(s) you want to order by
#' @param desc whether to sort sort_by in descending order
#' @export
order_along <- function(df, order_along, sort_by, desc = FALSE) {

  # https://adv-r.hadley.nz/quasiquotation.html?q=ensym#capturing-symbols 🤓
  along <- rlang::ensym(order_along)

  cols <- colnames(df)

  # Not yet tested to see if it can work when length(sort_by) > 1
  # That might be unnecessarily overcomplicating things...
  if (desc) {
    df <- df %>%
      dplyr::arrange(desc({{ sort_by }}))
  } else {
    df <- df %>%
      dplyr::arrange({{ sort_by }})
  }

  df %>%
    # create list of `order_along` variable, in order of appearance 😊
    dplyr::select({{ order_along }}) %>%
    dplyr::distinct() %>%

    # now ordered along `order_along` and sorted by `sort_by` 😄
    #
    # not _necessary_ to stipulate `by`, but it avoids the join message🤫.
    # rlang::as_name() reverses the rlang::ensym() above
    # (I previously used as.character() here, but as_name is preferable)
    dplyr::left_join(df, by = rlang::as_name(along)) %>%

    # quick way to restore the original column order 😙
    dplyr::select(dplyr::all_of(cols))
}



order_along.sf <- function(sf, order_along, sort_by, desc = FALSE) {

  # https://adv-r.hadley.nz/quasiquotation.html?q=ensym#capturing-symbols 🤓
  along <- rlang::ensym(order_along)

  # gotta split🍌, and work without geometry on one fork🍴, otherwise
  # the left_join later on will fail (can't `dplyr::*-join` two `sf` tbls)
  sf2 <- sf %>%
      sf::st_drop_geometry()

  cols <- colnames(sf2)

  if (desc) {
    sf2 <- sf2 %>%
      dplyr::arrange(desc({{ sort_by }}))
  } else {
    sf2 <- sf2 %>%
      dplyr::arrange({{ sort_by }})
  }

  sf2 %>%
    # create list of `order_along` variable, in order of appearance 😊
    dplyr::select({{ order_along }}) %>%
    dplyr::distinct() %>%

    # now ordered along `order_along` and sorted by `sort_by` 😄
    # not _necessary_ to stipulate `by`, but it avoids the join message 🤫.
    dplyr::left_join(sf2, by = rlang::as_name(along)) %>%

    # quick way to restore the original column order 😙
    dplyr::select(dplyr::all_of(cols)) %>%

    # `right_join()` saves the day by retrieving and joining to the original
    # geometry without losing our carefully crafted sorting and ordering 😌
    dplyr::right_join(sf)
}
