#' `myrmidon`
#'
#' "My R" Kitchen Drawer
#'
#' Fran's personal useful tools, tricks and toys. Some functions I want to have
#' available most times I start up R, as well as my own HTML5 RMarkdown
#' template. Please feel free to use and improve them.
#'
#' @docType package
#' @name myrmidon
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom magrittr %>%
## usethis namespace: end

#' @importFrom rlang `:=`
#' @importFrom dplyr across c_across all_of any_of desc
#' @importFrom dplyr starts_with ends_with contains matches
#' @importFrom dplyr everything last_col num_range
#' @importFrom usethis ui_info ui_stop ui_oops ui_nope ui_code

# Borrow `view()` from `{tibble}`
#' @export
view <- tibble::view

NULL
