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
#' @importFrom dplyr across c_across desc join_by
#' @importFrom magrittr %>%
#' @importFrom rlang `:=`
#' @importFrom tidyselect all_of any_of contains ends_with everything
#' @importFrom tidyselect last_col matches num_range starts_with
#' @importFrom usethis ui_info ui_stop ui_oops ui_nope ui_code

# Borrow `view()` from `{tibble}`
#' @export
view <- tibble::view

NULL
