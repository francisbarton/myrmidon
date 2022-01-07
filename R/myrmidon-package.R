#' \code{myrmidon} package
#'
#' Personal Functions Toolbox
#'
#' @docType package
#' @name myrmidon
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom magrittr %>%
#' @export
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", ".data"))

#' @importFrom dplyr across c_across all_of any_of desc
#' @importFrom dplyr starts_with ends_with contains matches
#' @importFrom dplyr everything last_col num_range
#' @importFrom usethis ui_info ui_stop ui_oops ui_nope ui_code
#' @importFrom rlang `:=`
NULL
