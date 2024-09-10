#' @export
auto_reprex <- function() {
  rstudioapi::getSourceEditorContext() |>
    purrr::pluck("contents") |>
    paste0("\n") |>
    reprex::reprex(input = _)
}
