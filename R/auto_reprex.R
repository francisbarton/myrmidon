#' @export
auto_reprex <- function() {
    reprex::reprex(
      input = paste0(
        rstudioapi::getSourceEditorContext() |>
          purrr::pluck("contents"), "\n")
    )
}
