#' Custom HTML5 knit function
#'
#' @param input input document to be knitted
#' @param ... other arguments to pass to rmarkdown::output_format()
#' @export
knit_to_html5 <- function(input, ...) {
  # https://github.com/rstudio/rmarkdown/issues/1776#issuecomment-590095305
  rmarkdown::render(
    input,
    output_format = rmarkdown::output_format(
      knitr = NULL,
      pandoc = list(to = "html5"),
      ...
    )
  )
}
