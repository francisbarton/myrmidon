#' Insert magrittr assignment pipe
#'
#' RStudio API tweak to insert the reflexive assignment pipe
#' from magrittr at the cursor location when editing R code.
#' Designed to be assigned a keyboard shortcut.
#' - I use Ctrl+Alt+,
#'
#' @importFrom rstudioapi insertText
#'
#' @return None
#'
#' dont#@export
#'
insertAssPipe <- function() {
  rstudioapi::insertText(" %<>% ")
}
